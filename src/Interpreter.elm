module Interpreter exposing (interpret, patternMatch)

import Dict exposing (Dict)
import Interpreter.Debug as IDebug
import Interpreter.Helpers exposing (..)
import Interpreter.Types exposing (..)
import Set exposing (Set)


patternMatch : Env -> Pattern -> Expr -> Maybe Env
patternMatch env pat expr =
    case ( pat, expr ) of
        ( PAnything, e ) ->
            Just env

        ( PVar pvarName, e ) ->
            Just (Dict.insert pvarName e env)

        ( PCtor name1 pats, Ctor name2 exprs ) ->
            if name1 /= name2 then
                Nothing

            else
                let
                    matches =
                        List.map2 (patternMatch env) pats exprs
                in
                sequenceMaybe matches |> Maybe.map (List.foldl Dict.union env)

        ( PInt a, Value b ) ->
            if a == b then
                Just env

            else
                Nothing

        ( PRecord keys, Record body ) ->
            let
                liftSecondMaybe ( a, b ) =
                    case b of
                        Just v ->
                            Just ( a, v )

                        Nothing ->
                            Nothing
            in
            case Set.toList keys |> List.map (\k -> ( k, Dict.get k body )) |> List.map liftSecondMaybe |> sequenceMaybe of
                Just v ->
                    Just <| Dict.union (Dict.fromList v) env

                Nothing ->
                    Nothing

        _ ->
            Nothing


interpret : Dict Name Expr -> Expr -> Expr
interpret environment expr =
    case expr of
        Value v ->
            Value v

        BinOp e1 Add e2 ->
            apply2Ints (interpret environment e1) (+) (interpret environment e2)

        BinOp e1 Mul e2 ->
            apply2Ints (interpret environment e1) (*) (interpret environment e2)

        BinOp e1 Sub e2 ->
            apply2Ints (interpret environment e1) (-) (interpret environment e2)

        Variable var ->
            case Dict.get var environment of
                Just varExpr ->
                    interpret environment varExpr

                Nothing ->
                    Debug.todo ("unknown variable" ++ Debug.toString var ++ " in env \n" ++ IDebug.printEnv environment)

        Let bindings body ->
            let
                newEnv =
                    -- shadowing is disallowed in elm, so this union should never collide
                    Dict.union bindings environment

                envWithLetBindings =
                    Dict.map (always (interpret newEnv)) bindings

                inlineFunctions e =
                    case e of
                        Function argPat lambdaBody ->
                            -- if it's a function, we want it to reuse the context it was found in, so it can call the other mutrec fns
                            Lambda newEnv argPat lambdaBody

                        _ ->
                            e
            in
            interpret (Dict.map (always inlineFunctions) envWithLetBindings) body

        Ctor name args ->
            Ctor name args

        Function argPat lambdaBody ->
            Lambda environment argPat lambdaBody

        Lambda lambdaEnv argPat lambdaBody ->
            Lambda lambdaEnv argPat lambdaBody

        Apply fn argument ->
            case interpret environment fn of
                Lambda lambdaEnv argPat lambdaBody ->
                    let
                        newEnv =
                            unwrapMaybe <| patternMatch lambdaEnv argPat (interpret environment argument)
                    in
                    interpret newEnv lambdaBody

                Ctor name args ->
                    Ctor name (args ++ [ interpret environment argument ])

                _ ->
                    Debug.todo "type mismatch; the type checker should've forbidden this"

        Case e [] ->
            Debug.todo "exhaustiveness failure; didn't find a matching pattern"

        Case e (( pat, caseBody ) :: patExprs) ->
            case patternMatch environment pat (interpret environment e) of
                Just newEnv ->
                    interpret newEnv caseBody

                Nothing ->
                    interpret environment (Case e patExprs)

        Record body ->
            Record body

        Negate e ->
            case interpret environment e of
                Value i ->
                    Value -i

                nonValueExpr ->
                    Negate nonValueExpr
