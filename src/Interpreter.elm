module Interpreter exposing (interpret, patternMatch)

import Dict exposing (Dict)
import Interpreter.Debug as IDebug
import Interpreter.Helpers exposing (..)
import Interpreter.Types exposing (..)
import Maybe.Extra
import Set exposing (Set)


patternMatch : Env -> Pattern -> Expr -> Maybe Env
patternMatch env pat expr =
    case ( pat, expr ) of
        ( PAnything, _ ) ->
            Just env

        ( PVar pvarName, e ) ->
            Just (Dict.insert pvarName e env)

        ( PECtor name1 pats, ECtor name2 exprs ) ->
            if name1 /= name2 then
                Nothing

            else
                List.map2 (patternMatch env) pats exprs
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl Dict.union env)

        ( PInt a, EInt b ) ->
            if a == b then
                Just env

            else
                Nothing

        ( PERecord keys, ERecord body ) ->
            let
                liftSecondMaybe ( a, b ) =
                    case b of
                        Just v ->
                            Just ( a, v )

                        Nothing ->
                            Nothing
            in
            case
                Set.toList keys
                    |> List.map (\k -> ( k, Dict.get k body ))
                    |> List.map liftSecondMaybe
                    |> Maybe.Extra.combine
            of
                Just v ->
                    Just <| Dict.union (Dict.fromList v) env

                Nothing ->
                    Nothing

        _ ->
            Nothing


interpret : Dict Name Expr -> Expr -> Expr
interpret environment expr =
    case expr of
        EInt v ->
            EInt v

        EBinOp e1 op e2 ->
            binop (interpret environment e1) op (interpret environment e2)

        EVariable var ->
            case Dict.get var environment of
                Just varExpr ->
                    interpret environment varExpr

                Nothing ->
                    Debug.todo ("unknown variable" ++ Debug.toString var ++ " in env \n" ++ IDebug.printEnv environment)

        ELet bindings body ->
            let
                newEnv =
                    -- shadowing is disallowed in elm, so this union should never collide
                    Dict.union bindings environment

                envWithELetBindings =
                    Dict.map (always (interpret newEnv)) bindings

                inlineEFunctions e =
                    case e of
                        EFunction argPat lambdaBody ->
                            -- if it's a function, we want it to reuse the context it was found in, so it can call the other mutrec fns
                            ELambda newEnv argPat lambdaBody

                        _ ->
                            e
            in
            interpret (Dict.map (always inlineEFunctions) envWithELetBindings) body

        ECtor name args ->
            ECtor name args

        EFunction argPat lambdaBody ->
            ELambda environment argPat lambdaBody

        ELambda lambdaEnv argPat lambdaBody ->
            ELambda lambdaEnv argPat lambdaBody

        EApply fn argument ->
            case interpret environment fn of
                ELambda lambdaEnv argPat lambdaBody ->
                    let
                        newEnv =
                            unwrapMaybe <| patternMatch lambdaEnv argPat (interpret environment argument)
                    in
                    interpret newEnv lambdaBody

                ECtor name args ->
                    ECtor name (args ++ [ interpret environment argument ])

                _ ->
                    Debug.todo "type mismatch; the type checker should've forbidden this"

        ECase e [] ->
            Debug.todo ("exhaustiveness failure; didn't find a matching pattern for expr " ++ Debug.toString e)

        ECase e (( pat, caseBody ) :: otherECases) ->
            case patternMatch environment pat (interpret environment e) of
                Just newEnv ->
                    interpret newEnv caseBody

                Nothing ->
                    interpret environment (ECase e otherECases)

        ERecord body ->
            ERecord body

        ENegate e ->
            case interpret environment e of
                EInt i ->
                    EInt -i

                nonEIntExpr ->
                    ENegate nonEIntExpr
