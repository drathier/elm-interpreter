module Talk.EERecord exposing (Env, Expr(..), Name, Op(..), Pattern(..), binop, interpret, patternMatch, unwrapMaybe)

import Dict exposing (Dict)
import Maybe.Extra
import Set exposing (Set)



-- Maybe.Extra.combine : List (Maybe a) -> Maybe (List a)


type alias Env =
    Dict Name Expr


type Op
    = OpAdd
    | OpMul
    | OpSub


type alias Name =
    String


type Expr
    = EInt Int
    | EBinOp Expr Op Expr
    | EVariable Name
    | ELet ( Name, Expr ) Expr
    | EFunction Pattern Expr
    | EApply Expr Expr -- fn arg
    | ECtor Name (List Expr)
    | ECase Expr (List ( Pattern, Expr ))
    | ERecord (Dict Name Expr)


type Pattern
    = PAnything
    | PVar Name
    | PInt Int
    | PECtor Name (List Pattern)
    | PERecord (Set Name)


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
                    Debug.todo ("unknown variable" ++ Debug.toString var ++ " in env \n" ++ Debug.toString environment)

        ELet ( varName, varBody ) body ->
            let
                envWithELetBindings =
                    Dict.insert varName varBody environment
            in
            interpret envWithELetBindings body

        ECtor name args ->
            ECtor name args

        EFunction argPat lambdaBody ->
            EFunction argPat lambdaBody

        EApply fn argument ->
            case interpret environment fn of
                EFunction argPat lambdaBody ->
                    let
                        newEnv =
                            unwrapMaybe <| patternMatch environment argPat (interpret environment argument)
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


binop : Expr -> Op -> Expr -> Expr
binop first op second =
    case ( first, op, second ) of
        ( EInt a, OpAdd, EInt b ) ->
            EInt (a + b)

        ( EInt a, OpSub, EInt b ) ->
            EInt (a - b)

        ( EInt a, OpMul, EInt b ) ->
            EInt (a * b)

        _ ->
            Debug.todo <| "type mismatch; expected two `EInt` Expr, got " ++ Debug.toString ( first, op, second )


unwrapMaybe : Maybe a -> a
unwrapMaybe m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.todo "expected a Just value, got Nothing"
