module Talk.BRec exposing (Env, Expr(..), Name, Op(..), Pattern(..), binop, interpret, patternMatch, unwrapMaybe)

import Dict exposing (Dict)
import Maybe.Extra
import Set exposing (Set)



-- Maybe.Extra.combine : List (Maybe a) -> Maybe (List a)


type alias Env =
    Dict Name Expr


type Op
    = Add
    | Mul
    | Sub


type alias Name =
    String


type Expr
    = VInt Int
    | BinOp Expr Op Expr
    | Variable Name
    | Let ( Name, Expr ) Expr
    | Function Pattern Expr
    | Lambda Env Pattern Expr
    | Apply Expr Expr -- fn arg
    | Ctor Name (List Expr)
    | Case Expr (List ( Pattern, Expr ))
    | Record (Dict Name Expr)
    | Negate Expr


type Pattern
    = PAnything
    | PVar Name
    | PInt Int
    | PCtor Name (List Pattern)
    | PRecord (Set Name)


interpret : Dict Name Expr -> Expr -> Expr
interpret environment expr =
    case expr of
        VInt v ->
            VInt v

        BinOp e1 op e2 ->
            binop (interpret environment e1) op (interpret environment e2)

        Variable var ->
            case Dict.get var environment of
                Just varExpr ->
                    interpret environment varExpr

                Nothing ->
                    Debug.todo ("unknown variable" ++ Debug.toString var ++ " in env \n" ++ Debug.toString environment)

        Let ( varName, varBody ) body ->
            let
                envWithLetBindings =
                    Dict.insert varName (interpret environment varBody) environment

                inlineFunctions e =
                    case e of
                        Function argPat lambdaBody ->
                            -- if it's a function, we want it to reuse the context it was found in, so it can call the other mutrec fns
                            Lambda envWithLetBindings argPat lambdaBody

                        _ ->
                            e
            in
            interpret (Dict.map (\_ -> inlineFunctions) envWithLetBindings) body

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
            Debug.todo ("exhaustiveness failure; didn't find a matching pattern for expr " ++ Debug.toString e)

        Case e (( pat, caseBody ) :: otherCases) ->
            case patternMatch environment pat (interpret environment e) of
                Just newEnv ->
                    interpret newEnv caseBody

                Nothing ->
                    interpret environment (Case e otherCases)

        Record body ->
            Record body

        Negate e ->
            case interpret environment e of
                VInt i ->
                    VInt -i

                nonValueExpr ->
                    Negate nonValueExpr


patternMatch : Env -> Pattern -> Expr -> Maybe Env
patternMatch env pat expr =
    case ( pat, expr ) of
        ( PAnything, _ ) ->
            Just env

        ( PVar pvarName, e ) ->
            Just (Dict.insert pvarName e env)

        ( PCtor name1 pats, Ctor name2 exprs ) ->
            if name1 /= name2 then
                Nothing

            else
                List.map2 (patternMatch env) pats exprs
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl Dict.union env)

        ( PInt a, VInt b ) ->
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
        ( VInt a, Add, VInt b ) ->
            VInt (a + b)

        ( VInt a, Sub, VInt b ) ->
            VInt (a - b)

        ( VInt a, Mul, VInt b ) ->
            VInt (a * b)

        _ ->
            Debug.todo <| "type mismatch; expected two `VInt` Expr, got " ++ Debug.toString ( first, op, second )


unwrapMaybe : Maybe a -> a
unwrapMaybe m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.todo "expected a Just value, got Nothing"
