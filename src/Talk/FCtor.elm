module Talk.FCtor exposing (Env, Expr(..), Name, Op(..), Pattern(..), binop, interpret, patternMatch, unwrapMaybe)

import Dict exposing (Dict)
import Maybe.Extra



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
    | Apply Expr Expr -- fn arg
    | Ctor Name (List Expr)
    | Case Expr (List ( Pattern, Expr ))


type Pattern
    = PAnything
    | PVar Name
    | PInt Int
    | PCtor Name (List Pattern)


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
                    Dict.insert varName varBody environment
            in
            interpret envWithLetBindings body

        Ctor name args ->
            Ctor name args

        Function argPat lambdaBody ->
            Function argPat lambdaBody

        Apply fn argument ->
            case interpret environment fn of
                Function argPat lambdaBody ->
                    let
                        newEnv =
                            unwrapMaybe <| patternMatch environment argPat (interpret environment argument)
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
