module Talk.LVariable exposing (Env, Expr(..), Name, Op(..), binop, interpret)

import Dict exposing (Dict)


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
