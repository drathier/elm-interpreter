module Talk.KELet exposing (Env, Expr(..), Name, Op(..), binop, interpret)

import Dict exposing (Dict)


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
