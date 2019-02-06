module Talk.MBinopOpSub exposing (Expr(..), Op(..), binop, interpret)


type Op
    = OpAdd
    | OpMul
    | OpSub


type Expr
    = EInt Int
    | EBinOp Expr Op Expr


interpret : Expr -> Expr
interpret expr =
    case expr of
        EInt v ->
            EInt v

        EBinOp e1 op e2 ->
            binop (interpret e1) op (interpret e2)


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
