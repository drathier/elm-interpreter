module Talk.NBinopMul exposing (Expr(..), Op(..), binop, interpret)


type Op
    = Add
    | Mul


type Expr
    = VInt Int
    | BinOp Expr Op Expr


interpret : Expr -> Expr
interpret expr =
    case expr of
        VInt v ->
            VInt v

        BinOp e1 op e2 ->
            binop (interpret e1) op (interpret e2)


binop : Expr -> Op -> Expr -> Expr
binop first op second =
    case ( first, op, second ) of
        ( VInt a, Add, VInt b ) ->
            VInt (a + b)

        ( VInt a, Mul, VInt b ) ->
            VInt (a * b)

        _ ->
            Debug.todo <| "type mismatch; expected two `VInt` Expr, got " ++ Debug.toString ( first, op, second )
