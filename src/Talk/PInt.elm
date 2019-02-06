module Talk.PInt exposing (Expr(..), interpret)


type Expr
    = VInt Int


interpret : Expr -> Expr
interpret expr =
    case expr of
        VInt v ->
            VInt v
