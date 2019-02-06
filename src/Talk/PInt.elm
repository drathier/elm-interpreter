module Talk.PInt exposing (Expr(..), interpret)


type Expr
    = EInt Int


interpret : Expr -> Expr
interpret expr =
    case expr of
        EInt v ->
            EInt v
