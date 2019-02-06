module Interpreter.Helpers exposing (binop, unwrapMaybe)

import Interpreter.Types exposing (..)


binop : Expr -> Op -> Expr -> Expr
binop first op second =
    case ( first, op, second ) of
        ( VInt a, Add, VInt b ) -> VInt (a + b)
        ( VInt a, Sub, VInt b ) -> VInt (a - b)
        ( VInt a, Mul, VInt b ) -> VInt (a * b)
        _ ->
            Debug.todo <| "type mismatch; expected two `VInt` Expr, got " ++ Debug.toString ( first, op, second )




unwrapMaybe : Maybe a -> a
unwrapMaybe m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.todo "expected a Just value, got Nothing"
