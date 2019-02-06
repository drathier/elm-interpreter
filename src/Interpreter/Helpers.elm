module Interpreter.Helpers exposing (binop, unwrapMaybe)

import Interpreter.Types exposing (..)


binop : Expr -> Op -> Expr -> Expr
binop first op second =
    case ( first, op, second ) of
        ( EInt a, OpAdd, EInt b ) -> EInt (a + b)
        ( EInt a, OpSub, EInt b ) -> EInt (a - b)
        ( EInt a, OpMul, EInt b ) -> EInt (a * b)
        _ ->
            Debug.todo <| "type mismatch; expected two `EInt` Expr, got " ++ Debug.toString ( first, op, second )




unwrapMaybe : Maybe a -> a
unwrapMaybe m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.todo "expected a Just value, got Nothing"
