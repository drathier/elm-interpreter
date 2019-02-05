module Interpreter.Helpers exposing (apply2Ints, sequenceMaybe, unwrapMaybe)

import Interpreter.Types exposing (..)


apply2Ints : Expr -> (Int -> Int -> Int) -> Expr -> Expr
apply2Ints a op b =
    case ( a, b ) of
        ( Value a1, Value b1 ) ->
            Value (op a1 b1)

        _ ->
            Debug.todo <| "type mismatch; expected two `Value` Expr, got " ++ Debug.toString ( a, op, b )


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe maybes =
    let
        seqM m res =
            case m of
                [] ->
                    Just res

                Nothing :: _ ->
                    Nothing

                (Just v) :: rest ->
                    seqM rest (v :: res)
    in
    seqM maybes []


unwrapMaybe : Maybe a -> a
unwrapMaybe m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.todo "expected a Just value, got Nothing"
