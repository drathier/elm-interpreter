module Interpreter.Debug exposing (inTrace, outTrace, printEnv, showExpr)

import Dict exposing (Dict)
import Interpreter.Types exposing (..)



-- Mutual recursion (ELetRec)


inTrace a =
    let
        _ =
            Debug.log ("> " ++ showExpr a) ()
    in
    a


outTrace a =
    let
        _ =
            Debug.log ("< " ++ showExpr a) ()
    in
    a


showExpr e =
    case e of
        --ELambda _ pats body ->
        --    "ELambda <env> (" ++ Debug.toString pats ++ ") (" ++ Debug.toString body ++ ")"
        ELambda env pats body ->
            "ELambda (>>\n" ++ printEnv env ++ "\n>>) (" ++ Debug.toString pats ++ ") (" ++ Debug.toString body ++ ")"

        _ ->
            Debug.toString e


printEnv env =
    env |> Dict.toList |> List.map (\( k, v ) -> "VAR: " ++ k ++ " => " ++ Debug.toString v) |> String.join "\n"
