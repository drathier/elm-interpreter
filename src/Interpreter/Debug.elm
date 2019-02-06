module Interpreter.Debug exposing (inTrace, outTrace, printEnv, showExpr)

import Dict exposing (Dict)
import Interpreter.Types exposing (..)



-- Mutual recursion (LetRec)


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
        --Lambda _ pats body ->
        --    "Lambda <env> (" ++ Debug.toString pats ++ ") (" ++ Debug.toString body ++ ")"
        Lambda env pats body ->
            "Lambda (>>\n" ++ printEnv env ++ "\n>>) (" ++ Debug.toString pats ++ ") (" ++ Debug.toString body ++ ")"

        _ ->
            Debug.toString e


printEnv env =
    env |> Dict.toList |> List.map (\( k, v ) -> "VAR: " ++ k ++ " => " ++ Debug.toString v) |> String.join "\n"
