module TestInterpreter exposing (basics, closures, letExpressions, mutuallyRecursiveFunctions, nestedFunctions, patterns, recursiveFunctions)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Interpreter exposing (..)
import Interpreter.Debug as IDebug
import Interpreter.Helpers exposing (..)
import Interpreter.Types exposing (..)
import Test exposing (..)


basics : Test
basics =
    describe "simple values"
        [ fuzz int "value returns self without crashing" <|
            \i -> interpret Dict.empty (Value i) |> Expect.equal (Value i)
        , fuzz int "variable is dereferenced" <|
            \i -> interpret (Dict.singleton "x" (Value i)) (Variable "x") |> Expect.equal (Value i)
        , fuzz2 int int "simple Add expression" <|
            \a b -> interpret Dict.empty (BinOp (Value a) Add (Value b)) |> Expect.equal (Value (a + b))
        , fuzz2 int int "simple Mul expression" <|
            \a b -> interpret Dict.empty (BinOp (Value a) Mul (Value b)) |> Expect.equal (Value (a * b))
        , fuzz3 int int string "function application" <|
            \a b varname ->
                Apply
                    (Function (PVar varname) (BinOp (Variable varname) Mul (Value b)))
                    (Value a)
                    |> interpret Dict.empty
                    |> Expect.equal (Value (a * b))
        , fuzz2 int string "case on Maybe" <|
            \i varName ->
                let
                    nothing =
                        Ctor "Nothing" []

                    just v =
                        Ctor "Just" [ v ]

                    caseExpr =
                        Case (just (Value i)) [ ( PCtor "Nothing" [], Value 42 ), ( PCtor "Just" [ PVar varName ], Variable varName ) ]
                in
                interpret Dict.empty caseExpr |> Expect.equal (Value i)
        ]


patterns : Test
patterns =
    describe "pattern matching"
        [ test "wildcard pattern accepts any type" <|
            \() ->
                let
                    appl arg =
                        Apply (Function PAnything (Value 3)) arg
                in
                interpret Dict.empty (appl (Value 42))
                    |> Expect.equal
                        (interpret Dict.empty (appl (Ctor "ctorName" [])))
        , test "int pattern accepts ints" <|
            \() ->
                interpret Dict.empty (Apply (Function (PInt 5) (Value 3)) (Value 5))
                    |> Expect.equal (Value 3)
        , fuzz3 int int string "variable pattern accepts anything, and binds that variable" <|
            \a b varname ->
                Apply
                    (Function (PVar varname) (BinOp (Variable varname) Mul (Value b)))
                    (Value a)
                    |> interpret Dict.empty
                    |> Expect.equal (Value (a * b))
        , fuzz3 int int string "ctor pattern binds the ctor arguments" <|
            \a b varname ->
                Apply
                    (Function (PVar varname) (BinOp (Variable varname) Mul (Value b)))
                    (Value a)
                    |> interpret Dict.empty
                    |> Expect.equal (Value (a * b))
        ]


letExpressions =
    describe "let-expressions"
        [ test "empty let-expressions (which are illegal Elm) are equal to their bodies" <|
            \() ->
                let
                    fn =
                        Lambda (Dict.singleton "testvariable" (Value 4711)) PAnything (Value 11147)

                    env =
                        Dict.singleton "testenv" (Ctor "Nada" [])
                in
                interpret env (Let Dict.empty fn) |> Expect.equal (interpret env fn)
        , fuzz string "let-expressions can introduce variables" <|
            \varname ->
                let
                    env =
                        Dict.singleton "testenv" (Ctor "Nada" [])
                in
                interpret env (Let (Dict.singleton varname (Value 42)) (Variable varname)) |> Expect.equal (Value 42)
        , test "let-expressions can reference each other non-recursively" <|
            \_ ->
                let
                    env =
                        Dict.singleton "testenv" (Ctor "Nada" [])

                    letBody =
                        Dict.fromList
                            [ ( "a", Value 3 )
                            , ( "c", Variable "a" )
                            , ( "b", Variable "c" )
                            ]
                in
                interpret env (Let letBody (BinOp (Variable "c") Mul (Variable "b"))) |> Expect.equal (Value (3 * 3))
        ]


nestedFunctions : Test
nestedFunctions =
    describe "nested functions"
        [ test "always fn" <|
            \() ->
                let
                    always =
                        Function (PVar "a") (Function (PVar "b") (Variable "a"))
                in
                interpret Dict.empty (Apply (Apply always (Value 3)) (Value 4)) |> Expect.equal (Value 3)
        ]


recursiveFunctions : Test
recursiveFunctions =
    describe "recursive functions"
        [ fuzz (intRange 0 10) "factorial" <|
            \i ->
                let
                    fact a =
                        if a == 0 then
                            1

                        else
                            a * fact (a - 1)

                    letBody =
                        Dict.fromList
                            [ ( "fact"
                              , Function (PVar "x")
                                    (Case (Variable "x")
                                        [ ( PInt 0, Value 1 )
                                        , ( PAnything, BinOp (Variable "x") Mul (Apply (Variable "fact") (BinOp (Variable "x") Sub (Value 1))) )
                                        ]
                                    )
                              )
                            ]
                in
                interpret Dict.empty (Let letBody (Apply (Variable "fact") (Value i))) |> Expect.equal (Value (fact i))
        , fuzz2 (Fuzz.intRange 0 3) (Fuzz.intRange 0 4) "Ackermann" <|
            \x y ->
                let
                    ackermann m n =
                        if m == 0 then
                            n + 1

                        else if n == 0 then
                            ackermann (m - 1) 1

                        else
                            ackermann (m - 1) (ackermann m (n - 1))

                    rec a b =
                        Apply (Apply (Variable "ackermann") a) b

                    dec a =
                        BinOp a Sub (Value 1)

                    letBody =
                        Dict.fromList
                            [ ( "ackermann"
                              , Function (PVar "m") <|
                                    Function (PVar "n") <|
                                        Case (Variable "m")
                                            [ ( PInt 0, BinOp (Variable "n") Add (Value 1) )
                                            , ( PAnything
                                              , Case (Variable "n")
                                                    [ ( PInt 0, rec (dec (Variable "m")) (Value 1) )
                                                    , ( PAnything, rec (dec (Variable "m")) (rec (Variable "m") (dec (Variable "n"))) )
                                                    ]
                                              )
                                            ]
                              )
                            ]
                in
                interpret Dict.empty (Let letBody (Apply (Apply (Variable "ackermann") (Value x)) (Value y))) |> Expect.equal (Value (ackermann x y))
        ]


mutuallyRecursiveFunctions : Test
mutuallyRecursiveFunctions =
    describe "mutually recursive functions"
        [ fuzz (Fuzz.intRange 0 30) "mutrec" <|
            \i ->
                let
                    isOdd v =
                        case modBy 2 v == 1 of
                            True ->
                                Ctor "True" []

                            False ->
                                Ctor "False" []

                    letBody =
                        Dict.fromList
                            [ ( "not"
                              , Function (PVar "xnt")
                                    (Case (Variable "xnt")
                                        [ ( PCtor "True" [], Ctor "False" [] )
                                        , ( PCtor "False" [], Ctor "True" [] )
                                        ]
                                    )
                              )
                            , ( "even"
                              , Function (PVar "xev")
                                    (Case (Variable "xev")
                                        [ ( PInt 0, Ctor "True" [] )
                                        , ( PAnything, Apply (Variable "odd") (BinOp (Variable "xev") Sub (Value 1)) )
                                        ]
                                    )
                              )
                            , ( "odd"
                              , Function (PVar "xdd")
                                    (Case (Variable "xdd")
                                        [ ( PInt 0, Ctor "False" [] )
                                        , ( PAnything, Apply (Variable "even") (BinOp (Variable "xdd") Sub (Value 1)) )
                                        ]
                                    )
                              )
                            ]
                in
                interpret Dict.empty (Let letBody (Apply (Variable "odd") (Value i))) |> Expect.equal (isOdd i)
        , fuzz (intRange 0 30) "mutrec fact" <|
            \i ->
                let
                    fact a =
                        if a == 0 then
                            1

                        else
                            a * fact (a - 1)

                    letBody =
                        Dict.fromList
                            [ ( "fact"
                              , Function (PVar "x")
                                    (Case (Variable "x")
                                        [ ( PInt 0, Value 1 )
                                        , ( PAnything, BinOp (Variable "x") Mul (Apply (Variable "fact") (BinOp (Apply (Variable "identity") (Variable "x")) Sub (Value 1))) )
                                        ]
                                    )
                              )
                            , ( "identity", Function (PVar "x") (Variable "x") )
                            ]
                in
                interpret Dict.empty (Let letBody (Apply (Variable "fact") (Value i))) |> Expect.equal (Value (fact i))
        ]


closures =
    describe "closures"
        [ test "closure" <|
            \() ->
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , Let
                                    (Dict.fromList
                                        [ ( "innerValue", Value 3 )
                                        , ( "retFn", Function (PInt 5) (Variable "innerValue") )
                                        ]
                                    )
                                    (Variable "retFn")
                              )
                            , ( "innerValue", Value 9 )
                            ]
                in
                interpret (Dict.singleton "innerValue" (Value 1)) (Let letBody (Apply (Variable "closure") (Value 5))) |> Expect.equal (Value 3)
        , test "closure2" <|
            \() ->
                -- test uses name shadowing to figure out where a closure fails, if it fails, even though elm doesn't allow shadowing
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , Let
                                    (Dict.fromList
                                        [ ( "middle"
                                          , Let
                                                (Dict.fromList
                                                    [ ( "innerLet"
                                                      , Let
                                                            (Dict.fromList
                                                                [ ( "retFn", Function (PInt 5) (Variable "innerValue") )
                                                                ]
                                                            )
                                                            (Variable "retFn")
                                                      )
                                                    ]
                                                )
                                                (Variable "innerLet")
                                          )
                                        , ( "innerValue", Value 3 )
                                        ]
                                    )
                                    (Variable "middle")
                              )
                            , ( "innerValue", Value 9 )
                            ]
                in
                interpret (Dict.singleton "innerValue" (Value 1)) (Let letBody (Apply (Variable "closure") (Value 5))) |> Expect.equal (Value 3)
        , test "faked closure" <|
            \() ->
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , Let
                                    (Dict.fromList
                                        [ ( "innerValue", Value 3 )
                                        , ( "retFn", Lambda (Dict.singleton "innerValue" (Value 3)) (PInt 5) (Variable "innerValue") )
                                        ]
                                    )
                                    (Variable "retFn")
                              )
                            , ( "innerValue", Value 9 )
                            ]
                in
                interpret (Dict.singleton "innerValue" (Value 1)) (Let letBody (Apply (Variable "closure") (Value 5))) |> Expect.equal (Value 3)
        ]
