module TestInterpreter exposing (basics, closures, letExpressions, mutuallyRecursiveEFunctions, nestedEFunctions, patterns, recursiveEFunctions)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, string)
import Interpreter exposing (..)
import Interpreter.Types exposing (..)
import Test exposing (..)


basics : Test
basics =
    describe "simple values"
        [ fuzz int "value returns self without crashing" <|
            \i -> interpret Dict.empty (EInt i) |> Expect.equal (EInt i)
        , fuzz int "variable is dereferenced" <|
            \i -> interpret (Dict.singleton "x" (EInt i)) (EVariable "x") |> Expect.equal (EInt i)
        , fuzz2 int int "simple OpAdd expression" <|
            \a b -> interpret Dict.empty (EBinOp (EInt a) OpAdd (EInt b)) |> Expect.equal (EInt (a + b))
        , fuzz2 int int "simple OpMul expression" <|
            \a b -> interpret Dict.empty (EBinOp (EInt a) OpMul (EInt b)) |> Expect.equal (EInt (a * b))
        , fuzz3 int int string "function application" <|
            \a b varname ->
                EApply
                    (EFunction (PVar varname) (EBinOp (EVariable varname) OpMul (EInt b)))
                    (EInt a)
                    |> interpret Dict.empty
                    |> Expect.equal (EInt (a * b))
        , fuzz2 int string "case on Maybe" <|
            \i varName ->
                ECase (ECtor "Just" [ EInt i ])
                    [ ( PECtor "Nothing" [], EInt 42 )
                    , ( PECtor "Just" [ PVar varName ], EVariable varName )
                    ]
                    |> interpret Dict.empty
                    |> Expect.equal (EInt i)
        ]


patterns : Test
patterns =
    describe "pattern matching"
        [ test "wildcard pattern accepts any type" <|
            \() ->
                let
                    appl arg =
                        EApply (EFunction PAnything (EInt 3)) arg
                in
                interpret Dict.empty (appl (EInt 42))
                    |> Expect.equal
                        (interpret Dict.empty (appl (ECtor "ctorName" [])))
        , test "int pattern accepts ints" <|
            \() ->
                interpret Dict.empty (EApply (EFunction (PInt 5) (EInt 3)) (EInt 5))
                    |> Expect.equal (EInt 3)
        , fuzz3 int int string "variable pattern accepts anything, and binds that variable" <|
            \a b varname ->
                EApply
                    (EFunction (PVar varname) (EBinOp (EVariable varname) OpMul (EInt b)))
                    (EInt a)
                    |> interpret Dict.empty
                    |> Expect.equal (EInt (a * b))
        , fuzz3 int int string "ctor pattern binds the ctor arguments" <|
            \a b varname ->
                EApply
                    (EFunction (PVar varname) (EBinOp (EVariable varname) OpMul (EInt b)))
                    (EInt a)
                    |> interpret Dict.empty
                    |> Expect.equal (EInt (a * b))
        ]


letExpressions : Test
letExpressions =
    describe "let-expressions"
        [ test "empty let-expressions (which are illegal Elm) are equal to their bodies" <|
            \() ->
                let
                    fn =
                        ELambda (Dict.singleton "testvariable" (EInt 4711)) PAnything (EInt 11147)

                    env =
                        Dict.singleton "testenv" (ECtor "Nada" [])
                in
                interpret env (ELet Dict.empty fn) |> Expect.equal (interpret env fn)
        , fuzz string "let-expressions can introduce variables" <|
            \varname ->
                let
                    env =
                        Dict.singleton "testenv" (ECtor "Nada" [])
                in
                interpret env (ELet (Dict.singleton varname (EInt 42)) (EVariable varname)) |> Expect.equal (EInt 42)
        , test "let-expressions can reference each other non-recursively" <|
            \_ ->
                let
                    env =
                        Dict.singleton "testenv" (ECtor "Nada" [])

                    letBody =
                        Dict.fromList
                            [ ( "a", EInt 3 )
                            , ( "c", EVariable "a" )
                            , ( "b", EVariable "c" )
                            ]
                in
                interpret env (ELet letBody (EBinOp (EVariable "c") OpMul (EVariable "b"))) |> Expect.equal (EInt (3 * 3))
        ]


nestedEFunctions : Test
nestedEFunctions =
    describe "nested functions"
        [ test "always fn" <|
            \() ->
                let
                    always =
                        EFunction (PVar "a") (EFunction (PVar "b") (EVariable "a"))
                in
                interpret Dict.empty (EApply (EApply always (EInt 3)) (EInt 4)) |> Expect.equal (EInt 3)
        ]


recursiveEFunctions : Test
recursiveEFunctions =
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
                              , EFunction (PVar "x")
                                    (ECase (EVariable "x")
                                        [ ( PInt 0, EInt 1 )
                                        , ( PAnything, EBinOp (EVariable "x") OpMul (EApply (EVariable "fact") (EBinOp (EVariable "x") OpSub (EInt 1))) )
                                        ]
                                    )
                              )
                            ]
                in
                interpret Dict.empty (ELet letBody (EApply (EVariable "fact") (EInt i))) |> Expect.equal (EInt (fact i))
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
                        EApply (EApply (EVariable "ackermann") a) b

                    dec a =
                        EBinOp a OpSub (EInt 1)

                    letBody =
                        Dict.fromList
                            [ ( "ackermann"
                              , EFunction (PVar "m") <|
                                    EFunction (PVar "n") <|
                                        ECase (EVariable "m")
                                            [ ( PInt 0, EBinOp (EVariable "n") OpAdd (EInt 1) )
                                            , ( PAnything
                                              , ECase (EVariable "n")
                                                    [ ( PInt 0, rec (dec (EVariable "m")) (EInt 1) )
                                                    , ( PAnything, rec (dec (EVariable "m")) (rec (EVariable "m") (dec (EVariable "n"))) )
                                                    ]
                                              )
                                            ]
                              )
                            ]
                in
                interpret Dict.empty (ELet letBody (EApply (EApply (EVariable "ackermann") (EInt x)) (EInt y))) |> Expect.equal (EInt (ackermann x y))
        ]


mutuallyRecursiveEFunctions : Test
mutuallyRecursiveEFunctions =
    describe "mutually recursive functions"
        [ fuzz (Fuzz.intRange 0 30) "mutrec" <|
            \i ->
                let
                    isOdd v =
                        case modBy 2 v == 1 of
                            True ->
                                ECtor "True" []

                            False ->
                                ECtor "False" []

                    letBody =
                        Dict.fromList
                            [ ( "not"
                              , EFunction (PVar "xnt")
                                    (ECase (EVariable "xnt")
                                        [ ( PECtor "True" [], ECtor "False" [] )
                                        , ( PECtor "False" [], ECtor "True" [] )
                                        ]
                                    )
                              )
                            , ( "even"
                              , EFunction (PVar "xev")
                                    (ECase (EVariable "xev")
                                        [ ( PInt 0, ECtor "True" [] )
                                        , ( PAnything, EApply (EVariable "odd") (EBinOp (EVariable "xev") OpSub (EInt 1)) )
                                        ]
                                    )
                              )
                            , ( "odd"
                              , EFunction (PVar "xdd")
                                    (ECase (EVariable "xdd")
                                        [ ( PInt 0, ECtor "False" [] )
                                        , ( PAnything, EApply (EVariable "even") (EBinOp (EVariable "xdd") OpSub (EInt 1)) )
                                        ]
                                    )
                              )
                            ]
                in
                interpret Dict.empty (ELet letBody (EApply (EVariable "odd") (EInt i))) |> Expect.equal (isOdd i)
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
                              , EFunction (PVar "x")
                                    (ECase (EVariable "x")
                                        [ ( PInt 0, EInt 1 )
                                        , ( PAnything, EBinOp (EVariable "x") OpMul (EApply (EVariable "fact") (EBinOp (EApply (EVariable "identity") (EVariable "x")) OpSub (EInt 1))) )
                                        ]
                                    )
                              )
                            , ( "identity", EFunction (PVar "x") (EVariable "x") )
                            ]
                in
                interpret Dict.empty (ELet letBody (EApply (EVariable "fact") (EInt i))) |> Expect.equal (EInt (fact i))
        ]


closures : Test
closures =
    describe "closures"
        [ test "closure" <|
            \() ->
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , ELet
                                    (Dict.fromList
                                        [ ( "innerEInt", EInt 3 )
                                        , ( "retFn", EFunction (PInt 5) (EVariable "innerEInt") )
                                        ]
                                    )
                                    (EVariable "retFn")
                              )
                            , ( "innerEInt", EInt 9 )
                            ]
                in
                interpret (Dict.singleton "innerEInt" (EInt 1)) (ELet letBody (EApply (EVariable "closure") (EInt 5))) |> Expect.equal (EInt 3)
        , test "closure2" <|
            \() ->
                -- test uses name shadowing to figure out where a closure fails, if it fails, even though elm doesn't allow shadowing
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , ELet
                                    (Dict.fromList
                                        [ ( "middle"
                                          , ELet
                                                (Dict.fromList
                                                    [ ( "innerELet"
                                                      , ELet
                                                            (Dict.fromList
                                                                [ ( "retFn", EFunction (PInt 5) (EVariable "innerEInt") )
                                                                ]
                                                            )
                                                            (EVariable "retFn")
                                                      )
                                                    ]
                                                )
                                                (EVariable "innerELet")
                                          )
                                        , ( "innerEInt", EInt 3 )
                                        ]
                                    )
                                    (EVariable "middle")
                              )
                            , ( "innerEInt", EInt 9 )
                            ]
                in
                interpret (Dict.singleton "innerEInt" (EInt 1)) (ELet letBody (EApply (EVariable "closure") (EInt 5))) |> Expect.equal (EInt 3)
        , test "faked closure" <|
            \() ->
                let
                    letBody =
                        Dict.fromList
                            [ ( "closure"
                              , ELet
                                    (Dict.fromList
                                        [ ( "innerEInt", EInt 3 )
                                        , ( "retFn", ELambda (Dict.singleton "innerEInt" (EInt 3)) (PInt 5) (EVariable "innerEInt") )
                                        ]
                                    )
                                    (EVariable "retFn")
                              )
                            , ( "innerEInt", EInt 9 )
                            ]
                in
                interpret (Dict.singleton "innerEInt" (EInt 1)) (ELet letBody (EApply (EVariable "closure") (EInt 5))) |> Expect.equal (EInt 3)
        ]
