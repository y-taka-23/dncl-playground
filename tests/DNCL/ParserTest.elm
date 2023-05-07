module DNCL.ParserTest exposing (suite)

import DNCL.AST exposing (..)
import DNCL.Parser exposing (..)
import Expect
import List.Nonempty exposing (Nonempty(..), singleton)
import Parser
import Result
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Parser module"
        [ describe "variable_"
            [ describe "scalar "
                [ test "starts with a lowercase" <|
                    \_ ->
                        Parser.run variable_ "kosu"
                            |> Expect.equal (Result.Ok (Scalar "kosu"))
                , test "can contain an uppercase" <|
                    \_ ->
                        Parser.run variable_ "kosuGokei"
                            |> Expect.equal (Result.Ok (Scalar "kosuGokei"))
                , test "can contain a underscore" <|
                    \_ ->
                        Parser.run variable_ "kosu_gokei"
                            |> Expect.equal (Result.Ok (Scalar "kosu_gokei"))
                , test "can contain a numeric" <|
                    \_ ->
                        Parser.run variable_ "kosu0"
                            |> Expect.equal (Result.Ok (Scalar "kosu0"))
                , test "cannot start with a numeric" <|
                    \_ ->
                        Parser.run variable_ "0_bamme"
                            |> Expect.err
                , test "cannot start with a symbol" <|
                    \_ ->
                        Parser.run variable_ "_kosu"
                            |> Expect.err
                , test "cannot start with a multi-byte" <|
                    \_ ->
                        Parser.run variable_ "個数"
                            |> Expect.err
                , test "cannot contain a symbol other than a underscore" <|
                    \_ ->
                        Parser.run variable_ "kosu-gokei"
                            |> Expect.equal (Result.Ok (Scalar "kosu"))
                , test "cannot contain a single-byte space" <|
                    \_ ->
                        Parser.run variable_ "kosu gokei"
                            |> Expect.equal (Result.Ok (Scalar "kosu"))
                , test "cannot contain a multi-byte space" <|
                    \_ ->
                        Parser.run variable_ "kosu\u{3000}gokei"
                            |> Expect.equal (Result.Ok (Scalar "kosu"))
                ]
            , describe "const"
                [ test "can contain uppercase only" <|
                    \_ ->
                        Parser.run variable_ "KOSU"
                            |> Expect.equal (Result.Ok (Const "KOSU"))
                , test "can be a single uppercase" <|
                    \_ ->
                        Parser.run variable_ "N"
                            |> Expect.equal (Result.Ok (Const "N"))
                , test "cannot start with a numeric" <|
                    \_ ->
                        Parser.run variable_ "0_BAMME"
                            |> Expect.err
                , test "cannot start with a symbol" <|
                    \_ ->
                        Parser.run variable_ "_KOSU"
                            |> Expect.err
                , test "cannot contain a symbol other than a underscore" <|
                    \_ ->
                        Parser.run variable_ "KOSU-GOKEI"
                            |> Expect.equal (Result.Ok (Const "KOSU"))
                , test "cannot contain a single-byte space" <|
                    \_ ->
                        Parser.run variable_ "KOSU GOKEI"
                            |> Expect.equal (Result.Ok (Const "KOSU"))
                , test "cannot contain a multi-byte space" <|
                    \_ ->
                        Parser.run variable_ "KOSU\u{3000}GOKEI"
                            |> Expect.equal (Result.Ok (Const "KOSU"))
                ]
            , describe "array"
                [ test "starts with a uppercate" <|
                    \_ ->
                        Parser.run variable_ "Tokuten"
                            |> Expect.equal (Result.Ok (Array "Tokuten" []))
                , test "can contain a underscore" <|
                    \_ ->
                        Parser.run variable_ "Gokei_Tokuten"
                            |> Expect.equal (Result.Ok (Array "Gokei_Tokuten" []))
                , test "can contain a numeric" <|
                    \_ ->
                        Parser.run variable_ "Tokuten0"
                            |> Expect.equal (Result.Ok (Array "Tokuten0" []))
                , test "cannot start with a numeric" <|
                    \_ ->
                        Parser.run variable_ "0_Tokuten"
                            |> Expect.err
                , test "cannot start with a symbol" <|
                    \_ ->
                        Parser.run variable_ "_Tokuten"
                            |> Expect.err
                , test "cannot contain a symbol other than a underscore" <|
                    \_ ->
                        Parser.run variable_ "Gokei-Tokuten"
                            |> Expect.equal (Result.Ok (Array "Gokei" []))
                , test "cannot contain a single-byte space" <|
                    \_ ->
                        Parser.run variable_ "Gokei Tokuten"
                            |> Expect.equal (Result.Ok (Array "Gokei" []))
                , test "cannot contain a multi-byte space" <|
                    \_ ->
                        Parser.run variable_ "Gokei\u{3000}Tokuten"
                            |> Expect.equal (Result.Ok (Array "Gokei" []))
                , test "can have a following index" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[0]"
                            |> Expect.equal (Result.Ok (Array "Tokuten" [ Lit (NumberVal 0) ]))
                , test "can have a following index with spaces" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[ 0 ]"
                            |> Expect.equal (Result.Ok (Array "Tokuten" [ Lit (NumberVal 0) ]))
                , test "can have a following indices" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[0，1，2]"
                            |> Expect.equal
                                (Result.Ok
                                    (Array "Tokuten" [ Lit (NumberVal 0), Lit (NumberVal 1), Lit (NumberVal 2) ])
                                )
                , test "can have a following indices with spaces" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[ 0， 1， 2 ]"
                            |> Expect.equal
                                (Result.Ok
                                    (Array "Tokuten" [ Lit (NumberVal 0), Lit (NumberVal 1), Lit (NumberVal 2) ])
                                )
                , test "cannot have the empty bracket" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[]"
                            |> Expect.err
                , test "cannot have nested brackets" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[[0]]"
                            |> Expect.err
                , test "cannot have a separeted bracket" <|
                    \_ ->
                        Parser.run variable_ "Tokuten [0]"
                            |> Expect.equal (Result.Ok (Array "Tokuten" []))
                , test "can have an index of an arithmetic expression" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[i＋1]"
                            |> Expect.equal
                                (Result.Ok
                                    (Array "Tokuten" [ Plus (Var (Scalar "i")) (Lit (NumberVal 1)) ])
                                )
                , test "can have indices of arithmetic expressions without spaces" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[i＋1，j＋1]"
                            |> Expect.equal
                                (Result.Ok
                                    (Array "Tokuten"
                                        [ Plus (Var (Scalar "i")) (Lit (NumberVal 1))
                                        , Plus (Var (Scalar "j")) (Lit (NumberVal 1))
                                        ]
                                    )
                                )
                , test "can have an index of a string" <|
                    \_ ->
                        Parser.run variable_ "Tokuten[\"0\"]"
                            |> Expect.equal (Result.Ok (Array "Tokuten" [ Lit (StringVal "0") ]))
                , test "can have nexted brackets" <|
                    \_ ->
                        Parser.run variable_ "Gokei[Tokuten[0]]"
                            |> Expect.equal
                                (Result.Ok
                                    (Array "Gokei" [ Var (Array "Tokuten" [ Lit (NumberVal 0) ]) ])
                                )
                ]
            ]
        , describe "value"
            [ describe "number values"
                [ test "is a positive integer" <|
                    \_ ->
                        Parser.run value "100"
                            |> Expect.equal (Result.Ok (NumberVal 100))
                , test "is a negative integer" <|
                    \_ ->
                        Parser.run value "-100"
                            |> Expect.equal (Result.Ok (NumberVal -100))
                , test "cannot start with a plus symbol" <|
                    \_ ->
                        Parser.run value "+100"
                            |> Expect.err
                , test "cannot contain a point" <|
                    \_ ->
                        Parser.run value "99.99"
                            |> Expect.err
                , test "cannot contain a symbol" <|
                    \_ ->
                        Parser.run value "99-99"
                            |> Expect.equal (Result.Ok (NumberVal 99))
                ]
            , describe "string values"
                [ test "is surrounded by kagikakkoes" <|
                    \_ ->
                        Parser.run value "「見つかりました」"
                            |> Expect.equal (Result.Ok (StringVal "見つかりました"))
                , test "is surrounded by kagikakkoes and empty" <|
                    \_ ->
                        Parser.run value "「」"
                            |> Expect.equal (Result.Ok (StringVal ""))
                , test "is surrounded by doublequotes" <|
                    \_ ->
                        Parser.run value "\"It was found.\""
                            |> Expect.equal (Result.Ok (StringVal "It was found."))
                , test "is surrounded by doublequotes and empty" <|
                    \_ ->
                        Parser.run value "\"\""
                            |> Expect.equal (Result.Ok (StringVal ""))
                , test "cannot start with a multi-byte other than hirakikakko" <|
                    \_ ->
                        Parser.run value "『見つかりました』"
                            |> Expect.err
                , test "cannot start with a single-byte other than doublequote" <|
                    \_ ->
                        Parser.run value "'It was found.'"
                            |> Expect.err
                , test "cannot contain nested kagikakkoes" <|
                    \_ ->
                        Parser.run value "「見つ「かり」ました」"
                            |> Expect.equal (Result.Ok (StringVal "見つ「かり"))
                , test "cannot contain nested doublequotes" <|
                    \_ ->
                        Parser.run value "\"It \"was\" found.\""
                            |> Expect.equal (Result.Ok (StringVal "It "))
                ]
            ]
        , describe "arithExp"
            [ describe "literals"
                [ test "parses a single number" <|
                    \_ ->
                        Parser.run arithExp "0"
                            |> Expect.equal (Result.Ok (Lit (NumberVal 0)))
                , test "parses a single string" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\""
                            |> Expect.equal (Result.Ok (Lit (StringVal "Hello")))
                ]
            , describe "variables"
                [ test "parses a single variable" <|
                    \_ ->
                        Parser.run arithExp "kosu"
                            |> Expect.equal (Result.Ok (Var (Scalar "kosu")))
                , test "parses an indexed variable" <|
                    \_ ->
                        Parser.run arithExp "Tokuten[0]"
                            |> Expect.equal (Result.Ok (Var (Array "Tokuten" [ Lit (NumberVal 0) ])))
                ]
            , describe "round brackets"
                [ test "parses non-spaced parens" <|
                    \_ ->
                        Parser.run arithExp "(0)"
                            |> Expect.equal (Result.Ok (Lit (NumberVal 0)))
                , test "parses redundant non-spaced parens" <|
                    \_ ->
                        Parser.run arithExp "((0))"
                            |> Expect.equal (Result.Ok (Lit (NumberVal 0)))
                , test "parses spaced parens" <|
                    \_ ->
                        Parser.run arithExp "( 0 )"
                            |> Expect.equal (Result.Ok (Lit (NumberVal 0)))
                , test "parses redundant spaced parens" <|
                    \_ ->
                        Parser.run arithExp "( ( 0 ) )"
                            |> Expect.equal (Result.Ok (Lit (NumberVal 0)))
                ]
            , describe "addition"
                [ test "parses 2-number addition without spaces" <|
                    \_ ->
                        Parser.run arithExp "0＋1"
                            |> Expect.equal (Result.Ok (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-number addition with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ 1"
                            |> Expect.equal (Result.Ok (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-string addition" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\" ＋ \"World\""
                            |> Expect.equal (Result.Ok (Plus (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses 2-Scalar addition" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ＋ kosu1"
                            |> Expect.equal (Result.Ok (Plus (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "parses 2-array addition" <|
                    \_ ->
                        Parser.run arithExp "Gokei ＋ Tokuten"
                            |> Expect.equal (Result.Ok (Plus (Var (Array "Gokei" [])) (Var (Array "Tokuten" []))))
                , test "parses indexed 2-array addition" <|
                    \_ ->
                        Parser.run arithExp "Gokei[0] ＋ Tokuten[0]"
                            |> Expect.equal
                                (Result.Ok
                                    (Plus
                                        (Var (Array "Gokei" [ Lit (NumberVal 0) ]))
                                        (Var (Array "Tokuten" [ Lit (NumberVal 0) ]))
                                    )
                                )
                , test "parses 3-number addition without spaces" <|
                    \_ ->
                        Parser.run arithExp "0＋1＋2"
                            |> Expect.equal (Result.Ok (Plus (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number addition with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ 1 ＋ 2"
                            |> Expect.equal (Result.Ok (Plus (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number addition with explicit associativity" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ (1 ＋ 2)"
                            |> Expect.equal (Result.Ok (Plus (Lit (NumberVal 0)) (Plus (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                ]
            , describe "subtraction"
                [ test "parses 2-number subtraction without spaces" <|
                    \_ ->
                        Parser.run arithExp "0－1"
                            |> Expect.equal (Result.Ok (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-number subtraction with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 － 1"
                            |> Expect.equal (Result.Ok (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-string subtraction" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\" － \"World\""
                            |> Expect.equal (Result.Ok (Minus (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses 2-Scalar subtraction" <|
                    \_ ->
                        Parser.run arithExp "kosu0 － kosu1"
                            |> Expect.equal (Result.Ok (Minus (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "parses 2-array subtraction" <|
                    \_ ->
                        Parser.run arithExp "Gokei － Tokuten"
                            |> Expect.equal (Result.Ok (Minus (Var (Array "Gokei" [])) (Var (Array "Tokuten" []))))
                , test "parses indexed 2-array subtraction" <|
                    \_ ->
                        Parser.run arithExp "Gokei[0] － Tokuten[0]"
                            |> Expect.equal
                                (Result.Ok
                                    (Minus
                                        (Var (Array "Gokei" [ Lit (NumberVal 0) ]))
                                        (Var (Array "Tokuten" [ Lit (NumberVal 0) ]))
                                    )
                                )
                , test "parses 3-number subtraction without spaces" <|
                    \_ ->
                        Parser.run arithExp "0－1－2"
                            |> Expect.equal (Result.Ok (Minus (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number subtraction with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 － 1 － 2"
                            |> Expect.equal (Result.Ok (Minus (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number subtraction with explicit associativity" <|
                    \_ ->
                        Parser.run arithExp "0 － (1 － 2)"
                            |> Expect.equal (Result.Ok (Minus (Lit (NumberVal 0)) (Minus (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                ]
            , describe "multiplication"
                [ test "parses 2-number multiplication without spaces" <|
                    \_ ->
                        Parser.run arithExp "0×1"
                            |> Expect.equal (Result.Ok (Times (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-number multiplication with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 × 1"
                            |> Expect.equal (Result.Ok (Times (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-string multiplication" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\" × \"World\""
                            |> Expect.equal (Result.Ok (Times (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses 2-Scalar multiplication" <|
                    \_ ->
                        Parser.run arithExp "kosu0 × kosu1"
                            |> Expect.equal (Result.Ok (Times (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "parses 2-array multiplication" <|
                    \_ ->
                        Parser.run arithExp "Gokei × Tokuten"
                            |> Expect.equal (Result.Ok (Times (Var (Array "Gokei" [])) (Var (Array "Tokuten" []))))
                , test "parses indexed 2-array multiplication" <|
                    \_ ->
                        Parser.run arithExp "Gokei[0] × Tokuten[0]"
                            |> Expect.equal
                                (Result.Ok
                                    (Times
                                        (Var (Array "Gokei" [ Lit (NumberVal 0) ]))
                                        (Var (Array "Tokuten" [ Lit (NumberVal 0) ]))
                                    )
                                )
                , test "parses 3-number multiplication without spaces" <|
                    \_ ->
                        Parser.run arithExp "0×1×2"
                            |> Expect.equal (Result.Ok (Times (Times (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number multiplication with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 × 1 × 2"
                            |> Expect.equal (Result.Ok (Times (Times (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number addition with explicit associativity" <|
                    \_ ->
                        Parser.run arithExp "0 × (1 × 2)"
                            |> Expect.equal (Result.Ok (Times (Lit (NumberVal 0)) (Times (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                ]
            , describe "integral quotient"
                [ test "parses 2-number quotient without spaces" <|
                    \_ ->
                        Parser.run arithExp "0÷1"
                            |> Expect.equal (Result.Ok (Quot (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-number quotient with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ÷ 1"
                            |> Expect.equal (Result.Ok (Quot (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-string quotient" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\" ÷ \"World\""
                            |> Expect.equal (Result.Ok (Quot (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses 2-Scalar quotient" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ÷ kosu1"
                            |> Expect.equal (Result.Ok (Quot (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "parses 2-array quotient" <|
                    \_ ->
                        Parser.run arithExp "Gokei ÷ Tokuten"
                            |> Expect.equal (Result.Ok (Quot (Var (Array "Gokei" [])) (Var (Array "Tokuten" []))))
                , test "parses indexed 2-array quotient" <|
                    \_ ->
                        Parser.run arithExp "Gokei[0] ÷ Tokuten[0]"
                            |> Expect.equal
                                (Result.Ok
                                    (Quot
                                        (Var (Array "Gokei" [ Lit (NumberVal 0) ]))
                                        (Var (Array "Tokuten" [ Lit (NumberVal 0) ]))
                                    )
                                )
                , test "parses 3-number quotient without spaces" <|
                    \_ ->
                        Parser.run arithExp "0÷1÷2"
                            |> Expect.equal (Result.Ok (Quot (Quot (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number quotient with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ÷ 1 ÷ 2"
                            |> Expect.equal (Result.Ok (Quot (Quot (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number quotient with explicit associativity" <|
                    \_ ->
                        Parser.run arithExp "0 ÷ (1 ÷ 2)"
                            |> Expect.equal (Result.Ok (Quot (Lit (NumberVal 0)) (Quot (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                ]
            , describe "integral remainder"
                [ test "parses 2-number remainder without spaces" <|
                    \_ ->
                        Parser.run arithExp "0％1"
                            |> Expect.equal (Result.Ok (Mod (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-number remainder with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ％ 1"
                            |> Expect.equal (Result.Ok (Mod (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses 2-string remainder" <|
                    \_ ->
                        Parser.run arithExp "\"Hello\" ％ \"World\""
                            |> Expect.equal (Result.Ok (Mod (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses 2-Scalar remainder" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ％ kosu1"
                            |> Expect.equal (Result.Ok (Mod (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "parses 3-number remainder without spaces" <|
                    \_ ->
                        Parser.run arithExp "0％1％2"
                            |> Expect.equal (Result.Ok (Mod (Mod (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 2-array remainder" <|
                    \_ ->
                        Parser.run arithExp "Gokei ％ Tokuten"
                            |> Expect.equal (Result.Ok (Mod (Var (Array "Gokei" [])) (Var (Array "Tokuten" []))))
                , test "parses indexed 2-array remainder" <|
                    \_ ->
                        Parser.run arithExp "Gokei[0] ％ Tokuten[0]"
                            |> Expect.equal
                                (Result.Ok
                                    (Mod
                                        (Var (Array "Gokei" [ Lit (NumberVal 0) ]))
                                        (Var (Array "Tokuten" [ Lit (NumberVal 0) ]))
                                    )
                                )
                , test "parses 3-number remainder with spaces" <|
                    \_ ->
                        Parser.run arithExp "0 ％ 1 ％ 2"
                            |> Expect.equal (Result.Ok (Mod (Mod (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses 3-number remainder with explicit associativity" <|
                    \_ ->
                        Parser.run arithExp "0 ％ (1 ％ 2)"
                            |> Expect.equal (Result.Ok (Mod (Lit (NumberVal 0)) (Mod (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                ]
            , describe "associativity"
                [ test "parses as subst is equaly associative to succeeding add" <|
                    \_ ->
                        Parser.run arithExp "0 － 1 ＋ 2"
                            |> Expect.equal (Result.Ok (Plus (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses as subst is equaly associative to preceding add" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ 1 － 2"
                            |> Expect.equal (Result.Ok (Minus (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses as add with parens is stronger than preceding subst" <|
                    \_ ->
                        Parser.run arithExp "0 － (1 ＋ 2)"
                            |> Expect.equal (Result.Ok (Minus (Lit (NumberVal 0)) (Plus (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                , test "parses as add with parens is stronger than succeeding subst" <|
                    \_ ->
                        Parser.run arithExp "(0 ＋ 1) － 2"
                            |> Expect.equal (Result.Ok (Minus (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses as subst with parens is stronger than preceding add" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ (1 － 2)"
                            |> Expect.equal (Result.Ok (Plus (Lit (NumberVal 0)) (Minus (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                , test "parses as subst with parens is stronger than succeeding add" <|
                    \_ ->
                        Parser.run arithExp "(0 － 1) ＋ 2"
                            |> Expect.equal (Result.Ok (Plus (Minus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses as mult is stronger than succeeding add" <|
                    \_ ->
                        Parser.run arithExp "0 × 1 ＋ 2"
                            |> Expect.equal (Result.Ok (Plus (Times (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                , test "parses as mult is stronger than preceding add" <|
                    \_ ->
                        Parser.run arithExp "0 ＋ 1 × 2"
                            |> Expect.equal (Result.Ok (Plus (Lit (NumberVal 0)) (Times (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                , test "parses as add with parens is stronger than preceding mult" <|
                    \_ ->
                        Parser.run arithExp "0 × (1 ＋ 2)"
                            |> Expect.equal (Result.Ok (Times (Lit (NumberVal 0)) (Plus (Lit (NumberVal 1)) (Lit (NumberVal 2)))))
                , test "parses as add with parens is stronger than succeeding mult" <|
                    \_ ->
                        Parser.run arithExp "(0 ＋ 1) × 2"
                            |> Expect.equal (Result.Ok (Times (Plus (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
                ]
            , describe "arrays"
                [ test "parses the empty array" <|
                    \_ ->
                        Parser.run arithExp "{}"
                            |> Expect.equal (Result.Ok (Arr []))
                , test "parses an array of a single nummeric element" <|
                    \_ ->
                        Parser.run arithExp "{0}"
                            |> Expect.equal (Result.Ok (Arr [ Lit (NumberVal 0) ]))
                , test "parses an array of multiple nummeric elements" <|
                    \_ ->
                        Parser.run arithExp "{0，1，2}"
                            |> Expect.equal
                                (Result.Ok
                                    (Arr [ Lit (NumberVal 0), Lit (NumberVal 1), Lit (NumberVal 2) ])
                                )
                , test "parses an array of multiple arithmetic expressions" <|
                    \_ ->
                        Parser.run arithExp "{i＋1，j＋2，k＋3}"
                            |> Expect.equal
                                (Result.Ok
                                    (Arr
                                        [ Plus (Var (Scalar "i")) (Lit (NumberVal 1))
                                        , Plus (Var (Scalar "j")) (Lit (NumberVal 2))
                                        , Plus (Var (Scalar "k")) (Lit (NumberVal 3))
                                        ]
                                    )
                                )
                ]
            , describe "function"
                [ test "parses a function with a single value arg" <|
                    \_ ->
                        Parser.run arithExp "二乗 (0)"
                            |> Expect.equal (Result.Ok (Fun (Function "二乗") [ Lit (NumberVal 0) ]))
                , test "parses a function with a single scalar variable arg" <|
                    \_ ->
                        Parser.run arithExp "二乗 (x)"
                            |> Expect.equal (Result.Ok (Fun (Function "二乗") [ Var (Scalar "x") ]))
                , test "parses a function with a single array variable arg" <|
                    \_ ->
                        Parser.run arithExp "二乗 (MyArr[0])"
                            |> Expect.equal
                                (Result.Ok
                                    (Fun (Function "二乗") [ Var (Array "MyArr" [ Lit (NumberVal 0) ]) ])
                                )
                , test "parses a function with multiple value args" <|
                    \_ ->
                        Parser.run arithExp "二乗 (0，1)"
                            |> Expect.equal
                                (Result.Ok
                                    (Fun (Function "二乗") [ Lit (NumberVal 0), Lit (NumberVal 1) ])
                                )
                , test "parses a function with multiple scalar variable args" <|
                    \_ ->
                        Parser.run arithExp "二乗 (x，y)"
                            |> Expect.equal
                                (Result.Ok
                                    (Fun (Function "二乗") [ Var (Scalar "x"), Var (Scalar "y") ])
                                )
                , test "parses a function with multiple array variable args" <|
                    \_ ->
                        Parser.run arithExp "二乗 (MyArr[0]，MyArr[1])"
                            |> Expect.equal
                                (Result.Ok
                                    (Fun (Function "二乗")
                                        [ Var (Array "MyArr" [ Lit (NumberVal 0) ])
                                        , Var (Array "MyArr" [ Lit (NumberVal 1) ])
                                        ]
                                    )
                                )
                , test "parses a function without a space" <|
                    \_ ->
                        Parser.run arithExp "二乗(0)"
                            |> Expect.equal (Result.Ok (Fun (Function "二乗") [ Lit (NumberVal 0) ]))
                , test "parses a function with optional spaces" <|
                    \_ ->
                        Parser.run arithExp "二乗 ( 0， 1 )"
                            |> Expect.equal (Result.Ok (Fun (Function "二乗") [ Lit (NumberVal 0), Lit (NumberVal 1) ]))
                , test "parses nested functions" <|
                    \_ ->
                        Parser.run arithExp "二乗 (べき乗 (0))"
                            |> Expect.equal
                                (Result.Ok
                                    (Fun (Function "二乗")
                                        [ Fun (Function "べき乗") [ Lit (NumberVal 0) ]
                                        ]
                                    )
                                )
                , test "parses a function with no arg" <|
                    \_ ->
                        Parser.run arithExp "二乗 ()"
                            |> Expect.equal (Result.Ok (Fun (Function "二乗") []))
                , test "cannot parse a function of ascii name" <|
                    \_ ->
                        Parser.run arithExp "square (0)"
                            |> Expect.equal (Result.Ok (Var (Scalar "square")))
                ]
            ]
        , describe "boolExp"
            [ describe "parens"
                [ test "parses non-spaced parens" <|
                    \_ ->
                        Parser.run boolExp "(0 ＝ 1)"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses redundant non-spaced parens" <|
                    \_ ->
                        Parser.run boolExp "((0 ＝ 1))"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses spaced parens" <|
                    \_ ->
                        Parser.run boolExp "( 0 ＝ 1 )"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses redundant spaced parens" <|
                    \_ ->
                        Parser.run boolExp "( ( 0 ＝ 1 ) )"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "equality"
                [ test "parses equality of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0＝1"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses equality of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses equality of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ＝ \"World\""
                            |> Expect.equal (Result.Ok (Eq (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses equality of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ＝ kosu1"
                            |> Expect.equal (Result.Ok (Eq (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse equality of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 ＝ 2"
                            |> Expect.equal (Result.Ok (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "inequality"
                [ test "parses inequality of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0≠1"
                            |> Expect.equal (Result.Ok (Neq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses inequality of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ≠ 1"
                            |> Expect.equal (Result.Ok (Neq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses inequality of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ≠ \"World\""
                            |> Expect.equal (Result.Ok (Neq (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses inequality of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ≠ kosu1"
                            |> Expect.equal (Result.Ok (Neq (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse inequality of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ≠ 1 ≠ 2"
                            |> Expect.equal (Result.Ok (Neq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "greater-than"
                [ test "parses greater-than inequation of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0＞1"
                            |> Expect.equal (Result.Ok (Gt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses greater-than inequation of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＞ 1"
                            |> Expect.equal (Result.Ok (Gt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses greater-than inequation of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ＞ \"World\""
                            |> Expect.equal (Result.Ok (Gt (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses greater-than inequation of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ＞ kosu1"
                            |> Expect.equal (Result.Ok (Gt (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse greater-than inequation of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ＞ 1 ＞ 2"
                            |> Expect.equal (Result.Ok (Gt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "greater-than-or-equal"
                [ test "parses greater-than-or-equal inequation of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0≧1"
                            |> Expect.equal (Result.Ok (Ge (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses greater-than-or-equal inequation of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ≧ 1"
                            |> Expect.equal (Result.Ok (Ge (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses greater-than-or-equal inequation of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ≧ \"World\""
                            |> Expect.equal (Result.Ok (Ge (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses greater-than-or-equal inequation of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ≧ kosu1"
                            |> Expect.equal (Result.Ok (Ge (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse greater-than-or-equal inequation of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ≧ 1 ≧ 2"
                            |> Expect.equal (Result.Ok (Ge (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "less-than-or-equal"
                [ test "parses less-than-or-equal inequation of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0≦1"
                            |> Expect.equal (Result.Ok (Le (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses less-than-or-equal inequation of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ≦ 1"
                            |> Expect.equal (Result.Ok (Le (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses less-than-or-equal inequation of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ≦ \"World\""
                            |> Expect.equal (Result.Ok (Le (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses less-than-or-equal inequation of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ≦ kosu1"
                            |> Expect.equal (Result.Ok (Le (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse less-than-or-equal inequation of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ≦ 1 ≦ 2"
                            |> Expect.equal (Result.Ok (Le (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "less-than"
                [ test "parses less-than inequation of 2 numbers without spaces" <|
                    \_ ->
                        Parser.run boolExp "0＜1"
                            |> Expect.equal (Result.Ok (Lt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses less-than inequation of 2 numbers with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＜ 1"
                            |> Expect.equal (Result.Ok (Lt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                , test "parses less-than inequation of 2 strings" <|
                    \_ ->
                        Parser.run boolExp "\"Hello\" ＜ \"World\""
                            |> Expect.equal (Result.Ok (Lt (Lit (StringVal "Hello")) (Lit (StringVal "World"))))
                , test "parses less-than inequation of 2 variables" <|
                    \_ ->
                        Parser.run boolExp "kosu0 ＜ kosu1"
                            |> Expect.equal (Result.Ok (Lt (Var (Scalar "kosu0")) (Var (Scalar "kosu1"))))
                , test "cannot parse less-than inequation of 3 numbers" <|
                    \_ ->
                        Parser.run boolExp "0 ＜ 1 ＜ 2"
                            |> Expect.equal (Result.Ok (Lt (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                ]
            , describe "conjunction"
                [ test "parses conjunction of 2 equations without spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1かつ2 ＝ 3"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                    )
                                )
                , test "parses conjunction of 2 equations with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 かつ 2 ＝ 3"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                    )
                                )
                , test "parses conjunction of 3 equations" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 かつ 2 ＝ 3 かつ 4 ＝ 5"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (And
                                            (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                        )
                                        (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                    )
                                )
                , test "parses conjunction of 3 equations with explicit associativity" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 かつ (2 ＝ 3 かつ 4 ＝ 5)"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (And
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                            (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                        )
                                    )
                                )
                ]
            , describe "disjunction"
                [ test "parses disjunction of 2 equations without spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1または2 ＝ 3"
                            |> Expect.equal
                                (Result.Ok
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                    )
                                )
                , test "parses disjunction of 2 equations with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 または 2 ＝ 3"
                            |> Expect.equal
                                (Result.Ok
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                    )
                                )
                , test "parses disjunction of 3 equations" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 または 2 ＝ 3 または 4 ＝ 5"
                            |> Expect.equal
                                (Result.Ok
                                    (Or
                                        (Or
                                            (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                        )
                                        (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                    )
                                )
                , test "parses disjunction of 3 equations with explicit associativity" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 または (2 ＝ 3 または 4 ＝ 5)"
                            |> Expect.equal
                                (Result.Ok
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Or
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                            (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                        )
                                    )
                                )
                ]
            , describe "nagation"
                [ test "parses negation without spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1でない"
                            |> Expect.equal (Result.Ok (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))))
                , test "parses negation with spaces" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 でない"
                            |> Expect.equal (Result.Ok (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))))
                , test "parses double negation" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 でないでない"
                            |> Expect.equal (Result.Ok (Not (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))))
                ]
            , describe "associativity"
                [ test "parses conjunction and disjunction as equaly left-associative" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 かつ 2 ＝ 3 または 4 ＝ 5"
                            |> Expect.equal
                                (Result.Ok
                                    (Or
                                        (And
                                            (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                        )
                                        (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                    )
                                )
                , test "parses disjunction and conjunction as equaly left-associative" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 または 2 ＝ 3 かつ 4 ＝ 5"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (Or
                                            (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                        )
                                        (Eq (Lit (NumberVal 4)) (Lit (NumberVal 5)))
                                    )
                                )
                , test "parses conjunction and negation as equaly left-associative" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 かつ 2 ＝ 3 でない"
                            |> Expect.equal
                                (Result.Ok
                                    (Not
                                        (And
                                            (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                            (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                        )
                                    )
                                )
                , test "parses negation and conjunction as equaly left-associative" <|
                    \_ ->
                        Parser.run boolExp "0 ＝ 1 でないかつ 2 ＝ 3"
                            |> Expect.equal
                                (Result.Ok
                                    (And
                                        (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                                        (Eq (Lit (NumberVal 2)) (Lit (NumberVal 3)))
                                    )
                                )
                ]
            ]
        , describe "statement"
            [ describe "assign"
                [ test "parses assignment statement without spaces" <|
                    \_ ->
                        Parser.run statement "kosu←3"
                            |> Expect.equal (Result.Ok (Assign (Scalar "kosu") (Lit (NumberVal 3))))
                , test "parses assignment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu ← 3"
                            |> Expect.equal (Result.Ok (Assign (Scalar "kosu") (Lit (NumberVal 3))))
                , test "parses assignment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokuten←kosu×(kosu＋1)"
                            |> Expect.equal
                                (Result.Ok
                                    (Assign
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses assignment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten ← kosu × (kosu ＋ 1)"
                            |> Expect.equal
                                (Result.Ok
                                    (Assign
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                ]
            , describe "printLn"
                [ test "parses print-line statement for a Japanese string value without spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print-line statement for a Japanese string value with spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」 を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print-line statement for an English string value without spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\"を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print-line statement for an English string value with spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\" を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print-line statement for a numeric value without spaces" <|
                    \_ ->
                        Parser.run statement "3を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (NumberVal 3)))))
                , test "parses print-line statement for a numeric value with spaces" <|
                    \_ ->
                        Parser.run statement "3 を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVal (NumberVal 3)))))
                , test "parses print-line statement for a Scalar without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVar (Scalar "kosu")))))
                , test "parses print-line statement for a Scalar value with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を表示する"
                            |> Expect.equal (Result.Ok (PrintLn (singleton (PrintVar (Scalar "kosu")))))
                , test "parses print-line statement for 2 items" <|
                    \_ ->
                        Parser.run statement "kosu と「個見つかった」を表示する"
                            |> Expect.equal
                                (Result.Ok
                                    (PrintLn
                                        (Nonempty
                                            (PrintVar (Scalar "kosu"))
                                            [ PrintVal (StringVal "個見つかった") ]
                                        )
                                    )
                                )
                ]
            , describe "print"
                [ test "parses print statement for a Japanese string value without spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print statement for a Japanese string value with spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」 を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print statement for an English string value without spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\"を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print statement for an English string value with spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\" を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print statement for a numeric value without spaces" <|
                    \_ ->
                        Parser.run statement "3を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (NumberVal 3)))))
                , test "parses print statement for a numeric value with spaces" <|
                    \_ ->
                        Parser.run statement "3 を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (NumberVal 3)))))
                , test "parses print statement for a Scalar without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVar (Scalar "kosu")))))
                , test "parses print statement for a Scalar value with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を改行なしで表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVar (Scalar "kosu")))))
                , test "parses print statement for 2 items" <|
                    \_ ->
                        Parser.run statement "kosu と「個見つかった」を改行なしで表示する"
                            |> Expect.equal
                                (Result.Ok
                                    (Print
                                        (Nonempty
                                            (PrintVar (Scalar "kosu"))
                                            [ PrintVal (StringVal "個見つかった") ]
                                        )
                                    )
                                )
                ]
            , describe "print new line"
                [ test "parses print-new-line statement" <|
                    \_ ->
                        Parser.run statement "改行を表示する"
                            |> Expect.equal (Result.Ok PrintNewLine)
                ]
            , describe "increment"
                [ test "parses increment statement without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを1増やす"
                            |> Expect.equal (Result.Ok (Increment (Scalar "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を 1 増やす"
                            |> Expect.equal (Result.Ok (Increment (Scalar "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokutenをkosu×(kosu＋1)増やす"
                            |> Expect.equal
                                (Result.Ok
                                    (Increment
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses increment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten を kosu × (kosu ＋ 1) 増やす"
                            |> Expect.equal
                                (Result.Ok
                                    (Increment
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                ]
            , describe "decrement"
                [ test "parses increment statement without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを1減らす"
                            |> Expect.equal (Result.Ok (Decrement (Scalar "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を 1 減らす"
                            |> Expect.equal (Result.Ok (Decrement (Scalar "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokutenをkosu×(kosu＋1)減らす"
                            |> Expect.equal
                                (Result.Ok
                                    (Decrement
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses increment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten を kosu × (kosu ＋ 1) 減らす"
                            |> Expect.equal
                                (Result.Ok
                                    (Decrement
                                        (Scalar "tokuten")
                                        (Times (Var (Scalar "kosu"))
                                            (Plus (Var (Scalar "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                ]
            , describe "if"
                [ test "parses if statement with empty body" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば


                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1))) ]
                                    )
                                )
                , test "parses if statement with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                    y ← y － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if statement with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                    x ← x ＋ 1
                                    y ← y － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if statement with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1

                                    y ← y － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if statement with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                    y ← y － 1

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if statement with nested if" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    もし y ＜ 3 ならば
                                    を実行する
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ If (Lt (Var (Scalar "y")) (Lit (NumberVal 3))) [] ]
                                    )
                                )
                ]
            , describe "ifElse"
                [ test "parses if-else statement with empty body" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                を実行し，そうでなければ
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) [] []))
                , test "parses if-else statement with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                を実行し，そうでなければ

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) [] []))
                , test "parses if-else statement with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば


                                を実行し，そうでなければ


                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3))) [] []))
                , test "parses if-else statement with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                を実行し，そうでなければ
                                    y ← y － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1))) ]
                                        [ Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1))) ]
                                    )
                                )
                , test "parses if-then statement with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                    y ← y － 1
                                を実行し，そうでなければ
                                    z ← z ＋ 1
                                    w ← w － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Scalar "z") (Plus (Var (Scalar "z")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "w") (Minus (Var (Scalar "w")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if-else statement with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                    x ← x ＋ 1
                                    y ← y － 1
                                を実行し，そうでなければ

                                    z ← z ＋ 1
                                    w ← w － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Scalar "z") (Plus (Var (Scalar "z")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "w") (Minus (Var (Scalar "w")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if-else statement with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1

                                    y ← y － 1
                                を実行し，そうでなければ
                                    z ← z ＋ 1

                                    w ← w － 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Scalar "z") (Plus (Var (Scalar "z")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "w") (Minus (Var (Scalar "w")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if-else statement with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                    y ← y － 1

                                を実行し，そうでなければ
                                    z ← z ＋ 1
                                    w ← w － 1

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "y") (Minus (Var (Scalar "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Scalar "z") (Plus (Var (Scalar "z")) (Lit (NumberVal 1)))
                                        , Assign (Scalar "w") (Minus (Var (Scalar "w")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses if-else statement with nested if" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    もし y ＜ 3 ならば
                                    を実行し，そうでなければ
                                    を実行する
                                を実行し，そうでなければ
                                    もし z ＜ 3 ならば
                                    を実行し，そうでなければ
                                    を実行する
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (IfElse (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                        [ IfElse (Lt (Var (Scalar "y")) (Lit (NumberVal 3))) [] [] ]
                                        [ IfElse (Lt (Var (Scalar "z")) (Lit (NumberVal 3))) [] [] ]
                                    )
                                )
                ]
            , describe "preCheckLoop"
                [ test "parses pre-check loop with empty body" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with a blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，


                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    gokei ← gokei ＋ x
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x"))) ]
                                    )
                                )
                , test "parses pre-check loop with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses pre-check loop with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，

                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses pre-check loop with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    gokei ← gokei ＋ x

                                    x ← x ＋ 1
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses pre-check loop with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                    )
                                )
                , test "parses pre-check loop with a nested pre-check loop" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    y ＜ 10 の間，
                                    を繰り返す
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 10)))
                                        [ PreCheckLoop (Lt (Var (Scalar "y")) (Lit (NumberVal 10))) [] ]
                                    )
                                )
                ]
            , describe "postCheckLoop"
                [ test "parses post-check loop with empty body" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，

                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，


                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    gokei ← gokei ＋ x
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x"))) ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                , test "parses post-check loop with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                , test "parses post-check loop with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，

                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                , test "parses post-check loop with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    gokei ← gokei ＋ x

                                    x ← x ＋ 1
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                , test "parses post-check loop with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    gokei ← gokei ＋ x
                                    x ← x ＋ 1

                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Scalar "gokei") (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , Assign (Scalar "x") (Plus (Var (Scalar "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                , test "parses post-check loop with a nested post-check loop" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    繰り返し，
                                    を，y ≧ 10 になるまで実行する
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ PostCheckLoop [] (Ge (Var (Scalar "y")) (Lit (NumberVal 10))) ]
                                        (Ge (Var (Scalar "x")) (Lit (NumberVal 10)))
                                    )
                                )
                ]
            , describe "incrementLoop"
                [ test "parses increment loop with the empty body" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses increment loop with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses increment loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，


                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses increment loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，
                                    gokei ← gokei ＋ x
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses increment loop with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，
                                    gokei ← gokei ＋ x
                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses increment loop with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，

                                    gokei ← gokei ＋ x
                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses increment loop with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，
                                    gokei ← gokei ＋ x

                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses increment loop with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ増やしながら，
                                    gokei ← gokei ＋ x
                                    x を表示する

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (IncrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                ]
            , describe "decrementLoop"
                [ test "parses decrement loop with the empty body" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses decrement loop with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses decrement loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，


                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        []
                                    )
                                )
                , test "parses decrement loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，
                                    gokei ← gokei ＋ x
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses decrement loop with multiple statements" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，
                                    gokei ← gokei ＋ x
                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses decrement loop with a preceding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，

                                    gokei ← gokei ＋ x
                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses decrement loop with a blank line in the middle" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，
                                    gokei ← gokei ＋ x

                                    x を表示する
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                , test "parses decrement loop with a succeeding blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x を 0 から 4 まで 1 ずつ減らしながら，
                                    gokei ← gokei ＋ x
                                    x を表示する

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (DecrementLoop (Scalar "x")
                                        (Lit (NumberVal 0))
                                        (Lit (NumberVal 4))
                                        (Lit (NumberVal 1))
                                        [ Assign (Scalar "gokei")
                                            (Plus (Var (Scalar "gokei")) (Var (Scalar "x")))
                                        , PrintLn (singleton (PrintVar (Scalar "x")))
                                        ]
                                    )
                                )
                ]
            ]
        , describe "dnclProgram"
            [ test "parses the empty procedure" <|
                \_ ->
                    Parser.run dnclProgram
                        """"""
                        |> Expect.equal (Result.Ok [])
            , test "parses the empty procedure with blank lines" <|
                \_ ->
                    Parser.run dnclProgram
                        """

                        """
                        |> Expect.equal (Result.Ok [])
            , test "parses a single statement withou blank lines" <|
                \_ ->
                    Parser.run dnclProgram
                        """ 「こんにちは、世界」を表示する"""
                        |> Expect.equal (Result.Ok [ PrintLn (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses a single statment with proceding line break" <|
                \_ ->
                    Parser.run dnclProgram
                        """
                            「こんにちは、世界」を表示する"""
                        |> Expect.equal (Result.Ok [ PrintLn (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses a single statement with succeeding line break" <|
                \_ ->
                    Parser.run dnclProgram
                        """ 「こんにちは、世界」を表示する
                        """
                        |> Expect.equal (Result.Ok [ PrintLn (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses multiple line-statements" <|
                \_ ->
                    Parser.run dnclProgram
                        """ x ← 42
                            x を表示する
                        """
                        |> Expect.equal
                            (Result.Ok
                                [ Assign (Scalar "x") (Lit (NumberVal 42))
                                , PrintLn (Nonempty (PrintVar (Scalar "x")) [])
                                ]
                            )
            , test "parses multiple block-statements" <|
                \_ ->
                    Parser.run dnclProgram
                        """ もし x ＜ 3 ならば
                                x ← 3
                            を実行する
                            もし y ＜ 3 ならば
                                y ← 3
                            を実行する
                        """
                        |> Expect.equal
                            (Result.Ok
                                [ If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                    [ Assign (Scalar "x") (Lit (NumberVal 3)) ]
                                , If (Lt (Var (Scalar "y")) (Lit (NumberVal 3)))
                                    [ Assign (Scalar "y") (Lit (NumberVal 3)) ]
                                ]
                            )
            , test "parses a line-statements and a block-statement" <|
                \_ ->
                    Parser.run dnclProgram
                        """ x ← 0
                            もし x ＜ 3 ならば
                                x ← 3
                            を実行する
                        """
                        |> Expect.equal
                            (Result.Ok
                                [ Assign (Scalar "x") (Lit (NumberVal 0))
                                , If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                    [ Assign (Scalar "x") (Lit (NumberVal 3)) ]
                                ]
                            )
            , test "parses a block-statement and a line-statements" <|
                \_ ->
                    Parser.run dnclProgram
                        """ もし x ＜ 3 ならば
                                x ← 3
                            を実行する
                            x を表示する
                        """
                        |> Expect.equal
                            (Result.Ok
                                [ If (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                    [ Assign (Scalar "x") (Lit (NumberVal 3)) ]
                                , PrintLn (Nonempty (PrintVar (Scalar "x")) [])
                                ]
                            )
            , test "cannot parse a program with a trailing erroneous fragment" <|
                \_ ->
                    Parser.run dnclProgram
                        """ もし x ＜ 3 ならば
                                x ← 3
                            を実行する
                            x を表示する
                            err
                        """
                        |> Expect.err
            ]
        ]
