module MainTest exposing (suite)

import Expect
import Main exposing (..)
import Parser
import Result
import Test exposing (Test, describe, test, todo)


suite : Test
suite =
    describe "The Main module"
        [ describe "greet"
            [ test "appends the name to the message" <|
                \_ ->
                    greet "Elm"
                        |> Expect.equal "Hello, Elm!"
            ]
        , describe "variable_"
            [ test "starts with a lowercase" <|
                \_ ->
                    Parser.run variable_ "kosu"
                        |> Expect.equal (Result.Ok (Variable "kosu"))
            , test "can contain an uppercase" <|
                \_ ->
                    Parser.run variable_ "kosuGokei"
                        |> Expect.equal (Result.Ok (Variable "kosuGokei"))
            , test "can contain a underscore" <|
                \_ ->
                    Parser.run variable_ "kosu_gokei"
                        |> Expect.equal (Result.Ok (Variable "kosu_gokei"))
            , test "can contain a numeric" <|
                \_ ->
                    Parser.run variable_ "kosu0"
                        |> Expect.equal (Result.Ok (Variable "kosu0"))
            , test "cannot start with an uppercase" <|
                \_ ->
                    Parser.run variable_ "Tokuten"
                        |> Expect.err
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
            , test "cannot contain a symbol other than a hyphen" <|
                \_ ->
                    Parser.run variable_ "kosu-gokei"
                        |> Expect.equal (Result.Ok (Variable "kosu"))
            , test "cannot contain a single-byte space" <|
                \_ ->
                    Parser.run variable_ "kosu gokei"
                        |> Expect.equal (Result.Ok (Variable "kosu"))
            , test "cannot contain a multi-byte space" <|
                \_ ->
                    Parser.run variable_ "kosu\u{3000}gokei"
                        |> Expect.equal (Result.Ok (Variable "kosu"))
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
                , test "is surrounded by doublequotes" <|
                    \_ ->
                        Parser.run value "\"It was found.\""
                            |> Expect.equal (Result.Ok (StringVal "It was found."))
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
                            |> Expect.equal (Result.Ok (Var "kosu"))
                ]
            , describe "parens"
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
                , test "parses 2-variable addition" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ＋ kosu1"
                            |> Expect.equal (Result.Ok (Plus (Var "kosu0") (Var "kosu1")))
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
                , test "parses 2-variable subtraction" <|
                    \_ ->
                        Parser.run arithExp "kosu0 － kosu1"
                            |> Expect.equal (Result.Ok (Minus (Var "kosu0") (Var "kosu1")))
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
                , test "parses 2-variable multiplication" <|
                    \_ ->
                        Parser.run arithExp "kosu0 × kosu1"
                            |> Expect.equal (Result.Ok (Times (Var "kosu0") (Var "kosu1")))
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
                , test "parses 2-variable quotient" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ÷ kosu1"
                            |> Expect.equal (Result.Ok (Quot (Var "kosu0") (Var "kosu1")))
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
                , test "parses 2-variable remainder" <|
                    \_ ->
                        Parser.run arithExp "kosu0 ％ kosu1"
                            |> Expect.equal (Result.Ok (Mod (Var "kosu0") (Var "kosu1")))
                , test "parses 3-number remainder without spaces" <|
                    \_ ->
                        Parser.run arithExp "0％1％2"
                            |> Expect.equal (Result.Ok (Mod (Mod (Lit (NumberVal 0)) (Lit (NumberVal 1))) (Lit (NumberVal 2))))
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
                            |> Expect.equal (Result.Ok (Eq (Var "kosu0") (Var "kosu1")))
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
                            |> Expect.equal (Result.Ok (Neq (Var "kosu0") (Var "kosu1")))
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
                            |> Expect.equal (Result.Ok (Gt (Var "kosu0") (Var "kosu1")))
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
                            |> Expect.equal (Result.Ok (Ge (Var "kosu0") (Var "kosu1")))
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
                            |> Expect.equal (Result.Ok (Le (Var "kosu0") (Var "kosu1")))
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
                            |> Expect.equal (Result.Ok (Lt (Var "kosu0") (Var "kosu1")))
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
        ]
