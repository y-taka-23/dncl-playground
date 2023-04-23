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
                            |> Expect.equal (Result.Ok (Var (Variable "kosu")))
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
                            |> Expect.equal (Result.Ok (Plus (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Minus (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Times (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Quot (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Mod (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Eq (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Neq (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Gt (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Ge (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Le (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Lt (Var (Variable "kosu0")) (Var (Variable "kosu1"))))
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
                            |> Expect.equal (Result.Ok (Assign (Variable "kosu") (Lit (NumberVal 3))))
                , test "parses assignment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu ← 3"
                            |> Expect.equal (Result.Ok (Assign (Variable "kosu") (Lit (NumberVal 3))))
                , test "parses assignment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokuten←kosu×(kosu＋1)"
                            |> Expect.equal
                                (Result.Ok
                                    (Assign
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses assignment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten ← kosu × (kosu ＋ 1)"
                            |> Expect.equal
                                (Result.Ok
                                    (Assign
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                ]
            , describe "print"
                [ test "parses print statement for a Japanese string value without spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print statement for a Japanese string value with spaces" <|
                    \_ ->
                        Parser.run statement "「整いました」 を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "整いました")))))
                , test "parses print statement for an English string value without spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\"を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print statement for an English string value with spaces" <|
                    \_ ->
                        Parser.run statement "\"It was found.\" を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (StringVal "It was found.")))))
                , test "parses print statement for a numeric value without spaces" <|
                    \_ ->
                        Parser.run statement "3を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (NumberVal 3)))))
                , test "parses print statement for a numeric value with spaces" <|
                    \_ ->
                        Parser.run statement "3 を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVal (NumberVal 3)))))
                , test "parses print statement for a variable without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVar (Variable "kosu")))))
                , test "parses print statement for a variable value with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を表示する"
                            |> Expect.equal (Result.Ok (Print (singleton (PrintVar (Variable "kosu")))))
                , test "parses print statement for 2 items" <|
                    \_ ->
                        Parser.run statement "kosu と「個見つかった」を表示する"
                            |> Expect.equal
                                (Result.Ok
                                    (Print
                                        (Nonempty
                                            (PrintVar (Variable "kosu"))
                                            [ PrintVal (StringVal "個見つかった") ]
                                        )
                                    )
                                )
                , test "parses print statement for multiple items" <|
                    \_ ->
                        Parser.run statement "\"(\" と x と \", \" と y と \")\" を表示する"
                            |> Expect.equal
                                (Result.Ok
                                    (Print
                                        (Nonempty
                                            (PrintVal (StringVal "("))
                                            [ PrintVar (Variable "x")
                                            , PrintVal (StringVal ", ")
                                            , PrintVar (Variable "y")
                                            , PrintVal (StringVal ")")
                                            ]
                                        )
                                    )
                                )
                ]
            , describe "increment"
                [ test "parses increment statement without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを1増やす"
                            |> Expect.equal (Result.Ok (Increment (Variable "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を 1 増やす"
                            |> Expect.equal (Result.Ok (Increment (Variable "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokutenをkosu×(kosu＋1)増やす"
                            |> Expect.equal
                                (Result.Ok
                                    (Increment
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses increment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten を kosu × (kosu ＋ 1) 増やす"
                            |> Expect.equal
                                (Result.Ok
                                    (Increment
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                ]
            , describe "decrement"
                [ test "parses increment statement without spaces" <|
                    \_ ->
                        Parser.run statement "kosuを1減らす"
                            |> Expect.equal (Result.Ok (Decrement (Variable "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement with spaces" <|
                    \_ ->
                        Parser.run statement "kosu を 1 減らす"
                            |> Expect.equal (Result.Ok (Decrement (Variable "kosu") (Lit (NumberVal 1))))
                , test "parses increment statement for compound expressions without spaces" <|
                    \_ ->
                        Parser.run statement "tokutenをkosu×(kosu＋1)減らす"
                            |> Expect.equal
                                (Result.Ok
                                    (Decrement
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
                                        )
                                    )
                                )
                , test "parses increment statement for compound expressions with spaces" <|
                    \_ ->
                        Parser.run statement "tokuten を kosu × (kosu ＋ 1) 減らす"
                            |> Expect.equal
                                (Result.Ok
                                    (Decrement
                                        (Variable "tokuten")
                                        (Times (Var (Variable "kosu"))
                                            (Plus (Var (Variable "kosu")) (Lit (NumberVal 1)))
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
                                (Result.Ok (If (Lt (Var (Variable "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (If (Lt (Var (Variable "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば


                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (If (Lt (Var (Variable "x")) (Lit (NumberVal 3))) []))
                , test "parses if statement with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば
                                    x ← x ＋ 1
                                を実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1))) ]
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
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
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
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
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
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
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
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
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
                                    (If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ If (Lt (Var (Variable "y")) (Lit (NumberVal 3))) [] ]
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
                                (Result.Ok (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3))) [] []))
                , test "parses if-else statement with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば

                                を実行し，そうでなければ

                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3))) [] []))
                , test "parses if-else statement with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ もし x ＜ 3 ならば


                                を実行し，そうでなければ


                                を実行する"""
                            |> Expect.equal
                                (Result.Ok (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3))) [] []))
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1))) ]
                                        [ Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1))) ]
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Variable "z") (Plus (Var (Variable "z")) (Lit (NumberVal 1)))
                                        , Assign (Variable "w") (Minus (Var (Variable "w")) (Lit (NumberVal 1)))
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Variable "z") (Plus (Var (Variable "z")) (Lit (NumberVal 1)))
                                        , Assign (Variable "w") (Minus (Var (Variable "w")) (Lit (NumberVal 1)))
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Variable "z") (Plus (Var (Variable "z")) (Lit (NumberVal 1)))
                                        , Assign (Variable "w") (Minus (Var (Variable "w")) (Lit (NumberVal 1)))
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        , Assign (Variable "y") (Minus (Var (Variable "y")) (Lit (NumberVal 1)))
                                        ]
                                        [ Assign (Variable "z") (Plus (Var (Variable "z")) (Lit (NumberVal 1)))
                                        , Assign (Variable "w") (Minus (Var (Variable "w")) (Lit (NumberVal 1)))
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
                                    (IfElse (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                        [ IfElse (Lt (Var (Variable "y")) (Lit (NumberVal 3))) [] [] ]
                                        [ IfElse (Lt (Var (Variable "z")) (Lit (NumberVal 3))) [] [] ]
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
                                (Result.Ok (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with a blank line" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，

                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，


                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10))) []))
                , test "parses pre-check loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ x ＜ 10 の間，
                                    gokei ← gokei ＋ x
                                を繰り返す"""
                            |> Expect.equal
                                (Result.Ok
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x"))) ]
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
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
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
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
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
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
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
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
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
                                    (PreCheckLoop (Lt (Var (Variable "x")) (Lit (NumberVal 10)))
                                        [ PreCheckLoop (Lt (Var (Variable "y")) (Lit (NumberVal 10))) [] ]
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
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Variable "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with a single blank line" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，

                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Variable "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with multiple blank lines" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，


                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok (PostCheckLoop [] (Ge (Var (Variable "x")) (Lit (NumberVal 10)))))
                , test "parses post-check loop with a single statement" <|
                    \_ ->
                        Parser.run statement
                            """ 繰り返し，
                                    gokei ← gokei ＋ x
                                を，x ≧ 10 になるまで実行する"""
                            |> Expect.equal
                                (Result.Ok
                                    (PostCheckLoop
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x"))) ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                                        [ Assign (Variable "gokei") (Plus (Var (Variable "gokei")) (Var (Variable "x")))
                                        , Assign (Variable "x") (Plus (Var (Variable "x")) (Lit (NumberVal 1)))
                                        ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                                        [ PostCheckLoop [] (Ge (Var (Variable "y")) (Lit (NumberVal 10))) ]
                                        (Ge (Var (Variable "x")) (Lit (NumberVal 10)))
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
                        |> Expect.equal (Result.Ok [ Print (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses a single statment with proceding line break" <|
                \_ ->
                    Parser.run dnclProgram
                        """
                            「こんにちは、世界」を表示する"""
                        |> Expect.equal (Result.Ok [ Print (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses a single statement with succeeding line break" <|
                \_ ->
                    Parser.run dnclProgram
                        """ 「こんにちは、世界」を表示する
                        """
                        |> Expect.equal (Result.Ok [ Print (singleton (PrintVal (StringVal "こんにちは、世界"))) ])
            , test "parses multiple line-statements" <|
                \_ ->
                    Parser.run dnclProgram
                        """ x ← 42
                            x を表示する
                        """
                        |> Expect.equal
                            (Result.Ok
                                [ Assign (Variable "x") (Lit (NumberVal 42))
                                , Print (Nonempty (PrintVar (Variable "x")) [])
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
                                [ If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                    [ Assign (Variable "x") (Lit (NumberVal 3)) ]
                                , If (Lt (Var (Variable "y")) (Lit (NumberVal 3)))
                                    [ Assign (Variable "y") (Lit (NumberVal 3)) ]
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
                                [ Assign (Variable "x") (Lit (NumberVal 0))
                                , If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                    [ Assign (Variable "x") (Lit (NumberVal 3)) ]
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
                                [ If (Lt (Var (Variable "x")) (Lit (NumberVal 3)))
                                    [ Assign (Variable "x") (Lit (NumberVal 3)) ]
                                , Print (Nonempty (PrintVar (Variable "x")) [])
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
