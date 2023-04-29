module DNCL.EvaluatorTest exposing (suite)

import DNCL.AST exposing (..)
import DNCL.Evaluator exposing (..)
import Expect
import Fuzz exposing (Fuzzer, filter, int, pair, string)
import List.Nonempty exposing (Nonempty(..), singleton)
import Test exposing (Test, describe, fuzz, test)


nonzero : Fuzzer Int
nonzero =
    filter (\n -> n /= 0) int


suite : Test
suite =
    describe "The Evaluator module"
        [ describe "run"
            [ test "outputs nothing for the empty program" <|
                \_ ->
                    run []
                        |> Expect.equal (Result.Ok [])
            , describe "assign"
                [ test "assigns a numeric value to a variable" <|
                    \_ ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal 42))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "cannot assign a string value to a variable" <|
                    \_ ->
                        run
                            [ Assign (Variable "x") (Lit (StringVal "Hello")) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , test "assigns values to multiple variables individually" <|
                    \_ ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal 0))
                            , Assign (Variable "y") (Lit (NumberVal 1))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "0" ])
                , test "overrides a variable when it assigns twice" <|
                    \_ ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal 0))
                            , Assign (Variable "x") (Lit (NumberVal 1))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "1" ])
                , fuzz (pair int int) "calculates the addition of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Plus (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair int int) "calculates the addision of a variable and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal n))
                            , Assign (Variable "x") (Plus (Var (Variable "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair int int) "calculates the addision of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal m))
                            , Assign (Variable "x") (Plus (Lit (NumberVal n)) (Var (Variable "y")))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair string int) "throws an exception if the left of addition is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Variable "x") (Plus (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of addition is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Plus (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of addition is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Variable "x") (Plus (Var (Variable y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int string) "throws an exception if the right of addition is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Variable "x") (Plus (Lit (NumberVal n)) (Var (Variable y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int int) "calculates the subtraction of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Minus (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair int int) "calculates the subtraction of a variable and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal n))
                            , Assign (Variable "x") (Minus (Var (Variable "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair int int) "calculates the subtraction of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal m))
                            , Assign (Variable "x") (Minus (Lit (NumberVal n)) (Var (Variable "y")))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair string int) "throws an exception if the left of subtraction is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Variable "x") (Minus (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of subtraction is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Minus (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of subtraction is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Variable "x") (Minus (Var (Variable y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int string) "throws an exception if the right of subtraction is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Variable "x") (Minus (Lit (NumberVal n)) (Var (Variable y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int int) "calculates the multiplication of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Times (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair int int) "calculates the multiplication of a variable and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal n))
                            , Assign (Variable "x") (Times (Var (Variable "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair int int) "calculates the multiplication of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal m))
                            , Assign (Variable "x") (Times (Lit (NumberVal n)) (Var (Variable "y")))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair string int) "throws an exception if the left of multiplication is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Variable "x") (Times (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of multiplication is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Times (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of multiplication is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Variable "x") (Times (Var (Variable y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int string) "throws an exception if the right of multiplication is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Variable "x") (Times (Lit (NumberVal n)) (Var (Variable y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int nonzero) "calculates the quotient of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Quot (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz (pair int nonzero) "calculates the quotient of a variable and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal n))
                            , Assign (Variable "x") (Quot (Var (Variable "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz (pair int nonzero) "calculates the quotient of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal m))
                            , Assign (Variable "x") (Quot (Lit (NumberVal n)) (Var (Variable "y")))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz int "throws an exception if the right of quotient is zero" <|
                    \n ->
                        run
                            [ Assign (Variable "x") (Quot (Lit (NumberVal n)) (Lit (NumberVal 0))) ]
                            |> Expect.equal (Result.Err ZeroDivision)
                , fuzz (pair string int) "throws an exception if the left of quotient is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Variable "x") (Quot (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of quotient is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Quot (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string nonzero) "throws an exception if the left of quotient is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Variable "x") (Quot (Var (Variable y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int string) "throws an exception if the right of quotient is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Variable "x") (Quot (Lit (NumberVal n)) (Var (Variable y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int nonzero) "calculates the remainder of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Mod (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz (pair int nonzero) "calculates the remainder of a variable and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal n))
                            , Assign (Variable "x") (Mod (Var (Variable "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz (pair int nonzero) "calculates the remainder of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "y") (Lit (NumberVal m))
                            , Assign (Variable "x") (Mod (Lit (NumberVal n)) (Var (Variable "y")))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz int "throws an exception if the right of remainder is zero" <|
                    \n ->
                        run
                            [ Assign (Variable "x") (Mod (Lit (NumberVal n)) (Lit (NumberVal 0))) ]
                            |> Expect.equal (Result.Err ZeroDivision)
                , fuzz (pair string int) "throws an exception if the left of remainder is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Variable "x") (Mod (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of remainder is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Mod (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of remainder is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Variable "x") (Mod (Var (Variable y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                , fuzz (pair int string) "throws an exception if the right of remainder is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Variable "x") (Mod (Lit (NumberVal n)) (Var (Variable y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable y)))
                ]
            , describe "print"
                [ test "outputs a number value" <|
                    \_ ->
                        run [ Print (singleton (PrintVal (NumberVal 42))) ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "outputs a string value" <|
                    \_ ->
                        run [ Print (singleton (PrintVal (StringVal "こんにちは、世界"))) ]
                            |> Expect.equal (Result.Ok [ "こんにちは、世界" ])
                , test "outputs a variable" <|
                    \_ ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal 42))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "outputs multiple items by concatination" <|
                    \_ ->
                        run
                            [ Assign (Variable "kosu") (Lit (NumberVal 3))
                            , Print
                                (Nonempty (PrintVar (Variable "kosu"))
                                    [ PrintVal (StringVal " 個見つかった") ]
                                )
                            ]
                            |> Expect.equal (Result.Ok [ "3 個見つかった" ])
                , test "outputs multiple lines in the reverse order" <|
                    \_ ->
                        run
                            [ Print (singleton (PrintVal (NumberVal 0)))
                            , Print (singleton (PrintVal (NumberVal 1)))
                            , Print (singleton (PrintVal (NumberVal 2)))
                            ]
                            |> Expect.equal (Result.Ok [ "2", "1", "0" ])
                , test "throws an exception by an undefined variable" <|
                    \_ ->
                        run
                            [ Print (singleton (PrintVar (Variable "x"))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable "x")))
                , test "throws an exception when one of items is an undefined variable" <|
                    \_ ->
                        run
                            [ Print
                                (Nonempty (PrintVar (Variable "kosu"))
                                    [ PrintVal (StringVal " 個見つかった") ]
                                )
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable "kosu")))
                ]
            , describe "increment"
                [ fuzz (pair int int) "increments a value of the variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal n))
                            , Increment (Variable "x") (Lit (NumberVal m))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz int "throws an exception if the variable is undefined" <|
                    \m ->
                        run
                            [ Increment (Variable "x") (Lit (NumberVal m)) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable "x")))
                , fuzz (pair int string) "throws an exception if the argument is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal n))
                            , Increment (Variable "x") (Lit (StringVal s))
                            ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                ]
            , describe "decrement"
                [ fuzz (pair int int) "decrements a value of the variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal n))
                            , Decrement (Variable "x") (Lit (NumberVal m))
                            , Print (singleton (PrintVar (Variable "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz int "throws an exception if the variable is undefined" <|
                    \m ->
                        run
                            [ Decrement (Variable "x") (Lit (NumberVal m)) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Variable "x")))
                , fuzz (pair int string) "throws an exception if the argument is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Variable "x") (Lit (NumberVal n))
                            , Decrement (Variable "x") (Lit (StringVal s))
                            ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                ]
            ]
        ]
