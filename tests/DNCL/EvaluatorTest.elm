module DNCL.EvaluatorTest exposing (suite)

import DNCL.AST exposing (..)
import DNCL.Evaluator exposing (..)
import Dict
import Expect
import Fuzz exposing (Fuzzer, filter, int, pair, string)
import List.Nonempty exposing (Nonempty(..), singleton)
import Test exposing (Test, describe, fuzz, test)


nonzero : Fuzzer Int
nonzero =
    filter (\n -> n /= 0) int


run : DNCLProgram -> Result Exception Output
run prog =
    load prog |> eval


eval : Evaluator -> Result Exception Output
eval ev =
    case step ev of
        Err e ->
            Err e

        Ok (Completed end) ->
            Ok end.output

        Ok (Continued next) ->
            eval next


suite : Test
suite =
    describe "The Evaluator module"
        [ describe "step"
            [ test "outputs nothing for the empty program" <|
                \_ ->
                    run []
                        |> Expect.equal (Result.Ok [])
            , describe "assign"
                [ test "assigns a numeric value to a variable" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 42))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "assigns a numeric value to a constant if it is undefined" <|
                    \_ ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 42))
                            , Print (singleton (PrintVar (Const "X")))
                            ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "cannot assign a numeric value to a constant if it is already defined" <|
                    \_ ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 42))
                            , Assign (Const "X") (Lit (NumberVal 42))
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                , test "assigns a string value to a variable" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (StringVal "Hello"))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "Hello" ])
                , test "assigns an array value to an array variable" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{100, 200, 300}" ])
                , test "assigns an element to an array variable" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ 1 ]) (Lit (NumberVal 999))
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{100, 999, 300}" ])
                , test "assigns an array to an array variable" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ 1 ])
                                (Lit (ArrayVal <| Dict.fromList [ ( 0, NumberVal 888 ), ( 1, NumberVal 999 ) ]))
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{100, {888, 999}, 300}" ])
                , test "assigns an element to a 2-dim array variable" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ) ] )
                                        , ( 1, ArrayVal <| Dict.fromList [ ( 0, NumberVal 300 ), ( 1, NumberVal 400 ) ] )
                                        ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ 0, 1 ]) (Lit (NumberVal 999))
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{{100, 999}, {300, 400}}" ])
                , test "ossigns an element of a 1-dim array to a scalar" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Scalar "x") (Var (Array "MyArr" [ 1 ]))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "200" ])
                , test "ossigns an element of a 2-dim array to a scalar" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ) ] )
                                        , ( 1, ArrayVal <| Dict.fromList [ ( 0, NumberVal 300 ), ( 1, NumberVal 400 ) ] )
                                        ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Scalar "x") (Var (Array "MyArr" [ 0, 1 ]))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "200" ])
                , test "throws an exception when the index of the array variable is negative" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Scalar "x") (Var (Array "MyArr" [ -1 ]))
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Array "MyArr" [ -1 ])))
                , test "throws an exception when the index of the array exceeds the bound" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Scalar "x") (Var (Array "MyArr" [ 3 ]))
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Array "MyArr" [ 3 ])))
                , test "ossigns a 1-dim array of expressions" <|
                    \_ ->
                        run
                            [ Assign (Array "MyArr" [])
                                (Arr [ Lit (NumberVal 100), Lit (NumberVal 200), Lit (NumberVal 300) ])
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{100, 200, 300}" ])
                , test "ossigns a 2-dim array of expressions" <|
                    \_ ->
                        run
                            [ Assign (Array "MyArr" [])
                                (Arr
                                    [ Arr [ Lit (NumberVal 100), Lit (NumberVal 200) ]
                                    , Arr [ Lit (NumberVal 300), Lit (NumberVal 400) ]
                                    ]
                                )
                            , Print (singleton (PrintVar (Array "MyArr" [])))
                            ]
                            |> Expect.equal (Result.Ok [ "{{100, 200}, {300, 400}}" ])
                , test "throws an exception when an array is assigned to a scalar variable" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Scalar "x") (Lit arr) ]
                            |> Expect.equal (Result.Err (InvalidArrayAssignment (Scalar "x")))
                , test "throws an exception when an array is assigned to a constant" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Const "X") (Lit arr) ]
                            |> Expect.equal (Result.Err (InvalidArrayAssignment (Const "X")))
                , test "throws an exception when a number is assigned to an array variable" <|
                    \_ ->
                        run
                            [ Assign (Array "MyArr" []) (Lit (NumberVal 0)) ]
                            |> Expect.equal (Result.Err (InvalidArrayAssignment (Array "MyArr" [])))
                , test "throws an exception when a string is assigned to an array variable" <|
                    \_ ->
                        run
                            [ Assign (Array "MyArr" []) (Lit (StringVal "Hello")) ]
                            |> Expect.equal (Result.Err (InvalidArrayAssignment (Array "MyArr" [])))
                , test "throws an exception when the index is negative" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ -1 ]) (Lit (NumberVal 999))
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Array "MyArr" [ -1 ])))
                , test "throws an exception when the index exceeds the bound" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ 3 ]) (Lit (NumberVal 999))
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Array "MyArr" [ 3 ])))
                , test "throws an exception when the index is missing" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ) ] )
                                        , ( 1, ArrayVal <| Dict.fromList [ ( 0, NumberVal 300 ), ( 1, NumberVal 400 ) ] )
                                        ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Assign (Array "MyArr" [ 0, 1 ]) (Lit (NumberVal 999))
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Array "MyArr" [ 0, 1 ])))
                , test "assigns values to multiple variables individually" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , Assign (Scalar "y") (Lit (NumberVal 1))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "0" ])
                , test "overrides a Scalar when it assigns twice" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , Assign (Scalar "x") (Lit (NumberVal 1))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "1" ])
                , test "overrides a number by s string" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , Assign (Scalar "x") (Lit (StringVal "Hello"))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "Hello" ])
                , fuzz (pair int int) "calculates the addition of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair int int) "calculates the addision of a Scalar and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal n))
                            , Assign (Scalar "x") (Plus (Var (Scalar "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair int int) "calculates the addision of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal m))
                            , Assign (Scalar "x") (Plus (Lit (NumberVal n)) (Var (Scalar "y")))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz (pair string string) "throws an exception if the both of addition are strings" <|
                    \( s, t ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Lit (StringVal s)) (Lit (StringVal t))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of addition is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of addition is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of addition is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Var (Scalar y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int string) "throws an exception if the right of addition is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Scalar "x") (Plus (Lit (NumberVal n)) (Var (Scalar y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int int) "calculates the subtraction of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair int int) "calculates the subtraction of a Scalar and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal n))
                            , Assign (Scalar "x") (Minus (Var (Scalar "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair int int) "calculates the subtraction of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal m))
                            , Assign (Scalar "x") (Minus (Lit (NumberVal n)) (Var (Scalar "y")))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz (pair string string) "throws an exception if the both of subtraction are strings" <|
                    \( s, t ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Lit (StringVal s)) (Lit (StringVal t))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of subtraction is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of subtraction is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of subtraction is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Var (Scalar y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int string) "throws an exception if the right of subtraction is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Scalar "x") (Minus (Lit (NumberVal n)) (Var (Scalar y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int int) "calculates the multiplication of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Times (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair int int) "calculates the multiplication of a Scalar and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal n))
                            , Assign (Scalar "x") (Times (Var (Scalar "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair int int) "calculates the multiplication of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal m))
                            , Assign (Scalar "x") (Times (Lit (NumberVal n)) (Var (Scalar "y")))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n * m) ])
                , fuzz (pair string string) "throws an exception if the both of multiplication are strings" <|
                    \( s, t ) ->
                        run
                            [ Assign (Scalar "x") (Times (Lit (StringVal s)) (Lit (StringVal t))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of multiplication is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Scalar "x") (Times (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of multiplication is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Times (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of multiplication is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Scalar "x") (Times (Var (Scalar y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int string) "throws an exception if the right of multiplication is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Scalar "x") (Times (Lit (NumberVal n)) (Var (Scalar y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int nonzero) "calculates the quotient of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz (pair int nonzero) "calculates the quotient of a Scalar and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal n))
                            , Assign (Scalar "x") (Quot (Var (Scalar "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz (pair int nonzero) "calculates the quotient of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal m))
                            , Assign (Scalar "x") (Quot (Lit (NumberVal n)) (Var (Scalar "y")))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n // m) ])
                , fuzz (pair string string) "throws an exception if the both of quotient are strings" <|
                    \( s, t ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (StringVal s)) (Lit (StringVal t))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz int "throws an exception if the right of quotient is zero" <|
                    \n ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (NumberVal n)) (Lit (NumberVal 0))) ]
                            |> Expect.equal (Result.Err ZeroDivision)
                , fuzz (pair string int) "throws an exception if the left of quotient is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of quotient is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string nonzero) "throws an exception if the left of quotient is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Var (Scalar y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int string) "throws an exception if the right of quotient is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Scalar "x") (Quot (Lit (NumberVal n)) (Var (Scalar y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int nonzero) "calculates the remainder of two numbers" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (NumberVal n)) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz (pair int nonzero) "calculates the remainder of a Scalar and a number" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal n))
                            , Assign (Scalar "x") (Mod (Var (Scalar "y")) (Lit (NumberVal m)))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz (pair int nonzero) "calculates the remainder of a number and a variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "y") (Lit (NumberVal m))
                            , Assign (Scalar "x") (Mod (Lit (NumberVal n)) (Var (Scalar "y")))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (modBy m n) ])
                , fuzz (pair string string) "throws an exception if the both of remainder are strings" <|
                    \( s, t ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (StringVal s)) (Lit (StringVal t))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz int "throws an exception if the right of remainder is zero" <|
                    \n ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (NumberVal n)) (Lit (NumberVal 0))) ]
                            |> Expect.equal (Result.Err ZeroDivision)
                , fuzz (pair string int) "throws an exception if the left of remainder is non numeric" <|
                    \( s, n ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (StringVal s)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair int string) "throws an exception if the right of remainder is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (NumberVal n)) (Lit (StringVal s))) ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                , fuzz (pair string int) "throws an exception if the left of remainder is undefined" <|
                    \( y, n ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Var (Scalar y)) (Lit (NumberVal n))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
                , fuzz (pair int string) "throws an exception if the right of remainder is undefined" <|
                    \( n, y ) ->
                        run
                            [ Assign (Scalar "x") (Mod (Lit (NumberVal n)) (Var (Scalar y))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar y)))
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
                , test "outputs the empty array value" <|
                    \_ ->
                        run [ Print (singleton (PrintVal (ArrayVal Dict.empty))) ]
                            |> Expect.equal (Result.Ok [ "{}" ])
                , test "outputs a 1-dim numeric array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run [ Print (singleton (PrintVal arr)) ]
                            |> Expect.equal (Result.Ok [ "{100, 200, 300}" ])
                , test "outputs a 1-dim string array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, StringVal "A" ), ( 1, StringVal "B" ), ( 2, StringVal "C" ) ]
                        in
                        run [ Print (singleton (PrintVal arr)) ]
                            |> Expect.equal (Result.Ok [ "{\"A\", \"B\", \"C\"}" ])
                , test "outputs a 2-dim array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ) ] )
                                        , ( 1, ArrayVal <| Dict.fromList [ ( 0, NumberVal 300 ), ( 1, NumberVal 400 ) ] )
                                        ]
                        in
                        run [ Print (singleton (PrintVal arr)) ]
                            |> Expect.equal (Result.Ok [ "{{100, 200}, {300, 400}}" ])
                , test "outputs a heterogeneous array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, NumberVal 100 )
                                        , ( 1, StringVal "B" )
                                        , ( 2, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ) ] )
                                        ]
                        in
                        run [ Print (singleton (PrintVal arr)) ]
                            |> Expect.equal (Result.Ok [ "{100, \"B\", {100, 200}}" ])
                , test "outputs an array with undefined elements" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 1, NumberVal 100 ), ( 3, NumberVal 200 ) ]
                        in
                        run [ Print (singleton (PrintVal arr)) ]
                            |> Expect.equal (Result.Ok [ "{unreachable, 100, unreachable, 200}" ])
                , test "outputs a variable" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 42))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "42" ])
                , test "outputs an element of a 1-dim array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ), ( 2, NumberVal 300 ) ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Print (singleton (PrintVar (Array "MyArr" [ 1 ])))
                            ]
                            |> Expect.equal (Result.Ok [ "200" ])
                , test "outputs an element of a 2-dim array value" <|
                    \_ ->
                        let
                            arr =
                                ArrayVal <|
                                    Dict.fromList
                                        [ ( 0, ArrayVal <| Dict.fromList [ ( 0, NumberVal 100 ), ( 1, NumberVal 200 ) ] )
                                        , ( 1, ArrayVal <| Dict.fromList [ ( 0, NumberVal 300 ), ( 1, NumberVal 400 ) ] )
                                        ]
                        in
                        run
                            [ Assign (Array "MyArr" []) (Lit arr)
                            , Print (singleton (PrintVar (Array "MyArr" [ 0, 1 ])))
                            ]
                            |> Expect.equal (Result.Ok [ "200" ])
                , test "outputs multiple items by concatination" <|
                    \_ ->
                        run
                            [ Assign (Scalar "kosu") (Lit (NumberVal 3))
                            , Print
                                (Nonempty (PrintVar (Scalar "kosu"))
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
                            [ Print (singleton (PrintVar (Scalar "x"))) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar "x")))
                , test "throws an exception when one of items is an undefined variable" <|
                    \_ ->
                        run
                            [ Print
                                (Nonempty (PrintVar (Scalar "kosu"))
                                    [ PrintVal (StringVal " 個見つかった") ]
                                )
                            ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar "kosu")))
                ]
            , describe "increment"
                [ fuzz (pair int int) "increments a value of the variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal n))
                            , Increment (Scalar "x") (Lit (NumberVal m))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n + m) ])
                , fuzz int "throws an exception if the variable is constant" <|
                    \m ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 0))
                            , Increment (Const "X") (Lit (NumberVal m))
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                , fuzz int "throws an exception if the Scalar is undefined" <|
                    \m ->
                        run
                            [ Increment (Scalar "x") (Lit (NumberVal m)) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar "x")))
                , fuzz (pair int string) "throws an exception if the argument is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal n))
                            , Increment (Scalar "x") (Lit (StringVal s))
                            ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                ]
            , describe "decrement"
                [ fuzz (pair int int) "decrements a value of the variable" <|
                    \( n, m ) ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal n))
                            , Decrement (Scalar "x") (Lit (NumberVal m))
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ String.fromInt (n - m) ])
                , fuzz int "throws an exception if the variable is constant" <|
                    \m ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 0))
                            , Decrement (Const "X") (Lit (NumberVal m))
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                , fuzz int "throws an exception if the Scalar is undefined" <|
                    \m ->
                        run
                            [ Decrement (Scalar "x") (Lit (NumberVal m)) ]
                            |> Expect.equal (Result.Err (UndefinedVariable (Scalar "x")))
                , fuzz (pair int string) "throws an exception if the argument is non numeric" <|
                    \( n, s ) ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal n))
                            , Decrement (Scalar "x") (Lit (StringVal s))
                            ]
                            |> Expect.equal (Result.Err UnsupportedOperation)
                ]
            , describe "if"
                [ describe "eq"
                    [ test "evaluates the then clause if n1 == n2" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluate the then clause if n1 /= n2" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if s1 == s2" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (StringVal "True")) (Lit (StringVal "True")))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluates the then clause if s1 /= s2" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (StringVal "True")) (Lit (StringVal "False")))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "throws an exception if the condition is string == number" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (StringVal "True")) (Lit (NumberVal 0))) []
                                ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    , test "throws an exception if the condition is number == string" <|
                        \_ ->
                            run
                                [ If (Eq (Lit (NumberVal 0)) (Lit (StringVal "True"))) []
                                ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    , test "throws an exception if the left of == is undefined" <|
                        \_ ->
                            run
                                [ If (Eq (Var (Scalar "x")) (Lit (StringVal "True"))) []
                                ]
                                |> Expect.equal (Result.Err (UndefinedVariable (Scalar "x")))
                    ]
                , describe "neq"
                    [ test "doesn't evaluate the then clause if n1 == n2" <|
                        \_ ->
                            run
                                [ If (Neq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if n1 /= n2" <|
                        \_ ->
                            run
                                [ If (Neq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluate the then clause if s1 == s2" <|
                        \_ ->
                            run
                                [ If (Neq (Lit (StringVal "True")) (Lit (StringVal "True")))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if s1 /= s2" <|
                        \_ ->
                            run
                                [ If (Neq (Lit (StringVal "True")) (Lit (StringVal "False")))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    ]
                , describe "gt"
                    [ test "doesn't evaluate the then clause if e1 == e2" <|
                        \_ ->
                            run
                                [ If (Gt (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if e1 > e2" <|
                        \_ ->
                            run
                                [ If (Gt (Lit (NumberVal 1)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluate the then clause if e1 < e2" <|
                        \_ ->
                            run
                                [ If (Gt (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "throws an exception if the both of innequation are strings" <|
                        \_ ->
                            run
                                [ If (Gt (Lit (StringVal "True")) (Lit (StringVal "True"))) [] ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    ]
                , describe "ge"
                    [ test "evaluates the then clause if e1 == e2" <|
                        \_ ->
                            run
                                [ If (Ge (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "evaluates the then clause if e1 > e2" <|
                        \_ ->
                            run
                                [ If (Ge (Lit (NumberVal 1)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluate the then clause if e1 < e2" <|
                        \_ ->
                            run
                                [ If (Ge (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "throws an exception if the both of innequation are strings" <|
                        \_ ->
                            run
                                [ If (Ge (Lit (StringVal "True")) (Lit (StringVal "True"))) [] ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    ]
                , describe "le"
                    [ test "evaluates the then clause if e1 == e2" <|
                        \_ ->
                            run
                                [ If (Le (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doesn't evaluate the then clause if e1 > e2" <|
                        \_ ->
                            run
                                [ If (Le (Lit (NumberVal 1)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if e1 < e2" <|
                        \_ ->
                            run
                                [ If (Le (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "throws an exception if the both of innequation are strings" <|
                        \_ ->
                            run
                                [ If (Le (Lit (StringVal "True")) (Lit (StringVal "True"))) [] ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    ]
                , describe "lt"
                    [ test "doens't evaluate the then clause if e1 == e2" <|
                        \_ ->
                            run
                                [ If (Lt (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "doesn't evaluate the then clause if e1 > e2" <|
                        \_ ->
                            run
                                [ If (Lt (Lit (NumberVal 1)) (Lit (NumberVal 0)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if e1 < e2" <|
                        \_ ->
                            run
                                [ If (Lt (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "throws an exception if the both of innequation are strings" <|
                        \_ ->
                            run
                                [ If (Lt (Lit (StringVal "True")) (Lit (StringVal "True"))) [] ]
                                |> Expect.equal (Result.Err UnsupportedOperation)
                    ]
                , describe "and"
                    [ test "evaluates the then clause if True && True" <|
                        \_ ->
                            run
                                [ If
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doens't evaluate the then clause if False && True" <|
                        \_ ->
                            run
                                [ If
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "doens't evaluate the then clause if True && False" <|
                        \_ ->
                            run
                                [ If
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "doens't evaluate the then clause if False && False" <|
                        \_ ->
                            run
                                [ If
                                    (And
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    ]
                , describe "or"
                    [ test "evaluates the then clause if True || True" <|
                        \_ ->
                            run
                                [ If
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "evaluates the then clause if False || True" <|
                        \_ ->
                            run
                                [ If
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "evaluates the then clause if True || False" <|
                        \_ ->
                            run
                                [ If
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    , test "doens't evaluate the then clause if False || False" <|
                        \_ ->
                            run
                                [ If
                                    (Or
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                        (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                    )
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    ]
                , describe "not"
                    [ test "doesn't evaluate the then clause if the original condition holds" <|
                        \_ ->
                            run
                                [ If
                                    (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0))))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [])
                    , test "evaluates the then clause if the original condition doesn' hold" <|
                        \_ ->
                            run
                                [ If
                                    (Not (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1))))
                                    [ Print (singleton (PrintVal (StringVal "True"))) ]
                                ]
                                |> Expect.equal (Result.Ok [ "True" ])
                    ]
                ]
            , describe "if-else"
                [ test "evaluate the then clause if the guard condition holds" <|
                    \_ ->
                        run
                            [ IfElse (Eq (Lit (NumberVal 0)) (Lit (NumberVal 0)))
                                [ Print (singleton (PrintVal (StringVal "True"))) ]
                                [ Print (singleton (PrintVal (StringVal "False"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "True" ])
                , test "evaluates the else clause if the guard doesn' hold" <|
                    \_ ->
                        run
                            [ IfElse (Eq (Lit (NumberVal 0)) (Lit (NumberVal 1)))
                                [ Print (singleton (PrintVal (StringVal "True"))) ]
                                [ Print (singleton (PrintVal (StringVal "False"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "False" ])
                ]
            , describe "pre-check loop"
                [ test "doen't evaluate the loop contents if the condition initially doesn't hold" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 0)))
                                [ Print (singleton (PrintVal (StringVal "Loop")))
                                , Increment (Scalar "x") (Lit (NumberVal 1))
                                ]
                            ]
                            |> Expect.equal (Result.Ok [])
                , test "evaluates the loop contents only once" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 1)))
                                [ Print (singleton (PrintVal (StringVal "Loop")))
                                , Increment (Scalar "x") (Lit (NumberVal 1))
                                ]
                            ]
                            |> Expect.equal (Result.Ok [ "Loop" ])
                , test "evaluates the loop contents multiple times" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PreCheckLoop (Lt (Var (Scalar "x")) (Lit (NumberVal 3)))
                                [ Print (singleton (PrintVal (StringVal "Loop")))
                                , Increment (Scalar "x") (Lit (NumberVal 1))
                                ]
                            ]
                            |> Expect.equal (Result.Ok [ "Loop", "Loop", "Loop" ])
                ]
            , describe "post-check loop"
                [ test "evaluates the loop contents even if the condition initially doesn't hold" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PostCheckLoop
                                [ Print (singleton (PrintVal (StringVal "Loop"))) ]
                                (Lt (Var (Scalar "x")) (Lit (NumberVal 0)))
                            ]
                            |> Expect.equal (Result.Ok [ "Loop" ])
                , test "evaluates the loop contents only twice" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PostCheckLoop
                                [ Print (singleton (PrintVal (StringVal "Loop")))
                                , Increment (Scalar "x") (Lit (NumberVal 1))
                                ]
                                (Lt (Var (Scalar "x")) (Lit (NumberVal 2)))
                            ]
                            |> Expect.equal (Result.Ok [ "Loop", "Loop" ])
                , test "evaluates the loop contents multiple times" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 0))
                            , PostCheckLoop
                                [ Print (singleton (PrintVal (StringVal "Loop")))
                                , Increment (Scalar "x") (Lit (NumberVal 1))
                                ]
                                (Lt (Var (Scalar "x")) (Lit (NumberVal 4)))
                            ]
                            |> Expect.equal (Result.Ok [ "Loop", "Loop", "Loop", "Loop" ])
                ]
            , describe "increment loop"
                [ test "evaluates the loop contents during the condition holds" <|
                    \_ ->
                        run
                            [ IncrementLoop (Scalar "x")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 4))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "4", "3", "2", "1", "0" ])
                , test "doen't evaluate the loop contents if the condition initially doesn't hold" <|
                    \_ ->
                        run
                            [ IncrementLoop (Scalar "x")
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [])
                , test "destructs an already defined Scalar as a counter" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 42))
                            , IncrementLoop (Scalar "x")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "0" ])
                , test "leaves the counter Scalar after the loop" <|
                    \_ ->
                        run
                            [ IncrementLoop (Scalar "x")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 4))
                                (Lit (NumberVal 1))
                                []
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "5" ])
                , test "evaluates the loop if the counter is a constant but undefined" <|
                    \_ ->
                        run
                            [ IncrementLoop (Const "X")
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Const "X"))) ]
                            ]
                            |> Expect.equal (Result.Ok [])
                , test "throws an exception if the counter is a pre-defined constant" <|
                    \_ ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 0))
                            , IncrementLoop (Const "X")
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                []
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                , test "throws an exception if the counter is constant and updated" <|
                    \_ ->
                        run
                            [ IncrementLoop (Const "X")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                []
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                ]
            , describe "decrement loop"
                [ test "evaluates the loop contents during the condition holds" <|
                    \_ ->
                        run
                            [ DecrementLoop (Scalar "x")
                                (Lit (NumberVal 4))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "0", "1", "2", "3", "4" ])
                , test "doen't evaluate the loop contents if the condition initially doesn't hold" <|
                    \_ ->
                        run
                            [ DecrementLoop (Scalar "x")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [])
                , test "destructs an already defined Scalar as a counter" <|
                    \_ ->
                        run
                            [ Assign (Scalar "x") (Lit (NumberVal 42))
                            , DecrementLoop (Scalar "x")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Scalar "x"))) ]
                            ]
                            |> Expect.equal (Result.Ok [ "0" ])
                , test "leaves the counter Scalar after the loop" <|
                    \_ ->
                        run
                            [ DecrementLoop (Scalar "x")
                                (Lit (NumberVal 4))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                []
                            , Print (singleton (PrintVar (Scalar "x")))
                            ]
                            |> Expect.equal (Result.Ok [ "-1" ])
                , test "evaluates the loop if the counter is a constant but undefined" <|
                    \_ ->
                        run
                            [ DecrementLoop (Const "X")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 1))
                                [ Print (singleton (PrintVar (Const "X"))) ]
                            ]
                            |> Expect.equal (Result.Ok [])
                , test "throws an exception if the counter is a pre-defined constant" <|
                    \_ ->
                        run
                            [ Assign (Const "X") (Lit (NumberVal 0))
                            , DecrementLoop (Const "X")
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 1))
                                []
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                , test "throws an exception if the counter is constant and updated" <|
                    \_ ->
                        run
                            [ DecrementLoop (Const "X")
                                (Lit (NumberVal 1))
                                (Lit (NumberVal 0))
                                (Lit (NumberVal 1))
                                []
                            ]
                            |> Expect.equal (Result.Err (ConstReassignment (Const "X")))
                ]
            ]
        ]
