module DNCL.Evaluator exposing
    ( Evaluator
    , Exception(..)
    , Output
    , StepResult(..)
    , load
    , step
    )

import DNCL.AST exposing (..)
import Dict exposing (Dict)
import List.Nonempty as Nonempty exposing (Nonempty)


type alias Evaluator =
    { continuation : DNCLProgram
    , variables : Variables
    , output : Output
    }


type alias Variables =
    Dict Name Int


type alias Output =
    List String


type StepResult
    = Completed Evaluator
    | Continued Evaluator


type Exception
    = UndefinedVariable Variable
    | ConstReassignment Variable
    | ZeroDivision
    | UnsupportedOperation


load : DNCLProgram -> Evaluator
load prog =
    { continuation = prog
    , variables = Dict.empty
    , output = []
    }


step : Evaluator -> Result Exception StepResult
step ev =
    case ev.continuation of
        [] ->
            Ok <| Completed ev

        (Assign v aexp) :: stmts ->
            case evalArith ev.variables aexp of
                Err e ->
                    Err e

                Ok n ->
                    case assignVariable v n ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = stmts, variables = vs }

        (Print ps) :: stmts ->
            case format ev.variables ps of
                Err e ->
                    Err e

                Ok s ->
                    let
                        out =
                            s :: ev.output
                    in
                    Ok <| Continued { ev | continuation = stmts, output = out }

        (Increment v aexp) :: stmts ->
            case ( refVariable v ev.variables, evalArith ev.variables aexp ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e

                ( Ok n, Ok m ) ->
                    case assignVariable v (n + m) ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = stmts, variables = vs }

        (Decrement v aexp) :: stmts ->
            case ( refVariable v ev.variables, evalArith ev.variables aexp ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e

                ( Ok n, Ok m ) ->
                    case assignVariable v (n - m) ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = stmts, variables = vs }

        -- TODO: List concatination looks show
        (If bexp thenStmts) :: stmts ->
            case evalBool ev.variables bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <| Continued { ev | continuation = thenStmts ++ stmts }

                Ok False ->
                    Ok <| Continued { ev | continuation = stmts }

        (IfElse bexp thenStmts elseStmts) :: stmts ->
            case evalBool ev.variables bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <| Continued { ev | continuation = thenStmts ++ stmts }

                Ok False ->
                    Ok <| Continued { ev | continuation = elseStmts ++ stmts }

        -- TODO: Handle the infinite loop
        (PreCheckLoop bexp loopStmts) :: stmts ->
            case evalBool ev.variables bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <| Continued { ev | continuation = loopStmts ++ PreCheckLoop bexp loopStmts :: stmts }

                Ok False ->
                    Ok <| Continued { ev | continuation = stmts }

        (PostCheckLoop loopStmts bexp) :: stmts ->
            Ok <| Continued { ev | continuation = loopStmts ++ PreCheckLoop bexp loopStmts :: stmts }

        (IncrementLoop v from to diff loopStmts) :: stmts ->
            case evalArith ev.variables from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        bexp =
                            Le (Var v) to

                        loop =
                            PreCheckLoop bexp (loopStmts ++ [ Increment v diff ])
                    in
                    case assignVariable v n ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = loop :: stmts, variables = vs }

        (DecrementLoop v from to diff loopStmts) :: stmts ->
            case evalArith ev.variables from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        bexp =
                            Ge (Var v) to

                        loop =
                            PreCheckLoop bexp (loopStmts ++ [ Decrement v diff ])
                    in
                    case assignVariable v n ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = loop :: stmts, variables = vs }


refVariable : Variable -> Variables -> Result Exception Int
refVariable v vs =
    case v of
        Scalar x ->
            case Dict.get x vs of
                Nothing ->
                    Err <| UndefinedVariable <| Scalar x

                Just n ->
                    Ok n

        Const x ->
            case Dict.get x vs of
                Nothing ->
                    Err <| UndefinedVariable <| Const x

                Just n ->
                    Ok n


assignVariable : Variable -> Int -> Variables -> Result Exception Variables
assignVariable v n vs =
    case v of
        Scalar x ->
            Ok <| Dict.insert x n vs

        Const x ->
            case refVariable v vs of
                Err (UndefinedVariable _) ->
                    Ok <| Dict.insert x n vs

                Err e ->
                    Err e

                Ok _ ->
                    Err <| ConstReassignment (Const x)


evalArith : Variables -> ArithExp -> Result Exception Int
evalArith vs aexp =
    case aexp of
        Lit (NumberVal n) ->
            Ok n

        -- TODO: The new style DNCL has string concatination
        Lit (StringVal _) ->
            Err UnsupportedOperation

        Var v ->
            refVariable v vs

        Plus e1 e2 ->
            Result.map2 (+) (evalArith vs e1) (evalArith vs e2)

        Minus e1 e2 ->
            Result.map2 (-) (evalArith vs e1) (evalArith vs e2)

        Times e1 e2 ->
            Result.map2 (*) (evalArith vs e1) (evalArith vs e2)

        Quot e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok _, Ok 0 ) ->
                    Err ZeroDivision

                ( Ok n, Ok m ) ->
                    Ok (n // m)

        Mod e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok _, Ok 0 ) ->
                    Err ZeroDivision

                ( Ok n, Ok m ) ->
                    Ok (modBy m n)


evalBool : Variables -> BoolExp -> Result Exception Bool
evalBool vs bexp =
    case bexp of
        Eq e1 e2 ->
            Result.map2 (==) (evalArith vs e1) (evalArith vs e2)

        Neq e1 e2 ->
            Result.map2 (/=) (evalArith vs e1) (evalArith vs e2)

        Gt e1 e2 ->
            Result.map2 (>) (evalArith vs e1) (evalArith vs e2)

        Ge e1 e2 ->
            Result.map2 (>=) (evalArith vs e1) (evalArith vs e2)

        Le e1 e2 ->
            Result.map2 (<=) (evalArith vs e1) (evalArith vs e2)

        Lt e1 e2 ->
            Result.map2 (<) (evalArith vs e1) (evalArith vs e2)

        And e1 e2 ->
            Result.map2 (&&) (evalBool vs e1) (evalBool vs e2)

        Or e1 e2 ->
            Result.map2 (||) (evalBool vs e1) (evalBool vs e2)

        Not e ->
            Result.map not (evalBool vs e)


format : Variables -> Nonempty Printable -> Result Exception String
format vs ps =
    let
        toString p =
            case p of
                PrintVal (NumberVal n) ->
                    Ok (String.fromInt n)

                PrintVal (StringVal s) ->
                    Ok s

                PrintVar v ->
                    case refVariable v vs of
                        Err e ->
                            Err e

                        Ok n ->
                            Ok (String.fromInt n)

        concat r1 r2 =
            case ( r1, r2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                -- List.Nonempty.foldl1 accumulates items in the reverse order
                ( Ok x, Ok y ) ->
                    Ok (y ++ x)
    in
    Nonempty.foldl1 concat <| Nonempty.map toString ps
