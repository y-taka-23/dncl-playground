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

        (Assign (Variable x) aexp) :: stmts ->
            case evalArith ev.variables aexp of
                Err e ->
                    Err e

                Ok n ->
                    let
                        vs =
                            Dict.insert x n ev.variables
                    in
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

        (Increment (Variable x) aexp) :: stmts ->
            case ( Dict.get x ev.variables, evalArith ev.variables aexp ) of
                ( Nothing, _ ) ->
                    Err (UndefinedVariable (Variable x))

                ( Just _, Err e ) ->
                    Err e

                ( Just n, Ok m ) ->
                    let
                        vs =
                            Dict.insert x (n + m) ev.variables
                    in
                    Ok <| Continued { ev | continuation = stmts, variables = vs }

        (Decrement (Variable x) aexp) :: stmts ->
            case ( Dict.get x ev.variables, evalArith ev.variables aexp ) of
                ( Nothing, _ ) ->
                    Err (UndefinedVariable (Variable x))

                ( Just _, Err e ) ->
                    Err e

                ( Just n, Ok m ) ->
                    let
                        vs =
                            Dict.insert x (n - m) ev.variables
                    in
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

        (IncrementLoop (Variable x) from to diff loopStmts) :: stmts ->
            case evalArith ev.variables from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        vs =
                            Dict.insert x n ev.variables

                        bexp =
                            Le (Var (Variable x)) to

                        loop =
                            PreCheckLoop bexp (loopStmts ++ [ Increment (Variable x) diff ])
                    in
                    Ok <| Continued { ev | continuation = loop :: stmts, variables = vs }

        (DecrementLoop (Variable x) from to diff loopStmts) :: stmts ->
            case evalArith ev.variables from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        vs =
                            Dict.insert x n ev.variables

                        bexp =
                            Ge (Var (Variable x)) to

                        loop =
                            PreCheckLoop bexp (loopStmts ++ [ Decrement (Variable x) diff ])
                    in
                    Ok <| Continued { ev | continuation = loop :: stmts, variables = vs }


evalArith : Variables -> ArithExp -> Result Exception Int
evalArith vs aexp =
    case aexp of
        Lit (NumberVal n) ->
            Ok n

        -- TODO: The new style DNCL has string concatination
        Lit (StringVal _) ->
            Err UnsupportedOperation

        Var (Variable x) ->
            case Dict.get x vs of
                Nothing ->
                    Err (UndefinedVariable (Variable x))

                Just n ->
                    Ok n

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

                PrintVar (Variable x) ->
                    case Dict.get x vs of
                        Just n ->
                            Ok (String.fromInt n)

                        Nothing ->
                            Err (UndefinedVariable (Variable x))

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
