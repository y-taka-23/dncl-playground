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
    Dict Name Value


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
            let
                stmt =
                    Assign v (Plus (Var v) aexp)
            in
            Ok <| Continued { ev | continuation = stmt :: stmts }

        (Decrement v aexp) :: stmts ->
            let
                stmt =
                    Assign v (Minus (Var v) aexp)
            in
            Ok <| Continued { ev | continuation = stmt :: stmts }

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


refVariable : Variable -> Variables -> Result Exception Value
refVariable v vs =
    case v of
        Scalar x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x vs

        Const x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x vs


assignVariable : Variable -> Value -> Variables -> Result Exception Variables
assignVariable v val vs =
    case v of
        Scalar x ->
            Ok <| Dict.insert x val vs

        Const x ->
            case refVariable v vs of
                Err (UndefinedVariable _) ->
                    Ok <| Dict.insert x val vs

                Err e ->
                    Err e

                Ok _ ->
                    Err <| ConstReassignment (Const x)


evalArith : Variables -> ArithExp -> Result Exception Value
evalArith vs aexp =
    case aexp of
        Lit val ->
            Ok val

        Var v ->
            refVariable v vs

        Plus e1 e2 ->
            Result.map NumberVal <| opNums (+) (evalArith vs e1) (evalArith vs e2)

        Minus e1 e2 ->
            Result.map NumberVal <| opNums (-) (evalArith vs e1) (evalArith vs e2)

        Times e1 e2 ->
            Result.map NumberVal <| opNums (*) (evalArith vs e1) (evalArith vs e2)

        Quot e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok (NumberVal _), Ok (NumberVal 0) ) ->
                    Err ZeroDivision

                ( Ok (NumberVal n), Ok (NumberVal m) ) ->
                    Ok <| NumberVal <| n // m

                ( Ok _, Ok _ ) ->
                    Err UnsupportedOperation

        Mod e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok (NumberVal _), Ok (NumberVal 0) ) ->
                    Err ZeroDivision

                ( Ok (NumberVal n), Ok (NumberVal m) ) ->
                    Ok <| NumberVal <| modBy m n

                ( Ok _, Ok _ ) ->
                    Err UnsupportedOperation


evalBool : Variables -> BoolExp -> Result Exception Bool
evalBool vs bexp =
    case bexp of
        Eq e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok (NumberVal n), Ok (NumberVal m) ) ->
                    Ok <| n == m

                ( Ok (StringVal s1), Ok (StringVal s2) ) ->
                    Ok <| s1 == s2

                ( Ok _, Ok _ ) ->
                    Err UnsupportedOperation

        Neq e1 e2 ->
            case ( evalArith vs e1, evalArith vs e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok _, Err e ) ->
                    Err e

                ( Ok (NumberVal n), Ok (NumberVal m) ) ->
                    Ok <| n /= m

                ( Ok (StringVal s1), Ok (StringVal s2) ) ->
                    Ok <| s1 /= s2

                ( Ok _, Ok _ ) ->
                    Err UnsupportedOperation

        Gt e1 e2 ->
            opNums (>) (evalArith vs e1) (evalArith vs e2)

        Ge e1 e2 ->
            opNums (>=) (evalArith vs e1) (evalArith vs e2)

        Le e1 e2 ->
            opNums (<=) (evalArith vs e1) (evalArith vs e2)

        Lt e1 e2 ->
            opNums (<) (evalArith vs e1) (evalArith vs e2)

        And e1 e2 ->
            Result.map2 (&&) (evalBool vs e1) (evalBool vs e2)

        Or e1 e2 ->
            Result.map2 (||) (evalBool vs e1) (evalBool vs e2)

        Not e ->
            Result.map not (evalBool vs e)


opNums : (Int -> Int -> a) -> Result Exception Value -> Result Exception Value -> Result Exception a
opNums op r1 r2 =
    case ( r1, r2 ) of
        ( Err e, _ ) ->
            Err e

        ( Ok _, Err e ) ->
            Err e

        ( Ok (NumberVal n), Ok (NumberVal m) ) ->
            Ok <| op n m

        ( Ok _, Ok _ ) ->
            -- TODO: The new style of DNCL supports string concatination
            Err UnsupportedOperation


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

                        Ok (NumberVal n) ->
                            Ok (String.fromInt n)

                        Ok (StringVal s) ->
                            Ok s

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
