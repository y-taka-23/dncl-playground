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
import Result.Extra as Result


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
    | InvalidArrayAssignment Variable
    | NonNumericArrayIndex Value
    | IndexOutOfBound Variable
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
                    case assignVar v n ev.variables of
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
                    case assignVar v n ev.variables of
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
                    case assignVar v n ev.variables of
                        Err e ->
                            Err e

                        Ok vs ->
                            Ok <| Continued { ev | continuation = loop :: stmts, variables = vs }


lookupVar : Variable -> Variables -> Result Exception Value
lookupVar v vs =
    case v of
        Scalar x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x vs

        Const x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x vs

        Array x aexps ->
            case ( evalIndices vs aexps, Dict.get x vs ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Nothing ) ->
                    Err <| UndefinedVariable v

                ( Ok idxs, Just arr ) ->
                    Result.fromMaybe (IndexOutOfBound v) <| lookupArray arr idxs


lookupArray : Value -> List Index -> Maybe Value
lookupArray val idxs =
    case ( val, idxs ) of
        ( _, [] ) ->
            Just val

        ( ArrayVal elems, i :: is ) ->
            case Dict.get i elems of
                Nothing ->
                    Nothing

                Just elem ->
                    lookupArray elem is

        ( _, _ :: _ ) ->
            Nothing


assignVar : Variable -> Value -> Variables -> Result Exception Variables
assignVar v val vs =
    case ( v, val ) of
        ( Scalar x, ArrayVal _ ) ->
            Err <| InvalidArrayAssignment v

        ( Scalar x, _ ) ->
            Ok <| Dict.insert x val vs

        ( Const x, ArrayVal _ ) ->
            Err <| InvalidArrayAssignment v

        ( Const x, _ ) ->
            case lookupVar v vs of
                Err (UndefinedVariable _) ->
                    Ok <| Dict.insert x val vs

                Err e ->
                    Err e

                Ok _ ->
                    Err <| ConstReassignment (Const x)

        ( Array x [], ArrayVal _ ) ->
            Ok <| Dict.insert x val vs

        ( Array _ [], _ ) ->
            Err <| InvalidArrayAssignment v

        ( Array x aexps, _ ) ->
            case ( evalIndices vs aexps, lookupVar (Array x []) vs ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e

                ( Ok idxs, Ok root ) ->
                    case assignArray root idxs val of
                        Nothing ->
                            Err <| IndexOutOfBound v

                        Just newRoot ->
                            Ok <| Dict.insert x newRoot vs


assignArray : Value -> List Index -> Value -> Maybe Value
assignArray root idxs newElem =
    case ( root, idxs ) of
        ( _, [] ) ->
            Just newElem

        ( ArrayVal elems, i :: is ) ->
            case Dict.get i elems of
                Nothing ->
                    Nothing

                Just elem ->
                    case assignArray elem is newElem of
                        Nothing ->
                            Nothing

                        Just newRoot ->
                            Just <| ArrayVal <| Dict.insert i newRoot elems

        ( _, _ :: _ ) ->
            Nothing


evalIndices : Variables -> List ArithExp -> Result Exception (List Index)
evalIndices vs aexps =
    case Result.combineMap (evalArith vs) aexps of
        Err e ->
            Err e

        Ok vals ->
            let
                toIndex val =
                    case val of
                        NumberVal n ->
                            Ok n

                        _ ->
                            Err val
            in
            Result.mapError NonNumericArrayIndex <| Result.combineMap toIndex vals


evalArith : Variables -> ArithExp -> Result Exception Value
evalArith vs aexp =
    case aexp of
        Lit val ->
            Ok val

        Var v ->
            lookupVar v vs

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

        Arr es ->
            case Result.combineMap (evalArith vs) es of
                Err e ->
                    Err e

                Ok vals ->
                    Ok <| ArrayVal <| Dict.fromList <| List.indexedMap Tuple.pair vals


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
        concat r1 r2 =
            -- List.Nonempty.foldl1 accumulates items in the reverse order
            Result.map2 (\x y -> y ++ x) r1 r2
    in
    Nonempty.foldl1 concat <| Nonempty.map (formatItem vs) ps


formatItem : Variables -> Printable -> Result Exception String
formatItem vs p =
    case p of
        PrintVal val ->
            Ok <| formatValue val

        PrintVar v ->
            case lookupVar v vs of
                Err e ->
                    Err e

                Ok val ->
                    Ok <| formatValue val


formatValue : Value -> String
formatValue val =
    case val of
        NumberVal n ->
            String.fromInt n

        StringVal s ->
            s

        ArrayVal _ ->
            formatArray <| Just val


formatArray : Maybe Value -> String
formatArray mval =
    case mval of
        Nothing ->
            -- As far as parsed from source code, an array has 0..size-1 indices
            "unreachable"

        Just (NumberVal n) ->
            String.fromInt n

        Just (StringVal s) ->
            "\"" ++ s ++ "\""

        Just (ArrayVal elems) ->
            let
                maxIndex =
                    Maybe.withDefault -1 <| List.maximum <| Dict.keys elems

                filled =
                    List.map (\i -> Dict.get i elems) <| List.range 0 maxIndex

                formatted =
                    List.map formatArray filled
            in
            "{" ++ String.join ", " formatted ++ "}"
