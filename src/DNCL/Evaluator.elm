module DNCL.Evaluator exposing
    ( Evaluator
    , Exception(..)
    , StepResult(..)
    , flushBuffer
    , load
    , step
    )

import DNCL.AST exposing (..)
import Debug
import Dict exposing (Dict)
import List.Nonempty as Nonempty exposing (Nonempty)
import Result.Extra as Result


type alias Evaluator =
    { callStack : List DNCLProgram
    , symbolTable : SymbolTable
    , output : Buffer
    }


type alias SymbolTable =
    { variables : Variables
    , functions : Functions
    }


type alias Variables =
    Dict Name Value


updateVars : SymbolTable -> Variables -> SymbolTable
updateVars st vs =
    { st | variables = vs }


type alias Functions =
    Dict Name FunctionDef


type alias FunctionDef =
    List Value -> Result Exception Value


type alias Buffer =
    { buffer : String
    , lines : List String
    }


flushBuffer : Buffer -> List String
flushBuffer buf =
    if String.isEmpty buf.buffer then
        buf.lines

    else
        buf.buffer :: buf.lines


type StepResult
    = Completed Evaluator
    | Continued Evaluator


type Exception
    = UndefinedVariable Variable
    | UndefinedFunction Function
    | ConstReassignment Variable
    | InvalidArrayAssignment Variable
    | NonNumericArrayIndex Value
    | NegativeArrayIndex Variable
    | ZeroDivision
    | UnsupportedOperation
    | InvalidArgument (List Value)
    | UnreachableBranch String


load : DNCLProgram -> Evaluator
load prog =
    { callStack = [ prog ]
    , symbolTable =
        { variables = Dict.empty
        , functions = builtinFuns
        }
    , output = { buffer = "", lines = [] }
    }


step : Evaluator -> Result Exception StepResult
step ev =
    case ev.callStack of
        [] ->
            Ok <| Completed ev

        [] :: cont ->
            Ok <| Continued { ev | callStack = cont }

        ((Stmt (Assign v aexp)) :: snips) :: cont ->
            case evalArith ev.symbolTable aexp of
                Err e ->
                    Err e

                Ok n ->
                    case assignVar ev.symbolTable v n of
                        Err e ->
                            Err e

                        Ok vs ->
                            let
                                st =
                                    updateVars ev.symbolTable vs
                            in
                            Ok <| Continued { ev | callStack = snips :: cont, symbolTable = st }

        ((Stmt (PrintLn ps)) :: snips) :: cont ->
            case format ev.symbolTable ps of
                Err e ->
                    Err e

                Ok s ->
                    let
                        out =
                            { buffer = "", lines = (ev.output.buffer ++ s) :: ev.output.lines }
                    in
                    Ok <| Continued { ev | callStack = snips :: cont, output = out }

        ((Stmt (Print ps)) :: snips) :: cont ->
            case format ev.symbolTable ps of
                Err e ->
                    Err e

                Ok s ->
                    let
                        out =
                            { buffer = ev.output.buffer ++ s, lines = ev.output.lines }
                    in
                    Ok <| Continued { ev | callStack = snips :: cont, output = out }

        ((Stmt PrintNewLine) :: snips) :: cont ->
            let
                out =
                    { buffer = "", lines = ev.output.buffer :: ev.output.lines }
            in
            Ok <| Continued { ev | callStack = snips :: cont, output = out }

        ((Stmt (Increment v aexp)) :: snips) :: cont ->
            let
                stmt =
                    Stmt <| Assign v (Plus (Var v) aexp)
            in
            Ok <| Continued { ev | callStack = (stmt :: snips) :: cont }

        ((Stmt (Decrement v aexp)) :: snips) :: cont ->
            let
                stmt =
                    Stmt <| Assign v (Minus (Var v) aexp)
            in
            Ok <| Continued { ev | callStack = (stmt :: snips) :: cont }

        -- TODO: List concatination looks show
        ((Stmt (If bexp thenStmts)) :: snips) :: cont ->
            case evalBool ev.symbolTable bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <| Continued { ev | callStack = (List.map Stmt thenStmts ++ snips) :: cont }

                Ok False ->
                    Ok <| Continued { ev | callStack = snips :: cont }

        ((Stmt (IfElse bexp thenStmts elseStmts)) :: snips) :: cont ->
            case evalBool ev.symbolTable bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <| Continued { ev | callStack = (List.map Stmt thenStmts ++ snips) :: cont }

                Ok False ->
                    Ok <| Continued { ev | callStack = (List.map Stmt elseStmts ++ snips) :: cont }

        ((Stmt (PreCheckLoop bexp loopStmts)) :: snips) :: cont ->
            case evalBool ev.symbolTable bexp of
                Err e ->
                    Err e

                Ok True ->
                    Ok <|
                        Continued
                            { ev
                                | callStack =
                                    (List.map Stmt loopStmts ++ Stmt (PreCheckLoop bexp loopStmts) :: snips) :: cont
                            }

                Ok False ->
                    Ok <| Continued { ev | callStack = snips :: cont }

        ((Stmt (PostCheckLoop loopStmts bexp)) :: snips) :: cont ->
            Ok <|
                Continued
                    { ev
                        | callStack =
                            (List.map Stmt loopStmts ++ Stmt (PreCheckLoop bexp loopStmts) :: snips) :: cont
                    }

        ((Stmt (IncrementLoop v from to diff loopStmts)) :: snips) :: cont ->
            case evalArith ev.symbolTable from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        bexp =
                            Le (Var v) to

                        loop =
                            Stmt <| PreCheckLoop bexp (loopStmts ++ [ Increment v diff ])
                    in
                    case assignVar ev.symbolTable v n of
                        Err e ->
                            Err e

                        Ok vs ->
                            let
                                st =
                                    updateVars ev.symbolTable vs
                            in
                            Ok <| Continued { ev | callStack = (loop :: snips) :: cont, symbolTable = st }

        ((Stmt (DecrementLoop v from to diff loopStmts)) :: snips) :: cont ->
            case evalArith ev.symbolTable from of
                Err e ->
                    Err e

                Ok n ->
                    let
                        bexp =
                            Ge (Var v) to

                        loop =
                            Stmt <| PreCheckLoop bexp (loopStmts ++ [ Decrement v diff ])
                    in
                    case assignVar ev.symbolTable v n of
                        Err e ->
                            Err e

                        Ok vs ->
                            let
                                st =
                                    updateVars ev.symbolTable vs
                            in
                            Ok <| Continued { ev | callStack = (loop :: snips) :: cont, symbolTable = st }

        ((FunDecl decl) :: snips) :: cont ->
            Debug.todo "Function Declaration"


lookupVar : SymbolTable -> Variable -> Result Exception Value
lookupVar st v =
    case v of
        Scalar x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x st.variables

        Const x ->
            Result.fromMaybe (UndefinedVariable v) <| Dict.get x st.variables

        Array x aexps ->
            case ( evalIndices st aexps, Dict.get x st.variables ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Nothing ) ->
                    Err <| UndefinedVariable v

                ( Ok idxs, Just arr ) ->
                    lookupArray v arr idxs


lookupArray : Variable -> Value -> List Index -> Result Exception Value
lookupArray v val idxs =
    case ( val, idxs ) of
        ( _, [] ) ->
            Ok val

        ( ArrayVal elems, i :: is ) ->
            if i < 0 then
                Err <| NegativeArrayIndex v

            else
                case Dict.get i elems of
                    Nothing ->
                        Err <| UndefinedVariable v

                    Just elem ->
                        lookupArray v elem is

        ( _, _ :: _ ) ->
            Err <| UnreachableBranch "lookupArray"


assignVar : SymbolTable -> Variable -> Value -> Result Exception Variables
assignVar st v val =
    case ( v, val ) of
        ( Scalar x, ArrayVal _ ) ->
            Err <| InvalidArrayAssignment v

        ( Scalar x, _ ) ->
            Ok <| Dict.insert x val st.variables

        ( Const x, ArrayVal _ ) ->
            Err <| InvalidArrayAssignment v

        ( Const x, _ ) ->
            case lookupVar st v of
                Err (UndefinedVariable _) ->
                    Ok <| Dict.insert x val st.variables

                Err e ->
                    Err e

                Ok _ ->
                    Err <| ConstReassignment (Const x)

        ( Array x [], ArrayVal _ ) ->
            Ok <| Dict.insert x val st.variables

        ( Array _ [], _ ) ->
            Err <| InvalidArrayAssignment v

        ( Array x aexps, _ ) ->
            case ( evalIndices st aexps, lookupVar st (Array x []) ) of
                ( Err e, _ ) ->
                    Err e

                ( Ok idxs, Err (UndefinedVariable _) ) ->
                    case assignArray v (ArrayVal Dict.empty) idxs val of
                        Err e ->
                            Err e

                        Ok newRoot ->
                            Ok <| Dict.insert x newRoot st.variables

                ( Ok _, Err e ) ->
                    Err e

                ( Ok idxs, Ok root ) ->
                    case assignArray v root idxs val of
                        Err e ->
                            Err e

                        Ok newRoot ->
                            Ok <| Dict.insert x newRoot st.variables


assignArray : Variable -> Value -> List Index -> Value -> Result Exception Value
assignArray v root idxs newElem =
    case ( root, idxs ) of
        ( _, [] ) ->
            Ok newElem

        ( ArrayVal elems, i :: is ) ->
            if i < 0 then
                Err <| NegativeArrayIndex v

            else
                let
                    child =
                        Maybe.withDefault (ArrayVal Dict.empty) <| Dict.get i elems
                in
                case assignArray v child is newElem of
                    Err e ->
                        Err e

                    Ok newRoot ->
                        Ok <| ArrayVal <| Dict.insert i newRoot elems

        ( _, _ :: _ ) ->
            Err <| UnreachableBranch "assignArray"


evalIndices : SymbolTable -> List ArithExp -> Result Exception (List Index)
evalIndices st aexps =
    case Result.combineMap (evalArith st) aexps of
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


evalArith : SymbolTable -> ArithExp -> Result Exception Value
evalArith st aexp =
    case aexp of
        Lit val ->
            Ok val

        Var v ->
            lookupVar st v

        Plus e1 e2 ->
            Result.map NumberVal <| opNums (+) (evalArith st e1) (evalArith st e2)

        Minus e1 e2 ->
            Result.map NumberVal <| opNums (-) (evalArith st e1) (evalArith st e2)

        Times e1 e2 ->
            Result.map NumberVal <| opNums (*) (evalArith st e1) (evalArith st e2)

        Quot e1 e2 ->
            case ( evalArith st e1, evalArith st e2 ) of
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
            case ( evalArith st e1, evalArith st e2 ) of
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
            case Result.combineMap (evalArith st) es of
                Err e ->
                    Err e

                Ok vals ->
                    Ok <| ArrayVal <| Dict.fromList <| List.indexedMap Tuple.pair vals

        Fun (Function f) es ->
            case ( Result.combineMap (evalArith st) es, Dict.get f st.functions ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Nothing ) ->
                    Err <| UndefinedFunction <| Function f

                ( Ok vals, Just fdef ) ->
                    fdef vals


evalBool : SymbolTable -> BoolExp -> Result Exception Bool
evalBool st bexp =
    case bexp of
        Eq e1 e2 ->
            case ( evalArith st e1, evalArith st e2 ) of
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
            case ( evalArith st e1, evalArith st e2 ) of
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
            opNums (>) (evalArith st e1) (evalArith st e2)

        Ge e1 e2 ->
            opNums (>=) (evalArith st e1) (evalArith st e2)

        Le e1 e2 ->
            opNums (<=) (evalArith st e1) (evalArith st e2)

        Lt e1 e2 ->
            opNums (<) (evalArith st e1) (evalArith st e2)

        And e1 e2 ->
            Result.map2 (&&) (evalBool st e1) (evalBool st e2)

        Or e1 e2 ->
            Result.map2 (||) (evalBool st e1) (evalBool st e2)

        Not e ->
            Result.map not (evalBool st e)


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


format : SymbolTable -> Nonempty ArithExp -> Result Exception String
format st aexps =
    let
        concat r1 r2 =
            -- List.Nonempty.foldl1 accumulates items in the reverse order
            Result.map2 (\x y -> y ++ x) r1 r2
    in
    Nonempty.foldl1 concat <| Nonempty.map (formatExp st) aexps


formatExp : SymbolTable -> ArithExp -> Result Exception String
formatExp st aexp =
    Result.map formatValue <| evalArith st aexp


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
            "undefined"

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
            "{" ++ String.join "， " formatted ++ "}"


builtinFuns : Functions
builtinFuns =
    Dict.fromList
        [ ( "二乗", square )
        , ( "べき乗", power )
        , ( "要素数", countElems )
        ]


square : FunctionDef
square vals =
    case vals of
        (NumberVal n) :: [] ->
            Ok <| NumberVal (n * n)

        _ ->
            Err <| InvalidArgument vals


power : FunctionDef
power vals =
    case vals of
        (NumberVal n) :: (NumberVal m) :: [] ->
            Ok <| NumberVal (n ^ m)

        _ ->
            Err <| InvalidArgument vals


countElems : FunctionDef
countElems vals =
    case vals of
        (ArrayVal elems) :: [] ->
            Ok <| NumberVal <| Dict.size elems

        _ ->
            Err <| InvalidArgument vals
