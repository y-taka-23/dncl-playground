module DNCL.AST exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)


type alias SourceCode =
    String


type alias Name =
    String


type alias Index =
    Int


type Variable
    = Scalar Name
    | Const Name
    | Array Name (List ArithExp)


type Value
    = NumberVal Int
    | StringVal String
    | ArrayVal (Dict Index Value)


type Function
    = Function Name


type VoidFunction
    = VoidFunction Name


type ArithExp
    = Lit Value
    | Var Variable
    | Plus ArithExp ArithExp
    | Minus ArithExp ArithExp
    | Times ArithExp ArithExp
    | Quot ArithExp ArithExp
    | Mod ArithExp ArithExp
    | Arr (List ArithExp)
    | Fun Function (List ArithExp)


type BoolExp
    = Eq ArithExp ArithExp
    | Neq ArithExp ArithExp
    | Gt ArithExp ArithExp
    | Ge ArithExp ArithExp
    | Le ArithExp ArithExp
    | Lt ArithExp ArithExp
    | And BoolExp BoolExp
    | Or BoolExp BoolExp
    | Not BoolExp


type alias Procedure =
    List Statement


type Statement
    = Assign Variable ArithExp
    | PrintLn (Nonempty ArithExp)
    | Print (Nonempty ArithExp)
    | PrintNewLine
    | Increment Variable ArithExp
    | Decrement Variable ArithExp
    | If BoolExp Procedure
    | IfElse BoolExp Procedure Procedure
    | PreCheckLoop BoolExp Procedure
    | PostCheckLoop Procedure BoolExp
    | IncrementLoop Variable ArithExp ArithExp ArithExp Procedure
    | DecrementLoop Variable ArithExp ArithExp ArithExp Procedure
    | Invoke VoidFunction (List ArithExp)


type Parameter
    = ScalarParam Name
    | ArrayParam Name


type FunctionDecl
    = Decl VoidFunction (List Parameter) Procedure


type Snippet
    = Stmt Statement
    | FunDecl FunctionDecl


type alias DNCLProgram =
    List Snippet
