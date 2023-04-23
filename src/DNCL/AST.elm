module DNCL.AST exposing (..)

import List.Nonempty exposing (Nonempty)


type alias Name =
    String


type Variable
    = Variable Name


type Value
    = NumberVal Int
    | StringVal String


type Printable
    = PrintVal Value
    | PrintVar Variable


type ArithExp
    = Lit Value
    | Var Variable
    | Plus ArithExp ArithExp
    | Minus ArithExp ArithExp
    | Times ArithExp ArithExp
    | Quot ArithExp ArithExp
    | Mod ArithExp ArithExp


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
    | Print (Nonempty Printable)
    | Increment Variable ArithExp
    | Decrement Variable ArithExp
    | If BoolExp Procedure
    | IfElse BoolExp Procedure Procedure
    | PreCheckLoop BoolExp Procedure
    | PostCheckLoop Procedure BoolExp


type alias DNCLProgram =
    Procedure
