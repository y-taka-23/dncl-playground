module DNCL.Parser exposing
    ( arithExp
    , boolExp
    , dnclProgram
    , parse
    , statement
    , value
    , variable_
    )

import Char
import DNCL.AST exposing (..)
import List.Nonempty exposing (Nonempty(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompWhile
        , end
        , int
        , lazy
        , loop
        , map
        , oneOf
        , run
        , succeed
        , symbol
        , variable
        )
import Set


parse : SourceCode -> Maybe DNCLProgram
parse code =
    case run dnclProgram code of
        Ok prog ->
            Just prog

        Err _ ->
            Nothing


variable_ : Parser Variable
variable_ =
    oneOf
        [ scalar
        , constOrArray
        ]


scalar : Parser Variable
scalar =
    succeed Scalar
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || (c == '_')
            , reserved = Set.empty
            }


constOrArray : Parser Variable
constOrArray =
    succeed toConstOrArray
        |= variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlphaNum c || (c == '_')
            , reserved = Set.empty
            }
        |= oneOf
            [ squareBrackets (lazy (\_ -> arithExpSeq))
            , succeed []
            ]


toConstOrArray : Name -> List ArithExp -> Variable
toConstOrArray x aexps =
    if String.all Char.isUpper x && List.isEmpty aexps then
        Const x

    else
        Array x aexps


arithExpSeq : Parser (List ArithExp)
arithExpSeq =
    arithExp
        |> andThen (\e -> loop [ e ] arithExpLoop)


value : Parser Value
value =
    oneOf [ numberVal, stringVal ]


numberVal : Parser Value
numberVal =
    oneOf [ numberValPos, numberValNeg ]


numberValPos : Parser Value
numberValPos =
    succeed NumberVal
        |= int


numberValNeg : Parser Value
numberValNeg =
    succeed (negate >> NumberVal)
        |. symbol "-"
        |= int


stringVal : Parser Value
stringVal =
    oneOf [ stringValJa, stringValEn ]


stringValJa : Parser Value
stringValJa =
    succeed StringVal
        |. symbol "「"
        |= variable
            { start = \c -> True
            , inner = \c -> c /= '」'
            , reserved = Set.empty
            }
        |. symbol "」"


stringValEn : Parser Value
stringValEn =
    succeed StringVal
        |. symbol "\""
        |= variable
            { start = \c -> True
            , inner = \c -> c /= '"'
            , reserved = Set.empty
            }
        |. symbol "\""


blanks : Parser ()
blanks =
    chompWhile (\c -> c == ' ')


brackets : String -> String -> Parser a -> Parser a
brackets left right p =
    succeed identity
        |. symbol left
        |. blanks
        |= p
        |. blanks
        |. symbol right


roundBrackets : Parser a -> Parser a
roundBrackets =
    brackets "(" ")"


squareBrackets : Parser a -> Parser a
squareBrackets =
    brackets "[" "]"


curlyBrackets : Parser a -> Parser a
curlyBrackets =
    brackets "{" "}"


arithExp : Parser ArithExp
arithExp =
    arithTerm
        |> andThen (\e -> loop e arithTermLoop)


arithExpLoop : List ArithExp -> Parser (Step (List ArithExp) (List ArithExp))
arithExpLoop acc =
    oneOf
        [ succeed (\e -> Loop (e :: acc))
            |. backtrackable blanks
            |. symbol "，"
            |. blanks
            |= arithExp
        , succeed ()
            |> map (\_ -> Done <| List.reverse acc)
        ]


arithTermLoop : ArithExp -> Parser (Step ArithExp ArithExp)
arithTermLoop acc =
    oneOf
        [ succeed (\e -> Loop (Plus acc e))
            |. backtrackable blanks
            |. symbol "＋"
            |. blanks
            |= arithTerm
        , succeed (\e -> Loop (Minus acc e))
            |. backtrackable blanks
            |. symbol "－"
            |. blanks
            |= arithTerm
        , succeed ()
            |> map (\_ -> Done acc)
        ]


arithTerm : Parser ArithExp
arithTerm =
    arithFactor
        |> andThen (\e -> loop e arithFactorLoop)


arithFactorLoop : ArithExp -> Parser (Step ArithExp ArithExp)
arithFactorLoop acc =
    oneOf
        [ succeed (\e -> Loop (Times acc e))
            |. backtrackable blanks
            |. symbol "×"
            |. blanks
            |= arithFactor
        , succeed (\e -> Loop (Quot acc e))
            |. backtrackable blanks
            |. symbol "÷"
            |. blanks
            |= arithFactor
        , succeed (\e -> Loop (Mod acc e))
            |. backtrackable blanks
            |. symbol "％"
            |. blanks
            |= arithFactor
        , succeed ()
            |> map (\_ -> Done acc)
        ]


arithFactor : Parser ArithExp
arithFactor =
    oneOf
        [ arithLit
        , arithVar
        , arithArr
        , roundBrackets (lazy (\_ -> arithExp))
        ]


arithLit : Parser ArithExp
arithLit =
    succeed Lit
        |= value


arithVar : Parser ArithExp
arithVar =
    succeed Var
        |= variable_


arithArr : Parser ArithExp
arithArr =
    succeed Arr
        -- TODO: Commas should be not '，' but ', '?
        |= curlyBrackets (lazy (\_ -> arithArrElems))


arithArrElems : Parser (List ArithExp)
arithArrElems =
    oneOf
        [ arithExpSeq
        , succeed []
        ]


boolExp : Parser BoolExp
boolExp =
    boolFactor
        |> andThen (\e -> loop e boolFactorLoop)


boolFactorLoop : BoolExp -> Parser (Step BoolExp BoolExp)
boolFactorLoop acc =
    oneOf
        [ succeed (\e -> Loop (And acc e))
            |. backtrackable blanks
            |. symbol "かつ"
            |. blanks
            |= boolFactor
        , succeed (\e -> Loop (Or acc e))
            |. backtrackable blanks
            |. symbol "または"
            |. blanks
            |= boolFactor
        , succeed (Loop (Not acc))
            |. backtrackable blanks
            |. symbol "でない"
        , succeed ()
            |> map (\_ -> Done acc)
        ]


boolFactor : Parser BoolExp
boolFactor =
    oneOf
        [ succeed Eq
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "＝"
            |. blanks
            |= arithExp
        , succeed Neq
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "≠"
            |. blanks
            |= arithExp
        , succeed Gt
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "＞"
            |. blanks
            |= arithExp
        , succeed Ge
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "≧"
            |. blanks
            |= arithExp
        , succeed Le
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "≦"
            |. blanks
            |= arithExp
        , succeed Lt
            |= backtrackable arithExp
            |. backtrackable blanks
            |. symbol "＜"
            |. blanks
            |= arithExp
        , roundBrackets (lazy (\_ -> boolExp))
        ]


statement : Parser Statement
statement =
    oneOf
        [ lineStatement
        , blockStatement
        ]


lineStatement : Parser Statement
lineStatement =
    oneOf
        [ assign
        , print
        , increment
        , decrement
        ]


blockStatement : Parser Statement
blockStatement =
    oneOf
        [ if_
        , ifElse
        , preCheckLoop
        , postCheckLoop
        , incrementLoop
        , decrementLoop
        ]


assign : Parser Statement
assign =
    succeed Assign
        |= backtrackable variable_
        |. backtrackable blanks
        |. symbol "←"
        |. blanks
        |= arithExp


print : Parser Statement
print =
    succeed (\p ps -> Print (Nonempty p ps))
        |= backtrackable printable
        |= loop [] printableLoop


printableLoop : List Printable -> Parser (Step (List Printable) (List Printable))
printableLoop ps =
    oneOf
        [ succeed (\p -> Loop (p :: ps))
            |. backtrackable blanks
            |. symbol "と"
            |. blanks
            |= printable
        , succeed (Done (List.reverse ps))
            |. backtrackable blanks
            |. symbol "を表示する"
        ]


printable : Parser Printable
printable =
    oneOf
        [ succeed PrintVar
            |= variable_
        , succeed PrintVal
            |= value
        ]


increment : Parser Statement
increment =
    succeed Increment
        |= backtrackable variable_
        |. backtrackable blanks
        |. backtrackable (symbol "を")
        |. backtrackable blanks
        |= backtrackable arithExp
        |. backtrackable blanks
        |. symbol "増やす"


decrement : Parser Statement
decrement =
    succeed Decrement
        |= backtrackable variable_
        |. backtrackable blanks
        |. backtrackable (symbol "を")
        |. backtrackable blanks
        |= backtrackable arithExp
        |. backtrackable blanks
        |. symbol "減らす"


line : Parser a -> Parser a
line p =
    succeed identity
        |. backtrackable blanks
        |= p
        |. blanks
        |. oneOf [ symbol "\n", end ]


blankLine : Parser ()
blankLine =
    succeed ()
        |. backtrackable blanks
        |. symbol "\n"


procedure : Parser Procedure
procedure =
    loop [] statementLoop


statementLoop : Procedure -> Parser (Step Procedure Procedure)
statementLoop proc =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: proc))
            |= line lineStatement
        , succeed (\stmt -> Loop (stmt :: proc))
            |= blockStatement
        , succeed (Loop proc)
            |. blankLine
        , succeed ()
            |> map (\_ -> Done (List.reverse proc))
        ]


if_ : Parser Statement
if_ =
    succeed If
        |= backtrackable
            (line
                (succeed identity
                    |. symbol "もし"
                    |. blanks
                    |= boolExp
                    |. blanks
                    |. symbol "ならば"
                )
            )
        |= backtrackable procedure
        |. line (symbol "を実行する")


ifElse : Parser Statement
ifElse =
    succeed IfElse
        |= backtrackable
            (line
                (succeed identity
                    |. symbol "もし"
                    |. blanks
                    |= boolExp
                    |. blanks
                    |. symbol "ならば"
                )
            )
        |= backtrackable procedure
        |. line (symbol "を実行し，そうでなければ")
        |= procedure
        |. line (symbol "を実行する")


preCheckLoop : Parser Statement
preCheckLoop =
    succeed PreCheckLoop
        |= line
            (succeed identity
                |= boolExp
                |. blanks
                |. symbol "の間，"
            )
        |= procedure
        |. line (symbol "を繰り返す")


postCheckLoop : Parser Statement
postCheckLoop =
    succeed PostCheckLoop
        |. line (symbol "繰り返し，")
        |= procedure
        |= line
            (succeed identity
                |. symbol "を，"
                |. blanks
                |= boolExp
                |. blanks
                |. symbol "になるまで実行する"
            )


incrementLoop : Parser Statement
incrementLoop =
    succeed (\( v, ( f, t, d ) ) p -> IncrementLoop v f t d p)
        |= line
            (succeed (\v f t d -> ( v, ( f, t, d ) ))
                |= backtrackable variable_
                |. backtrackable blanks
                |. backtrackable (symbol "を")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. backtrackable (symbol "から")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. backtrackable (symbol "まで")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. symbol "ずつ増やしながら，"
            )
        |= procedure
        |. line (symbol "を繰り返す")


decrementLoop : Parser Statement
decrementLoop =
    succeed (\( v, ( f, t, d ) ) p -> DecrementLoop v f t d p)
        |= line
            (succeed (\v f t d -> ( v, ( f, t, d ) ))
                |= backtrackable variable_
                |. backtrackable blanks
                |. backtrackable (symbol "を")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. backtrackable (symbol "から")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. backtrackable (symbol "まで")
                |. backtrackable blanks
                |= backtrackable arithExp
                |. backtrackable blanks
                |. symbol "ずつ減らしながら，"
            )
        |= procedure
        |. line (symbol "を繰り返す")


dnclProgram : Parser DNCLProgram
dnclProgram =
    loop [] statementLoop
        |. blanks
        |. end
