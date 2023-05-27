module DNCL.Parser exposing
    ( arithExp
    , boolExp
    , dnclProgram
    , parameterMany
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
        , problem
        , run
        , succeed
        , symbol
        , variable
        )
import Set exposing (Set)


parse : SourceCode -> Maybe DNCLProgram
parse code =
    case run dnclProgram code of
        Ok prog ->
            Just prog

        Err _ ->
            Nothing


reserved : Set String
reserved =
    Set.fromList
        [ "と"
        , "を表示する"
        , "を改行なしで表示する"
        , "改行を表示する"
        , "増やす"
        , "減らす"
        , "かつ"
        , "または"
        , "でない"
        , "もし"
        , "ならば"
        , "を実行し，そうでなければ"
        , "を実行する"
        , "の間，"
        , "を繰り返す"
        , "繰り返し，"
        , "を，"
        , "になるまで実行する"
        , " を"
        , "から"
        , "まで"
        , "ずつ増やしながら，"
        , "ずつ減らしながら，"
        , "を繰り返す"
        , "関数"
        , "と定義する"
        ]


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
            , reserved = reserved
            }


constOrArray : Parser Variable
constOrArray =
    succeed toConstOrArray
        |= variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlphaNum c || (c == '_')
            , reserved = reserved
            }
        |= oneOf
            [ squareBrackets (lazy (\_ -> arithExpMany1))
            , succeed []
            ]


toConstOrArray : Name -> List ArithExp -> Variable
toConstOrArray x aexps =
    if String.all Char.isUpper x && List.isEmpty aexps then
        Const x

    else
        Array x aexps


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
        |= oneOf
            [ variable
                { start = \c -> c /= '」'
                , inner = \c -> c /= '」'
                , reserved = reserved
                }
            , succeed ""
            ]
        |. symbol "」"


stringValEn : Parser Value
stringValEn =
    succeed StringVal
        |. symbol "\""
        |= oneOf
            [ variable
                { start = \c -> c /= '"'
                , inner = \c -> c /= '"'
                , reserved = reserved
                }
            , succeed ""
            ]
        |. symbol "\""


function : Parser Function
function =
    succeed Function
        |= functionName


voidFunction : Parser VoidFunction
voidFunction =
    succeed VoidFunction
        |= functionName


functionName : Parser Name
functionName =
    succeed identity
        |= variable
            { start = \c -> Char.toCode c > 0x7F
            , inner = \c -> Char.toCode c > 0x7F
            , reserved = reserved
            }


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


arithExpMany1 : Parser (List ArithExp)
arithExpMany1 =
    arithExp
        |> andThen (\e -> loop [ e ] arithExpLoop)


arithExpMany : Parser (List ArithExp)
arithExpMany =
    oneOf
        [ arithExpMany1
        , succeed []
        ]


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
        , arithFun
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
        |= curlyBrackets (lazy (\_ -> arithExpMany))


arithFun : Parser ArithExp
arithFun =
    succeed Fun
        |= backtrackable function
        |. blanks
        |= roundBrackets (lazy (\_ -> arithExpMany))


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
        [ line lineStatement
        , blockStatement
        ]


lineStatement : Parser Statement
lineStatement =
    oneOf
        [ assign
        , printLn
        , print
        , printNewLine
        , increment
        , decrement
        , invoke
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


printLn : Parser Statement
printLn =
    succeed (\p ps -> PrintLn (Nonempty p ps))
        |= backtrackable arithExp
        |= backtrackable (loop [] printLoop)
        |. backtrackable blanks
        |. symbol "を表示する"


print : Parser Statement
print =
    succeed (\p ps -> Print (Nonempty p ps))
        |= backtrackable arithExp
        |= backtrackable (loop [] printLoop)
        |. backtrackable blanks
        |. symbol "を改行なしで表示する"


printLoop : List ArithExp -> Parser (Step (List ArithExp) (List ArithExp))
printLoop ps =
    oneOf
        [ succeed (\p -> Loop (p :: ps))
            |. backtrackable blanks
            |. symbol "と"
            |. blanks
            |= arithExp
        , succeed (Done (List.reverse ps))
        ]


printNewLine : Parser Statement
printNewLine =
    succeed PrintNewLine
        |. symbol "改行を表示する"


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


invoke : Parser Statement
invoke =
    succeed Invoke
        |= backtrackable voidFunction
        |. blanks
        |= roundBrackets arithExpMany


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
            |= statement
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


parameter : Parser Parameter
parameter =
    oneOf
        [ scalarParam
        , arrayParam
        ]


scalarParam : Parser Parameter
scalarParam =
    succeed ScalarParam
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || (c == '_')
            , reserved = reserved
            }


arrayParam : Parser Parameter
arrayParam =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || (c == '_')
        , reserved = reserved
        }
        |> andThen arrayOrErr


arrayOrErr : Name -> Parser Parameter
arrayOrErr x =
    if String.all Char.isUpper x then
        problem <| "constant " ++ x ++ " is used as a parameter"

    else
        succeed <| ArrayParam x


parameterMany1 : Parser (List Parameter)
parameterMany1 =
    parameter
        |> andThen (\p -> loop [ p ] parameterLoop)


parameterMany : Parser (List Parameter)
parameterMany =
    oneOf
        [ parameterMany1
        , succeed []
        ]


parameterLoop : List Parameter -> Parser (Step (List Parameter) (List Parameter))
parameterLoop acc =
    oneOf
        [ succeed (\e -> Loop (e :: acc))
            |. backtrackable blanks
            |. symbol "，"
            |. blanks
            |= parameter
        , succeed ()
            |> map (\_ -> Done <| List.reverse acc)
        ]


functionDecl : Parser FunctionDecl
functionDecl =
    succeed (\( fun, ps ) proc -> Decl fun ps proc)
        |= line
            (succeed Tuple.pair
                |. symbol "関数"
                |. symbol " "
                |. blanks
                |= voidFunction
                |. blanks
                |= roundBrackets parameterMany
                |. blanks
                |. symbol "を"
            )
        |= procedure
        |. line (symbol "と定義する")


snippet =
    oneOf
        [ succeed Stmt
            |= statement
        , succeed FunDecl
            |= functionDecl
        ]


snippetLoop : List Snippet -> Parser (Step (List Snippet) (List Snippet))
snippetLoop snips =
    oneOf
        [ succeed (\snip -> Loop (snip :: snips))
            |= snippet
        , succeed (Loop snips)
            |. blankLine
        , succeed ()
            |> map (\_ -> Done (List.reverse snips))
        ]


dnclProgram : Parser DNCLProgram
dnclProgram =
    loop [] snippetLoop
        |. blanks
        |. end
