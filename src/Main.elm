module Main exposing (..)

import Browser
import Char
import Html exposing (Html, text)
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
        , succeed
        , symbol
        , variable
        )
import Set


type alias Name =
    String


type Variable
    = Variable Name


type Value
    = NumberVal Int
    | StringVal String


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
    | Increment Variable ArithExp
    | Decrement Variable ArithExp
    | If BoolExp Procedure
    | IfElse BoolExp Procedure Procedure
    | PreLoop BoolExp Procedure Procedure
    | PostLoop Procedure BoolExp


type alias DNCLProgram =
    Procedure


variable_ : Parser Variable
variable_ =
    succeed Variable
        |= name


name : Parser String
name =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || (c == '_')
        , reserved = Set.empty
        }


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


parens : Parser a -> Parser a
parens p =
    succeed identity
        |. symbol "("
        |. blanks
        |= p
        |. blanks
        |. symbol ")"


arithExp : Parser ArithExp
arithExp =
    arithTerm
        |> andThen (\e -> loop e arithTermLoop)


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
        , parens (lazy (\_ -> arithExp))
        ]


arithLit : Parser ArithExp
arithLit =
    succeed Lit
        |= value


arithVar : Parser ArithExp
arithVar =
    succeed Var
        |= variable_


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
        , parens (lazy (\_ -> boolExp))
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
        , increment
        , decrement
        ]


blockStatement : Parser Statement
blockStatement =
    oneOf
        [ if_
        , ifElse
        ]


assign : Parser Statement
assign =
    succeed Assign
        |= backtrackable variable_
        |. backtrackable blanks
        |. symbol "←"
        |. blanks
        |= arithExp


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
        |. oneOf [ symbol "\n", end ]


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


type alias Model =
    { name : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "Elm" }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update NoOp model =
    ( model, Cmd.none )


greet : String -> String
greet str =
    "Hello, " ++ str ++ "!"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    text <| greet model.name


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
