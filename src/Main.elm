module Main exposing (..)

import Browser
import Char
import Html exposing (Html, text)
import List.Nonempty exposing (Nonempty)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompWhile
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
    | Var Name
    | Plus ArithExp ArithExp
    | Minus ArithExp ArithExp
    | Times ArithExp ArithExp
    | Quot ArithExp ArithExp
    | Mod ArithExp ArithExp


type BoolExp
    = Eq ArithExp ArithExp
    | Neq ArithExp ArithExp
    | GT ArithExp ArithExp
    | GE ArithExp ArithExp
    | LE ArithExp ArithExp
    | LT ArithExp ArithExp
    | And BoolExp BoolExp
    | Or BoolExp BoolExp
    | Not BoolExp


type alias Procedure =
    Nonempty Statement


type Statement
    = Assign Name ArithExp
    | Increment Name ArithExp
    | Decrement Name ArithExp
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
        |= name


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
