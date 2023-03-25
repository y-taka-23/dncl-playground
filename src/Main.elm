module Main exposing (greet, main)

import Browser
import Html exposing (Html, text)
import List.Nonempty exposing (Nonempty)


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
    | Div ArithExp ArithExp
    | Percent ArithExp ArithExp


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
greet name =
    "Hello, " ++ name ++ "!"


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
