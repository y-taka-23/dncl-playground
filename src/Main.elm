module Main exposing (greet, main)

import Browser
import Html exposing (Html, text)


type alias Model =
    { name : String
    }


init : Model
init =
    { name = "Elm"
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update NoOp model =
    model


greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"


view : Model -> Html Msg
view model =
    text <| greet model.name


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
