module Main exposing (..)

import Browser
import Html exposing (Html, text)


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
