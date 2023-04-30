module Main exposing (..)

import Browser
import Debug
import Element
    exposing
        ( Element
        , Option
        , column
        , el
        , fill
        , focusStyle
        , height
        , layoutWith
        , maximum
        , px
        , rgb255
        , row
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias SourceCode =
    String


type alias Model =
    { sourceCode : SourceCode }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { sourceCode = helloWorld }
    , Cmd.none
    )


helloWorld : SourceCode
helloWorld =
    "「こんにちは、世界」と表示する"


type Msg
    = ReadSourceCode SourceCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadSourceCode code ->
            ( { model | sourceCode = code }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


noFocus : Option
noFocus =
    focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }


view : Model -> Html Msg
view model =
    layoutWith { options = [ noFocus ] } [] <|
        column [ width fill, height fill ]
            [ header
            , contents model.sourceCode
            , toolbar
            ]


header : Element msg
header =
    el
        [ Background.color <| rgb255 255 100 100
        , width fill
        , height <| px 75
        ]
    <|
        Element.text "Header"


contents : SourceCode -> Element Msg
contents code =
    row
        [ width fill
        , height fill
        ]
        [ editor code, output code ]


editor : SourceCode -> Element Msg
editor code =
    Input.multiline
        [ height fill
        , Border.width 0
        , Font.size 18
        , Font.family
            [ Font.typeface "Consolas"
            , Font.typeface "Courier New"
            , Font.monospace
            ]
        ]
    <|
        { onChange = ReadSourceCode
        , text = code
        , label = Input.labelHidden "Editor"
        , placeholder = Nothing
        , spellcheck = False
        }


output : SourceCode -> Element msg
output code =
    el
        [ Background.color (rgb255 100 255 255)
        , width <| maximum 550 fill
        , height fill
        ]
    <|
        Element.text <|
            String.fromInt <|
                String.length code


toolbar : Element msg
toolbar =
    el
        [ Background.color (rgb255 100 100 255)
        , width fill
        , height <| px 75
        ]
    <|
        Element.text "Toolbar"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
