module Main exposing (..)

import Browser
import DNCL.Evaluator exposing (Output, run)
import DNCL.Parser exposing (parse)
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
    { sourceCode : SourceCode
    , output : Output
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { sourceCode = euclid
      , output = []
      }
    , Cmd.none
    )


helloWorld : SourceCode
helloWorld =
    "「こんにちは、世界」を表示する"


euclid : SourceCode
euclid =
    """x ← 1071
y ← 1029

copy_x ← x
copy_y ← y

y ≠ 0 の間，
    tmp ← y
    y ← x ％ y
    x ← tmp
を繰り返す

"gcd(" と copy_x と ", " と copy_y と ") = " と x を表示する"""


type Msg
    = ReadSourceCode SourceCode
    | RunProgram


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadSourceCode code ->
            ( { model | sourceCode = code }, Cmd.none )

        RunProgram ->
            case parse model.sourceCode of
                Nothing ->
                    ( { model | output = [ "構文エラーです" ] }, Cmd.none )

                Just prog ->
                    case run prog of
                        Err _ ->
                            ( { model | output = [ "実行時エラーです" ] }, Cmd.none )

                        Ok out ->
                            ( { model | output = out }, Cmd.none )


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
            , contents model
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


contents : Model -> Element Msg
contents model =
    row
        [ width fill
        , height fill
        ]
        [ editor model.sourceCode, output model.output ]


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


output : Output -> Element msg
output out =
    el
        [ width <| maximum 550 fill
        , height fill
        , Font.family
            [ Font.typeface "Consolas"
            , Font.typeface "Courier New"
            , Font.monospace
            ]
        ]
    <|
        Element.text <|
            String.join "\n" <|
                List.reverse out


toolbar : Element Msg
toolbar =
    el
        [ Background.color <| rgb255 100 100 255
        , width fill
        , height <| px 75
        ]
    <|
        Input.button
            [ Background.color <| rgb255 100 255 100
            ]
            { onPress = Just RunProgram
            , label = Element.text "Run"
            }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
