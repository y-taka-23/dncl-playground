module Main exposing (..)

import Browser
import DNCL.AST exposing (DNCLProgram, SourceCode)
import DNCL.Evaluator
    exposing
        ( Evaluator
        , Exception(..)
        , StepResult(..)
        , flushBuffer
        , load
        , step
        )
import DNCL.Example exposing (..)
import DNCL.Parser exposing (parse)
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , Option
        , alignRight
        , centerY
        , column
        , el
        , fill
        , focusStyle
        , height
        , layoutWith
        , maximum
        , padding
        , paddingXY
        , px
        , rgb255
        , row
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Task
import Time exposing (Posix)


type ExecState
    = Stopped
    | Running { since : Posix, evaluator : Evaluator }


type alias Model =
    { sourceCode : SourceCode
    , log : List String
    , execState : ExecState
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { sourceCode = pastExam2022
      , log = []
      , execState = Stopped
      }
    , Cmd.none
    )


type Msg
    = ReadSourceCode SourceCode
    | TriggerProgram
    | RunProgram Posix
    | StepForward Posix


timeoutSeconds : Int
timeoutSeconds =
    10


stepIntervalMillis : Float
stepIntervalMillis =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadSourceCode code ->
            ( { model | sourceCode = code }, Cmd.none )

        TriggerProgram ->
            ( model, Task.perform RunProgram Time.now )

        RunProgram now ->
            case parse model.sourceCode of
                Nothing ->
                    ( { model | log = [ "構文エラーです" ], execState = Stopped }, Cmd.none )

                Just prog ->
                    ( { model | log = [], execState = Running { since = now, evaluator = load prog } }
                    , Cmd.none
                    )

        StepForward current ->
            case model.execState of
                Stopped ->
                    ( model, Cmd.none )

                Running exec ->
                    if Time.posixToMillis current - Time.posixToMillis exec.since > timeoutSeconds * 1000 then
                        ( { model | log = "制限時間を超過しました" :: model.log, execState = Stopped }
                        , Cmd.none
                        )

                    else
                        case step exec.evaluator of
                            Err _ ->
                                ( { model | log = "実行時エラーです" :: model.log, execState = Stopped }
                                , Cmd.none
                                )

                            Ok (Completed end) ->
                                ( { model | log = flushBuffer end.output, execState = Stopped }, Cmd.none )

                            Ok (Continued next) ->
                                ( { model | log = flushBuffer next.output, execState = Running { exec | evaluator = next } }
                                , Cmd.none
                                )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.execState of
        Stopped ->
            Sub.none

        Running _ ->
            Time.every stepIntervalMillis StepForward


view : Model -> Html Msg
view model =
    layoutWith
        { options = [ noFocus ] }
        [ Background.color darkgray
        , sansSerif
        ]
    <|
        column
            [ width fill, height fill ]
            [ header
            , row [ width fill, height fill ]
                [ inputPanel model
                , logPanel model.log
                ]
            ]


noFocus : Option
noFocus =
    focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }


white : Color
white =
    rgb255 0xFF 0xFF 0xFF


black : Color
black =
    rgb255 0x17 0x17 0x17


lightgray : Color
lightgray =
    rgb255 0x27 0x27 0x27


darkgray : Color
darkgray =
    rgb255 0x1E 0x1E 0x1E


palegray : Color
palegray =
    rgb255 0x3D 0x3D 0x3D


accent : Color
accent =
    -- Candidates: 0x2D 0xEE 0x2C (green), 0x1A 0x9C 0xEE (blue), 0xEE 0x1A 0x8C (pink)
    rgb255 0x2D 0xEE 0x2C


sansSerif : Attribute msg
sansSerif =
    Font.family
        [ Font.typeface "Helvetica Neue"
        , Font.typeface "Helvetica"
        , Font.typeface "Hiragino Sans"
        , Font.typeface "Hiragino Kaku Gothic ProN"
        , Font.typeface "Arial"
        , Font.typeface "Yu Gothic"
        , Font.typeface "Meiryo"
        , Font.sansSerif
        ]


monospace : Attribute msg
monospace =
    Font.family
        [ Font.typeface "Consolas"
        , Font.typeface "Courier"
        , Font.typeface "Courier New"
        , Font.typeface "Monaco"
        , Font.monospace
        ]


header : Element msg
header =
    el
        [ Background.color black
        , width fill
        , height <| px 80
        , paddingXY 20 0
        ]
    <|
        el
            [ Region.heading 1
            , Font.color white
            , Font.size 28
            , Font.semiBold
            , centerY
            ]
        <|
            text "DNCL Playground"


inputPanel : Model -> Element Msg
inputPanel model =
    column
        [ width fill
        , height fill
        ]
        [ editor model.sourceCode
        , toolbar
        ]


editor : SourceCode -> Element Msg
editor code =
    el
        [ width fill
        , height fill
        , scrollbarY
        ]
    <|
        Input.multiline
            [ width fill
            , height fill
            , padding 20
            , Border.width 0
            , Background.color darkgray
            , Font.color white
            , Font.size 18
            , monospace
            ]
        <|
            { onChange = ReadSourceCode
            , text = code
            , label = Input.labelHidden "Editor"
            , placeholder = Nothing
            , spellcheck = False
            }


toolbar : Element Msg
toolbar =
    el
        [ Background.color darkgray
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Border.color palegray
        , width fill
        , height <| px 80
        , paddingXY 30 0
        ]
    <|
        Input.button
            [ alignRight
            , centerY
            , height <| px 50
            , width <| px 100
            , Border.width 1
            , Border.color accent
            , Border.rounded 6
            , Font.color accent
            , Font.size 18
            , Font.center
            ]
            { onPress = Just TriggerProgram
            , label = text "実行"
            }


logPanel : List String -> Element msg
logPanel log =
    column
        [ width <| maximum 550 fill
        , height fill
        , Background.color lightgray
        , Font.size 18
        ]
    <|
        [ el
            [ Region.heading 2
            , width fill
            , height <| px 60
            , paddingXY 15 0
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color palegray
            , Font.color accent
            ]
          <|
            el [ centerY ] <|
                text "出力"
        , column
            [ width fill
            , height fill
            , scrollbarY
            , padding 15
            , spacing 6
            , Font.color white
            , monospace
            ]
          <|
            List.map text <|
                List.reverse log
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
