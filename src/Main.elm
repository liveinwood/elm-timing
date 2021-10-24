module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Maybe
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = Init
    | Running
    | AllBlack
    | JumpStart
    | Stopped


type alias Model =
    { state : State
    , count : Int
    , startAt : Maybe Time.Posix
    , stopAt : Maybe Time.Posix
    , reactionTime : Maybe Float
    }


initModel : Model
initModel =
    Model Init 0 Nothing Nothing Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


reactionTime : Model -> Maybe Float
reactionTime model =
    Maybe.map millToSecond (Maybe.map2 (-) (Maybe.map Time.posixToMillis model.stopAt) (Maybe.map Time.posixToMillis model.startAt))


millToSecond : Int -> Float
millToSecond n =
    toFloat n / 1000



-- UPDATE


type Msg
    = Start
    | Tick Time.Posix
    | SetStartAt Time.Posix
    | SetStopAt Time.Posix
    | Stop
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            model.state

        count =
            model.count
    in
    case state of
        Init ->
            case msg of
                Start ->
                    ( { model | state = Running }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Running ->
            case msg of
                Tick _ ->
                    if count >= 5 then
                        ( model, Task.perform SetStartAt Time.now )

                    else
                        ( { model | count = count + 1 }, Cmd.none )

                SetStartAt time ->
                    ( { model | state = AllBlack, count = 0, startAt = Just time }, Cmd.none )

                Stop ->
                    ( { model | state = JumpStart }, Cmd.none )

                Reset ->
                    ( initModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AllBlack ->
            case msg of
                Stop ->
                    ( model, Task.perform SetStopAt Time.now )

                SetStopAt time ->
                    ( { model | state = Stopped, stopAt = Just time, reactionTime = reactionTime model }, Cmd.none )

                Reset ->
                    ( initModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        JumpStart ->
            case msg of
                Reset ->
                    ( initModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Stopped ->
            case msg of
                Reset ->
                    ( initModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ lightBox model ]
        , button [ onClick Start ] [ text "start" ]
        , button [ onClick Stop ] [ text "stop" ]
        , button [ onClick Reset ] [ text "reset" ]
        , showResult model
        ]


lightBox : Model -> Html Msg
lightBox model =
    svg
        [ width "500", height "100", viewBox "100 100 600 100" ]
        (rect [ x "100", y "100", width "500", height "100", rx "10", ry "10", fill "black" ] []
            :: List.reverse (fiveLights 0 150 model.count)
        )


fiveLights : Int -> Int -> Int -> List (Svg Msg)
fiveLights i x count =
    if i >= 5 then
        []

    else
        circle
            [ cx (String.fromInt x)
            , cy "150"
            , r "40"
            , fill
                (if i < count then
                    "red"

                 else
                    "#222"
                )
            ]
            []
            :: fiveLights (i + 1) (x + 100) count


showResult : Model -> Html Msg
showResult model =
    case model.state of
        Init ->
            div [] [ text "Are You Ready ?" ]

        JumpStart ->
            div [] [ text "Jump Start !" ]

        Stopped ->
            div [] [ text ("Your Result is " ++ String.fromFloat (Maybe.withDefault 0 (reactionTime model))) ]

        _ ->
            div [] []
