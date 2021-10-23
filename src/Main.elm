module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Model
    = Init
    | Running Int
    | AllBlack
    | Stopped


init : Model
init =
    Init



-- UPDATE


type Msg
    = Start
    | Tick
    | Stop
    | Reset


update : Msg -> Model -> Model
update msg model =
    case model of
        Init ->
            case msg of
                Start ->
                    Running 0

                _ ->
                    model

        Running 3 ->
            case msg of
                Tick ->
                    AllBlack

                Stop ->
                    Stopped

                Reset ->
                    Init

                _ ->
                    model

        Running n ->
            case msg of
                Tick ->
                    Running (n + 1)

                Stop ->
                    Stopped

                Reset ->
                    Init

                _ ->
                    model

        AllBlack ->
            case msg of
                Stop ->
                    Stopped

                Reset ->
                    Init

                _ ->
                    model

        Stopped ->
            case msg of
                Reset ->
                    Init

                _ ->
                    model



-- VIEW


toString : Model -> String
toString model =
    case model of
        Init ->
            "○○○"

        Running n ->
            String.append (String.repeat n "◎")
                (String.repeat (3 - n) "●")

        AllBlack ->
            "●●●"

        Stopped ->
            "stopped"


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , button [ onClick Start ] [ text "start" ]
        , button [ onClick Tick ] [ text "tick" ]
        , button [ onClick Stop ] [ text "stop" ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
