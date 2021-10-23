module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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
    | Running Int
    | AllBlack
    | Stopped


type alias Model =
    { state : State
    , startAt : Maybe Int
    , stopAt : Maybe Int
    }


initModel : Model
initModel =
    Model Init Nothing Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = Start
    | Tick Time.Posix
    | Stop
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            model.state
    in
    case state of
        Init ->
            case msg of
                Start ->
                    ( { model | state = Running 0 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Running n ->
            case msg of
                Tick _ ->
                    if n >= 3 then
                        ( { model | state = AllBlack, startAt = Just 0 }, Cmd.none )

                    else
                        ( { model | state = Running (n + 1) }, Cmd.none )

                Stop ->
                    ( { model | state = Stopped }, Cmd.none )

                Reset ->
                    ( initModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AllBlack ->
            case msg of
                Stop ->
                    ( { model | state = Stopped, stopAt = Just 1 }, Cmd.none )

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
    Time.every 2000 Tick



-- VIEW


light : Model -> String
light model =
    case model.state of
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
        [ div [] [ text (light model) ]
        , button [ onClick Start ] [ text "start" ]
        , button [ onClick Stop ] [ text "stop" ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
