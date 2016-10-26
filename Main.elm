module Main exposing (..)

import Array exposing (Array)
import Html.App as App
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)


main =
    App.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }


type alias Model =
    Array (Maybe Marker)


type alias Index =
    Int


type Marker
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( Array.repeat 9 Nothing, Cmd.none )


type Msg
    = Noop
    | Move Index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Move index ->
            ( Array.set index (Just X) model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        cellWithIndex =
            Array.indexedMap (\index -> viewCell index)
    in
        div []
            (model |> cellWithIndex |> Array.toList)


viewCell : Index -> Maybe Marker -> Html Msg
viewCell index marker =
    let
        label =
            Maybe.map toString marker
                |> Maybe.withDefault "-"
    in
        button [ onClick (Move index) ] [ text label ]
