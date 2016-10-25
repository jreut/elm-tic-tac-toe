module Main exposing (..)

import Html.App as App
import Html exposing (Html, div, text, span)


main =
    App.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }


type alias Model =
    Int


type Marker
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewCell (Just X)
            , viewCell (Nothing)
            , viewCell (Nothing)
            ]
        , div []
            [ viewCell (Just X)
            , viewCell (Nothing)
            , viewCell (Nothing)
            ]
        , div []
            [ viewCell (Just X)
            , viewCell (Nothing)
            , viewCell (Nothing)
            ]
        ]


viewCell : Maybe Marker -> Html Msg
viewCell marker =
    let
        label =
            Maybe.map toString marker
                |> Maybe.withDefault "-"
    in
        span [] [ text label ]
