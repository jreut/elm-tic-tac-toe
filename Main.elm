module Main exposing (..)

import Board
import Marker
import Array exposing (Array)
import Maybe exposing (andThen)
import Html.App as App
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }


type alias Model =
    { board : Board.Model Marker.Model
    , currentPlayer : Marker.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Board.init 3) Marker.init, Cmd.none )


type Msg
    = Move Int
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move index ->
            ( makeMove model index, Cmd.none )

        Reset ->
            init


makeMove : Model -> Int -> Model
makeMove model index =
    if Board.isOccupied model.board index then
        { model
            | currentPlayer = Marker.update model.currentPlayer
            , board = Board.update model.board index model.currentPlayer
        }
    else
        model


view : Model -> Html Msg
view model =
    div [] [ viewBoard model, viewControls ]


viewControls : Html Msg
viewControls =
    button [ onClick Reset ] [ text "reset" ]


viewBoard : Model -> Html Msg
viewBoard model =
    Board.view cellRenderer rowRenderer boardRenderer model.board


boardRenderer : Board.BoardRenderer (Html Msg) (Html Msg)
boardRenderer rows =
    div [] rows


rowRenderer : Board.RowRenderer (Html Msg) (Html Msg)
rowRenderer row =
    div [] row


cellRenderer : Board.CellRenderer (Html Msg)
cellRenderer index contents =
    button [ onClick (Move index) ] [ text contents ]
