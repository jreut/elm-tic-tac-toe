module Main exposing (..)

import Board
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
    { board : Board
    , currentPlayer : Marker
    }


type alias Board =
    Board.Model Marker


type alias Index =
    Int


type Marker
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( Model (Board.init boardSize) X, Cmd.none )


boardSize =
    3


type Msg
    = Move Index
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move index ->
            ( makeMove model index, Cmd.none )

        Reset ->
            init


makeMove : Model -> Index -> Model
makeMove model index =
    if Board.isOccupied model.board index then
        { model
            | currentPlayer = switchPlayer model.currentPlayer
            , board = Board.set model.board index model.currentPlayer
        }
    else
        model


switchPlayer : Marker -> Marker
switchPlayer marker =
    case marker of
        X ->
            O

        O ->
            X


view : Model -> Html Msg
view model =
    div [] [ viewBoard model, viewControls ]


viewControls : Html Msg
viewControls =
    button [ onClick Reset ] [ text "reset" ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        rows =
            eachSlice boardSize model.board

        startingIndices =
            Array.initialize boardSize ((*) boardSize) |> Array.toList

        zipped =
            \renderer -> List.map2 renderer startingIndices rows
    in
        div [] (zipped viewRow)


viewRow : Index -> Board -> Html Msg
viewRow startingIndex array =
    array
        |> Array.indexedMap (\index -> viewCell (index + startingIndex))
        |> Array.toList
        |> div []


viewCell : Index -> Maybe Marker -> Html Msg
viewCell index marker =
    let
        label =
            Maybe.map toString marker
                |> Maybe.withDefault "-"
    in
        button [ onClick (Move index) ] [ text label ]


eachSlice : Int -> Array a -> List (Array a)
eachSlice size array =
    let
        chunks =
            (Array.length array) // size + 1

        indices =
            Array.initialize chunks (\index -> index * size)

        slicer =
            \index -> Array.slice index (index + size) array
    in
        indices
            |> Array.map slicer
            |> Array.toList
