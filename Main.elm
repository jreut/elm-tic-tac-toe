module Main exposing (..)

import Array exposing (Array)
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
    { board : Array (Maybe Marker)
    , currentPlayer : Marker
    }


type alias Board =
    Array (Maybe Marker)


type alias Index =
    Int


type Marker
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( Model (Array.repeat 9 Nothing) X, Cmd.none )


type Msg
    = Move Index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move index ->
            ( makeMove model index, Cmd.none )


makeMove : Model -> Index -> Model
makeMove model index =
    if isOccupied model.board index then
        { model
            | currentPlayer = switchPlayer model.currentPlayer
            , board = placeMarker model.board model.currentPlayer index
        }
    else
        model


placeMarker : Board -> Marker -> Index -> Board
placeMarker board marker index =
    Array.set index (Just marker) board


isOccupied : Board -> Index -> Bool
isOccupied board index =
    Array.get index board
        |> Maybe.withDefault Nothing
        |> (==) Nothing


switchPlayer : Marker -> Marker
switchPlayer marker =
    case marker of
        X ->
            O

        O ->
            X


view : Model -> Html Msg
view model =
    let
        boardSize =
            model.board
                |> Array.length
                |> toFloat
                |> sqrt
                |> round

        rows =
            eachSlice boardSize model.board

        startingIndices =
            Array.initialize boardSize ((*) boardSize) |> Array.toList

        zipped =
            \renderer -> List.map2 renderer startingIndices rows
    in
        div [] (zipped viewRow)


viewRow : Index -> Array (Maybe Marker) -> Html Msg
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
