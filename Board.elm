module Board
    exposing
        ( Model
        , init
        , set
        , isOccupied
        )

import Array exposing (Array)


type alias Model a =
    Array (Maybe a)


type Msg a
    = Move Int a


init : Int -> Model a
init size =
    Array.repeat (size * size) Nothing


set : Model a -> Int -> a -> Model a
set model index occupant =
    Array.set index (Just occupant) model


isOccupied : Model a -> Int -> Bool
isOccupied model index =
    Array.get index model
        |> Maybe.withDefault Nothing
        |> (==) Nothing
