module Board
    exposing
        ( Model
        , CellRenderer
        , RowRenderer
        , BoardRenderer
        , init
        , set
        , view
        , isOccupied
        )

import Array exposing (Array)


type alias Model a =
    Array (Maybe a)


type alias CellRenderer a =
    Int -> String -> a


type alias RowRenderer contained container =
    List contained -> container


type alias BoardRenderer contained container =
    RowRenderer contained container


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


view : CellRenderer a -> RowRenderer a b -> BoardRenderer b c -> Model d -> c
view cellRenderer rowRenderer boardRenderer model =
    let
        boardSize =
            (model |> Array.length |> toFloat |> sqrt |> round)

        indices =
            Array.initialize boardSize ((*) boardSize) |> Array.toList
    in
        boardRenderer
            (eachSlice boardSize model
                |> List.map2 (viewRow cellRenderer rowRenderer) indices
            )


viewRow : CellRenderer a -> RowRenderer a b -> Int -> Model c -> b
viewRow cellRenderer rowRenderer startingIndex row =
    Array.map (viewCell "-") row
        |> Array.indexedMap (\index cell -> cellRenderer (index + startingIndex) cell)
        |> Array.toList
        |> rowRenderer


viewCell : String -> Maybe a -> String
viewCell default cell =
    Maybe.map toString cell |> Maybe.withDefault default


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
