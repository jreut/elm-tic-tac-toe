module Board
    exposing
        ( Model
        , CellRenderer
        , RowRenderer
        , BoardRenderer
        , init
        , update
        , view
        , isOccupied
        )

import Array exposing (Array)
import List exposing (take, drop)


type alias Model a =
    List (Maybe a)


type alias CellRenderer a =
    Int -> String -> a


type alias RowRenderer contained container =
    List contained -> container


type alias BoardRenderer contained container =
    RowRenderer contained container


init : Int -> Model a
init size =
    List.repeat (size * size) Nothing


update : Model a -> Int -> a -> Model a
update model index occupant =
    let
        replacer =
            (\i original ->
                if index == i then
                    Just occupant
                else
                    original
            )
    in
        List.indexedMap replacer model


isOccupied : Model a -> Int -> Bool
isOccupied model index =
    List.drop index model
        |> List.head
        |> Maybe.map ((==) Nothing)
        |> Maybe.withDefault False


view : CellRenderer a -> RowRenderer a b -> BoardRenderer b c -> Model d -> c
view cellRenderer rowRenderer boardRenderer model =
    let
        modelArray =
            Array.fromList model

        boardSize =
            (model |> List.length |> toFloat |> sqrt |> round)

        indices =
            List.repeat boardSize boardSize
                |> List.indexedMap (*)
    in
        boardRenderer
            (eachSlice boardSize modelArray
                |> List.map2 (viewRow cellRenderer rowRenderer) indices
            )


slices : Int -> List a -> List (List a)
slices size list =
    -- TODO: broken
    let
        wanted =
            take size list

        rest =
            drop size list
    in
        case ( wanted, rest ) of
            ( [], _ ) ->
                [ [] ]

            ( w, r ) ->
                (slices size r)


viewRow : CellRenderer a -> RowRenderer a b -> Int -> Array (Maybe c) -> b
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
