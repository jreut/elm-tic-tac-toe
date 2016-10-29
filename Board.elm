module Board exposing (Model)

import Array exposing (Array)


type alias Model a =
    Array (Maybe a)
