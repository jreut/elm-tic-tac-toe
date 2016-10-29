module Marker
    exposing
        ( Model
        , init
        , update
        , view
        )


type Model
    = X
    | O


init : Model
init =
    X


update : Model -> Model
update model =
    case model of
        X ->
            O

        O ->
            X


view : Model -> String
view model =
    toString model
