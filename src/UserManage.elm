module Page exposing (hello)

import Browser
import Html exposing (Html,Attribute, text, div, h1, img )
import Html.Attributes exposing (src)


---- MODEL ----


type alias Model = String


init : Model
init = ""



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



---- VIEW ----


-- hello : List (Attribute msg) -> List (Html Msg) -> Html Msg
hello =
    div []
        [ text "hello world" ]



---- PROGRAM ----


-- main : Program () Model Msg
-- main =
--     Browser.sandbox
--         { view = hello
--         , init = init
--         , update = update
--         -- , subscriptions = always Sub.none
--         }
