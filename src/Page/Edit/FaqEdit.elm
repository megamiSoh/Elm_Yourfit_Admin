module Page.Edit.FaqEdit exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)

type alias Model = {
    session: Session
    }

init : Session -> (Model, Cmd Msg)
init session = ({
        session = session
    }, Cmd.none)

type Msg = NoOp



toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model ->(Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
             (model, Cmd.none)

view: Model -> {title : String ,content :Html Msg, menu: Html Msg}
view model =
    { title = ""
    , content = 
        div [] [text "hello"]
    , menu = div [] []
    }