module Page.Detail.FaqDetail exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Page.Page exposing(..)
import Route exposing(..)
import Html.Events exposing(..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias Model = {
    session: Session,
    question: String,
    onlyRead: Bool
    , menus : List Menus
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session = ({
        session = session,
        question = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.",
        onlyRead = False
        , menus = []
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

type Msg 
    = Disabled
    | GetMyInfo (Result Http.Error Decoder.DataWrap)



toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Disabled ->
             ({model | onlyRead = not model.onlyRead}, Cmd.none)
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )

view: Model -> {title: String, content : Html Msg, menu : Html Msg}
view model =
    if model.onlyRead then
    { title = ""
    , content = 
        div [class "container is-fluid"] [
            pageTitle "1 : 1 문의",
            div [class "faqtitle"] [
                span [] [text "제목 : "] ,
                span [] [text "blah blah"],
                span [class "faqDate"] [text "2019-12-12"]
            ],
            div [class "questionUser"] [ text model.question ],
            div [class "questionTitle"] [text "답변"],
            textarea [ class "questionStyle", disabled model.onlyRead] [],
                div [ class "buttons" ] [
                div [ class "button is-primary", onClick Disabled] [text "수정"],
                a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
        ]
        , menu = div [] []
    }
    else
    { title = ""
    , content = 
        div [class "container is-fluid"] [
            pageTitle "1 : 1 문의",
            div [class "faqtitle"] [
                span [] [text "제목 : "] ,
                span [] [text "blah blah"],
                span [class "faqDate"] [text "2019-12-12"]
            ],
            div [class "questionUser"] [ text model.question ],
            div [class "questionTitle"] [text "답변"],
            textarea [ class "questionStyle", disabled model.onlyRead] [],
                div [ class "buttons" ] [
                div [ class "button is-primary",  onClick Disabled] [text "저장"],
                a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
        ]
        , menu = div [] []
    }

