module Page.Regist.FaqRegist exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page exposing(..)
import Session exposing (Session)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias Model = 
    {
        disabled: Bool
        , session : Session
        , menus : List Menus
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session = 
    ({
        disabled = False
        , session = session
        , menus = []
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

toSession : Model -> Session
toSession model =
    model.session

type Msg = Regist | Edit | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Regist ->
            ({ model | disabled = True }, Cmd.none)
        Edit ->
            ({ model | disabled = False }, Cmd.none)
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )

view: Model -> {title: String, content : Html Msg , menu : Html Msg}
view model =
    { title = "1:1 문의"
    , content =
        div [ class "content" ] [
        columnsHtml [
            pageTitle "1:1 문의"
        ],
        columnsHtml [
            div [] [
                text "제목 :" ,
                span [] [ text " 문의 드립니다." ]
            ],
            div [ class "faqDate"] [
                text "2018-10-10"
            ]
        ],
        columnsHtml [
            div [ class "faqContents" ] [
                text 
                """
                ananananananananananabalagb
                """
            ]
        ],
        columnsHtml [
            textArea "답변" model.disabled
        ],
            if model.disabled then
            div [class "buttons"] [
                button [class "button is-primary", onClick Edit] [text "수정"],
                a [class "button is-warning", href "/faq"] [text "취소"]
            ]
            else 
            div [class "buttons"][ 
                button [class "button is-primary", onClick Regist] [text "등록"],
                a [class "button is-warning", href "/faq"] [text "취소"]
            ]
        ]
        , menu =  
        aside [ class "menu"] [
            ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
        ]
    }