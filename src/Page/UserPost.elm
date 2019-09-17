module Page.UserPost exposing (..)

import Browser
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Pagenation exposing (..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (Route)
import Page as Page
import Api.Decode as Decoder
import Http as Http
import Api as Api
import Api.Endpoint as Endpoint

type alias Model =
    { popUp : Bool
    , session : Session
    , menus : List Menus
    , username: String
    }

type alias Menus =
    { menu_auth_code: List String
    , menu_id : Int
    , menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session= 
    ({
        popUp = False
        , session = session
        , menus = []
        , username = ""
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)) 


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

type Msg 
    = PopClose 
    | PopOpen 
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GotSession Session

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotSession session ->
            ({model| session = session} ,
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        PopClose ->
            ({model | popUp = False}, Cmd.none)

        PopOpen ->
            ({model | popUp = True}, Cmd.none)

toSession : Model -> Session
toSession model =
    model.session


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model = 
    { title = "사용자 게시물 관리"
    , content =
        div [] [text "준비 중 입니다."]
        , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }

detailPop : Model -> Html Msg
detailPop model = 
    if model.popUp then
        div [ class "userPostPopWrap"] [
            columnsHtml [
                popTitle "게시물 상세",
                normalBtn "해당 사용자 게시물 전체 보기" ""
            ],
            columnsHtml [
                div[class "userPostImg"][]
            ],
            columnsHtml [
                text "사용자 아이디"
            ],
            columnsHtml [
                text "blah blah 이힝"
            ],
            columnsHtml [
                textArea "삭제 사유" False
            ],
            div [ class "userPostBtn"] [
                button [class "button is-primary", onClick PopClose ] [text "확인"],
                button [class "button is-danger", onClick PopClose ] [text "삭제"]
            ]
        ]
    else 
        span [] []

userPost : 
    List{ thumb : String 
        , userId : String
        , article : String
        , createDate : String }  
userPost =
    [
        {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        },
        {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        },
         {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        },
        {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        },
        {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        },
        {   
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            userId = "userID",
            article = "사용자 게시글 Blah blah." ,
            createDate = "2019-10-04"
        }
    ]