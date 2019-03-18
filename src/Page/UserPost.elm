module Page.UserPost exposing (..)

import Browser
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Pagenation exposing (..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (Route)

type alias Model =
    {
        popUp : Bool,
        session : Session
    }

init : Session -> (Model, Cmd Msg)
init session= 
    ({
        popUp = False
        , session = session
    }, Cmd.none)

type Msg = PopClose | PopOpen

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PopClose ->
            let _ = Debug.log "last Check" model.session
                
            in
            
            ({model | popUp = False}, Cmd.none)

        PopOpen ->
            ({model | popUp = True}, Cmd.none)

toSession : Model -> Session
toSession model =
    model.session


view : Model -> {title : String , content: Html Msg}
view model = 
    { title = "사용자 게시물 관리"
    , content =
        div [ class "container is-fluid" ]
        [ 
            columnsHtml [pageTitle "사용자 게시물 관리"],
            div [ class "searchWrap" ] [
                columnsHtml [
                    searchDataSet "등록일"
                ],
                columnsHtml [
                    formInput "사용자 ID" "사용자 ID을 입력 해 주세요." False,
                    searchBtn
                ]
                
            ],
            columnsHtml [
                userDataCount
            ],
            columnsHtml [
                div [class "userPostWrap"] (
                List.indexedMap userPostItem userPost
                )
            ]
            , Pagenation.pagenation,
            detailPop model
        ]
    }


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

userPostItem idx item =
    label [ class "media", onClick PopOpen ]
        [ 
            figure [ class "media-left" ]
            [ p [ class "image is-64x64" ]
                [ img [ src item.thumb ]
                    []
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ p []
                    [ strong []
                    [ text item.userId ]
                    , div []
                        [text item.article ] 
                    ]
                ]
            
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ p [class "dateWrap"]
                    [
                        text item.createDate
                    ]
                ]
            ]
        ]
    
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