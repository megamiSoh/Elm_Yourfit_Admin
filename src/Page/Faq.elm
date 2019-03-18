module Page.Faq exposing (..)

import Browser
import Route exposing (Route)
import Html exposing(..)
import Html.Attributes exposing(..)
import Pagenation exposing(..)
import Page.Page exposing(..)
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

            


view : Model -> {title : String , content : Html msg}
view model =
    { title = "1:1 문의"
    , content = 
        div [ class "container is-fluid" ]
        [ 
            columnsHtml [pageTitle "1:1 문의"],
            div [ class "searchWrap" ] [
                columnsHtml [
                    searchDataSet "등록일"
                ],
                columnsHtml [
                    formInput "제목명" "제목 명을 입력 해 주세요." False,
                    formSelect "답변" False
                ],
                columnsHtml [
                    formInput "사용자" "사용자 아이디를 입력 해 주세요." False,
                    searchBtn
                ]
                
            ],
            userDataCount
            ,  div [class "table"] (
                [headerTable] ++ (List.map tableLayout dataTable))
            ,Pagenation.pagenation
        ] 
    }

headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "사용자"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "답변"]
     ]

--, Route.href (Just Route.UvideoDetail)
tableLayout item = 
        div [class "tableRow"] [
                a [ class "tableCell" , Route.href (Just Route.FaqDetail) ] [text (String.fromInt(item.no))],
                a [ class "tableCell" , Route.href (Just Route.FaqDetail) ] [text item.title],
                a [ class "tableCell" , Route.href (Just Route.FaqDetail) ] [text item.userId],
                a [ class "tableCell" , Route.href (Just Route.FaqDetail) ] [text item.createDate],
                if item.answer then
                    div [ class "tableCell" ] [text "완료"]
                else
                    div [ class "tableCell" ] [text "미완료"]
         ]


dataTable = 
    [
        {
        no = 1,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 2,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 3,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 4,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 5,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 6,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 7,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 8,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 9,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        },
        {
        no = 10,
        title = "helloworld",
        userId = "finalcompany",
        createDate = "2019-02-01",
        answer = False
        }
    ]