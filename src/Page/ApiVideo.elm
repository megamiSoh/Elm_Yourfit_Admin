module Page.ApiVideo exposing (..)

import Browser
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing( class )
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (..)

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


view : Model -> {title : String , content : Html Msg}
view model =
    { title = "외부 API 영상"
    , content = 
        div [ class "container is-fluid" ]
        [ 
            
            columnsHtml [pageTitle "외부 API 영상"],
            div [ class "searchWrap" ] [
                columnsHtml [
                    searchDataSet "등록일"
                ],
                columnsHtml [
                    formSelect "카테고리" False ,
                    formInput "제목" "제목을 입력 해 주세요." False,
                    searchBtn
                ]
                
            ],
            (registRoute "영상 등록" Route.ApiVideoRegist),
            div [class "table"] 
            ([headerTable] ++ List.map tableLayout dataTable)
            ,Pagenation.pagenation
        ]
    } 




headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "영상개수"],
         div [ class "tableCell" ] [text "카테고리"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "활성화"],
         div [ class "tableCell" ] [text "미리보기"]
     ]

--, Route.href (Just Route.UvideoDetail)
tableLayout item = 
        div [class "tableRow"] [
                a [ class "tableCell" , Route.href (Just Route.ApiDetail) ] [text (String.fromInt(item.no))],
                a [ class "tableCell" , Route.href (Just Route.ApiDetail) ] [text item.title],
                a [ class "tableCell" , Route.href (Just Route.ApiDetail) ] [text (String.fromInt(item.videoAmount))],
                a [ class "tableCell" , Route.href (Just Route.ApiDetail) ] [text item.category],
                a [ class "tableCell" , Route.href (Just Route.ApiDetail) ] [text item.createDate],
                 div [ class "tableCell" ] [
                        if item.isActive then
                            button [ class "button is-small"] [ text "활성" ]
                        else
                            button [class "button is-small"] [text "비활성"]
                    ],
                 div [ class "tableCell" ] [
                        button [class "button is-small"] [text "미리보기"]
                    ]
         ]          


dataTable 
    = [
        {
            no = 1 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 2 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 3 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 4 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 5 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 6 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 7 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 8 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 9 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        },
        {
            no = 10 ,
            title = "다이어트",
            videoAmount = 5,
            category ="피트니스",
            createDate = "2019-02-01",
            isActive = True,
            videoShow = "https://youtu.be/hVC7BNGrZBI"
        }



    ] 