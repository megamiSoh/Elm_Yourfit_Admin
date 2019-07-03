module Page.ApiVideo exposing (..)

import Browser
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing( class )
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias Model = {
    session: Session
    , menus : List Menus
    , username : String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session = ({
        session = session
        , menus = []
        , username =""
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo))

type Msg 
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.Profile)
    | GotSession Session



toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ({ model | session = session}, 
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo)
            )
        NoOp ->
            ( model, Cmd.none )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            ( {model |  menus = Decoder.mymenu item, username = Decoder.myname item}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "외부 API 영상"
    , content = 
        div [] [text "준비 중 입니다."]
        -- div [ class "container is-fluid" ]
        -- [ 
            
        --     columnsHtml [pageTitle "외부 API 영상"],
        --     div [ class "searchWrap" ] [
        --         columnsHtml [
        --             searchDataSet "등록일"
        --         ],
        --         columnsHtml [
        --             formSelect "카테고리" False ,
        --             formInput "제목" "제목을 입력 해 주세요." False,
        --             searchBtn
        --         ]
                
        --     ],
        --     (registRoute "영상 등록" Route.ApiVideoRegist),
        --     div [class "table"] 
        --     ([headerTable] ++ List.map tableLayout dataTable)
        --     ,Pagenation.pagenation
        -- ]
          , menu =  
            aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
    
    } 




headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "영상개수"],
         div [ class "tableCell" ] [text "카테고리"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"],
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
                            button [ class "button is-small is-success"] [ text "게시 중" ]
                        else
                            button [class "button is-small"] [text "게시 하기"]
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