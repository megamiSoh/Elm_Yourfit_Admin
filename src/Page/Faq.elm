module Page.Faq exposing (..)

import Browser
import Route exposing (Route)
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode exposing (encode, null)
import Http as Http



type alias Model = {
    session: Session
    , menus : List Menus
    , username : String
    , faqList : Faq
    , sendData: SendData
    , id : Int
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias Faq = 
    { data : List Data
    , pagination : Page
    }

type alias Data = 
    { id : Int
    , inserted_at : String
    , is_answer : Bool
    , title : String
    }

type alias Page = 
    { asked_id : Maybe Int
    , end_date : String
    , is_answer : Maybe Bool
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    , username : String
    }
type alias SendData = 
    { page : Int
    , per_page : Int
    , title : String
    , is_answer: Maybe Bool
    , username : String
    , start_date : String
    , end_date : String
    } 



faqEncoder model session= 
    let
        body = 
            Encode.object
                [ ("page", Encode.int model.page)
                , ("per_page", Encode.int model.per_page)
                , ("title", Encode.string model.title)
                , ("is_answer", 
                    case model.is_answer of
                        Just a ->
                            Encode.bool (a)
                    
                        Nothing ->
                            null
                )
                , ("username", Encode.string model.username)
                , ("start_date", Encode.string model.start_date)
                , ("end_date", Encode.string model.end_date)]
                   |> Http.jsonBody 
    in
    Api.post Endpoint.faqList (Session.cred session) GetListData body (Decoder.faqlist Faq Data Page)


init : Session -> (Model, Cmd Msg)
init session = 
    let
        send = 
            { page = 1
            , per_page = 10
            , title = ""
            , is_answer= Nothing
            , username = ""
            , start_date = ""
            , end_date = ""
            } 
    in
    
    ({ session = session
        , menus = []
        , username = ""
        , sendData = send
        , id = 0
        , faqList = 
            { data = []
            , pagination = 
                { asked_id = Nothing
                , end_date = ""
                , is_answer = Nothing
                , page = 1
                , per_page = 10
                , start_date = ""
                , title = ""
                , total_count = 0
                , username = ""
                } }
    }, Cmd.batch[Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    , faqEncoder send session])

type Msg 
    = NoOp 
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GetListData (Result Http.Error Faq)
    | GoDetail Int
    | SaveId Encode.Value


subscriptions : Model -> Sub Msg
subscriptions model =
    Api.saveCheck SaveId

toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveId complete ->
            (model, Route.pushUrl (Session.navKey model.session) Route.FaqDetail)
        GoDetail id ->
            ({model | id = id}, Api.saveData (Encode.string (String.fromInt id)))
        GetListData (Ok ok)->
            ({model | faqList = ok}, Cmd.none)
        GetListData (Err err)->
            (model, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )

            


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
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
                [headerTable] ++ (List.indexedMap (\idx x -> tableLayout idx x model) model.faqList.data))
            , Pagenation.pagenation
        ] 
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
         div [ class "tableCell" ] [text "사용자"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "답변"]
     ]

--, Route.href (Just Route.UvideoDetail)
tableLayout idx item model = 
        div [ class "tableRow cursor", onClick (GoDetail item.id)] [
            div [ class "tableCell"] [ text (
                    String.fromInt(model.faqList.pagination.total_count - ((model.faqList.pagination.page - 1) * 10) - (idx)
            ))  ],
            div [ class "tableCell"] [text item.title],
            div [ class "tableCell"] [text "" ],
            div [ class "tableCell"] [text (String.dropRight 10 item.inserted_at)],
            if item.is_answer then
                div [ class "tableCell" ] [text "완료"]
            else
                div [ class "tableCell" ] [text "미완료"]
         ]

