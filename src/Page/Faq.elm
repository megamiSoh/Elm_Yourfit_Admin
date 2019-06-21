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
import Json.Decode as Decode
import Http as Http
import Date exposing (..)
import DatePicker exposing (Msg(..))


type alias Model = {
    firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , show : Bool
    , session: Session
    , menus : List Menus
    , username : String
    , faqList : Faq
    , sendData: SendData
    , id : Int
    , today : Maybe Date
    , endday : Maybe Date
    , endShow : Bool
    , todaySave : String
    , dateModel : String
    , pageNum : Int
    , goDetail : Bool
    , goRegist : Bool
    , auth : List String
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
    , is_use : Bool
    , title : String
    }

type alias Page = 
    { end_date : String
    , is_use : Maybe Bool
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    }
type alias SendData = 
    { page : Int
    , per_page : Int
    , title : String
    , start_date : String
    , end_date : String
    } 

usepost is_use session id =
    let
        body =
            Encode.object
                [( "is_use ", Encode.bool is_use)]
                    |> Http.jsonBody
    in
    
    Api.post (Endpoint.faqUse id) (Session.cred session) GetUseSuccess body (Decoder.result) 

faqEncoder model session start end= 
    let
        body = 
            Encode.object
                [ ("page", Encode.int model.page)
                , ("per_page", Encode.int model.per_page)
                , ("title", Encode.string model.title)
                , ("start_date", Encode.string start)
                , ("end_date", Encode.string end)]
                   |> Http.jsonBody 
    in
    Api.post Endpoint.faqList (Session.cred session) GetListData body (Decoder.faqNewList Faq Data Page)

send = 
    { page = 1
    , per_page = 10
    , title = ""
    , start_date = ""
    , end_date = ""
    } 

init : Session -> (Model, Cmd Msg)
init session = 
    let
        
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    
    ({ datePickerData = datePickerData
        , endDatePickerData = endDatePickerData
        , firstSelectedDate = Nothing
        , secondSelectedDate = Nothing
        , session = session
        , menus = []
        , username = ""
        , sendData = send
        , id = 0
        , pageNum = 1
        , today = Nothing
        , endday = Nothing
        , show = False
        , endShow = False
        , todaySave = ""
        , dateModel = "all"
        , goDetail = False
        , goRegist = False
        , auth = []
        , faqList = 
            { data = []
            , pagination = 
                { end_date = ""
                , is_use = Nothing
                , page = 1
                , per_page = 10
                , start_date = ""
                , title = ""
                , total_count = 0
                } }
    }, Cmd.batch[
        Cmd.map DatePickerMsg datePickerCmd
        , Cmd.map EndDatePickerMsg enddatePickerCmd
        , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    , faqEncoder send session "" ""])

type Msg 
    = NoOp 
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GetListData (Result Http.Error Faq)
    | GoDetail Int
    | SaveId Encode.Value
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | Search
    | Reset
    | Title String
    | UserId String
    | SelectAnswer String
    | PageBtn (Int, String)
    | GotSession Session
    | GetUseSuccess (Result Http.Error Decoder.Success)
    | UseGo Bool String
    | SuccessUse (Result Http.Error Decoder.Success)
    | ReceivePnum Encode.Value

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.saveCheck SaveId
    , Session.changes GotSession(Session.navKey model.session)
    , Api.sendPageNum ReceivePnum]

toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        old = model.sendData
    in
    case msg of
        ReceivePnum num ->
            let 
                val = Decode.decodeValue Decode.int num
            in
            
            case val of
                Ok ok ->
                    let
                        pageNum = 
                            if ok < 10 then 1 
                            else 
                                if (((ok // 10) * 10) - ok) == 0 then
                                ok // 10
                                else
                                ok // 10 + 1
                        
                    in
                    if model.dateModel == "all" then
                        let
                            new = {old | page = ok, end_date = "", start_date = ""}
                        in
                        
                        ({model | sendData = new , pageNum = pageNum} , faqEncoder new model.session "" "")
                    else
                        let
                            new = {old | page = ok}
                        in
                        
                        ({model | sendData = new , pageNum = pageNum} , faqEncoder new model.session old.start_date old.end_date)
                Err err ->
                    (model, Cmd.none)
        SuccessUse (Ok ok) ->
            if model.dateModel == "all" then
            (model, faqEncoder send model.session "" "")
            else
            (model, faqEncoder send model.session old.start_date old.end_date)
        SuccessUse (Err err) ->
            (model, Cmd.none)
        UseGo use id ->
            let
                body = Encode.object
                    [("is_use", Encode.bool (not use))]
                    |> Http.jsonBody
            in
            
            (model, Api.post (Endpoint.faqUse id) (Session.cred model.session) SuccessUse body (Decoder.result) )
        GetUseSuccess (Ok ok) ->
            (model, Cmd.none)
        GetUseSuccess (Err err) ->
            (model, Cmd.none)
        GotSession session ->
            if model.dateModel == "all" then
            ({model | session = session}, faqEncoder send session "" "")
            else
            ({model | session = session}, faqEncoder send session old.start_date old.end_date)
        PageBtn (idx, str) ->
            let
                result = {old | page = idx}
            in
            
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | sendData = result, pageNum = model.pageNum - 1}, Cmd.batch[faqEncoder result model.session "" ""
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | sendData = result, pageNum = model.pageNum + 1}, Cmd.batch[faqEncoder result model.session "" ""
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | sendData = result},Cmd.batch[faqEncoder result model.session "" ""
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else 
                case str of
                    "prev" ->
                        ({model | sendData = result, pageNum = model.pageNum - 1}, Cmd.batch[faqEncoder result model.session old.start_date old.end_date
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | sendData = result, pageNum = model.pageNum + 1}, Cmd.batch[faqEncoder result model.session old.start_date old.end_date
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | sendData = result}, Cmd.batch[faqEncoder result model.session old.start_date old.end_date
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
        SelectAnswer val ->
                    (model, Cmd.none)
        Title title ->
            let
                result = {old | title = title}
            in
            
            ({model | sendData = result}, Cmd.none)
        UserId id ->
            (model, Cmd.none)
        Search ->
            let
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, faqEncoder date model.session "" "")
            else
            ({model | sendData = date , pageNum = 1}, faqEncoder date model.session old.start_date old.end_date)
        Reset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
            
            ({model | sendData = send,  datePickerData = datePickerData, endDatePickerData = endDatePickerData, dateModel ="all"}, 
            Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        DateValue str->
            ({model | dateModel = str},Cmd.none)
        EndShow ->
            ( {model | endShow = not model.endShow, show = False}, Cmd.none )
        Show ->
            ( {model | show = not model.show, endShow = False }, Cmd.none )
        EndDatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.endDatePickerData
                |> (\( data, cmd ) ->
                        ( { model | endDatePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                        case datePickerMsg of    
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)
                                                        
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just currentSelectedDate) model.endday}
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, 
                                sendData = new, 
                                endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just todaydate) model.endday}
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, 
                                sendData = new , 
                                todaySave = getFormattedDate (Just todaydate) model.endday}
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        DatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.datePickerData
                |> (\( data, cmd ) ->
                        ( { model | datePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                        case datePickerMsg of         
                            CancelClicked ->
                                ({newModel | show = False}, cmd)                   
                            SubmitClicked currentSelectedDate ->
                                let
                                     new = {old | start_date = getFormattedDate (Just currentSelectedDate) model.today}
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, sendData = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | start_date = getFormattedDate (Just todaydate) model.today}
                                in
                                
                                ( { newModel | today = Just todaydate, sendData = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        SaveId complete ->
            (model, Route.pushUrl (Session.navKey model.session) Route.FaqDetail)
        GoDetail id ->
            if memberAuth "20" model then
            ({model | id = id}, Api.saveData (Encode.string (String.fromInt id)))
            else
            (model, Cmd.none)
        GetListData (Ok ok)->
            ({model | faqList = ok}, Cmd.none)
        GetListData (Err err)->
            (model, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        GetMyInfo (Err error) ->
            let
                serverErrors = 
                    Api.decodeErrors error
            in 
            ( model, Session.changeInterCeptor (Just serverErrors) )

        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 10) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                                ({model | auth = a.menu_auth_code, menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0))
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
            

memberAuth num model= List.member num model.auth


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.faqList.pagination.total_count > 0 then
        { title = "FAQ"
    , content = 
        div []
        [ 
            columnsHtml [pageTitle "FAQ"],
            div [ class "searchWrap" ] [
                columnsHtml [
                   searchDate 
                            "등록일"
                            Show 
                            (datepicker model DatePickerMsg) 
                            (getFormattedDate model.firstSelectedDate model.today) 
                            EndShow (endDatePicker model EndDatePickerMsg) 
                            (getFormattedDate model.secondSelectedDate model.endday) DateValue model.dateModel
                            (if model.dateModel == "all" then
                                "readOnly"
                            else
                                ""
                            )
                ],
                columnsHtml [
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False Title model.sendData.title
                ]
                ,
                columnsHtml [
                    searchB Search Reset
                ]
                
            ],
            dataCount (String.fromInt model.faqList.pagination.total_count)
            , div [] [
                if memberAuth "50" model then
                registRoute "등록" Route.FaqRegist
                else
                div [] []
            ]
            ,  div [class "table"] (
                [headerTable] ++ (List.indexedMap (\idx x -> tableLayout idx x model) model.faqList.data))
            , pagination 
                    PageBtn
                    model.faqList.pagination
                    model.pageNum
        ] 
      , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }
    else
    { title = "FAQ"
    , content = 
        div []
        [ 
            columnsHtml [pageTitle "FAQ"],
            div [ class "searchWrap" ] [
                columnsHtml [
                   searchDate 
                            "등록일"
                            Show 
                            (datepicker model DatePickerMsg) 
                            (getFormattedDate model.firstSelectedDate model.today) 
                            EndShow (endDatePicker model EndDatePickerMsg) 
                            (getFormattedDate model.secondSelectedDate model.endday) DateValue model.dateModel
                            (if model.dateModel == "all" then
                                "readOnly"
                            else
                                ""
                            )
                ],
                columnsHtml [
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False Title model.sendData.title
                ]
                , columnsHtml [
                    searchB Search Reset
                ]
                
            ],
            dataCount (String.fromInt model.faqList.pagination.total_count)
            , div [] [
                if memberAuth "50" model then
                registRoute "등록" Route.FaqRegist
                else
                div [] []
            ]
            ,  div [class "table"] [
                headerTable
                , tr [] [
                    td[colspan 5, class "noSearch"][
                        text "검색 결과가 없습니다."
                    ]
                ]
            ]
            
            , pagination 
                    PageBtn
                    model.faqList.pagination
                    model.pageNum 
            -- Pagenation.pagination PageBtn model.faqList.pagination
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
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"]
     ]

tableLayout idx item model = 
        div [ class "tableRow cursor"] [
            div [ class "tableCell", onClick (GoDetail item.id)] [ text (
                    String.fromInt(model.faqList.pagination.total_count - ((model.faqList.pagination.page - 1) * 10) - (idx)
            ))  ],
            div [ class "tableCell", onClick (GoDetail item.id)] [text item.title],
            div [ class "tableCell"] [text (String.dropRight 10 item.inserted_at)],
            if memberAuth "30" model then
                if item.is_use then
                div [ class "tableCell " ] [
                    p [class "button is-small is-success", onClick (UseGo item.is_use (String.fromInt item.id))][text "게시 중"]]
                else
                    div [ class "tableCell" ] [
                        p [class "button is-small", onClick (UseGo item.is_use (String.fromInt item.id))][text "게시 하기"]]
            else
                if item.is_use then
                div [ class "tableCell " ] [
                    p [class "button is-small is-success"][text "게시 중"]]
                else
                    div [ class "tableCell" ] [
                        p [class "button is-small"][text "게시 하기"]]
         ]

