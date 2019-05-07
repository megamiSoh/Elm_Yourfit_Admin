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
    , username : String
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



faqEncoder model session start end= 
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
                , ("start_date", Encode.string start)
                , ("end_date", Encode.string end)]
                   |> Http.jsonBody 
    in
    Api.post Endpoint.faqList (Session.cred session) GetListData body (Decoder.faqlist Faq Data Page)

send = 
    { page = 1
    , per_page = 10
    , title = ""
    , is_answer= Nothing
    , username = ""
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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.saveCheck SaveId
    , Session.changes GotSession(Session.navKey model.session)]

toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        old = model.sendData
    in
    case msg of
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
                        ({model | sendData = result, pageNum = model.pageNum - 1}, faqEncoder model.sendData model.session "" "")
                    "next" ->
                        ({model | sendData = result, pageNum = model.pageNum + 1}, faqEncoder model.sendData model.session "" "")
                    "go" -> 
                        ({model | sendData = result}, faqEncoder model.sendData model.session "" "")
                    _ ->
                        (model, Cmd.none)
            else 
                case str of
                    "prev" ->
                        ({model | sendData = result, pageNum = model.pageNum - 1}, faqEncoder model.sendData model.session old.start_date old.end_date)
                    "next" ->
                        ({model | sendData = result, pageNum = model.pageNum + 1}, faqEncoder model.sendData model.session old.start_date old.end_date)
                    "go" -> 
                        ({model | sendData = result}, faqEncoder model.sendData model.session old.start_date old.end_date)
                    _ ->
                        (model, Cmd.none)
        SelectAnswer val ->
            let
                result value = {old | is_answer = value}
            in
            
            case val of
                "all" ->
                    ({model | sendData = result Nothing}, Cmd.none)
                "True" ->
                    ({model | sendData = result (Just True)}, Cmd.none)
                "False" ->                    
                    ({model | sendData = result (Just False)}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Title title ->
            let
                result = {old | title = title}
            in
            
            ({model | sendData = result}, Cmd.none)
        UserId id ->
            let
                result = {old | username = id}
            in
            ({model | sendData = result}, Cmd.none)
        Search ->
            if model.dateModel == "all" then
            (model, faqEncoder model.sendData model.session "" "")
            else
            (model, faqEncoder model.sendData model.session old.start_date old.end_date)
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
            ({model | id = id}, Api.saveData (Encode.string (String.fromInt id)))
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
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )

            


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.faqList.pagination.total_count > 0 then
        { title = "1:1 문의"
    , content = 
        div []
        [ 
            columnsHtml [pageTitle "1:1 문의"],
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
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False Title model.sendData.title,
                    div [ class "field is-horizontal" ] [
                        labelWrap "답변"
                        , div [ class "field-body" ]
                        [ 
                            p [ class "control inputWidth" ]
                            [ 
                            div [ class "select inputWidth"] [
                                select [ class "inputWidth", onInput SelectAnswer  ]
                                [ option [ value "all" ]
                                    [ text "전체"]
                                , option [ value "True"]
                                    [ text "완료"]
                                , option [ value "False"]
                                    [text "미완료"]
                                ]
                            ]
                            ]
                        ] 
                        ]
                ],
                columnsHtml [
                    formInputEvent "사용자" "사용자 아이디를 입력 해 주세요." False UserId model.sendData.username,
                    searchB Search Reset
                ]
                
            ],
            dataCount (String.fromInt model.faqList.pagination.total_count)
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
    { title = "1:1 문의"
    , content = 
        div []
        [ 
            columnsHtml [pageTitle "1:1 문의"],
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
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False Title model.sendData.title,
                    div [ class "field is-horizontal" ] [
                        labelWrap "답변"
                        , div [ class "field-body" ]
                        [ 
                            p [ class "control inputWidth" ]
                            [ 
                            div [ class "select inputWidth"] [
                                select [ class "inputWidth", onInput SelectAnswer]
                                [ option [ value "all" , selected (model.sendData.is_answer == Nothing)]
                                    [ text "전체"]
                                , option [ value "True", selected (model.sendData.is_answer == Just True)]
                                    [ text "완료"]
                                , option [ value "False", selected (model.sendData.is_answer == Just False)]
                                    [text "미완료"]
                                ]
                            ]
                            ]
                        ] 
                        ]
                ],
                columnsHtml [
                    formInputEvent "사용자" "사용자 아이디를 입력 해 주세요." False UserId model.sendData.username,
                    searchB Search Reset
                ]
                
            ],
            dataCount (String.fromInt model.faqList.pagination.total_count)
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
            div [ class "tableCell"] [text item.username ],
            div [ class "tableCell"] [text (String.dropRight 10 item.inserted_at)],
            if item.is_answer then
                div [ class "tableCell" ] [text "완료"]
            else
                div [ class "tableCell" ] [text "미완료"]
         ]

