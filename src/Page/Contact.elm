module Page.Contact exposing (..)

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
    , errType : String
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
    , username : Maybe String
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
    Api.post Endpoint.contactList (Session.cred session) GetListData body (Decoder.faqlist Faq Data Page)

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
        , goDetail = False
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
        , errType = ""
    }, Cmd.batch[
        Cmd.map DatePickerMsg datePickerCmd
        , Cmd.map EndDatePickerMsg enddatePickerCmd
        , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    -- , faqEncoder send session "" ""
    ])

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
    | ReceivePnum Encode.Value

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.saveCheck SaveId
    , Session.changes GotSession(Session.navKey model.session)
    , Api.sendPageNum ReceivePnum
    ]

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
                            -- old = model.sendData
                            new = {old | page = ok}
                        in
                        
                        ({model | sendData = new , pageNum = pageNum} , faqEncoder new model.session old.start_date old.end_date)
                Err err ->
                    (model, Cmd.none)
        GotSession session ->
            update Search {model | session = session}
            -- ( {model | session = session} , 

            --         Cmd.batch
            --         [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            --         , if model.dateModel == "all" then
            --             faqEncoder send session "" ""
            --         else
            --             faqEncoder send session old.start_date old.end_date]
            
            -- )
            
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
            let
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | sendData = date , pageNum = 1}, 
            Cmd.batch
            [ faqEncoder date model.session "" ""
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)])
            else
            ({model | sendData = date, pageNum = 1 }, 
            Cmd.batch
            [ faqEncoder date model.session old.start_date old.end_date
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)])
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
            (model, Route.pushUrl (Session.navKey model.session) Route.CD)
        GoDetail id ->
            if model.goDetail then
            ({model | id = id}, Api.saveData (Encode.string (String.fromInt id)))
            else
            (model, Cmd.none)
        GetListData (Ok ok)->
            ({model | faqList = ok}, Cmd.none)
        GetListData (Err err)->
            -- let
            --     error = Api.decodeErrors err
            -- in
            -- if error == "401"then
            -- ({model | errType = "GetListData"}, Api.changeInterCeptor (Just error))
            -- else 
            (model, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetMyInfo"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            let 
                menuf = List.head (List.filter (\x -> x.menu_id == 9) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                            
                                if auth "20" then
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True}, Api.pageNum (Encode.int 0) )
                                else
                                ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
            


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
            div [ class "tableCell"] [text 
                (case item.username of
                    Just name ->
                        name
                    Nothing ->
                        "Guest"
                    ) ],
            div [ class "tableCell"] [text (String.dropRight 10 item.inserted_at)],
            if item.is_answer then
                div [ class "tableCell" ] [text "완료"]
            else
                div [ class "tableCell" ] [text "미완료"]
         ]

