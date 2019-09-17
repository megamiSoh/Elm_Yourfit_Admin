module Page.ProductManage exposing(..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http exposing (..)
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Api as Api
import Session exposing(Session)
import Route as Route
import Page as Page
import Page.Page exposing (..)
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Pagenation exposing(..)

type alias Model = 
    { session : Session 
    , menus : List Menus
    , username : String
    , firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , show : Bool
    , start_date : String
    , end_date : String
    , today : Maybe Date
    , endday : Maybe Date
    , endShow : Bool
    , todaySave : String
    , dateModel : String
    , listData : ProductList
    , page : Int
    , per_page : Int
    , name : String
    , is_pay : String
    , selectModel : List { code : String , name : String}
    , pageNum : Int
    , is_use : Bool
    , auth : List String
    }

type alias ProductList = 
    { data : List Data
    , paginate : Paginate}

type alias Data = 
    { day_name : Int
    , id : Int
    , inserted_at : String
    , is_pay : Bool
    , is_use: Bool
    , name : String
    , price : Int
    , product_code_name : String }

type alias Paginate = 
    { end_date : String
    , is_use : Maybe Bool
    , name : String
    , page : Int
    , per_page : Int
    , product_code : String
    , start_date : String
    , total_count : Int}

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

listApi : Int -> Int -> String -> String -> String -> String -> Session -> Cmd Msg
listApi page per_page name is_pay start_date end_date session = 
    let
        body =
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page)
                , ("name", Encode.string name)
                , ("is_pay", 
                    if is_pay == "true" then Encode.bool True
                    else if is_pay == "false" then Encode.bool False
                    else Encode.string is_pay)
                , ("start_date", Encode.string start_date)
                , ("end_date", Encode.string end_date) ]
                |> Http.jsonBody
    in
    Api.post Endpoint.productList (Session.cred session) ListComplete body (Decoder.productList ProductList Data Paginate)

init : Session -> (Model , Cmd Msg)
init session = 
    let
        
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    (
    { session = session
    , menus = []
    , username = ""
    , datePickerData = datePickerData
    , endDatePickerData = endDatePickerData
    , firstSelectedDate = Nothing
    , secondSelectedDate = Nothing
    , start_date = ""
    , end_date = ""
    , endday = Nothing
    , today = Nothing
    , endShow = False
    , todaySave = ""
    , show = False
    , dateModel = "all"
    , listData = 
        { data = []
        , paginate = 
            { end_date = ""
            , is_use = Nothing
            , name = ""
            , page = 1
            , per_page = 10
            , product_code = ""
            , start_date = ""
            , total_count = 0}
        }
    , page = 1
    , per_page = 10
    , name = ""
    , is_pay = "null"
    , selectModel = 
        [
            { code = "null"
            , name = "전체"} ,
            { code = "true"
            , name = "유료"} ,
            { code = "false"
            , name = "무료"}
        ]
    , pageNum = 1
    , is_use = False
    , auth = []
    }
    , Cmd.batch
    [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    ]
    )

toSession : Model -> Session
toSession model = 
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.saveCheck GoDetailComplete]

type Msg  
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | ListComplete (Result Http.Error ProductList)
    | NameInput String
    | Search
    | Reset
    | IsPaySelect String
    | IsUse Bool Int
    | IsActiveComplete (Result Http.Error Decoder.Success)
    | GoDetail Int
    | GoDetailComplete Encode.Value
    | GotSession Session
    | PageBtn (Int, String)
    | PageChange

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageChange ->   
            (model, 
            Cmd.batch 
            [ listApi model.page model.per_page model.name model.is_pay model.start_date model.end_date model.session
            ,  Api.pageNum (Encode.int model.page)])
        PageBtn (idx, str) ->
            let 
                list num = 
                    { model | page = idx,
                    start_date = model.start_date,
                    end_date = model.end_date,
                    pageNum = num
                    }
                listAll num = 
                    { model | page = idx,
                    start_date = "",
                    end_date = "",
                    pageNum = num
                    }
            in
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        update PageChange (listAll (model.pageNum - 1))
                    "next" ->
                        update PageChange  (listAll (model.pageNum + 1))
                    "go" -> 
                        update PageChange  (listAll model.pageNum)
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                    "prev" ->
                        update PageChange (list (model.pageNum - 1))
                    "next" ->
                        update PageChange (list (model.pageNum + 1))
                    "go" -> 
                        update PageChange (list model.pageNum)
                    _ ->
                        (model, Cmd.none)
        GotSession session ->
            ({ model | session = session}, 
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            )
        GoDetail id ->
            if memberAuth "20" model then
            (model, Cmd.batch[Api.saveData (Encode.string (String.fromInt id))
            ])
            else 
            (model, Cmd.none)
        GoDetailComplete go ->
            (model,Route.pushUrl (Session.navKey model.session) Route.PD)
        IsActiveComplete (Ok ok) ->
            if model.dateModel == "all" then
                (model,  listApi model.page model.per_page model.name model.is_pay "" "" model.session)
            else
                (model, listApi model.page model.per_page model.name model.is_pay model.start_date model.end_date model.session)
        IsActiveComplete (Err err) ->
            (model, Cmd.none)
        IsUse use id ->
            let
                body = Encode.object 
                    [ ("is_use", Encode.bool use) ]
                    |> Http.jsonBody
            in
            if memberAuth "30" model then
            (model, Api.post (Endpoint.productActive (String.fromInt id)) (Session.cred model.session) IsActiveComplete body Decoder.result)
            else
            (model, Cmd.none)
        IsPaySelect pay ->
            ({model | is_pay = pay}, Cmd.none)
        Search ->
            let
                old = model.listData.paginate
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, 
            Cmd.batch
            [ listApi 1 model.per_page model.name model.is_pay "" "" model.session
            ,Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
            else
            ({model | page = 1, pageNum = 1}, 
            Cmd.batch 
            [ listApi 1 model.per_page model.name model.is_pay  model.start_date model.end_date model.session
            ,Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
        Reset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
            ({model | page = 1, name = "", is_pay = "null",  datePickerData = datePickerData, endDatePickerData = endDatePickerData, dateModel ="all"}, 
            Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        NameInput name ->
            ({model | name = name}, Cmd.none)
        ListComplete (Ok ok) ->
            ({model | listData = ok}, Cmd.none)
        ListComplete (Err err) ->
            (model, Cmd.none)
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
                                    new = getFormattedDate (Just currentSelectedDate) model.endday
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, 
                                end_date = new, 
                                endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new =  getFormattedDate (Just todaydate) model.endday
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, 
                                end_date = new , 
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
                                     new =  getFormattedDate (Just currentSelectedDate) model.today
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, start_date = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new =getFormattedDate (Just todaydate) model.today
                                in
                                
                                ( { newModel | today = Just todaydate, start_date = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
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
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 11) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if model.dateModel == "all" then
                    ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code},
                    listApi model.page model.per_page model.name model.is_pay "" "" model.session)
                    else
                    ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code},
                    listApi model.page model.per_page model.name model.is_pay model.start_date model.end_date model.session)
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = []},
                    listApi model.page model.per_page model.name model.is_pay model.start_date model.end_date model.session)


memberAuth : String -> Model -> Bool
memberAuth num model = 
    List.member num model.auth


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "상품 관리"
    , content =
        div []
            [ columnsHtml [pageTitle "상품 관리"]
            , div [ class "searchWrap" ] [
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
                ]
                , columnsHtml [
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False NameInput model.name,
                    noEmptyselectForm "유 / 무료" False model.selectModel IsPaySelect model.is_pay,
                    searchB Search Reset 
                ]
        ]
        , if memberAuth "50" model then registRoute "상품 등록" Route.PR else div [][]
        , dataCount (String.fromInt model.listData.paginate.total_count)
        , if List.length model.listData.data > 0 then
         div [class "table"]
        ( [headerTable] ++
        (List.indexedMap (\idx item -> tableLayout idx item model) model.listData.data)
        )
        else
        div [class "table"] [
            headerTable 
            , tr[][
            td [colspan 8, class "noSearch"] [text "검색 결과가 없습니다."]
            ]
        ]
        , pagination 
            PageBtn
            model.listData.paginate
            model.pageNum 
    ]
    , menu =  
        aside [ class "menu"] [
            Page.header model.username
            ,ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
            ]
    }
            

headerTable : Html Msg
headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "유/무료"],
         div [ class "tableCell" ] [text "기간", span [class "productSmall"][text "(단위/일)"]],
         div [ class "tableCell" ] [text "가격", span [class "productSmall"][text "(단위/원)"]],
         div [ class "tableCell" ] [text "상품 분류"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"]
     ]

tableLayout : Int -> Data -> Model -> Html Msg
tableLayout idx item model = 
        div [class "tableRow"] [
                div [ class "tableCell", onClick (GoDetail item.id)] [
                    text ( String.fromInt(model.listData.paginate.total_count - ((model.listData.paginate.page - 1) * 10) - (idx)
                    )) 
                ],
                div [ class "tableCell", onClick (GoDetail item.id)] [text item.name],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (if item.is_pay then "유료" else "무료")],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.fromInt item.day_name)],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.fromInt item.price)],
                div [ class "tableCell", onClick (GoDetail item.id)] [text item.product_code_name],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.dropRight 10 item.inserted_at)],
                div [ class "tableCell"] [
                    if item.is_use then
                        div [class "button is-small is-success", onClick (IsUse (not item.is_use) item.id)][text "게시 중"]
                    else
                        div [class "button is-small", onClick (IsUse (not item.is_use) item.id)][text "게시하기"]
                ]
         ]