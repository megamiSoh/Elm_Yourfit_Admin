module Page.Info exposing(..)

import Browser
import Html exposing(..)
import Html.Attributes exposing(class, style, colspan)
import Pagenation exposing(..)
import Session exposing (Session)
import Page.Page exposing (..)
import Route exposing (Route)
import Json.Encode as Encode
import Http exposing (..)
import Html.Events exposing(..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Api.Endpoint as Endpoint
import Api as Api
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Page as Page
import Api.Decode as Decoder

type alias ListForm =
    { page : Int
    , per_page : Int
    , title : String
    , start_date : String
    , end_date : String
    }


type alias Model = 
    { session: Session
    , listInit : ListForm
    , resultForm : ResultForm
    , isActive : Bool
    , firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , today : Maybe Date
    , endday : Maybe Date
    , endShow : Bool
    , dateModel : String
    , show : Bool
    , todaySave : String
    , menus : List Menus
    , username : String
    , goRegist : Bool
    , goDetail : Bool
    , pageNum : Int
    , auth : List String
    , errType : String
    , activeId : String
    }

type alias Menus =
    { menu_auth_code : List String
    , menu_id : Int
    , menu_name : String }
    

type alias ResultForm = 
    { data : List Data
    , paginate : Paginate}

type alias Data =
    { id : Int
    , inserted_at : String
    , is_use : Bool
    , title : String
    }

type alias Paginate  =
    { end_date : String
    , page : Int
    , per_page : Int
    , start_date : String 
    , title : String
    , total_count : Int
    }

type alias ActiveModel = 
    { result : String }

dataDecoder : Decoder Data
dataDecoder = 
    Decode.succeed Data
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string

pagenateDecoder : Decoder Paginate
pagenateDecoder = 
    Decode.succeed Paginate
        |> required "end_date" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

resultFormDecoder : Decoder ResultForm
resultFormDecoder = 
    Decode.succeed ResultForm
        |> required "data" (list dataDecoder )
        |> required "paginate" pagenateDecoder

resultDecoder : Decoder ActiveModel
resultDecoder = 
    Decode.succeed ActiveModel
        |>required "result" string


listInitial : ListForm
listInitial = 
    { page= 1
    , per_page= 10
    , title= ""
    , start_date= ""
    , end_date= ""
    }

managelist : ListForm -> Session -> Cmd Msg
managelist form session =
    let
        list =
            Encode.object
                [ ("page", Encode.int form.page)
                , ("per_page", Encode.int form.per_page)
                , ("title", Encode.string form.title)
                , ("start_date", Encode.string form.start_date)
                , ("end_date", Encode.string form.end_date)
                ]
        body =
            list    
                |> Http.jsonBody
    in
    Api.post Endpoint.info (Session.cred session) GetList body resultFormDecoder 

encodeList : Bool -> String -> Session -> Cmd Msg
encodeList use idstr session=  
    let
        body = 
            Encode.object
                [("is_use", Encode.bool use)]   
                    |> Http.jsonBody 
    in
        Api.post (Endpoint.infoActive idstr) (Session.cred session) HttpResult body resultDecoder
    

init : Session -> (Model, Cmd Msg)
init session = 
    let
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    (
    { session = session
    , listInit = listInitial
    , isActive = False
    , datePickerData = datePickerData
    , endDatePickerData = endDatePickerData
    , firstSelectedDate = Nothing
    , secondSelectedDate = Nothing
    , show = False
    , today = Nothing
    , endShow = False
    , endday = Nothing
    , dateModel = "all"
    , todaySave = ""
    , goRegist = False
    , goDetail = False
    , username = ""
    , menus = []
    , pageNum = 1
    , auth = []
    , resultForm = 
        {
            data = [],
            paginate = {
                end_date = "",
                title = "",
                page = 0,
                per_page = 0,
                start_date = "",
                total_count = 0
            }
        }
    , errType = ""
    , activeId = ""
    }, 
    Cmd.batch
    [  Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    ])


toSession : Model -> Session
toSession model = 
    model.session

type Msg 
    = NoOp
    | GetList (Result Http.Error ResultForm)
    | Search 
    | Reset
    | Title String
    | GotSession Session
    | GetId String
    | Check Encode.Value
    | IsActive String Bool
    | HttpResult (Result Http.Error ActiveModel)
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String 
    | PageBtn (Int, String)
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | ReceivePnum Encode.Value
    | ShowList

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowList ->
            (model, encodeList model.isActive model.activeId model.session)
        ReceivePnum num ->
            let
                val = Decode.decodeValue Decode.int num
            in
            
            case val of
                Ok ok ->
                    let
                        old = model.listInit
                        list = 
                            { page = ok,
                            per_page = 10,
                            title= old.title,
                            start_date = old.start_date,
                            end_date = old.end_date}
                        listAll = 
                            { page = ok,
                            per_page = 10,
                            title= old.title,
                            start_date = "",
                            end_date = ""}

                        pageNum = 
                            if ok < 10 then 1 
                            else 
                                if (((ok // 10) * 10) - ok) == 0 then
                                ok // 10
                                else
                                ok // 10 + 1
                    in
                    
                    if model.dateModel == "all" then
                        
                        ({model | listInit = listAll , pageNum = pageNum} , managelist listAll model.session)
                    else
                        ({model | listInit = list , pageNum = pageNum} , managelist list model.session)
                Err err ->
                    (model, Cmd.none)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetList"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            let 
                menuf = List.head (List.filter (\x -> x.menu_id == 8) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                            
                                if auth "20" then
                                    if auth "50" then
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True, goRegist = True, auth = a.menu_auth_code}, Api.pageNum (Encode.int 0) )
                                    else
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True, auth = a.menu_auth_code}, Api.pageNum (Encode.int 0) )
                                else if auth "50" then
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, goRegist = True, auth = a.menu_auth_code}, Api.pageNum (Encode.int 0) )
                                else
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code}, Api.pageNum (Encode.int 0) )
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
        PageBtn (idx, str) ->
            let 
                old = model.listInit
                list = 
                    { page = idx,
                    per_page = 10,
                    title= old.title,
                    start_date = old.start_date,
                    end_date = old.end_date}
                listAll = 
                    { page = idx,
                    per_page = 10,
                    title= old.title,
                    start_date = "",
                    end_date = ""}
            in
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | listInit = listAll, pageNum = model.pageNum - 1}, Cmd.batch[managelist listAll model.session
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | listInit = listAll, pageNum = model.pageNum + 1}, Cmd.batch[managelist listAll model.session
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | listInit = listAll}, Cmd.batch[managelist listAll model.session
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                    "prev" ->
                        ({model | listInit = list, pageNum = model.pageNum - 1}, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | listInit = list, pageNum = model.pageNum + 1}, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | listInit = list}, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
        EndShow ->
            ( {model | endShow = not model.endShow, show = False}, Cmd.none )
        Show ->
            ( {model | show = not model.show, endShow = False }, Cmd.none )
        DateValue str->
            ({model | dateModel = str},Cmd.none)
        EndDatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.endDatePickerData
                |> (\( data, cmd ) ->
                        ( { model | endDatePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                    let
                        old = model.listInit
                    in
                        case datePickerMsg of     
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)                       
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just currentSelectedDate) model.endday}
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, listInit = new, endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just todaydate) model.endday}
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, listInit = new , todaySave = getFormattedDate (Just todaydate) model.endday}
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
                        let
                            old = model.listInit
                        in
                        case datePickerMsg of    
                            CancelClicked ->
                                ({newModel | show = False}, cmd)                        
                            SubmitClicked currentSelectedDate ->
                                let
                                     new = {old | start_date = getFormattedDate (Just currentSelectedDate) model.today}
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, listInit = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | start_date = getFormattedDate (Just todaydate) model.today}
                                in
                                
                                ( { newModel | today = Just todaydate, listInit = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        NoOp ->
            ( model, Cmd.none )
        GetList (Err err) ->
            (model, Cmd.none)
        GetList (Ok item) ->
            ( {model | resultForm = item}, Cmd.none )

        Search ->
            let
                old = model.listInit
                list = 
                    {
                    page= 1,
                    per_page= 10,
                    title= old.title,
                    start_date= "",
                    end_date= ""
                    }
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, 
            Cmd.batch
            [ managelist list model.session
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)
            ])
            else
            ({model | listInit = date , pageNum = 1}, 
            Cmd.batch
            [ managelist date model.session
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)
            ])
        Reset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
                old = model.listInit
                list = 
                    {
                    page= 1,
                    per_page= 10,
                    title= "",
                    start_date= model.todaySave,
                    end_date= model.todaySave
                    }
            in
                ( {model | dateModel = "all", datePickerData = datePickerData, endDatePickerData = endDatePickerData, listInit = list}
                , Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        Title str ->
            let
                old = model.listInit
                new = 
                    {old | title = str}    
            in
            
            ({model | listInit = new} , Cmd.none)

        GotSession session ->
            case model.errType of
                "GetList" ->
                    update Search {model | session = session}
                "HttpResult" ->
                    update ShowList {model | session = session}
                _ ->
                    (model, Cmd.none)
        GetId id ->
            let
                encode = Encode.string id
            in
            (model, Cmd.batch [
                 Api.infodata encode
            ])
        Check id ->
            let
                decodeId = Decode.decodeValue Decode.string id
            in
                if model.goDetail then
                case decodeId of
                    Ok go ->
                        (model , Route.pushUrl (Session.navKey model.session) Route.InfoDetail) 
                
                    Err _  ->
                        (model,Cmd.none)
                else
                (model, Cmd.none)
        IsActive id use->
            ({model | isActive = not use, activeId = id}, Cmd.batch[encodeList (not use) id model.session])
        HttpResult (Ok item)->
            let
                old = model.listInit
                list = 
                    {
                    page= 1,
                    per_page= 10,
                    title= old.title,
                    start_date= "",
                    end_date= ""
                    }
            in
                if model.dateModel == "all" then
                (model, managelist list model.session)
                else
                (model , managelist model.listInit model.session )
        HttpResult (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "HttpResult"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "공지사항"
    , content =
        div []
        [ 
            columnsHtml [pageTitle "공지사항"],
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
                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False Title model.listInit.title,
                    searchB Search Reset 
                ]
            ],
            div [] [
                if model.goRegist then
                registRoute "등록" Route.InfoRegist
                else
                div [] []
                , dataCount (String.fromInt(model.resultForm.paginate.total_count))
            ] 
            , div [] [
                if List.length model.resultForm.data > 0 then
                    div [class "table"](
                            [headerTable] ++ (List.indexedMap (\idx x -> tableLayout idx x model
                            
                            ) model.resultForm.data)
                        )
                    else 
                    div [class "table"] [
                        headerTable 
                        , tr[][
                        td [colspan 4, class "noSearch"] [text "검색 결과가 없습니다."]
                        ]
                    ]
           ]
            , pagination 
                    PageBtn
                    model.resultForm.paginate
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
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"]
     ]

tableLayout : Int -> Data -> Model -> Html Msg
tableLayout idx item model = 
        div [class "tableRow", style "cursor" (
            if model.goDetail then
            "pointer"
            else
            "no-drop"
        )] [
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text (
                    String.fromInt(model.resultForm.paginate.total_count - ((model.resultForm.paginate.page - 1) * 10) - (idx)
            )) ],
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.title],
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text (String.dropRight 10 (item.inserted_at))],
                div [ class "tableCell" ] [
                    if List.member "30" model.auth then
                        if item.is_use then
                        button [class "button is-small is-success", onClick (IsActive (String.fromInt item.id) item.is_use)] [text "게시 중"]
                        else 
                            button [class "button is-small",  onClick (IsActive (String.fromInt item.id) item.is_use)] [text "게시 하기"]
                    else
                        if item.is_use then
                            button [class "button is-small is-success"] [text "게시 중"]
                        else 
                            button [class "button is-small"] [text "게시 하기"]
                 ]
         ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Session.changes GotSession (Session.navKey model.session)
    , Api.infoCheck Check
    , Api.sendPageNum ReceivePnum
    ]