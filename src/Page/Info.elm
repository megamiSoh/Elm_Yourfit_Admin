module Page.Info exposing(..)

import Browser
import Html exposing(..)
import Html.Attributes exposing(class)
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
    {
        page : Int,
        per_page : Int,
        title: String,
        start_date : String,
        end_date : String
    }


type alias Model = {
    session: Session
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
    }
type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String}
    

type alias ResultForm = 
    { data : List Data
    , paginate : Paginate}

type alias Data =
    { id : Int
    , inserted_at: String
    , is_use: Bool
    , title: String
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
    {result : String}

dataDecoder = 
    Decode.succeed Data
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string

pagenateDecoder = 
    Decode.succeed Paginate
        |> required "end_date" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int
        -- |> hardcoded 1.0

resultFormDecoder = 
    Decode.succeed ResultForm
        |> required "data" (list dataDecoder )
        |> required "paginate" pagenateDecoder

listInitial = 
    {
    page= 1,
    per_page= 10,
    title= "",
    start_date= "",
    end_date= ""
    }
init : Session -> (Model, Cmd Msg)
init session = 
    let
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    
    ({
        session = session
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
        , username = ""
        , menus = []
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
    }, 
    Cmd.batch
    [ managelist listInitial session
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)])


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
    | IsActive String
    | HttpResult (Result Http.Error ActiveModel)
    | SessionCheck  Encode.Value
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String 
    | PageBtn (Int, String)
    | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
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
                        ({model | listInit = listAll}, managelist listAll model.session)
                    "next" ->
                        ({model | listInit = listAll}, managelist listAll model.session)
                    "go" -> 
                        ({model | listInit = listAll}, managelist listAll model.session)
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                    "prev" ->
                        ({model | listInit = list}, managelist list model.session)
                    "next" ->
                        ({model | listInit = list}, managelist list model.session)
                    "go" -> 
                        ({model | listInit = list}, managelist list model.session)
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
        GetList (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            ( model
           , Session.changeInterCeptor (Just serverErrors) 
            )

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
            in
            
            if model.dateModel == "all" then
            (model, managelist list model.session)
            else
            (model, managelist model.listInit model.session)
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
            ({model | session = session}
            , Cmd.batch [
            managelist listInitial session
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo) 
            ]
            )
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
                case decodeId of
                    Ok go ->
                        (model , Route.pushUrl (Session.navKey model.session) Route.InfoDetail) 
                
                    Err _  ->
                        (model,Cmd.none)
        IsActive id->
            ({model | isActive = not model.isActive}, Cmd.batch[encodeList model id])
        HttpResult (Ok item)->
            (model , managelist model.listInit model.session )
        HttpResult (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, managelist model.listInit model.session)
                    Err _ ->
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
            dataCount (String.fromInt(model.resultForm.paginate.total_count)),
            registRoute "등록" Route.InfoRegist,
            div [class "table"](
                    [headerTable] ++ (List.indexedMap (tableLayout) model.resultForm.data)
                )
            ,Pagenation.pagination 
                PageBtn
                model.resultForm.paginate
        ]
        , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    
    }

encodeList model idstr=  
    let
        body = 
            Encode.object
                [("is_use", Encode.bool (not model.isActive))]   
                    |> Http.jsonBody 
    in
        Api.post (Endpoint.infoActive idstr) (Session.cred model.session) HttpResult body resultDecoder
    
resultDecoder = 
    Decode.succeed ActiveModel
        |>required "result" string

headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"]
     ]

tableLayout idx item = 
        div [class "tableRow"] [
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text (String.fromInt(idx + 1))],
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.title],
                div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.inserted_at],
                div [ class "tableCell" ] [
                     if item.is_use then
                        button [class "button is-small", onClick (IsActive (String.fromInt(item.id)))] [text "활성화"]
                    else 
                        button [class "button is-small",  onClick (IsActive (String.fromInt(item.id)))] [text "비활성화"]
                 ]
         ]
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Session.changes GotSession (Session.navKey model.session)
    , Api.infoCheck Check
    , Api.onSucceesSession SessionCheck
    ]