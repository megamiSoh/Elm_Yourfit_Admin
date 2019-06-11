module Page.UserManage exposing (..)

import Html exposing (..)
import Browser
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Html.Attributes exposing (class, colspan)
import Html.Events exposing (..)
import Session exposing (Session)
import Route exposing (Route)
import Page.Page exposing(..)
import Pagenation exposing(..)
import Api as Api
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Page as Page



type alias Model =
    { firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , session: Session
    , show : Bool
    , today : Maybe Date
    , endday : Maybe Date
    , listForm : ListForm
    , resultForm : ResultForm
    , dataForm : List DataForm
    , problems : String
    , err : Maybe String
    , value : Encode.Value
    , endShow : Bool
    , dateModel : String
    , todaySave : String
    , menus : List Menus
    , username : String
    , goDetail : Bool
    , goRegist : Bool
    , pageNum : Int
    }



type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }


type Problem
    = ServerError String

type Msg
    = Show
    | EndShow
    | DatePickerMsg DatePicker.Msg
    | GotSession Session
    | GetList (Result Http.Error ResultForm)
    | Nickname String
    | Username String
    | Search
    | Reset
    | GetId String
    | Check Encode.Value
    -- | SessionCheck Encode.Value
    | EndDatePickerMsg DatePicker.Msg
    | DateValue String
    | PageBtn (Int, String)
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | ReceivePnum Encode.Value

listInit = 
    {
        page = 1,
        per_page = 10,
        username= "",
        nickname= "",
        start_date = "",
        end_date = ""
    }

init : Session-> ( Model, Cmd Msg )
init session=
    let
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    ({datePickerData = datePickerData
    , endDatePickerData = endDatePickerData
    , firstSelectedDate = Nothing
    , secondSelectedDate = Nothing
    , session = session
    , show = False
    , today = Nothing
    , username = ""
    , problems = ""
    , err = Nothing
    , listForm = listInit
    , value = Encode.object []
    , dataForm = []
    , endShow = False
    , endday = Nothing
    , dateModel = "all"
    , todaySave = ""
    , goDetail = False
    , goRegist = False
    , menus = []
    , pageNum = 1
    , resultForm = 
            {
                data = [],
                pagenate = {
                    end_date = "",
                    nickname = "",
                    page = 0,
                    per_page = 0,
                    start_date = "",
                    total_count = 0,
                    username  = "" 
                }
            }
      }
    , Cmd.batch
        [ Cmd.map DatePickerMsg datePickerCmd
        , Cmd.map EndDatePickerMsg enddatePickerCmd
        , managelist listInit session 
        , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
        ]
    )


type alias DataForm = 
    {
        connected_at : String,
        id : Int,
        joined_at : String,
        nickname : Maybe String,
        username : String
    }

type alias ResultForm = {
    data : List DataForm,
    pagenate : Pagenate
    }

type alias ListForm =
    {
        page : Int,
        per_page : Int,
        username: String,
        nickname: String,
        start_date : String,
        end_date : String
    }

type alias Pagenate = 
    {
        end_date: String,
        nickname: String,
        page: Int,
        per_page: Int,
        start_date: String,
        total_count: Int,
        username : String 
    }



managelist form session =
    let
        body =
            manageEncode form   
                |> Http.jsonBody
    in
   
         Api.post Endpoint.usermanageList (Session.cred session) GetList body (Decoder.userformDecoder ResultForm DataForm Pagenate)

toSession : Model -> Session
toSession model =
    model.session

manageEncode form =
    Encode.object
                [ ("page", Encode.int form.page)
                , ("per_page", Encode.int form.per_page)
                , ("username", Encode.string form.username)
                , ("nickname", Encode.string form.nickname)
                , ("start_date", Encode.string form.start_date)
                , ("end_date", Encode.string form.end_date)
                ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                            old = model.listForm
                            new = {old | page = ok, end_date = "", start_date = ""}
                        in
                        
                        ({model | listForm = new ,  pageNum = pageNum} , managelist new model.session)
                    else
                        let
                            old = model.listForm
                            new = {old | page = ok}
                        in
                        
                        ({model | listForm = new ,  pageNum = pageNum} , managelist new model.session)
                Err err ->
                    (model, Cmd.none)
        GetMyInfo (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            ( { model | problems = "Err" }
            , Cmd.none
            )

        GetMyInfo (Ok item) -> 
            let 
                menuf = List.head (List.filter (\x -> x.menu_id == 1) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                            
                                if auth "20" then
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True}, Api.pageNum (Encode.int 0) )
                                else if auth "50" then
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, goRegist = True}, Api.pageNum (Encode.int 0) )
                                else
                                ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
            
        PageBtn (idx, str) ->
            let
                old = model.listForm
                list = 
                    {
                        page = idx,
                        per_page = 10,
                        username= old.username,
                        nickname= old.nickname,
                        start_date = old.start_date,
                        end_date = old.end_date
                    }
                allList = 
                    {
                        page = idx,
                        per_page = 10,
                        username= old.username,
                        nickname= old.nickname,
                        start_date = "",
                        end_date = ""
                    }
            in
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | pageNum = model.pageNum - 1}, Cmd.batch[managelist allList model.session , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | pageNum = model.pageNum + 1}, Cmd.batch[managelist allList model.session
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        (model, Cmd.batch[managelist allList model.session
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else 
                case str of
                    "prev" ->
                        ({model | pageNum = model.pageNum - 1}, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | pageNum = model.pageNum + 1}, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        (model, Cmd.batch[managelist list model.session
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
        DateValue str->
            ({model | dateModel = str},Cmd.none)
        GetList (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error  
            in  
            ( { model | problems = serverErrors, err = Just (serverErrors) }
             , Session.changeInterCeptor (Just serverErrors) 
            )

        GetList (Ok item) ->
            ( {model | resultForm = item, dataForm = item.data}, Cmd.none )
        EndShow ->
            ( {model | endShow = not model.endShow, show = False}, Cmd.none )
        Show ->
            ( {model | show = not model.show, endShow = False }, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , Cmd.batch 
            [ managelist listInit session
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)]
            )
        EndDatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.endDatePickerData
                |> (\( data, cmd ) ->
                        ( { model | endDatePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                    let
                        old = model.listForm
                    in
                        case datePickerMsg of                    
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)       
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just currentSelectedDate) model.endday}
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, listForm = new, endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just todaydate) model.endday}
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, listForm = new , todaySave = getFormattedDate (Just todaydate) model.endday}
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
                            old = model.listForm
                        in
                        case datePickerMsg of              
                            CancelClicked ->
                                ({newModel | show = False}, cmd)              
                            SubmitClicked currentSelectedDate ->
                                let
                                     new = {old | start_date = getFormattedDate (Just currentSelectedDate) model.today}
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, listForm = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | start_date = getFormattedDate (Just todaydate) model.today}
                                in
                                
                                ( { newModel | today = Just todaydate, listForm = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        Check id ->
            let 
                decodeId = Decode.decodeValue Decode.string id
            in
                if model.goDetail then
                    case decodeId of
                        Ok go ->
                            (model , Route.pushUrl (Session.navKey model.session) Route.UserMDetail) 
                    
                        Err _  ->
                            (model,Cmd.none)
                else
                    (model, Cmd.none)
        -- SessionCheck check ->
        --     let
        --         decodeCheck = Decode.decodeValue Decode.string check
        --     in
        --         case decodeCheck of
        --             Ok continue ->
        --                 (model, managelist listInit model.session)
        --             Err _ ->
        --                 (model, Cmd.none)
            
        Nickname str ->
            let 
                listFirst =model.listForm
                new = 
                    {listFirst | nickname = str}
            in
            
            ({model | listForm = new}, Cmd.none)
        Username str ->      
            let
                listFirst = model.listForm
                new = {listFirst | username = str}
            in
              
            ({model | listForm = new} , Cmd.none)
        Search ->
            let 
                old = model.listForm
                list = 
                    { 
                    page = 1,
                    per_page = 10,
                    username= old.username,
                    nickname= old.nickname,
                    start_date = "",
                    end_date = ""
                    }
                date = 
                    {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, managelist list model.session)
            else
            ({model | listForm = date, pageNum = 1}, managelist date model.session)
        Reset ->
            let
                old = model.listForm
                list = 
                    { 
                    page = 1,
                    per_page = 10,
                    username= "",
                    nickname= "",
                    start_date = model.todaySave,
                    end_date = model.todaySave
                    }
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
                ( { model | dateModel = "all", listForm = list, datePickerData = datePickerData, endDatePickerData = endDatePickerData}
                , Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        GetId id ->
            let
                idEncoder = 
                    Encode.string id
            in
            (model ,Api.saveData idEncoder)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Session.changes GotSession (Session.navKey model.session),
    Api.saveCheck Check
    , Api.sendPageNum ReceivePnum
    -- , Api.onSucceesSession SessionCheck
    ]



viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    {
        title = "사용자 관리",
        content = 
            div []
                [               
                    columnsHtml [pageTitle "사용자 관리" ]
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
                            
                        ],
                        columnsHtml [
                            formInputEvent "닉네임" "닉네임을 입력 해 주세요" False Nickname model.listForm.nickname,
                            formInputEvent "아이디" "아이디를 입력 해 주세요" False Username model.listForm.username,
                            searchB Search Reset
                        ]
                        
                    ]
                , dataCount (String.fromInt(model.resultForm.pagenate.total_count)), 
                if List.length (model.resultForm.data) > 0 then
                listData model.resultForm.data model
                else
                table [class "table"] [
                        headerTable,
                        tr [] [
                            td [ colspan 8 , class "noSearch"] [
                                text "검색결과가 없습니다."
                            ]
                        ]
                    ]
                , pagination 
                    PageBtn
                    model.resultForm.pagenate
                    model.pageNum 
                
                -- Pagenation.pagination PageBtn model.resultForm.pagenate
            ]
            , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
        }



listData dataForm model=
    div [] [
        div [ class "table" ] ([headerTable] ++ (List.indexedMap(\ idx x -> tableLayout idx x model)  dataForm))
        ]



headerTable = 
     div [ class "tableRow headerStyle"] [
        div [ class "tableCell" ] [text "No"],
        div [ class "tableCell" ] [text "닉네임"],
        div [ class "tableCell" ] [text "아이디"],
        div [ class "tableCell" ] [text "등록일"]
    ]


tableLayout idx item model = 
        div[class "tableRow cursor", onClick (GetId (String.fromInt(item.id)))
        ]
         [
                div [ class "tableCell" ] [text (
                    String.fromInt(model.resultForm.pagenate.total_count - ((model.resultForm.pagenate.page - 1) * 10) - (idx)  )
                )],
                div [ class "tableCell" ] [
                    case item.nickname of
                        Just name ->
                            text name
                        Nothing ->
                            text ""
                        
                ],
                div [ class "tableCell" ] [text item.username],
                div [ class "tableCell" ] [text (String.dropRight 10 item.joined_at)]
        ]

