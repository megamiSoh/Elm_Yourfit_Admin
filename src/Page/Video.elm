module Page.Video exposing (..)

import Browser

import Html exposing (..)
import Html.Attributes exposing( class, colspan )
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (Route)
import Html.Events exposing (..)
import Api as Api
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Api.Decode as D
import Debug exposing(..)
import Date exposing (..)
import DatePicker exposing (Msg(..))

type alias Model = {
    session: Session,
    isActive : Bool
    , sendBody : SendBody
    , videoData : List VideoData
    , partData : List Level
    , levelData : List Level
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
    , paginate : Paginate
    }

type alias Success = 
    { result : String}

type alias ListData = 
    { data : List Level}

type alias Level = 
    { code : String
    , name : String }

type alias SendBody = 
    { page: Int
    , per_page: Int
    , title: String
    , difficulty_code: String
    , exercise_part_code: String
    , start_date: String
    , end_date: String 
    }

type alias GetBody =
    { data : List VideoData
    , paginate : Paginate}

type alias Paginate =
    { difficulty_code: String
    , end_date: String
    , exercise_part_code: String
    , make_code: String
    , page: Int
    , per_page: Int
    , start_date: String
    , title: String
    , total_count: Int
    }

type alias VideoData = 
    { difficulty_name: String
    , exercise_part_name: String
    , id: Int
    , inserted_at:String
    , is_use: Bool
    , title: String
    }

listInit = 
    { page = 1
    , per_page = 10
    , title = ""
    , difficulty_code = ""
    , exercise_part_code = ""
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
    ({
        paginate = 
            { difficulty_code = ""
            , end_date = ""
            , exercise_part_code = ""
            , make_code = ""
            , page = 0
            , per_page = 0
            , start_date = ""
            , title = ""
            , total_count = 0
            },
        session = session,
        isActive = False
        , sendBody = listInit
        , videoData = []
        , partData = []
        , levelData = []
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
    }, 
    Cmd.batch
    [ videoEncoder listInit session
    , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Api.post Endpoint.part (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
        ])

toSession : Model -> Session
toSession model = 
    model.session

videoEncoder model session=
    let
        list = 
            Encode.object 
                [ ("page", Encode.int model.page)
                , ("per_page", Encode.int model.per_page)
                , ("title", Encode.string model.title) 
                , ("difficulty_code", Encode.string model.difficulty_code) 
                , ("exercise_part_code", Encode.string model.exercise_part_code) 
                , ("start_date", Encode.string model.start_date) 
                , ("end_date", Encode.string model.end_date)]
        body = 
            list
                |> Http.jsonBody
 
    in
    Api.post Endpoint.videoRegist (Session.cred session) Getbody body (D.videoDecoder GetBody VideoData Paginate)
    

type Msg 
    = IsActive (Bool, String)
    | Getbody (Result Http.Error GetBody)
    | GetTitle String
    | GetLevel (Result Http.Error ListData)
    | GetPart (Result Http.Error ListData)
    | LevelValue String
    | PartValue String
    | Search
    | Reset
    | DetailGo String
    | Complete Encode.Value
    | GoActive (Result Http.Error Success)
    | SessionCheck Encode.Value
    | GotSession Session
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | PageBtn (Int, String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageBtn (idx, str) ->
            let _ = Debug.log "str" idx
                old = model.sendBody
                list = 
                    { page = idx
                    , per_page = 10
                    , title = old.title
                    , difficulty_code = old.difficulty_code
                    , exercise_part_code = old.exercise_part_code
                    , start_date = old.start_date
                    , end_date = old.end_date
                    }
                allList = 
                    { page = idx
                    , per_page = 10
                    , title = old.title
                    , difficulty_code = old.difficulty_code
                    , exercise_part_code = old.exercise_part_code
                    , start_date = ""
                    , end_date = ""
                    }
                
            in
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | sendBody = allList}, videoEncoder allList model.session)
                    "next" ->
                        ({model | sendBody = allList}, videoEncoder allList model.session)
                    "go" -> 
                        ({model | sendBody = allList}, videoEncoder allList model.session)
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                "prev" ->
                    ({model | sendBody = list}, videoEncoder list model.session)
                "next" ->
                    ({model | sendBody = list}, videoEncoder list model.session)
                "go" -> 
                    ({model | sendBody = list}, videoEncoder list model.session)
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
                        old = model.sendBody
                    in
                        case datePickerMsg of                            
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just currentSelectedDate) model.endday}
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, sendBody = new, endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just todaydate) model.endday}
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, sendBody = new }
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
                            old = model.sendBody
                        in
                        case datePickerMsg of                            
                            SubmitClicked currentSelectedDate ->
                                let
                                     new = {old | start_date = getFormattedDate (Just currentSelectedDate) model.today}
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, sendBody = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | start_date = getFormattedDate (Just todaydate) model.today}
                                in
                                
                                ( { newModel | today = Just todaydate, sendBody = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, videoEncoder model.sendBody model.session)
                    Err _ ->
                        (model, Cmd.none)
        GoActive (Ok ok) ->
            let 
                old = model.sendBody 
                list = 
                    {start_date = "", end_date = "", page = old.page, per_page = old.per_page, title = old.title, difficulty_code = old.difficulty_code, exercise_part_code = old.exercise_part_code}
                -- resultactive = {model | sendBody = list}

            in
            if model.dateModel == "all" then
                (model, videoEncoder list model.session)
            else
            (model, videoEncoder model.sendBody model.session)
        GoActive (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        Complete str ->
            let
                result = Decode.decodeValue Decode.string str
            in
            case result of
                Ok s ->
                    (model, Route.pushUrl (Session.navKey model.session) Route.VideoDetail)
                Err _ ->
                    (model, Cmd.none)
                    
        DetailGo id ->
            let
                new = Encode.string id
            in
            (model, Api.saveData new)
        Search ->
            let
                old = model.sendBody
                list = 
                    { page = 1
                    , per_page = 10
                    , title = old.title
                    , difficulty_code = old.difficulty_code
                    , exercise_part_code = old.exercise_part_code
                    , start_date = ""
                    , end_date = "" 
                    }
            in
            
            if model.dateModel == "all" then
            (model, videoEncoder list model.session)
            else
            (model, videoEncoder model.sendBody model.session)
        Reset ->
            let
                list = 
                    { page = 1
                    , per_page = 10
                    , title = ""
                    , difficulty_code = ""
                    , exercise_part_code = ""
                    , start_date = model.todaySave
                    , end_date = model.todaySave 
                    }
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
                ( {model | dateModel = "all", sendBody = list, datePickerData = datePickerData, endDatePickerData = endDatePickerData}
                , Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        PartValue str ->
            let
                old = model.sendBody
                new = {old | exercise_part_code = str}
            in
            ({model | sendBody = new}, Cmd.none)
        LevelValue str ->  
            let
                old = model.sendBody
                new = {old | difficulty_code = str}
            in
        
            ({model | sendBody = new}, Cmd.none)
        GetPart (Ok ok ) ->
            ({model | partData = ok.data}, Cmd.none)
        GetPart (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        GetLevel (Ok ok ) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        GetTitle str ->
            let
                old = model.sendBody
                new = {old | title = str}
            in
            
            ({model| sendBody = new}, Cmd.none)
        IsActive (avtive, id) ->
            let
                encodieng = 
                    Encode.object   
                        [("is_use", Encode.bool (not avtive))]
                    |> Http.jsonBody
            in
            ( {model | isActive = not model.isActive}, Api.post( Endpoint.videoActive id) (Session.cred model.session) GoActive encodieng (D.resultDecoder Success) )
        Getbody (Ok ok) ->
            ({model | videoData = ok.data , paginate = ok.paginate} , Cmd.none)
        Getbody (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))

            


view : Model -> {title : String , content : Html Msg}
view model =
    { title = "유어핏 영상"
    , content = 
        div [ class "container is-fluid" ]
        [ 
            columnsHtml [pageTitle "유어핏 영상"],
            div [class "searchWrap"] [
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
                    formInputEvent "제목" "제목을 입력 해 주세요." False GetTitle model.sendBody.title,
                    selectForm "난이도" False model.levelData LevelValue "" model.sendBody.difficulty_code
                ],
                columnsHtml [
                    selectForm "운동 부위" False model.partData PartValue "" model.sendBody.exercise_part_code
                    -- searchBtn
                    , searchB Search Reset
                ]
            ]
            ,
            registRoute "영상 등록" Route.VideoRegist,
            dataCount (String.fromInt(model.paginate.total_count)),
            if model.videoData == []  then
             table [class "table"] [
                        headerTable,
                        tr [] [
                            td [ colspan 8 , class "noSearch"] [
                                text "검색결과가 없습니다."
                            ]
                        ]
                    ]
            else
            div [ class "table" ] 
                ([headerTable] ++ (List.indexedMap (\idx x -> tableLayout idx x model) model.videoData))
            ,  Pagenation.pagination PageBtn model.paginate
        ]  
    }

headerTable = 
     div [ class "tableRow headerStyle"] [
        div [ class "tableCell" ] [text "No"],
        div [ class "tableCell" ] [text "제목"],
        div [ class "tableCell" ] [text "운동부위"],
        div [ class "tableCell" ] [text "난이도"],
        div [ class "tableCell" ] [text "운동시간"],
        div [ class "tableCell" ] [text "등록일"],
        div [ class "tableCell" ] [text "활성"],
        div [ class "tableCell" ] [text "미리보기"]
    ]


tableLayout idx item model= 
        div [class "tableRow"] [
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text (
                    String.fromInt(model.paginate.total_count - ((model.paginate.page - 1) * 10) - (idx)  )
                )],
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text item.title],
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text item.exercise_part_name],
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text item.difficulty_name],
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text "ㅡ"],
                div [ class "tableCell cursor", onClick (DetailGo (String.fromInt(item.id))) ] [text item.inserted_at],
                div [ class "tableCell" ] [
                    if item.is_use then
                    div [class "button is-small", onClick (IsActive( item.is_use, String.fromInt(item.id)))] [text "활성화"]
                    else 
                    div [class "button is-small", onClick (IsActive( item.is_use,  String.fromInt(item.id)))] [text "비활성화"]
                ],
                div [ class "tableCell" ] [
                    div [class "button is-small"] [text "미리보기"]
                ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.saveCheck Complete
    , Session.changes GotSession (Session.navKey model.session)
    , Api.onSucceesSession SessionCheck]