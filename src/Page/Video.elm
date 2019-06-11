module Page.Video exposing (..)

import Browser

import Html exposing (..)
import Html.Attributes exposing( class, colspan, style )
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
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Page as Page

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
    , videoShow : Bool
    , username: String
    , todaySave : String
    , paginate : Paginate
    , activeId : String
    , showId : String
    , yfvideo : YfVideoData
    , sort : String
    , menus : List Menus
    , goRegist : Bool
    , goDetail : Bool
    , pageNum : Int
    , auth : List String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias YfVideo = 
    { data : YfVideoData }

type alias YfVideoData = 
    { difficulty_name : String
    , duration : String
    , exercise_items : List YFVideoItems
    , exercise_part_name : String
    , id : Int
    , pairing : List Fairing
    , title : String
    }
type alias YFVideoItems = 
    { descriptions : Maybe String
    , duration : String
    , exercise_id: Int
    , is_rest : Bool
    , sort : Int
    , title : String
    , value : Int
    }
type alias Fairing = 
    { file : String
    , image : String
    , title : String}
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
    , duration: String
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
            }
        , yfvideo = 
            { difficulty_name = ""
            , duration = ""
            , exercise_items = []
            , exercise_part_name = ""
            , id = 0
            , pairing = []
            , title = ""
            }
        , session = session
        , isActive = False
        , menus = []
        , sendBody = listInit
        , videoData = []
        , goRegist = False
        , goDetail = False
        , partData = []
        , levelData = []
        , sort = "영상 x 세트 탭을 클릭하시면 설명글이 나타납니다."
        , showId = ""
        , datePickerData = datePickerData
        , endDatePickerData = endDatePickerData
        , firstSelectedDate = Nothing
        , secondSelectedDate = Nothing
        , show = False
        , username = ""
        , today = Nothing
        , endShow = False
        , endday = Nothing
        , dateModel = "all"
        , todaySave = ""
        , activeId = ""
        , videoShow = False
        , pageNum = 1
        , auth = []
    }, 
    Cmd.batch
    [ videoEncoder listInit session Getbody
    , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Api.post Endpoint.part (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
        ])

toSession : Model -> Session
toSession model = 
    model.session

videoEncoder model session getBody=
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
    Api.post Endpoint.videoRegist (Session.cred session) getBody body (D.videoDecoder GetBody  VideoData Paginate)

-- videoPostCode = 


-- yourfitVideoShow id

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
    | RetryRequest Session
    | GotSession Session
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | PageBtn (Int, String)
    | VideoIsShow String
    | VideoShowResult (Result Http.Error YfVideo)
    | VideoShowClose
    | Sort Int
    | VideoRetry Session
    | GetMyInfo (Result Http.Error D.DataWrap)
    | ReceivePnum Encode.Value
    | GetbodySecond (Result Http.Error GetBody)

oldModel model =
    model.sendBody
listDataSet idx old= 
    { page = idx
    , per_page = 10
    , title = old.title
    , difficulty_code = old.difficulty_code
    , exercise_part_code = old.exercise_part_code
    , start_date = old.start_date
    , end_date = old.end_date
    }
allListDataSet idx old = 
    { page = idx
    , per_page = 10
    , title = old.title
    , difficulty_code = old.difficulty_code
    , exercise_part_code = old.exercise_part_code
    , start_date = ""
    , end_date = ""
    }

allListDataSetComplete old = 
    { page = old.page
    , per_page = 10
    , title = old.title
    , difficulty_code = old.difficulty_code
    , exercise_part_code = old.exercise_part_code
    , start_date = ""
    , end_date = ""
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePnum num ->
            let 
                val = Decode.decodeValue Decode.int num
            in
            
            case val of
                Ok ok ->
                    if model.dateModel == "all" then
                        let
                            old = model.sendBody
                            new = {old | page = ok, end_date = "", start_date = ""}
                        in
                        
                        ({model | sendBody = new } , videoEncoder new model.session GetbodySecond)
                    else
                        let
                            old = model.sendBody
                            new = {old | page = ok}
                        in
                        
                        ({model | sendBody = new } , videoEncoder new model.session GetbodySecond)
                Err err ->
                    (model, Cmd.none)
        GetMyInfo (Err err) ->
           (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            let 
                menuf = List.head (List.filter (\x -> x.menu_id == 4) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                                if auth "20" then
                                    if auth "50" then
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True, goRegist = True, auth = a.menu_auth_code}, Cmd.none )
                                    else
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True, auth = a.menu_auth_code}, Cmd.none )
                                else if auth "50" then
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, goRegist = True, auth = a.menu_auth_code}, Cmd.none )
                                else
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code}, Cmd.none )
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        VideoRetry retry ->
            ({model | session = retry},
                Api.get VideoShowResult (Endpoint.yourfitVideoShow model.showId) (Session.cred retry) (D.yfVideo YfVideo YfVideoData YFVideoItems Fairing )
            )
        Sort id ->
            let
                s = List.head (
                    List.filter(\x ->
                        x.sort == id
                    ) model.yfvideo.exercise_items
                    )
            in
            case s of
                Just v ->
                    ({model | sort = 
                        case v.descriptions of
                                Just d ->
                                    d
                                Nothing ->
                                    "휴식"
                            }, Cmd.none)
                Nothing ->
                    ({model | sort = "영상 x 세트 탭을 클릭하시면 설명글이 나타납니다."}, Cmd.none)
            
        VideoShowResult (Ok ok) ->
            let
                videoList = 
                    Encode.object 
                        [("pairing", (Encode.list videoEncode) ok.data.pairing) ]

                videoEncode p=
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)
                        ]
            in
            ({model | yfvideo = ok.data}, Api.sendData videoList)
        VideoShowClose ->
            ({model | videoShow = not model.videoShow}, Api.heightControll (not model.videoShow))
        VideoShowResult (Err err) ->
            let 
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.fourRefreshFetch ())
            else 
            (model, Cmd.none)
        VideoIsShow id ->
            ({model | videoShow = not model.videoShow , showId = id}
            , Cmd.batch [
            Api.get VideoShowResult (Endpoint.yourfitVideoShow id) (Session.cred model.session) (D.yfVideo YfVideo YfVideoData YFVideoItems Fairing )
            , Api.heightControll (not model.videoShow)
            ]
            )
        PageBtn (idx, str) ->
            let
                old = model.sendBody
                new = {old | page = idx}
            in
            
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | sendBody = new, pageNum = model.pageNum - 1}, Cmd.batch[videoEncoder (allListDataSet idx (oldModel model)) model.session Getbody,
                        Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | sendBody = new, pageNum = model.pageNum + 1}, Cmd.batch[videoEncoder (allListDataSet idx (oldModel model)) model.session Getbody,
                        Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | sendBody = new}, Cmd.batch[videoEncoder (allListDataSet idx (oldModel model)) model.session Getbody,
                        Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                "prev" ->
                    ({model | sendBody = new, pageNum = model.pageNum - 1}, Cmd.batch[videoEncoder (listDataSet idx (oldModel model)) model.session Getbody
                    , Api.pageNum (Encode.int idx)])
                "next" ->
                    ({model | sendBody = new, pageNum = model.pageNum + 1}, Cmd.batch[videoEncoder (listDataSet idx (oldModel model)) model.session Getbody
                    , Api.pageNum (Encode.int idx)])
                "go" -> 
                    ({model | sendBody = new}, Cmd.batch[videoEncoder (listDataSet idx (oldModel model)) model.session Getbody
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
                        old = model.sendBody
                    in
                        case datePickerMsg of       
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)                     
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
                            CancelClicked ->
                                ({newModel | show = False}, cmd)                           
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
            , Cmd.batch [
            if model.dateModel == "all" then
                videoEncoder (allListDataSetComplete(oldModel model) ) session Getbody
            else 
            videoEncoder model.sendBody session Getbody
            , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
            , Api.post Endpoint.part (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            ]
            )
        RetryRequest retry ->
            ({model | session = retry}, 
            Cmd.batch[
            activeEncode retry model.isActive model.activeId
            ]
            )

        GoActive (Ok ok) ->
            if model.dateModel == "all" then
                (model, videoEncoder (allListDataSetComplete (oldModel model)) model.session Getbody)
            else
            (model, videoEncoder model.sendBody model.session Getbody)
        GoActive (Err err) ->
            (model, Api.thirdRefreshFetch ())
        Complete str ->
            let
                result = Decode.decodeValue Decode.string str
            in
            if model.goDetail then
            case result of
                Ok s ->
                    (model, Route.pushUrl (Session.navKey model.session) Route.VideoDetail)
                Err _ ->
                    (model, Cmd.none)
            else
            (model, Cmd.none)
                    
        DetailGo id ->
            let
                new = Encode.string id
            in
            (model, Api.saveData new)
        Search ->
            if model.dateModel == "all" then
            (model, videoEncoder (allListDataSetComplete (oldModel model)) model.session Getbody)
            else
            (model, videoEncoder model.sendBody model.session Getbody)
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
            (model,Cmd.none)
        GetLevel (Ok ok ) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            (model,Cmd.none)
        GetTitle str ->
            let
                old = model.sendBody
                new = {old | title = str}
            in
            ({model| sendBody = new}, Cmd.none)

        IsActive (active, id) ->
            ( {model | isActive = not active, activeId = id}, 
            Cmd.batch [
                activeEncode model.session (not active) id
            ])
        Getbody (Ok ok) ->
            ({model | videoData = ok.data ,  paginate = ok.paginate} ,  Api.pageNum (Encode.int 0))
        Getbody (Err err) ->
            let
                error = Api.decodeErrors err
            in
            (model,Session.changeInterCeptor (Just error))
        GetbodySecond (Ok ok)->
            ({model | videoData = ok.data ,  paginate = ok.paginate} ,  Cmd.none)
        GetbodySecond (Err err)->
            let
                error = Api.decodeErrors err
            in
            (model,Session.changeInterCeptor (Just error))

            


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.videoShow then
    { title = "유어핏 영상"
    , content = 
        div [ class "" ]
        [ 
            div [class "adminAuthMask"] []
            , 
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
            div [] [
                if model.goRegist then
                registRoute "영상 등록" Route.VideoRegist
                else
                div [] []
            ]
            , dataCount (String.fromInt(model.paginate.total_count)),
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
            , pagination 
                PageBtn
                model.paginate
                model.pageNum 
            -- Pagenation.pagination PageBtn model.paginate
            , (yfVideoShow model.videoShow VideoShowClose model.yfvideo model.sort Sort)
        ]  
        , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }
    else
    { title = "유어핏 영상"
    , content = 
        div [ class "" ]
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
            div [] [
                if model.goRegist then
                registRoute "영상 등록" Route.VideoRegist
                else
                div [] []
            ]
            , dataCount (String.fromInt(model.paginate.total_count)),
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
            ,  pagination 
                PageBtn
                model.paginate
                model.pageNum 
            , (yfVideoShow model.videoShow VideoShowClose model.yfvideo model.sort Sort)
        ]  
        , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }


activeEncode session active id= 
    let
        list = 
            Encode.object   
                [("is_use", Encode.bool active)]
                    |> Http.jsonBody
    in
    Api.post( Endpoint.videoActive id) (Session.cred session) GoActive list (D.resultDecoder Success)


headerTable = 
     div [ class "tableRow headerStyle"] [
        div [ class "tableCell" ] [text "No"],
        div [ class "tableCell" ] [text "제목"],
        div [ class "tableCell" ] [text "운동부위"],
        div [ class "tableCell" ] [text "난이도"],
        div [ class "tableCell" ] [text "운동시간"],
        div [ class "tableCell" ] [text "등록일"],
        div [ class "tableCell" ] [text "게시"],
        div [ class "tableCell" ] [text "미리보기"]
    ]


tableLayout idx item model= 
    let
        newinput text=
            text 
                |> String.replace "%26" "&" 
                |> String.replace "%25" "%" 
    in
    
        div [class "tableRow", style "cursor" (
                if model.goDetail then
                "pointer"
                else 
                "no-drop"
        )] [
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))) ] [text (
                    String.fromInt(model.paginate.total_count - ((model.paginate.page - 1) * 10) - (idx)  )
                )],
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))), style "width" "600px" ] [text (newinput item.title)],
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))) ] [text item.exercise_part_name],
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))) ] [text item.difficulty_name],
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))) ] [text item.duration],
                div [ class "tableCell", onClick (DetailGo (String.fromInt(item.id))) ] [text (String.dropRight 10 item.inserted_at)],
                div [ class "tableCell" ] [
                    if List.member "30" model.auth then
                        if item.is_use then
                        div [class "button is-small is-success", onClick (IsActive( item.is_use, String.fromInt(item.id)))] [text "게시 중"]
                        else 
                        div [class "button is-small", onClick (IsActive( item.is_use,  String.fromInt(item.id)))] [text "게시 하기"]
                    else
                        if item.is_use then
                        div [class "button is-small is-success"] [text "게시 중"]
                        else 
                        div [class "button is-small"] [text "게시 하기"]
                ],
                if item.is_use then
                div [ class "tableCell" ] [
                    div [class "button is-small", onClick (VideoIsShow (String.fromInt(item.id)))] [text "미리보기"]
                ]
                else
                div [ class "tableCell" ] []
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.saveCheck Complete
    , Session.changes GotSession (Session.navKey model.session)
    , Session.retryChange RetryRequest (Session.navKey model.session)
    , Session.secRetryChange VideoRetry (Session.navKey model.session)
    , Api.sendPageNum ReceivePnum
    ]