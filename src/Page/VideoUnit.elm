port module Page.VideoUnit exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Browser
import Session exposing(Session)
import Json.Encode as E
import Json.Decode as Decode
import Html.Attributes exposing( class, placeholder, disabled, title, id, colspan, style)
import Route exposing (Route)
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Api.Decode as D
import Api.Endpoint as Endpoint
import Api as Api
import Http exposing(..)
import Json.Encode as Encode exposing (..)
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Page as Page

type alias Model = {
     session: Session
    , test : String
    , fileName : String
    , title : String
    , videoShow : Bool
    , levels : List Level
    , username : String
    , instrument : List Level
    , part : List Level
    , listmodel : ListModel
    , getList : List UnitList
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
    , videoId : String
    , menus : List Menus
    , goDetail : Bool
    , goRegist : Bool
    , pageNum : Int
    , errType : String
     }

type alias Menus =
    { menu_auth_code: List String
    , menu_id : Int
    , menu_name : String
    }
type alias VideoData = 
    { data : VideoDetailData }

type alias VideoDetailData = 
    { file : String
    , image : String}

type alias ListModel = 
    { page : Int
    , per_page: Int
    , titleList: String
    , difficulty_code: String
    , exercise_code: String
    , instrument_code: String
    , start_date: String
    , end_date: String
    , part_detail_code : String
    }
type alias Data = 
    { data : List Level}

type alias Level = 
    { code : String
    , name : String }

type alias DataList = 
    { data : List UnitList
    , paginate : Paginate}

type alias UnitList = 
    { difficulty_name: String
    , exercise_name: String
    , id: Int
    , inserted_at: String
    , instrument_name: String
    , part_detail_name: List String
    , title: String }

type alias Paginate = 
    { difficulty_code : String
    , end_date : String
    , exercise_code : String
    , instrument_code : String
    , part_detail_code : String
    , page : Int
    , per_page : Int 
    , start_date : String
    , title : String
    , total_count: Int
    }

listInit : ListModel
listInit = 
    { page  = 1
    , per_page = 10
    , titleList =  ""
    , difficulty_code =  ""
    , exercise_code =  ""
    , instrument_code =  ""
    , start_date =  ""
    , end_date =  ""
    , part_detail_code= ""}

init : Session -> (Model, Cmd Msg)
init  session = 
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
            , exercise_code = ""
            , instrument_code = ""
            , part_detail_code = ""
            , page = 0
            , per_page = 0 
            , start_date = ""
            , title = ""
            , total_count= 0
            }
        , listmodel = listInit
        , session = session
        , menus = []
        , test = "",
         fileName = "",
         title = "",
         videoShow = False
        , username = ""
        , goRegist = False
        , goDetail = False
        , getList= []
        , levels = []
        , instrument = []
        , part = []
        , videoId = ""
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
        , pageNum = 1
        , errType = ""
     }
    , 
    
    Cmd.batch[ Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
    ]
    )

listEncode : ListModel -> Session -> Cmd Msg
listEncode model session= 
    let
        list = 
            Encode.object
                [ ("page", Encode.int  model.page)
                , ("per_page", Encode.int model.per_page)
                , ("title", Encode.string model.titleList)
                , ("difficulty_code", Encode.string model.difficulty_code)
                , ("exercise_code", Encode.string model.exercise_code)
                , ("instrument_code", Encode.string model.instrument_code)
                , ("part_detail_code", Encode.string model.part_detail_code)
                , ("start_date", Encode.string model.start_date)
                , ("end_date", Encode.string model.end_date)
                ]
        body = 
            list |> Http.jsonBody
    in
    Api.post Endpoint.unitList (Session.cred session) GetList body (D.unitListDecoder DataList UnitList Paginate)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
        Api.receiveData ReceivedDataFromJS
        , Api.saveCheck ReceiveId
        , Session.changes GotSession (Session.navKey model.session)
        , Api.sendPageNum ReceivePnum
    ]

toSession : Model -> Session
toSession model =
    model.session

type Msg
    = SendDataToJS (Result Http.Error VideoData)
    | ReceivedDataFromJS E.Value
    | GetVideoFile (String, String)
    | VideoClose
    | GetLevel (Result Http.Error Data)
    | GetTool (Result Http.Error Data)
    | GetPart (Result Http.Error Data)
    | GetList (Result Http.Error DataList)
    | SelectTool String
    | SelectPart String
    | SelectLevel String
    | Title String
    | Search
    | Reset
    | GetId String
    | ReceiveId E.Value
    | GotSession Session
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String 
    | PageBtn (Int, String)
    | GetMyInfo (Result Http.Error D.DataWrap)
    | ReceivePnum Encode.Value
    | PreviewVideo

originModel : Model -> ListModel
originModel model = 
    model.listmodel


dataListSet : Int -> ListModel -> ListModel
dataListSet idx old =  
    { page  = idx
    , per_page = 10
    , titleList =  old.titleList
    , difficulty_code =  old.difficulty_code
    , exercise_code =  old.exercise_code
    , instrument_code =  old.instrument_code
    , start_date =  old.start_date
    , end_date =  old.end_date
    , part_detail_code= old.part_detail_code
    }

alldataListSet : Int -> ListModel -> ListModel
alldataListSet idx old=
    { page  = idx
    , per_page = 10
    , titleList =  old.titleList
    , difficulty_code =  old.difficulty_code
    , exercise_code =  old.exercise_code
    , instrument_code =  old.instrument_code
    , start_date =  ""
    , end_date =  ""
    , part_detail_code= old.part_detail_code
    }

allDataList : ListModel -> ListModel
allDataList old = 
    { page  = old.page
    , per_page = 10
    , titleList =  old.titleList
    , difficulty_code =  old.difficulty_code
    , exercise_code =  old.exercise_code
    , instrument_code =  old.instrument_code
    , start_date =  ""
    , end_date =  ""
    , part_detail_code= old.part_detail_code
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PreviewVideo ->
            (model, Api.get SendDataToJS (Endpoint.unitVideoShow model.videoId) (Session.cred model.session) (D.videoData VideoData VideoDetailData))
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
                            old = model.listmodel
                            new = {old | page = ok, end_date = "", start_date = ""}
                        in
                        
                        ({model | listmodel = new, pageNum = pageNum
                       } , listEncode new model.session)
                    else
                        let
                            old = model.listmodel
                            new = {old | page = ok}
                        in
                        
                        ({model | listmodel = new
                        ,  pageNum = pageNum
                        } , listEncode new model.session)
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
                menuf = List.head (List.filter (\x -> x.menu_id == 3) item.data.menus)
            in
            case menuf of
                        Just a ->
                            let
                                auth num = List.member num a.menu_auth_code
                            in
                            
                                if auth "20" then
                                    if auth "50" then
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True, goRegist = True}, Api.pageNum (Encode.int 0) )
                                    else
                                    ( {model |  menus = item.data.menus, username = item.data.admin.username, goDetail = True}, Api.pageNum (Encode.int 0) )
                                else if auth "50" then
                                ( {model |  menus = item.data.menus, username = item.data.admin.username, goRegist = True}, Api.pageNum (Encode.int 0) )
                                else
                                ( {model |  menus = item.data.menus, username = item.data.admin.username}, Api.pageNum (Encode.int 0) )
                        Nothing ->
                            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        PageBtn (idx, str) ->
            let
                old = model.listmodel
                new = {old | page = idx}
            in
            
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        ({model | listmodel = new, pageNum = model.pageNum - 1}, Cmd.batch[listEncode (alldataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | listmodel = new, pageNum = model.pageNum + 1}, Cmd.batch[listEncode (alldataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | listmodel = new}, Cmd.batch[listEncode (alldataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                    "prev" ->
                        ({model | listmodel = new, pageNum = model.pageNum - 1}, Cmd.batch[listEncode (dataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | listmodel = new, pageNum = model.pageNum + 1}, Cmd.batch[listEncode (dataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | listmodel = new}, Cmd.batch[listEncode (dataListSet idx (originModel (model))) model.session, Api.pageNum (Encode.int idx)])
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
                        old = model.listmodel
                    in
                        case datePickerMsg of            
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)                
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just currentSelectedDate) model.endday}
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, listmodel = new, endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | end_date = getFormattedDate (Just todaydate) model.endday}
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, listmodel = new,todaySave = getFormattedDate (Just todaydate) model.endday }
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
                            old = model.listmodel
                        in
                        case datePickerMsg of      
                            CancelClicked ->
                                ({newModel | show = False}, cmd)                      
                            SubmitClicked currentSelectedDate ->
                                let
                                     new = {old | start_date = getFormattedDate (Just currentSelectedDate) model.today}
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, listmodel = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new = {old | start_date = getFormattedDate (Just todaydate) model.today}
                                in
                                
                                ( { newModel | today = Just todaydate, listmodel = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        GotSession session ->
            case model.errType of
                "GetList" ->
                    update Search {model | session = session}
                "SendDataToJS" ->
                    update PreviewVideo {model | session = session}
                _ ->
                    (model, Cmd.none)
            
        ReceiveId data ->
            let
                dId = Decode.decodeValue Decode.string data
            in
                if model.goDetail then
                case dId of
                    Ok id ->
                        (model ,Route.pushUrl (Session.navKey model.session) Route.UvideoDetail)
                
                    Err _ ->
                        (model, Cmd.none)
                else
                (model, Cmd.none)
        GetId id -> 
            let
                data = Encode.string id 
            in
            
            (model, Api.saveData data)
        Search ->
            let
                old = model.listmodel
                date = {old | page = 1 }
                new = {old | page = 1, end_date = "", start_date = ""} 
            in
            
            if model.dateModel == "all" then
            ({model | listmodel = new, pageNum = 1}, 
            Cmd.batch
            [ listEncode new model.session
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (D.muserInfo)
            , Api.pageNum (Encode.int 1)])
            else
            ({model | listmodel = date, pageNum = 1}, 
            Cmd.batch
            [ listEncode date model.session
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (D.muserInfo)
            , Api.pageNum (Encode.int 1)])
        Reset ->
            let
                list = 
                    { page  = 1
                    , per_page = 10
                    , titleList =  ""
                    , difficulty_code =  ""
                    , exercise_code = ""
                    , instrument_code = ""
                    , start_date =  model.todaySave
                    , end_date =  model.todaySave
                    , part_detail_code= ""
                    }
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
                ( {model | dateModel = "all", listmodel = list, datePickerData = datePickerData, endDatePickerData = endDatePickerData }
                , Cmd.batch 
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        Title str ->
            let
                old = model.listmodel
                new =  {old | titleList = str}
            in
            ({model | listmodel = new},Cmd.none)
        SelectTool str ->
            let
                old = model.listmodel
                new =  {old | instrument_code = str}
            in
            ({model | listmodel = new},Cmd.none)
        SelectPart str ->
            let
                old = model.listmodel
                new =  {old | part_detail_code = str}
            in
             ({model | listmodel = new},Cmd.none)
        SelectLevel str ->
            let
                old = model.listmodel
                new =  {old | difficulty_code = str}
            in
             ({model | listmodel = new},Cmd.none)
        GetList (Ok ok) ->  
            ({model | getList = ok.data, paginate = ok.paginate},
            Cmd.batch
            [ Api.post Endpoint.part (Session.cred model.session) GetPart Http.emptyBody (D.unitLevelsDecoder Data Level)
            , Api.post Endpoint.instrument (Session.cred model.session) GetTool Http.emptyBody (D.unitLevelsDecoder Data Level)
            , Api.post Endpoint.unitLevel (Session.cred model.session) GetLevel Http.emptyBody (D.unitLevelsDecoder Data Level)
            ]
            )
        GetList (Err err) ->
            (model, Cmd.none)
        GetPart (Ok ok) -> 
            ({model | part = ok.data} ,Cmd.none)
        GetPart (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetPart"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetTool"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetLevel (Ok ok ) ->
            ({model | levels = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetLevel"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        SendDataToJS (Ok ok)->     
            let
                data = 
                    Encode.object
                        [ ("file", Encode.string ok.data.file)
                        , ("image", Encode.string ok.data.image)]
            in
            ( model, Api.sendData data )
        SendDataToJS (Err err)->     
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "SendDataToJS"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetVideoFile (title, id)->   
                ({model | title = title, videoShow = not model.videoShow, videoId = id }, 
                Cmd.batch [
                Api.get SendDataToJS (Endpoint.unitVideoShow id) (Session.cred model.session) (D.videoData VideoData VideoDetailData)
                , Api.heightControll (not model.videoShow)
                ]
                )
        VideoClose ->
                ({model | videoShow = not model.videoShow}, Api.heightControll (not model.videoShow))
        ReceivedDataFromJS data -> 
            let
                result =
                    Decode.decodeValue Decode.string data
            in
            case result of
                Ok string ->
                     ( {model | test = string}, Cmd.none )
                Err _ -> 
                    ({model | test = "Silly JavaScript, you can't kill me!"}, Cmd.none)
                    

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
     {title = "유어핏 단위 영상",
     content = 
         div [ ]
             [ 
                if model.videoShow then
                    div [class "adminAuthMask"] []
                else
                    div [] []
                ,
                 columnsHtml [pageTitle "유어핏 단위 영상"],
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
                         formInputEvent "운동 명" "운동명을 입력 해 주세요." False Title model.listmodel.titleList,
                         selectForm "기구 종류" False model.instrument SelectTool "" model.listmodel.instrument_code
                         ,
                         selectForm "운동 부위" False model.part SelectPart "" model.listmodel.part_detail_code
                     ],
                     columnsHtml [
                         selectForm "난이도" False model.levels SelectLevel "" model.listmodel.difficulty_code,
                         searchB Search Reset
                     ]
                 ],
                 div [] [
                    if model.goRegist then
                    registRoute "영상 등록" Route.UvideoRegist
                    else
                    div [] []
                    , dataCount (String.fromInt(model.paginate.total_count))
                 ]
                 , if model.getList == [] then
                    table [class "table"] [
                        headerTable,
                        tr [] [
                            td [ colspan 8 , class "noSearch"] [
                                text "검색결과가 없습니다."
                            ]
                        ]
                    ]
                  else
                    div [ class "table" ] ([headerTable] ++ (List.indexedMap (\ idx x ->
                        tableLayout idx x model
                    ) model.getList) )
                 , pagination 
                    PageBtn
                    model.paginate
                    model.pageNum 
                 , (videoShow model.title model.videoShow  VideoClose)
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
         div [ class "tableCell" ] [text "운동명"],
         div [ class "tableCell" ] [text "기구종류"],
         div [ class "tableCell" ] [text "운동부위"],
         div [ class "tableCell" ] [text "난이도"],
         div [ class "tableCell" ] [text "운동종류"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "미리보기"]
     ]

tableLayout : Int -> UnitList -> Model -> Html Msg
tableLayout idx item model= 
        let
            newinput text=
                text 
                    |> String.replace "%26" "&"
                    |> String.replace "%25" "%"
        in
        div [class "tableRow",style "cursor" (if model.goDetail then
            "pointer"
            else
            "no-drop"
        )] [
                 div [ class "tableCell", onClick (GetId (String.fromInt(item.id)))] [text  (
                    String.fromInt(model.paginate.total_count - ((model.paginate.page - 1) * 10) - (idx)  )
                )],
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text (newinput item.title)],
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.instrument_name],
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] (
                     List.map (\x ->
                        span [] [text( x ++ "  ") ]
                     ) item.part_detail_name
                 ),
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.difficulty_name],
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text item.exercise_name],
                 div [ class "tableCell" , onClick (GetId (String.fromInt(item.id)))] [text (String.dropRight 10 item.inserted_at)],
                 div [ class "tableCell" ] [
                     button [class "button is-small"
                     , onClick (GetVideoFile (item.title, (String.fromInt(item.id))))
                     ] [text "미리보기"]
                 ]
         ]




