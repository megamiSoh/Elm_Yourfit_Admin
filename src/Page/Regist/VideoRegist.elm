module Page.Regist.VideoRegist exposing (..)

import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Page.Page exposing(..)
import ExpandEvent as ExEvent
import Json.Decode
import Page.Origin.Video as Video
import String
import Session exposing (Session)
import Route exposing (..)
import Api as Api
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Api.Decode as D
import Page as Page

type alias Model =
    { disabled : Bool
    , username : String
    , session : Session
    , menus : List Menus
    , partData : List SelectItem
    , levelData : List SelectItem
    , filter : ExerFilterList
    , filterData : List FilterItem
    , exerCode : List SelectItem
    , instrument : List SelectItem
    , loading : Bool
    , openFilter : Bool
    , partDetail : List SelectItem
    , filterName : List String
    , gofilter : List String
    , editItem : List EditItems
    , valueWarn : String
    , resultFilterItem : List FilterItem
    , newStyle : String
    , settingShowIdx : String
    , videoShow : Bool
    , value : List Value
    , setSet : String
    , setRest : String
    , description : String
    , btnTitle : String
    , topTitle : String
    , getId : String
    , levelModel : String
    , partModel : String
    , titleModel : String
    , validErrShow : Bool
    , validationErr : String
    , screenInfo : ScreenInfo
    , page : Int
    , per_page : Int
    , total_count : Int
    , filtertitle : String
    , pointCode : List Pointcode
    , checkPoint : List String
    , is_sex : String
    , is_pay : String
    , errType : String
    , previewId : Int
    , age : List Age
    , checkAge : List String
    }

type alias AgeData = 
    { data : List Age }

type alias Age =
    { code : String
    , name : String }

type alias PointCodeWrap = 
    { data : List Pointcode}

type alias Pointcode =  
    { code : String
    , name : String}

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias PreviewWrap =  
    { data : DataPreview }

type alias DataPreview = 
    { file : String
    , image : String }

type alias Menus =
    { menu_auth_code: List String
    , menu_id : Int
    , menu_name : String
    }

type alias Value = 
    { id : Maybe Int
    , value : Int}

type alias EditVideo =
    { title : String
    , difficulty : String
    , exercise_part: String
    , items : List EditItem }

type alias EditItem =
    { action_id :  Int
    , is_rest :  Bool
    , value :  Int}

type alias EditItems =
    { action_id :  String
    , is_rest :  String
    , value :  String}

type alias ExerFilterList = 
    { difficulty_code : List String
    , exercise_code : List String
    , instrument_code : List String
    , part_detail_code : List String
    , title : String
    , page : Int
    , per_page : Int
    }

type alias ListData = 
    { data : List SelectItem}

type alias SelectItem = 
    { code : String
    , name : String }

type alias FilterDetail = 
    { data : List FilterItem 
    , paginate : Paginate}

type alias Paginate =
    { difficulty_code : List String
    , exercise_code : List String
    , instrument_code : List String
    , page : Int
    , part_detail_code : List String
    , per_page : Int
    , title : String
    , total_count : Int}

type alias FilterItem =
    { difficulty_name : Maybe String
    , exercise_name : Maybe String
    , id : Maybe Int
    , instrument_name : Maybe String
    , part_detail_name : List (Maybe String)
    , title : Maybe String
    , value : Maybe Int
    , is_rest : Maybe Bool
    , thembnail : String
    , duration : String
    }

type alias ExerItem =
    { action_id : Maybe Int
    , difficulty_name :Maybe  String
    , exercise_id : Maybe Int
    , exercise_name : Maybe String
    , instrument_name : Maybe String
    , is_rest :Bool
    , part_detail_name : List (Maybe String)
    , sort: Int 
    , title : Maybe String
    , value: Int}

type alias Success = 
    { result : String }

scrollEvent : (ScreenInfo -> msg) -> Attribute msg 
scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder : Decode.Decoder ScreenInfo
scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int) 

editItenEncoder : EditItems -> String
editItenEncoder item = 
    urlencoded 
        [ ("action_id", 
            if item.action_id == "0"then
                "null"
            else
                item.action_id
            )
        , ("is_rest", item.is_rest)
        , ("value", item.value)]


editResult : EditItems -> String
editResult item= 
    editItenEncoder item

formUrlencoded : List (String , String) -> String
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"

urlencoded : List (String , String) -> String
urlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                "\"" ++ name ++ "\""
                    ++ ":"
                    ++ value
            )
        |> String.join ","


listformUrlencoded : List EditItems -> String
listformUrlencoded object =
    object
        |> List.map
            (\x ->
                "{"
                ++ editItenEncoder x
                ++ "}"
            )
        |> String.join ","

directionEncoded : List String -> String
directionEncoded object =
    object
        |> List.map
            (\x ->
               "\"" ++ x ++ "\""
            )
        |> String.join ","

registVideo : Model -> List EditItems -> Session -> Cmd Msg
registVideo model edit session=
    let
        newInput text = 
            text 
                |> String.replace "&" "%26"
                |> String.replace "%" "%25"
        list =
            formUrlencoded 
            [ ("title", (newInput model.titleModel))
            , ("description", (newInput model.description))
            , ("difficulty", model.levelModel)
            , ("exercise_part", model.partModel)
            , ("items", "[" ++ listformUrlencoded edit ++ "]")
            , ("exercise_points", "[" ++ directionEncoded model.checkPoint ++ "]")
            , ("is_male" , model.is_sex)
            , ("is_pay", model.is_pay)
            , ("age_ranges", "[" ++ directionEncoded model.checkAge ++ "]")
            ]

            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.videoRegistRegist)(Session.cred session) GoEditApi list ((D.resultDecoder Success))


videoFilterResult : ExerFilterList -> Session -> Int -> Int -> String -> Cmd Msg
videoFilterResult e session page perpage filtertitle= 
    let
        list = 
            Encode.object
                [ ("difficulty_code", (Encode.list Encode.string) e.difficulty_code)
                , ("exercise_code", (Encode.list Encode.string) e.exercise_code)
                , ("instrument_code", (Encode.list Encode.string) e.instrument_code)
                , ("part_detail_code", (Encode.list Encode.string) e.part_detail_code )
                , ("title", Encode.string filtertitle)
                , ("page", Encode.int page)
                , ("per_page", Encode.int perpage)
                ]    
        body =
            list
                |> Http.jsonBody
    in
    Api.post Endpoint.videoFilterResult (Session.cred session) SucceesEdit body (D.videoFilterDecoder FilterDetail FilterItem Paginate) 

init : Session -> ( Model, Cmd Msg )
init session = 
        let
            f = 
                { difficulty_code = []
                , exercise_code = []
                , instrument_code = []
                , part_detail_code = []
                , page = 1
                , per_page = 20
                , title = ""
                }
        in
            ({ 
             filter = f
            , disabled = True
            , session = session
            , menus = []
            , filterData = []   
            , partData = []
            , levelData = []
            , exerCode = []
            , instrument = []
            , openFilter = False
            , partDetail = []
            , filterName = []
            , username = ""
            , gofilter = []
            , editItem = []
            , description = ""
            , loading = True
            , valueWarn = ""
            , videoShow = False
            , resultFilterItem = []
            , newStyle = ""
            , settingShowIdx = ""
            , value = []
            , setSet = "3"
            , setRest = "1"
            , btnTitle = "수정"
            , topTitle = "유어핏영상 상세"
            , getId = ""
            , levelModel = "H1"
            , titleModel = ""
            , partModel = "10"
            , validErrShow = False
            , validationErr = ""
            , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
            , page = 1
            , per_page = 20
            , total_count = 0
            , filtertitle = ""
            , pointCode = []
            , checkPoint = []
            , is_sex = ""
            , is_pay = "false"
            , errType = ""
            , previewId = 0
            , age = []
            , checkAge = []
            }
            , Cmd.batch
            [ Api.getParams ()
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            ]
            )


toSession : Model -> Session
toSession model =
    model.session

takeLists : Int -> List FilterItem -> List FilterItem
takeLists idx model = 
    List.take idx model

dropLists : Int -> List FilterItem -> List FilterItem
dropLists idx model = 
    List.drop idx model

type Msg 
    = GetLevel (Result Http.Error ListData)
    | GetPart (Result Http.Error ListData)
    | LevelSelect String
    | GetMyInfo (Result Http.Error D.DataWrap)
    | PartSelect String
    | TitleChange String
    | SucceesEdit (Result Http.Error FilterDetail)
    | GetTool (Result Http.Error ListData)
    | GetPartDetail (Result Http.Error ListData)
    | ExerCode (Result Http.Error ListData)
    | OpenFilter
    | GetFilterItem (String, String, String)
    | FilterResultData 
    | AddItem (Maybe Int)
    | BackItem Int
    | SwitchItem Int
    | SettingShow Int
    | RestSetting Int String
    | PlusMinusDeleteSet Int String Int
    | GoRegist 
    | GoEditApi(Result Http.Error Success)
    | GotSession Session
    | GetPreview (Int, String)
    | PreviewComplete (Result Http.Error PreviewWrap)
    | VideoClose
    | TextAreaInput String
    | ScrollEvent ScreenInfo
    | FilterTitle String
    | GetPointData (Result Http.Error PointCodeWrap)
    | PointCheck (String, String)
    | Pay String
    | Sex String
    | GetAgeData (Result Http.Error AgeData)
    | AgeCheck (String, String)
    | AllAge

    
update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        AllAge ->
            ({model | checkAge = []}, Cmd.none)
        GetAgeData (Ok ok) ->
            ({model | age = ok.data}, Cmd.none)
        GetAgeData (Err err) ->
            (model, Cmd.none)
        AgeCheck (code, name) ->
            let
                f = List.filter(\x -> x /= code) model.checkAge
            in
            if List.member code model.checkAge then
            ({model | checkAge = f }, Cmd.none)
            else
            ({model | checkAge = code :: model.checkAge}, Cmd.none)
        Sex value ->
            ({model | is_sex = value }, Cmd.none)
        Pay value ->
            ({model | is_pay =  value }, Cmd.none)
        PointCheck (code, name) ->
            let
                f = List.filter(\x -> x /= code) model.checkPoint
            in
            if List.member code model.checkPoint then
            ({model | checkPoint = f }, Cmd.none)
            else
            ({model | checkPoint = code :: model.checkPoint}, Cmd.none)
        GetPointData (Ok ok) ->
            ({model | pointCode = ok.data}, Cmd.none)
        GetPointData (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetPointData"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        FilterTitle title ->
            ({model | filtertitle = title}, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= (offsetHeight + 77) then
                if  model.total_count // model.per_page + 1 > model.page then
                ({model | page = model.page + 1}, videoFilterResult model.filter model.session (model.page + 1) model.per_page model.filtertitle)
                else
                (model, Cmd.none)
            else
                (model, Cmd.none)
        TextAreaInput str ->
            ({model | description = str}, Cmd.none)
        VideoClose ->
                ({model | videoShow = not model.videoShow}, Api.heightControll (not model.videoShow))
        PreviewComplete (Ok ok) ->
            let
                data = 
                    Encode.object
                        [ ("file", Encode.string ok.data.file)
                        , ("image", Encode.string ok.data.image)]
            in
            (model, Api.sendData data)
        PreviewComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "PreviewComplete"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetPreview (id, title)->
            ({model | videoShow = not model.videoShow , previewId = id}, Cmd.batch[Api.get PreviewComplete (Endpoint.unitVideoShow (String.fromInt(id))) (Session.cred model.session) (D.videoData PreviewWrap DataPreview),
             Api.heightControll (not model.videoShow)])
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetMyInfo"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, 
            Cmd.batch
            [ Api.post Endpoint.unitLevel (Session.cred model.session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerPartCode (Session.cred model.session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.unitLevel (Session.cred model.session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.instrument (Session.cred model.session) GetTool  Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.part (Session.cred model.session) GetPartDetail Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerCode (Session.cred model.session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , videoFilterResult model.filter model.session model.page model.per_page model.filtertitle
            , Api.post Endpoint.pointCode (Session.cred model.session) GetPointData Http.emptyBody (D.pointCode PointCodeWrap Pointcode)
            , Api.post Endpoint.yfAge (Session.cred model.session) GetAgeData Http.emptyBody (D.pointCode AgeData Age)] )
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "PreviewComplete" ->
                    Api.get PreviewComplete (Endpoint.unitVideoShow (String.fromInt(model.previewId))) (Session.cred model.session) (D.videoData PreviewWrap DataPreview)
                "GetMyInfo" ->
                    Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
                "GoEditApi" ->
                    registVideo model model.editItem session
                "SucceesEdit" ->
                    videoFilterResult model.filter session 1 model.per_page model.filtertitle
                _ ->
                    Cmd.none
            )
        GoEditApi (Ok ok) ->
            (model, Cmd.batch[Route.pushUrl (Session.navKey model.session) Route.Video, Api.showToast (Encode.string "등록되었습니다.")])
        GoEditApi (Err err) -> 
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GoEditApi"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GoRegist ->
            let
                result = List.map (\i ->
                        { action_id =  
                        case i.id of
                                Just int ->
                                    String.fromInt (int)
                            
                                Nothing ->
                                    "0"
                        , is_rest = 
                            case i.is_rest of
                                Just a ->
                                    if a == True then
                                        "true"
                                    else
                                        "false"
                                Nothing ->
                                    "true"
                        , value =   
                            case i.value of
                                Just b ->
                                    String.fromInt(b)
                            
                                Nothing ->
                                    String.fromInt (3)
                        }
                        ) model.resultFilterItem
            in
            if String.isEmpty model.titleModel then
            ({model | validationErr = "운동제목을 입력 해 주세요.", validErrShow = True}, Api.validationHeight (True))
            else if String.length model.titleModel > 100 then
            ({model | validationErr = "운동제목은 100자까지 입력 가능합니다.", validErrShow = True} ,Api.validationHeight (True))
            else if model.is_pay == "true" && List.isEmpty model.checkPoint then
                ({model | validationErr = "운동방향을 하나 이상 선택 해 주세요.", validErrShow = True}, Api.validationHeight (True))
            else if String.isEmpty model.description then
            ({model | validationErr = "운동설명을 입력 해 주세요.", validErrShow = True}, Api.validationHeight (True))
            else if String.length model.description > 350 then
            ({model | validationErr = "운동 설명은 350자까지 입력 가능합니다.", validErrShow = True} ,Api.validationHeight (True))
            else if List.isEmpty result then
            ({model | validationErr = "운동영상을 선택 해 주세요.", validErrShow = True}, Api.validationHeight (True))
            else
           ({model | validErrShow = False, editItem = result, loading = True}, Cmd.batch [registVideo model result model.session
           , Api.validationHeight (False)
           ])
        PlusMinusDeleteSet idx pattern num->
            let
                target = model.resultFilterItem
                take = List.take (idx + 1) target
                current = List.drop idx take
                replace= 
                    List.map (\x ->
                        {x | value = 
                            Just 
                            (case x.value of
                                Just n ->
                                    if num == -1 then
                                        if n < 1 then
                                            0
                                        else 
                                            n +num
                                    else
                                        if n < 6 then
                                                    n +num
                                                else
                                                    n
                                    
                                Nothing ->
                                    0
                            )
                        }
                        ) current
                result = List.take idx target ++ replace ++ List.drop (idx + 1) target
            in
            
            case pattern of
                "plus" ->
                    ({model | resultFilterItem = result} , Cmd.none)
                "minus" ->
                    ({model | resultFilterItem = result} , Cmd.none)
                _ ->
                    (model, Cmd.none)
        RestSetting id val ->
            let 
                parseVal = 
                    String.toInt (val)
                target = model.resultFilterItem
                take = List.take (id + 1) target
                current = List.drop id take
                replace = 
                    List.map (\x ->
                    case parseVal of
                        Just m ->
                            if m < 7 then
                            {x | value = Just m}    
                            else
                            x   
                    
                        Nothing ->
                            {x | value = Nothing}                        
                        ) current
                result = List.take id target ++ replace ++ List.drop (id + 1) target
            in
            ({model | resultFilterItem = result} , Cmd.none)
        SettingShow idx->
            let
                f = List.filter (\x -> 
                        x.value == Just 0 || x.value == Nothing
                    ) model.resultFilterItem
                    
            in 
            if List.length (f) > 0 then
                    ({model | valueWarn = "1 이상 숫자를 입력 해 주세요.", newStyle = "newStyle"}, Cmd.none)
            else 
                if model.newStyle == "" then
                ({model | newStyle = "newStyle" , settingShowIdx = String.fromInt(idx), valueWarn = ""}, Cmd.none)
                else
                ({model | newStyle = "", settingShowIdx = "",  valueWarn = ""}, Cmd.none)
                
        SwitchItem idx ->
            let
                before = 
                    takeLists (len - 2) model.resultFilterItem
                after =
                    dropLists (idx + 1) model.resultFilterItem
                len = 
                    List.length (takeLists ( idx  + 1 ) model.resultFilterItem)
                getVal = 
                    dropLists ( len - 2 ) (takeLists ( idx  + 1 ) model.resultFilterItem)
                result item = 
                    List.reverse item
            in
            ({model | resultFilterItem = before ++ List.reverse getVal ++ after} , Cmd.none)

        BackItem idx ->
            let
                before = List.take idx model.resultFilterItem
                after = List.drop (idx + 1) model.resultFilterItem
                result = before ++ after
            in
            ({model | resultFilterItem = result, newStyle = ""}, Cmd.none)
        AddItem id->
            let
                result = 
                    case id of
                        Just a->
                            a
                        Nothing ->
                            0
                f = List.filter (\x -> (Video.justInt x.id) == result)model.filterData
                new = List.map (\x ->
                        {x | value = Just 3}
                    ) f
            in
            if List.length model.resultFilterItem < 20 then
                if id == Nothing then
                ({model | resultFilterItem = 
                model.resultFilterItem ++
                [
                    { difficulty_name = Nothing
                    , exercise_name = Nothing
                    , id = Nothing
                    , instrument_name = Nothing
                    , part_detail_name = []
                    , title = Nothing
                    , value = Just 1
                    , is_rest = Just True
                    , thembnail = ""
                    , duration = ""
                    }
                ]},Cmd.none)
                else
                ({model | resultFilterItem =  model.resultFilterItem ++ new},Cmd.none)
            else
            (model, Cmd.none)
        FilterResultData ->
            ({model | openFilter = False,gofilter = model.filtertitle::model.filterName, filterData = [], page = 1 }, videoFilterResult model.filter model.session 1 model.per_page model.filtertitle)
        GetFilterItem (str, category, n) ->
            let
                old = model.filter
                compare item = List.filter (\x -> x /= str) item
                name = List.filter (\x -> x /= n) model.filterName
                ifState item = List.member str item
            in
                case category of
                    "difficulty_code" ->
                        let
                            new = {old | difficulty_code = old.difficulty_code ++ [str]}
                            delete = {old | difficulty_code = (compare old.difficulty_code)}
                        in
                        if ifState old.difficulty_code then
                        ({model| filter = delete, filterName = name}, Cmd.none)
                        else
                        ({model| filter = new, filterName = model.filterName ++ [n] }, Cmd.none)
                    "exercise_code" ->
                        let
                            new = {old | exercise_code = old.exercise_code ++ [str]}
                            delete = {old | exercise_code = (compare old.exercise_code)}
                        in
                        if ifState old.exercise_code then
                        ({model| filter = delete, filterName = name}, Cmd.none)
                        else
                        ({model| filter = new, filterName = model.filterName ++ [n] }, Cmd.none)
                    "instrument_code" ->
                        let
                            new = {old | instrument_code = old.instrument_code ++ [str]}
                            delete = {old | instrument_code = (compare old.instrument_code)}
                        in
                        if ifState old.instrument_code then
                        ({model| filter = delete, filterName = name}, Cmd.none)
                        else
                        ({model| filter = new, filterName = model.filterName ++ [n] }, Cmd.none)
                    "part_detail_code" ->
                        let
                            new = {old | part_detail_code = old.part_detail_code ++ [str]}
                            delete = {old | part_detail_code = (compare old.part_detail_code)}
                        in
                        if ifState old.part_detail_code then
                        ({model| filter = delete, filterName = name}, Cmd.none)
                        else
                        ({model| filter = new, filterName = model.filterName ++ [n] }, Cmd.none)
                    _ ->
                        (model, Cmd.none)
        GetPartDetail (Ok ok) -> 
            ({model|partDetail = ok.data}, Cmd.none)
        GetPartDetail (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetPartDetail"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        OpenFilter ->
            ({model | openFilter = not model.openFilter} , Cmd.none)
        ExerCode (Ok ok) -> 
            ({model | exerCode = ok.data} ,Cmd.none)
        ExerCode (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "ExerCode"}, Api.changeInterCeptor (Just error))
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
        SucceesEdit (Ok ok) ->
            ({model | filterData = model.filterData ++ ok.data, loading = False, total_count = ok.paginate.total_count}, Cmd.none)
        SucceesEdit (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "SucceesEdit"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        TitleChange str ->
            ({model | titleModel = str}, Cmd.none)
        PartSelect str ->
            ({model | partModel = str}, Cmd.none)
        LevelSelect str ->
            ({model | levelModel = str}, Cmd.none)
        GetPart (Ok ok ) ->
            ({model | partData = ok.data}, Cmd.none)
        GetPart (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetPart"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetLevel (Ok ok ) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetLevel"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
            
-- helperFunction

exerciseMap : Model -> Html Msg
exerciseMap model=
       div [class "scrollStyle", scrollEvent ScrollEvent] [
        p [ class "title" ]
        (List.indexedMap (\idx x -> 
            (Video.exerciseItem idx x AddItem model.disabled GetPreview)
        ) model.filterData)
        ]

emptyList : Model -> Html Msg
emptyList model=
        div [] (List.indexedMap (
            \idx item ->
                (Video.exerciseBackItem idx item BackItem SwitchItem model.newStyle SettingShow model.settingShowIdx 
                 RestSetting model.setSet model.setRest PlusMinusDeleteSet model.valueWarn model.disabled GetPreview
                )
        ) model.resultFilterItem
        )
    

view: Model -> {title: String, content: Html Msg, menu : Html Msg}
view model=
    if model.loading then
        { title = ""
            , content =
                div [] [
                div [class "adminloadingMask"][spinner]
                 ]
                , menu =  
                    aside [ class "menu"] [
                        Page.header model.username
                        ,ul [ class "menu-list yf-list"] 
                            (List.map Page.viewMenu model.menus)
                    ]
            }
    else
        if model.videoShow then
        { title = ""
        , content =
            div [] [
            div [class "adminAuthMask"] []
            , Video.registformView
            (exerciseMap model) 
            (emptyList model)
            model.levelData
            LevelSelect
            model.partData
            PartSelect
            TitleChange
            Route.Video
            model
            OpenFilter
            GetFilterItem
            FilterResultData
            AddItem
            GoRegist
            TextAreaInput
            FilterTitle
            PointCheck
            Pay
            Sex
            AgeCheck
            AllAge
            , videoShow "영상 미리보기" model.videoShow VideoClose
            , validationErr model.validationErr model.validErrShow
            ]
            , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
        }
        else
        { title = ""
        , content =
            div [] [
            Video.registformView
            (exerciseMap model) 
            (emptyList model)
            model.levelData
            LevelSelect
            model.partData
            PartSelect
            TitleChange
            Route.Video
            model
            OpenFilter
            GetFilterItem
            FilterResultData
            AddItem
            GoRegist
            TextAreaInput
            FilterTitle
            PointCheck
            Pay
            Sex
            AgeCheck
            AllAge
            , div [] [
                videoShow "영상 미리보기" model.videoShow VideoClose
            ]
            , div [] [
                validationErr model.validationErr model.validErrShow
            ]
            ]
            , menu =  
                aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)