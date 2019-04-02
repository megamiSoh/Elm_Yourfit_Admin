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
    { 
     disabled : Bool
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
    , btnTitle : String
    , topTitle : String
    , getId : String
    , levelModel : String
    , partModel : String
    , titleModel : String
    , validErrShow : Bool
    , validationErr : String
    }

type alias PreviewWrap =  
    { data : DataPreview }
type alias DataPreview = 
    { file : String
    , image : String }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias Value = 
    { id : Maybe Int
    , value : Int}

type alias EditVideo =
    { title : String
    , difficulty: String
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
    }

type alias ListData = 
    { data : List SelectItem}

type alias SelectItem = 
    { code : String
    , name : String }

type alias FilterDetail 
    = { data : List FilterItem }


type alias FilterItem =
    { difficulty_name : String
    , exercise_name : String
    , id : Int
    , instrument_name : String
    , part_detail_name : List String
    , title : String
    , value : Maybe Int
    , is_rest : Maybe Bool
    , thembnail : String
    , duration : String
    }

type alias ExerItem =
    { action_id : Maybe Int
    , difficulty_name :Maybe  String
    , exercise_id : Int
    , exercise_name : Maybe String
    , instrument_name : Maybe String
    , is_rest :Bool
    , part_detail_name : List (Maybe String)
    , sort: Int 
    , title : Maybe String
    , value: Int}

type alias Success = 
    { result : String}

editItem edit=
    Encode.object   
        [ ("action_id", Encode.int edit.id)
        , ("is_rest", Encode.bool edit.is_rest)
        , ("value", Encode.int edit.value)]

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

editResult item= 
    editItenEncoder item

formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"

urlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                "\"" ++ name ++ "\""
                    ++ ":"
                    ++ value
            )
        |> String.join ","

listformUrlencoded object =
    object
        |> List.map
            (\x ->
                "{"
                ++ editItenEncoder x
                ++ "}"
            )
        |> String.join ","


registVideo model edit session=
    let
        
        list =
            formUrlencoded 
            [ ("title", model.titleModel)
            , ("difficulty", model.levelModel)
            , ("exercise_part", model.partModel)
            , ("items", "[" ++ listformUrlencoded edit ++ "]"
            )
            ]

            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.videoRegistRegist)(Session.cred session) GoEditApi list ((D.resultDecoder Success))


videoFilterResult e session= 
    let
        list = 
            Encode.object
                [ ("difficulty_code", (Encode.list Encode.string) e.difficulty_code)
                , ("exercise_code", (Encode.list Encode.string) e.exercise_code)
                , ("instrument_code", (Encode.list Encode.string) e.instrument_code)
                , ("part_detail_code", (Encode.list Encode.string) e.part_detail_code )
                ]    
        body =
            list
                |> Http.jsonBody
    in
    Api.post Endpoint.videoFilterResult (Session.cred session) SucceesEdit body (D.videoFilterDecoder FilterDetail FilterItem) 

init : Session -> ( Model, Cmd Msg )
init session = 
        let
            f = 
                { difficulty_code = []
                , exercise_code = []
                , instrument_code = []
                , part_detail_code = []
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
            }
            , Cmd.batch
            [ Api.getParams ()
            , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerPartCode (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.instrument (Session.cred session) GetTool
            Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.part (Session.cred session) GetPartDetail Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerCode (Session.cred session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , videoFilterResult f session
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            ]
            )


toSession : Model -> Session
toSession model =
    model.session

type Msg 
    = 
     GetLevel (Result Http.Error ListData)
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
    | RetryChange Session
    | GotSession Session
    | SecRetry Session
    | GetPreview (Int, String)
    | PreviewComplete (Result Http.Error PreviewWrap)
    | VideoClose

takeLists idx model = 
    List.take idx model

dropLists idx model = 
    List.drop idx model
    
update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        VideoClose ->
            let _ = Debug.log "videoClose" model.videoShow
                
            in
            
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
            (model, Cmd.none)
        GetPreview (id, title)->
            ({model | videoShow = not model.videoShow}, Cmd.batch[Api.get PreviewComplete (Endpoint.unitVideoShow (String.fromInt(id))) (Session.cred model.session) (D.videoData PreviewWrap DataPreview),
             Api.heightControll (not model.videoShow)])
        SecRetry session ->
            ({model | session = session }, videoFilterResult model.filter session)
        RetryChange session ->
            ({model | session = session} , registVideo model model.editItem session)
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            ,  Cmd.batch 
            [
                Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.exerPartCode (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.instrument (Session.cred session) GetTool
                Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.part (Session.cred session) GetPartDetail Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.exerCode (Session.cred session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
                , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            ]
            )
        -- SessionCheck check ->
        --     let
        --         decodeCheck = Decode.decodeValue Decode.string check
        --     in
        --         case decodeCheck of
        --             Ok continue ->
        --                 (model,)
        --             Err _ ->
        --                 (model, Cmd.none)
        GoEditApi (Ok ok) ->
            (model, Route.pushUrl (Session.navKey model.session) Route.Video)
        GoEditApi (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401" then
                (model, Api.thirdRefreshFetch ())
            else
                (model, Cmd.none)
        GoRegist ->
            let
                result = List.map (\i ->
                        { action_id =  String.fromInt(i.id)
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
            else if List.isEmpty result then
            ({model | validationErr = "운동영상을 선택 해 주세요.", validErrShow = True}, Api.validationHeight (True))
            else
           ({model | validErrShow = False, editItem = result}, Cmd.batch [registVideo model result model.session
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
                                        n +num
                                    
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
                            {x | value = Just m}    
                    
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
            ({model | resultFilterItem = result}, Cmd.none)
        AddItem id->
            let
                result = 
                    case id of
                        Just a->
                            a
                        Nothing ->
                            0
                f = List.filter (\x -> x.id == result)model.filterData
                new = List.map (\x ->
                        {x | value = Just 3}
                    ) f
            in
            
            if id == Nothing then
            ({model | resultFilterItem = 
            model.resultFilterItem ++
            [
                { difficulty_name = ""
                , exercise_name = ""
                , id = 0
                , instrument_name = ""
                , part_detail_name = []
                , title = ""
                , value = Just 1
                , is_rest = Just True
                , thembnail = ""
                , duration = ""
                }
            ]},Cmd.none)
            else
            ({model | resultFilterItem =  model.resultFilterItem ++ new},Cmd.none)
        FilterResultData ->
            ({model | openFilter = False,gofilter = model.filterName}, videoFilterResult model.filter model.session)
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
            (model,Cmd.none)
        OpenFilter ->
            ({model | openFilter = not model.openFilter} , Cmd.none)
        ExerCode (Ok ok) -> 
            ({model | exerCode = ok.data} ,Cmd.none)
        ExerCode (Err err) ->
            (model,Cmd.none)
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
            (model,Cmd.none)
        SucceesEdit (Ok ok) ->
            ({model | filterData = ok.data, loading = False}, Cmd.none)
        SucceesEdit (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401" then
                (model, Api.fourRefreshFetch ())
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
            
            (model,Session.changeInterCeptor (Just error))
        GetLevel (Ok ok ) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
            
-- helperFunction


exerciseMap model=
       div [class "scrollStyle"] [
        p [ class "title" ]
        (List.indexedMap (\idx x -> 
            (Video.exerciseItem idx x AddItem model.disabled GetPreview)
        ) model.filterData)
        ]

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
        { title = ""
        , content =
            div [] [
            if model.loading then
            div [class "adminloadingMask"][spinner]
            else 
            div [][] ,
                if model.videoShow then
                    div [class "adminAuthMask"] []
                else
                    div [] []
                ,
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




subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ 
        -- Api.onSucceesSession SessionCheck
    Session.changes GotSession (Session.navKey model.session)
    , Session.retryChange RetryChange (Session.navKey model.session)
    , Session.secRetryChange SecRetry (Session.navKey model.session)
    ]