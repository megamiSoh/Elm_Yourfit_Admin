module Page.Detail.VideoDetail exposing (..)

import Browser
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
    , session : Session
    , detaildata : DetailData
    , partData : List SelectItem
    , levelData : List SelectItem
    , filter : ExerFilterList
    , filterData : List FilterItem
    , disabledMask : Bool
    , loading : Bool
    , exerCode : List SelectItem
    , justFilter : Bool
    , instrument : List SelectItem
    , openFilter : Bool
    , partDetail : List SelectItem
    , filterName : List String
    , gofilter : List String
    , goEdit : Bool
    , editItem : List EditItems
    , resultFilterItem : List FilterItem
    , newStyle : String
    , settingShowIdx : String
    , value : List Value
    , setSet : String
    , setRest : String
    , btnTitle : String
    , videoShow : Bool
    , validationErr : String
    , validErrShow: Bool
    , topTitle : String
    , getId : String
    , menus : List Menus
    , valueWarn: String
    , preview : DataPreview
    , description: String
    , filtertitle : String
    , page : Int
    , per_page : Int
    , total_count : Int
    , screenInfo : ScreenInfo
    , filterItem : FilterItem
    , username : String
    }

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
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias EditItems =
    { action_id :  String
    , is_rest :  String
    , value :  String}

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

type alias Detail
    = { data : DetailData }

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
type alias DetailData =
    { difficulty_code : String
    , difficulty_name : String
    , exercise_items : List ExerItem
    , exercise_part_code: String
    , exercise_part_name :String
    , id: Int
    , title: String
    , description : Maybe String}

type alias ExerItem =
    { 
    --     id : Int
    -- ,
     difficulty_name :Maybe String
    , id : Maybe Int
    , exercise_name : Maybe String
    , instrument_name : Maybe String
    , is_rest :Maybe Bool
    , part_detail_name : List (Maybe String)
    -- , sort: Int 
    , title : Maybe String
    , value: Maybe Int
    , thembnail : String
    , duration : String}

type alias Success = 
    { result : String}
-- (D.resultDecoder Success)



scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int) 

editItem edit=
    Encode.object   
        [ ("action_id", Encode.int edit.id)
        , ("is_rest", Encode.bool edit.is_rest)
        , ("value", Encode.int edit.value)]

-- editIitems edit= 
    -- [(Encode.list editItenEncoder) edit]


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
-- formUrlencoded : List ( String, String ) -> String
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

editVideo detaildata edit session id description=
    let
        newText text = 
            text
                |> String.replace "&" "%26"
                |> String.replace "%" "%25"
        list =
            formUrlencoded 
            [ ("title", (newText detaildata.title))
            , ("difficulty", detaildata.difficulty_code)
            , ("exercise_part", detaildata.exercise_part_code)
            , ("description", (newText description))
            , ("items", "[" ++ listformUrlencoded edit ++ "]"
            )
            ]

            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.videoEdit id)(Session.cred session) GoEditApi list ((D.resultDecoder Success))

videoFilterResult e session page perpage title= 
    let
        list = 
            Encode.object
                [ ("difficulty_code", (Encode.list Encode.string) e.difficulty_code)
                , ("exercise_code", (Encode.list Encode.string) e.exercise_code)
                , ("instrument_code", (Encode.list Encode.string) e.instrument_code)
                , ("part_detail_code", (Encode.list Encode.string) e.part_detail_code )
                , ("title", Encode.string title)
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
                , title = ""
                , page = 1
                , per_page = 20
                }
            d = 
                { difficulty_code  = ""
                , difficulty_name  = ""
                , exercise_items  = []
                , exercise_part_code = ""
                , exercise_part_name  =""
                , id = 0
                , title = ""
                , description = Nothing}
        in
            ({ 
             filter = f
            , disabled = True
            , session = session
            , detaildata = d
            , filterData = []   
            , partData = []
            , valueWarn =""
            , menus = []
            , goEdit = False
            , levelData = []
            , videoShow = False
            , preview = 
                { file = ""
                , image = ""}
            , justFilter = False
            , disabledMask = False
            , loading = True
            , exerCode = []
            , instrument = []
            , openFilter = False
            , partDetail = []
            , filterName = []
            , username = ""
            , description = ""
            , gofilter = []
            , editItem = []
            , resultFilterItem = []
            , newStyle = ""
            , validationErr = ""
            , validErrShow = False
            , settingShowIdx = ""
            , value = []
            , setSet = "0"
            , setRest = "0"
            , btnTitle = "수정"
            , topTitle = "유어핏영상 상세"
            , getId = ""
            , filtertitle = ""
            , page = 1
            , per_page = 20
            , total_count = 0
            , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
            , filterItem = 
                { difficulty_name = Nothing
                , exercise_name = Nothing
                , id = Nothing
                , instrument_name = Nothing
                , part_detail_name = []
                , title = Nothing
                , value = Nothing
                , is_rest = Nothing
                , thembnail = ""
                , duration = ""
                }
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
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            -- , Api.get GetData ( Endpoint.videoDetail model.getId) (Session.cred session)  (D.videoDetailDecoder Detail DetailData ExerItem)
            ]
            )


toSession : Model -> Session
toSession model =
    model.session

type Msg 
    = GetData (Result Http.Error Detail)
    | GetId Encode.Value
    | GetLevel (Result Http.Error ListData)
    | GetPart (Result Http.Error ListData)
    | LevelSelect String
    | PartSelect String
    | TitleChange String
    | SucceesEdit (Result Http.Error FilterDetail)
    | DetailOrEdit
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
    | GetMyInfo (Result Http.Error D.DataWrap)
    | RestSetting Int String
    | PlusMinusDeleteSet Int String Int
    | GoEdit 
    | GoEditApi(Result Http.Error Success)
    | RetryRequest Session
    -- | SessionCheck Encode.Value
    | GotSession Session
    | VideoRetry Session
    | GetPreview (Int, String)
    | PreviewComplete (Result Http.Error PreviewWrap)
    | VideoClose
    | TextAreaInput String
    | FilterTitle String
    | ScrollEvent ScreenInfo


takeLists idx model = 
    List.take idx model

dropLists idx model = 
    List.drop idx model
    
update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= (offsetHeight + 77) then
                if  model.total_count // model.per_page + 1 > model.page then
                ({model | page = model.page + 1}, videoFilterResult model.filter model.session (model.page + 1) model.per_page model.filtertitle)
                else
                (model, Cmd.none)
            else
                (model, Cmd.none)
        FilterTitle title ->
            ({model|filtertitle = title}, Cmd.none)
        TextAreaInput str ->
            -- let 
            --     old = model.detaildata
            --     descriptionInput = {old | description = (Just str)}
            -- in
            
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
            (model, Cmd.none)
        GetPreview (id, title)->
            ({model | videoShow = not model.videoShow}, Cmd.batch[Api.get PreviewComplete (Endpoint.unitVideoShow (String.fromInt(id))) (Session.cred model.session) (D.videoData PreviewWrap DataPreview)
            , Api.heightControll (not model.videoShow)]
            )
        VideoRetry session ->
            ({model | session = session}, editVideo model.detaildata model.editItem session model.getId model.description)
        RetryRequest session ->
            ({model | session = session }, videoFilterResult model.filter session model.page model.per_page model.filtertitle)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            (model,Session.changeInterCeptor (Just error))
        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 4) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        ( {model |  menus = item.data.menus, goEdit = True, username = item.data.admin.username}, Cmd.none )
                    else
                        ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , Cmd.batch 
            [
            Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerPartCode (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.instrument (Session.cred session) GetTool
            Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.part (Session.cred session) GetPartDetail Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.post Endpoint.exerCode (Session.cred session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData SelectItem)
            , Api.get GetData ( Endpoint.videoDetail model.getId) (Session.cred session)  (D.videoDetailDecoder Detail DetailData ExerItem)
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            ])
        GoEditApi (Ok ok) ->
            update DetailOrEdit {model | loading = False}
        GoEditApi (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401" then
                (model, Api.fourRefreshFetch ())
            else
                (model, Cmd.none)
        GoEdit ->
            let
                old = model.editItem
                result = List.map (\i ->
                        { action_id =  
                        -- String.fromInt(i.id)
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
                detailFind = model.detaildata
            in
            if model.detaildata.title == "" then
                ({model | validationErr = "운동 제목을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if List.length result == 0 then
                ({model | validationErr = "운동을 선택 해 주세요.", validErrShow = True}, Cmd.none)
            else if model.description == "" then
                ({model | validationErr = "운동 설명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else
            (
                {model | editItem = result, loading = True, validationErr = "" , validErrShow = False}
            , editVideo model.detaildata result model.session model.getId model.description
            )
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
                f = List.filter (\x -> (justInt x.id) == result) model.filterData
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
                    , value =  Just 1
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
            ({model | openFilter = False,gofilter = model.filtertitle :: model.filterName, justFilter = True,filterData= [], page = 1}, videoFilterResult model.filter model.session model.page model.per_page model.filtertitle)
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
            (model, Cmd.none)
        OpenFilter ->
            ({model | openFilter = not model.openFilter} , Cmd.none)
        ExerCode (Ok ok) -> 
            ({model | exerCode = ok.data} ,Cmd.none)
        ExerCode (Err err) ->
            (model, Cmd.none)
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
            (model, Cmd.none)
        DetailOrEdit ->
             let 
                editEncode = Encode.string "수정 완료 되었습니다."
            in
            
            ({model | disabledMask = not model.disabledMask
            , disabled = not model.disabled
            , newStyle = ""
            , btnTitle = 
                if model.btnTitle == "수정" then
                    "저장"
                    else
                    "수정"
            , topTitle =    
                if model.topTitle == "유어핏영상 상세" then
                    "유어핏영상 수정"
                else
                    "유어핏영상 상세"
            }, if model.btnTitle == "저장" then
                Api.showToast editEncode
                else
                Cmd.none
            )
        SucceesEdit (Ok ok) ->
            ({model | filterData = model.filterData ++ ok.data
            , justFilter = False , total_count = ok.paginate.total_count
            }, Cmd.none)
        SucceesEdit (Err err) ->
            let 
                error = Api.decodeErrors err
            in
            if error == "401" then
            (model, Api.thirdRefreshFetch ())
            else
            (model, Cmd.none)
        TitleChange str ->
            let
                old = model.detaildata
                new = {old  | title = str} 
            in
            
            ({model | detaildata = new} , Cmd.none)
        PartSelect str ->
            let
                old = model.detaildata
                new = {old | exercise_part_code = str}
            in
            ({model | detaildata = new}, Cmd.none)
        LevelSelect str ->
            let 
                old = model.detaildata
                new = {old | difficulty_code = str}
            in
            ({model | detaildata = new}, Cmd.none)
        GetPart (Ok ok ) ->
            ({model | partData = ok.data}, Cmd.none)
        GetPart (Err err) ->
            (model, Cmd.none)
        GetLevel (Ok ok ) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            (model, Cmd.none)
        GetId id ->
            let
                idDecode = Decode.decodeValue Decode.string id
            in
            case idDecode of
                Ok i ->
                    ({model | getId = i},
                    Api.get GetData ( Endpoint.videoDetail i) (Session.cred model.session)  (D.videoDetailDecoder Detail DetailData ExerItem))
            
                Err _ ->
                    (model , Cmd.none)    

        GetData (Ok ok) ->
            let
                newInput text = 
                    text
                        |> String.replace "%26" "&"
                        |> String.replace "%25" "%"
            in
            ({model | detaildata = ok.data, description = 
            (case ok.data.description of
                Just desc ->
                    newInput desc
            
                Nothing ->
                    "") ,
              loading = False,
               resultFilterItem = ok.data.exercise_items}, videoFilterResult model.filter model.session model.page model.per_page model.filtertitle)
        GetData(Err err) ->
            (model, Cmd.none)
            
            
-- helperFunction


exerciseMap model=
       div [class "scrollStyle", scrollEvent ScrollEvent] [
        p [ class "title" ]
        (List.indexedMap (\idx x -> 
            (Video.exerciseItem idx x AddItem model.disabledMask GetPreview)
        ) model.filterData)
        ]

emptyList model=
        div [] (List.indexedMap (
            \idx item ->
                (Video.exerciseBackItem idx item BackItem SwitchItem model.newStyle SettingShow model.settingShowIdx 
                 RestSetting model.setSet model.setRest PlusMinusDeleteSet
                 model.valueWarn model.disabledMask GetPreview
                )
        ) model.resultFilterItem
        )

justInt item = 
    case item of
        Just int ->
            int
    
        Nothing ->
            0

view: Model -> {title: String, content: Html Msg, menu : Html Msg}
view model=
    if model.loading then
        -- if model.videoShow then
        { title = ""
            , content =
                div [] [
                div [class "adminloadingMask"][spinner]
                -- , div [class "adminAuthMask"] []
                   
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
                , Video.formView
                model.disabled 
                (exerciseMap model) 
                (emptyList model)
                model.levelData
                LevelSelect
                model.detaildata
                model.partData
                PartSelect
                TitleChange
                model.disabledMask
                Route.Video
                DetailOrEdit
                model
                OpenFilter
                GetFilterItem
                FilterResultData
                AddItem
                model.btnTitle
                model.topTitle
                GoEdit
                model.description
                TextAreaInput
                FilterTitle
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
                div [] [
                    Video.formView
                    model.disabled 
                    (exerciseMap model) 
                    (emptyList model)
                    model.levelData
                    LevelSelect
                    model.detaildata
                    model.partData
                    PartSelect
                    TitleChange
                    model.disabledMask
                    Route.Video
                    DetailOrEdit
                    model
                    OpenFilter
                    GetFilterItem
                    FilterResultData
                    AddItem
                    model.btnTitle
                    model.topTitle
                    GoEdit
                    model.description
                    TextAreaInput
                    FilterTitle
                ]
                , div [] [
                    videoShow "영상 미리보기" model.videoShow VideoClose
                ]
                , div [][
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
    Sub.batch 
        [  Session.changes GotSession (Session.navKey model.session)
        , Api.params GetId
        , Session.retryChange RetryRequest (Session.navKey model.session)
        , Session.secRetryChange VideoRetry (Session.navKey model.session)
        ]
    