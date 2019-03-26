module Page.Edit.VideoEdit exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Page.Page exposing(..)
import ExpandEvent as ExEvent
import Page.Origin.Video as Video
import Json.Decode
import String
import Session exposing (Session)
import Route exposing(..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias SelectModel = 
    {
        title : String,
        thum : String,
        bodyPart : List String,
        level : String,
        exercise : List String,
        time : Int,
        index: Int,
        upNdown : String,
        settingClass : String,
        settingInput : String,
        set : String
    }
type alias ExerModel =
    {
        title :  String,
        class: String,
        index: Int
    }
type alias Model =
    {
        -- exerciseInfo : List SelectModel,
        -- choiceItem : List SelectModel,
        -- filterEvent : Bool,
        -- exer: List ExerModel,
        -- level : List ExerModel,
        -- exerItem : List ExerModel,
        -- exerTool : List ExerModel,
        -- selectItem : List ExerModel,
        -- levelItem : List ExerModel,
        -- emptyexerType : List ExerModel,
        -- emptytool : List ExerModel,
        -- total : List ExerModel,
        -- setting : Bool,
        -- fileName : String,
        -- disabled : Bool,
        session: Session
        , menus : List Menus
    }
type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }


init : Session -> (Model, Cmd Msg) 
init session = 
        -- let
        --     exers x =  (List.indexedMap (\idx item  -> {title =  item, class = "", index = idx }) x)

        --     info =  
        --         (List.indexedMap 
        --             (\idx item  -> {
        --                 title =  item.title, 
        --                 thum = item.thum, 
        --                 bodyPart = item.bodyPart, 
        --                 level = item.level, 
        --                 exercise = item.exercise, 
        --                 time = item.time, 
        --                 index = idx,
        --                 upNdown = "fas fa-chevron-down",
        --                 settingClass = "",
        --                 settingInput = "3",
        --                 set = "3" }
        --             ) infoList
        --         )
        -- in
            ({
                --  exerciseInfo = info , choiceItem = [], filterEvent = False, exer = exers exerPart, selectItem = [], level = exers level, exerItem = exers exeritem, exerTool = exers exerTool, levelItem = [], emptyexerType = [], emptytool = [], total = [], setting = False, fileName = "", disabled = False, 
                 menus = []
                 , session = session}, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))


toSession: Model -> Session
toSession model = 
    model.session

type Msg 
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
--  ChoiceItem Int | BackItem Int | FilterOpen | FilterClose | Check Int | Level Int | Exer Int | Tool Int | Total | SwitchItem Int | Setting Int | SettingInput String | SetCal (String, String, Int) | SetSave (Int, String) | GetFile String

           
update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )
        -- ChoiceItem idx ->
        --         ({model | choiceItem = model.choiceItem ++ getItem idx model.exerciseInfo 1}, Cmd.none)
        -- BackItem index ->
        --     let
        --         addItem =
        --             takeLists index model.choiceItem ++ dropLists (index + 1) model.choiceItem
        --     in
        --         ({model | choiceItem = addItem }, Cmd.none)
        -- FilterClose ->
        --         ({model | filterEvent = False}, Cmd.none)
        -- FilterOpen->
            
        --          ({model | filterEvent =  True }, Cmd.none)
        -- Check idx->
        --         if List.length( findItem idx model.selectItem) > 0 then
        --             ({model | selectItem = resultItem model.selectItem idx, 
        --             exer = selectedItem "" model.exer idx
        --             }, Cmd.none)
        --         else
        --             ({model | selectItem = model.selectItem ++ getItem idx model.exer 1, 
        --             exer = selectedItem "is-dark" model.exer idx
        --             }, Cmd.none)

        -- Level idx->
        --         if List.length( findItem idx model.levelItem) > 0 then
        --             ({model | levelItem = resultItem model.levelItem idx,
        --             level = selectedItem "" model.level idx
        --             }, Cmd.none)
        --         else
        --             ({model | levelItem = model.levelItem ++ getItem idx model.level 1, 
        --             level = selectedItem "is-dark" model.level idx
        --             }, Cmd.none)  

        -- Exer idx->
        --         if List.length( findItem idx model.emptyexerType) > 0 then
        --             ({model | emptyexerType = resultItem model.emptyexerType idx, 
        --             exerItem = selectedItem "" model.exerItem idx
        --             }, Cmd.none)
        --         else
        --             ({model | emptyexerType = model.emptyexerType ++ getItem idx model.exerItem 1,
        --             exerItem = selectedItem "is-dark" model.exerItem idx
        --              }, Cmd.none) 

        -- Tool idx->
        --         if List.length( findItem idx model.emptytool) > 0 then
        --             ({model | emptytool = resultItem model.emptytool idx,
        --             exerTool = selectedItem "" model.exerTool idx
        --             }, Cmd.none)
        --         else
        --             ({model | emptytool = model.emptytool ++ getItem idx model.exerTool 1, 
        --             exerTool = selectedItem "is-dark" model.exerTool idx
        --             }, Cmd.none) 
        -- Total ->
        --     ({model | total = model.total ++ model.emptytool ++ model.emptyexerType ++ model.levelItem ++ model.selectItem , filterEvent =  False}, Cmd.none)
        -- SwitchItem idx ->
        --     let
        --         before = 
        --             takeLists (len - 2) model.choiceItem
        --         after =
        --             dropLists (idx + 1) model.choiceItem
        --         len = 
        --             List.length (takeLists ( idx  + 1 ) model.choiceItem)
        --         getVal = 
        --             dropLists ( len - 2 ) (takeLists ( idx  + 1 ) model.choiceItem)
        --         result item = 
        --             List.reverse item
        --     in
        --         if (idx+1) > 2 then
        --             ({model | choiceItem = before ++ List.reverse getVal ++ after}, Cmd.none)
        --         else
        --             ({model | choiceItem =  before ++ List.reverse getVal ++ after}, Cmd.none)
        -- Setting idx ->
        --     let
        --         bvalue = 
        --             List.take idx model.choiceItem
        --         avalue =
        --             List.drop (idx + 1) model.choiceItem
        --         aAfter = 
        --             getItem idx model.choiceItem 1
        --     in
        --     if List.length(findValue model.choiceItem) > 0 then
        --         ({ model | choiceItem =  bvalue ++ (testtesttest "fas fa-chevron-down" aAfter "") ++ avalue}, Cmd.none)
        --     else
        --         ({model |  choiceItem =  bvalue ++ (testtesttest "fas fa-chevron-up" aAfter "newStyle") ++ avalue}, Cmd.none) 
       
        -- SettingInput val->
        --     let
        --         setting = 
        --             List.map (\x -> {x | settingInput = val}) model.choiceItem
        --     in
        --         ({ model | choiceItem = setting }, Cmd.none) 
        -- SetCal (oper , val, idx)->
        --     let
        --         before = 
        --             takeLists (len - 1) model.choiceItem
        --         after =
        --             dropLists (idx + 1) model.choiceItem
        --         len = 
        --             List.length (takeLists ( idx  + 1 ) model.choiceItem)
        --         getVal = 
        --             dropLists ( len - 1 ) (takeLists ( idx  + 1 ) model.choiceItem)
        --         setting new = 
        --             List.map (\x -> {x | settingInput = new}) getVal
        --         stringToInt = String.toInt(val)
        --         resultMinus item =
        --             String.fromInt(item - 1)
        --         resultPlus item =
        --             String.fromInt(item + 1)   
        --     in
        --     case stringToInt of
        --         Just result ->
        --             if oper == "minus" then
        --                 ({model | choiceItem = before ++ setting (resultMinus result) ++ after }, Cmd.none)
        --             else 
        --                 ({model | choiceItem = before ++ setting (resultPlus result) ++ after }, Cmd.none)
        --         Nothing ->
        --             ({model | choiceItem = setting val}, Cmd.none)
        -- SetSave (idx, val) -> 
        --     let
        --         before = 
        --             takeLists (len - 1) model.choiceItem
        --         after =
        --             dropLists (idx + 1) model.choiceItem
        --         len = 
        --             List.length (takeLists ( idx  + 1 ) model.choiceItem)
        --         getVal = 
        --             dropLists ( len - 1 ) (takeLists ( idx  + 1 ) model.choiceItem)
        --         result = 
        --              List.map (\x -> {x | set = val, upNdown = "fas fa-chevron-down", settingClass = ""}) getVal
        --     in
        --         ({model | choiceItem = before ++ result ++ after}, Cmd.none)

        -- GetFile filename ->
        --         ({model | fileName = filename}, Cmd.none)
            
            
-- -- helperFunction
onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Json.Decode.map tagger targetValue)

targetFiles : Json.Decode.Decoder (List String)
targetFiles = 
    Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.string)

dropLists idx model = 
    List.drop idx model

takeLists idx model = 
    List.take idx model
resultBefore class sec = 
    List.map(\x -> {x|class = class}) sec

classclass class sec = 
    List.map(\x -> {x | settingClass = class}) sec

testtesttest class sec value= 
    List.map(\x -> {x | upNdown = class, settingClass = value}) sec

resultItem item idx = 
    List.filter (\z -> z.index /= idx) item

findItem idx model = 
    List.filter (\x -> x.index == idx) model

findValue model =
    List.filter (\x -> x.settingClass == "newStyle") model
    
getItem idx model num =
    List.take num (dropLists idx model)

selectedItem class model idx =
    List.sortBy .index (resultBefore class (getItem idx model 1) ++ resultItem model idx)

-- exerciseMap model=
--        div [class "scrollStyle"] [
--         p [ class "title" ]
--         (List.indexedMap (\idx x -> 
--             (Video.exerciseItem idx x ChoiceItem)
--         ) model.exerciseInfo)
--                                    ]
-- videoFilter model = 
--     if model.filterEvent then
--         Video.videoFilter 
--         (videoFilterItem model.exer Check) 
--         (videoFilterItem model.level Level) 
--         (videoFilterItem model.exerItem Exer) 
--         (videoFilterItem model.exerTool Tool) 
--         FilterClose 
--         Total
--     else 
--         div [] []
-- videoFilterItem model msg=
--     div[]
--         (List.indexedMap 
--             (\idx x ->
--                 Video.itemText idx x msg
--             )model)

-- inputBtnx btn model thumb =
--     Video.inputBtnx btn model.disabled GetFile thumb 

-- emptyList model=
--     if List.length model.choiceItem > 0 then
--         div [] (List.indexedMap (
--             \idx item ->
--                 (Video.exerciseBackItem idx item SwitchItem BackItem Setting SetCal SettingInput SetSave )
--         ) model.choiceItem
--                 )
    
--     else 
--         div [class "emptyText"] [
--             p [] [
--                 text "운동을 선택 해 주세요."
--             ]
--         ]

view model=
        { title = "유어핏 영상 등록" 
        , content = 
            div [] [
            -- Video.formView
            -- model.disabled 
            -- (exerciseMap model) 
            -- FilterOpen 
            -- (videoFilter model)
            -- (emptyList model)
            -- (inputBtnx "찾아보기" model (thumbInput model "썸네일을 선택 해 주세요."))
            -- (choiceItemAll model)
            -- (routeRegist Route.Video)
            -- "유어핏 영상 등록"
        ]
        , menu =  
        aside [ class "menu"] [
            ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
        ]
    }

-- choiceItemAll model = 
--     div [  class "field is-grouped is-grouped-multiline filterStyle" ] 
--     (List.indexedMap 
--         (\idx item ->
--             (Video.filterItem idx item)
--             )
--      model.total)


-- thumbInput model ph= 
--     if model.fileName == "" then
--         text ph
--     else 
--         text model.fileName



        
-- restMark item = 
--     span [] [text (item ++ ",")]
-- hypen item = 
--     span [] [text (item ++ " - ")]







    
-- exerPart = ["가슴","팔","등","복부","허벅지","엉덩이","종아리"]
-- level = ["상","중","하"]
-- exeritem = ["헬스","필라테스","요가","스트레칭"]
-- exerTool = ["맨손운동","덤벨","케틀벨","짐볼","바벨"]
-- infoList = 
--         [
--             {
--                 title = "운동1",
--                 thum = "이미지",
--                 bodyPart= ["팔","다리"],
--                 level = "상",
--                 exercise = ["헬스", "맨손"],
--                 time = 130
--             }
--             ,
--             {
--                 title = "운동2",
--                 thum = "이미지3",
--                 bodyPart= ["팔","다리"],
--                 level = "상",
--                 exercise = ["헬스", "맨손"],
--                 time = 130
--             },
--             {
--                 title = "운동3",
--                 thum = "이미지1",
--                 bodyPart= ["팔","다리"],
--                 level = "상",
--                 exercise = ["헬스", "맨손"],
--                 time = 130
--             },
--             {
--                 title = "운동4",
--                 thum = "이미지5",
--                 bodyPart= ["팔","다리"],
--                 level = "상",
--                 exercise = ["헬스", "맨손"],
--                 time = 130
--             },
--             {
--                 title = "운동5",
--                 thum = "이미지d",
--                 bodyPart= ["팔","다리"],
--                 level = "상",
--                 exercise = ["헬스", "맨손"],
--                 time = 130
--             }
--         ]