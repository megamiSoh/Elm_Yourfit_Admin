module Page.Detail.VideoDetail exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Page.Page exposing(..)
import ExpandEvent as ExEvent
import Json.Decode
import String

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
        exerciseInfo : List SelectModel,
        choiceItem : List SelectModel,
        filterEvent : Bool,
        exer: List ExerModel,
        level : List ExerModel,
        exerItem : List ExerModel,
        exerTool : List ExerModel,
        selectItem : List ExerModel,
        levelItem : List ExerModel,
        emptyexerType : List ExerModel,
        emptytool : List ExerModel,
        total : List ExerModel,
        setting : Bool,
        fileName : String,
        disabled : Bool
    }


init : Model 
init = 
        let
            exers x =  (List.indexedMap (\idx item  -> {title =  item, class = "", index = idx }) x)

            info =  
                (List.indexedMap 
                    (\idx item  -> {
                        title =  item.title, 
                        thum = item.thum, 
                        bodyPart = item.bodyPart, 
                        level = item.level, 
                        exercise = item.exercise, 
                        time = item.time, 
                        index = idx,
                        upNdown = "fas fa-chevron-down",
                        settingClass = "",
                        settingInput = "3",
                        set = "3" }
                    ) infoList
                )
        in
            { exerciseInfo = info , choiceItem = [], filterEvent = False, exer = exers exerPart, selectItem = [], level = exers level, exerItem = exers exeritem, exerTool = exers exerTool, levelItem = [], emptyexerType = [], emptytool = [], total = [], setting = False, fileName = "", disabled = True}



type Msg = ChoiceItem Int | BackItem Int | FilterOpen | FilterClose | Check Int | Level Int | Exer Int | Tool Int | Total | SwitchItem Int | Setting Int | SettingInput String | SetCal (String, String, Int) | SetSave (Int, String) | GetFile String

           
update : Msg -> Model ->  Model
update msg model =
    case msg of
        ChoiceItem idx ->
                {model | choiceItem = model.choiceItem ++ getItem idx model.exerciseInfo 1}
        BackItem index ->
            let
                addItem =
                    takeLists index model.choiceItem ++ dropLists (index + 1) model.choiceItem
            in
                {model | choiceItem = addItem }
        FilterClose ->
                {model | filterEvent = False}
        FilterOpen ->
                {model | filterEvent =  True}
        Check idx->
                if List.length( findItem idx model.selectItem) > 0 then
                    {model | selectItem = resultItem model.selectItem idx, 
                    exer = selectedItem "" model.exer idx
                    }
                else
                    {model | selectItem = model.selectItem ++ getItem idx model.exer 1, 
                    exer = selectedItem "is-dark" model.exer idx
                    }

        Level idx->
                if List.length( findItem idx model.levelItem) > 0 then
                    {model | levelItem = resultItem model.levelItem idx,
                    level = selectedItem "" model.level idx
                    }
                else
                    {model | levelItem = model.levelItem ++ getItem idx model.level 1, 
                    level = selectedItem "is-dark" model.level idx
                    }  

        Exer idx->
                if List.length( findItem idx model.emptyexerType) > 0 then
                    {model | emptyexerType = resultItem model.emptyexerType idx, 
                    exerItem = selectedItem "" model.exerItem idx
                    }
                else
                    {model | emptyexerType = model.emptyexerType ++ getItem idx model.exerItem 1,
                    exerItem = selectedItem "is-dark" model.exerItem idx
                     } 

        Tool idx->
                if List.length( findItem idx model.emptytool) > 0 then
                    {model | emptytool = resultItem model.emptytool idx,
                    exerTool = selectedItem "" model.exerTool idx
                    }
                else
                    {model | emptytool = model.emptytool ++ getItem idx model.exerTool 1, 
                    exerTool = selectedItem "is-dark" model.exerTool idx
                    } 
        Total ->
            {model | total = model.total ++ model.emptytool ++ model.emptyexerType ++ model.levelItem ++ model.selectItem , filterEvent =  False}
        SwitchItem idx ->
            let
                before = 
                    takeLists (len - 2) model.choiceItem
                after =
                    dropLists (idx + 1) model.choiceItem
                len = 
                    List.length (takeLists ( idx  + 1 ) model.choiceItem)
                getVal = 
                    dropLists ( len - 2 ) (takeLists ( idx  + 1 ) model.choiceItem)
                result item = 
                    List.reverse item
            in
                if (idx+1) > 2 then
                    {model | choiceItem = before ++ List.reverse getVal ++ after}
                else
                    {model | choiceItem =  before ++ List.reverse getVal ++ after}
        Setting idx ->
            let
                bvalue = 
                    List.take idx model.choiceItem
                avalue =
                    List.drop (idx + 1) model.choiceItem
                aAfter = 
                    getItem idx model.choiceItem 1
            in
            if List.length(findValue model.choiceItem) > 0 then
                { model | choiceItem =  bvalue ++ (testtesttest "fas fa-chevron-down" aAfter "") ++ avalue}
            else
                {model |  choiceItem =  bvalue ++ (testtesttest "fas fa-chevron-up" aAfter "newStyle") ++ avalue} 
       
        SettingInput val->
            let
                setting = 
                    List.map (\x -> {x | settingInput = val}) model.choiceItem
            in
                { model | choiceItem = setting } 
        SetCal (oper , val, idx)->
            let
                before = 
                    takeLists (len - 1) model.choiceItem
                after =
                    dropLists (idx + 1) model.choiceItem
                len = 
                    List.length (takeLists ( idx  + 1 ) model.choiceItem)
                getVal = 
                    dropLists ( len - 1 ) (takeLists ( idx  + 1 ) model.choiceItem)
                setting new = 
                    List.map (\x -> {x | settingInput = new}) getVal
                stringToInt = String.toInt(val)
                resultMinus item =
                    String.fromInt(item - 1)
                resultPlus item =
                    String.fromInt(item + 1)   
            in
            case stringToInt of
                Just result ->
                    if oper == "minus" then
                        {model | choiceItem = before ++ setting (resultMinus result) ++ after }
                    else 
                        {model | choiceItem = before ++ setting (resultPlus result) ++ after }
                Nothing ->
                    {model | choiceItem = setting val}
        SetSave (idx, val) -> 
            let
                before = 
                    takeLists (len - 1) model.choiceItem
                after =
                    dropLists (idx + 1) model.choiceItem
                len = 
                    List.length (takeLists ( idx  + 1 ) model.choiceItem)
                getVal = 
                    dropLists ( len - 1 ) (takeLists ( idx  + 1 ) model.choiceItem)
                result = 
                     List.map (\x -> {x | set = val, upNdown = "fas fa-chevron-down", settingClass = ""}) getVal
            in
                {model | choiceItem = before ++ result ++ after}

        GetFile filename ->
                ({model | fileName = filename})
            
            
-- helperFunction
onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Json.Decode.map tagger targetValue)

targetFiles : Json.Decode.Decoder (List String)
targetFiles = 
    Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.string)



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


view : Model -> Html Msg
view  model =
    div[ class "container is-fluid"] [
        columnsHtml [pageTitle "유어핏 영상 상세"],
        columnsHtml [
            pageTitle "기본설정" 
        ],
        columnsHtml [
            formInput "운동 제목" "운동 제목을 입력 해 주세요." model.disabled ,
            formSelect "난이도" model.disabled
        ],
        columnsHtml [
           div [class "field is-horizontal"] [
                labelWrap "썸네일",
                inputBtnx "썸네일을 선택 해 주세요" model.disabled "찾아보기"  model
           ],
           formSelect "운동 부위" model.disabled
        ],
        columnsHtml [
             div [ class "field is-horizontal" ] [
                labelWrap "운동 조합",
                button [ class "button is-small is-primary" ] [
                text "필터 설정"
                ],
                videoFilter model model.exer model.level model.exerItem model.exerTool,
                div [  class "field is-grouped is-grouped-multiline filterStyle" ] 
                (List.indexedMap filterItem model.total)
            ] 
        ],
        columnsHtml [
            div [ class "field-body  customBox" ]
            [ div [ class "field  is-fullwidth" ]
                [ p [ class "control" ] [
                        p [ class "title" ]
                                [   
                                    div [ class "media restStyle"] [
                                    i [ class "far fa-plus-square" ]
                                    [],
                                  div [ class "media-left" ]
                                    [ figure [ class "image is-85x85" ]
                                        [ img [ src "https://bulma.io/images/placeholders/96x96.png", alt "Placeholder image" ]
                                            []
                                        ]
                                    ]
                                    , div [ class "media-content" ]
                                    [ p [ class "title is-4" ]
                                        [ text "휴식" ]
                                    , p [ class "subtitle is-6" ]
                                        [ text "2" ]
                                    ]
                                    
                                ],
                                   div [class "scrollStyle"] [
                                        p [ class "title" ]
                                        (List.indexedMap exerciseItem model.exerciseInfo)
                                   ]
                                ]
                            ]
                    
                ]
            ],
            div [ class "field-body customBox scrollon" ]
            [ div [ class "field  is-fullwidth" ]
                [ p [ class "control" ]
                     [ 
                         emptyList model
                     ]
                ]
            ]
        ],
        columnsHtml [
            registSetBtn "/video"
        ]
    ]


thumbInput model ph= 
    if model.fileName == "" then
        text ph
    else 
        text model.fileName
-- inputBtnx : String -> Bool -> String -> String -> Html msg
inputBtnx ph read btn model=
    div [ class "file has-name is-right is-fullwidth" ]
        [ label [ class "file-label" ]
            [ input [ class "file-input", disabled read, type_ "file", multiple False, id "thumbFile", onChange GetFile ]
                []
            , span [ class "file-cta" ]
                [ span [ class "file-icon" ]
                    [ i [ class "fas fa-upload" ]
                        []
                    ]
                , span [ class "file-label" ]
                    [ text btn ]
                ]
            , span [ class "file-name" ]
                [  thumbInput model ph ]
            ]
        ]


emptyList model=
    if List.length model.choiceItem > 0 then
        div [] (List.indexedMap exerciseBackItem model.choiceItem
                )
    
    else 
        div [class "emptyText"] [
            p [] [
                text "운동을 선택 해 주세요."
            ]
        ]
-- m : Int -> Model -> Html msg
exerciseItem idx item =
        div [ class "media restStyle" ] [
            i [ class "far fa-plus-square", onClick( ChoiceItem idx) ]
                [  ],
            div [ class "media-left" ]
            [ figure [ class "image is-85x85" ]
                [ img [ src "https://bulma.io/images/placeholders/96x96.png", alt "Placeholder image" ]
                    []
                ]
            ]
            , div [ class "media-content" ]
            [ p [ class "title is-4" ]
                [ text (item.title ++ " - " ++ "3 세트")  ]
            , p [ class "subtitle is-6" ]
               [
                   p []  ( List.map restMark item.bodyPart ),
                   p [] [text (item.level ++ " - "),
                    span [] (List.map hypen item.exercise) ,
                    text (String.fromInt (item.time // 60) ++ "분" ++ String.fromInt ( item.time - (item.time //60 * 60)) ++ "초")
                   ]
               ]
            ]
        ]

exerciseBackItem idx item =
    div [ class "restStyle"] [
        div [ class "media" ] [
            div [] [
                i [ class ("fas fa-arrows-alt-v " ++ "style"++String.fromInt(idx) ), onClick (SwitchItem idx) ]
                []
            ],
            i [ class "far fa-minus-square", onClick( BackItem idx) ]
                [],
            div [ class "media-left" ]
            [ figure [ class "image is-85x85" ]
                [ img [ src "https://bulma.io/images/placeholders/96x96.png", alt "Placeholder image" ]
                    []
                ]
            ]
            , div [ class "media-content" ]
            [ p [ class "title is-4" ]
                [ text (item.title ++ " - " ++ item.set ++ "세트" )  ]
            , p [ class "subtitle is-6" ]
               [
                   p []  ( List.map restMark item.bodyPart ),
                   p [] [text (item.level ++ " - "),
                    span [] (List.map hypen item.exercise) ,
                    text (String.fromInt (item.time // 60) ++ "분" ++ String.fromInt ( item.time - (item.time //60 * 60)) ++ "초")
                   ]
               ]
            ]
            , p [] [
                i [ class item.upNdown, onClick (Setting idx) ]
                []
            ]
        ],
        div [ class ("overlay  settingStyle " ++ item.settingClass) ] [
            setSetting item.settingClass item.settingInput idx
            ]
        ]


setSetting style settingInput idx=
        div [ class ("is-small content " ++ style) ]
        [
           div [ class "settingItem" ] [
                h4[] [text "세트 설정"],
            div[ class "level"] [
                div [ class "level-item"] [
                    div [ class "button" , onClick (SetCal ("minus" ,settingInput, idx))  ] [
                        i [ class "fas fa-minus"]
                        []
                     ]
                ],
                div [ class "level-item"] [ 
                    input [ class "input", type_ "text", placeholder "세트를 설정해 주세요." ,onInput SettingInput , value settingInput]
                    []
                ],
                div [ class "level-item" ] [
                    
                    text " SET "
                ],
                div [ class "level-item"] [ 
                    div [ class "button", onClick (SetCal ("plus", settingInput , idx)) ] [ 
                        i [ class "fas fa-plus"]
                        []
                     ]
                 ]
            ],
            div[ class "level"] [
                div [ class "level-item"] [
                    div [class "button is-primary", onClick (SetSave (idx, settingInput))] [text "저장"]
                ],
                div [ class "level-item"] [
                    div [class "button is-danger", onClick( BackItem idx)] [text "삭제"]
                ]
            ]
           ]
        ]
        
restMark item = 
    span [] [text (item ++ ",")]
hypen item = 
    span [] [text (item ++ " - ")]


itemText idx item check= 
   label [ class( "button is-light filterBtn " ++ item.class)]
                [ input [  type_ "checkbox" ,  onClick (check  idx), value item.title]
                    [ ] ,text item.title   
                ]
filterItem idx item= 
             div [ class "control" ]
                [ div [ class "tags has-addons" ]
                    [ a [ class "tag is-link size-small" ]
                        [ text item.title ]
                    , a [ class "tag is-delete size-small" ]
                        []
                    ]
                ]


videoFilter model item levelitem exerItem tool=
    if model.filterEvent then
        div [] [
        div [ class "widePop"] [
            popTitle "필터 설정" ,
            div [ class "closeBtn"][
                    i [ class "far fa-times-circle" , onClick FilterClose]
                    []
                ],
            columnsHtml[
                div [] [
                    text "운동부위"
                ]
            ],
            columnsHtml [
                    div[]
                    (List.indexedMap 
                        (\idx x ->
                            itemText idx x Check
                        )item)
            ],
            
            columnsHtml[
                div [] [
                    text "난이도"
                ]
            ],
            columnsHtml [
                 div[]
                    (List.indexedMap 
                        (\idx x ->
                            itemText idx x Level
                        )levelitem)
            ],
            columnsHtml[
                div [] [
                    text "운동종류"
                ]
            ],
             columnsHtml [
                  div[]
                    (List.indexedMap 
                        (\idx x ->
                            itemText idx x Exer
                        )exerItem)
            ],
            columnsHtml[
                div [] [
                    text "기구"
                ]
            ],
             columnsHtml [
                  div[]
                    (List.indexedMap 
                        (\idx x ->
                            itemText idx x Tool
                        )tool)
            ],
            columnsHtml [
                div [ class "layerBtn"] [
                    div [ class "button is-primary" , onClick Total]
                    [ text "확인" ]
                    ,  div [ class "button is-warning"  , onClick FilterClose]
                    [ text "취소" ]
                ]
            ]
        ]
        ]
    else
        span [] []








    
exerPart = ["가슴","팔","등","복부","허벅지","엉덩이","종아리"]
level = ["상","중","하"]
exeritem = ["헬스","필라테스","요가","스트레칭"]
exerTool = ["맨손운동","덤벨","케틀벨","짐볼","바벨"]
infoList = 
        [
            {
                title = "운동1",
                thum = "이미지",
                bodyPart= ["팔","다리"],
                level = "상",
                exercise = ["헬스", "맨손"],
                time = 130
            }
            ,
            {
                title = "운동2",
                thum = "이미지3",
                bodyPart= ["팔","다리"],
                level = "상",
                exercise = ["헬스", "맨손"],
                time = 130
            },
            {
                title = "운동3",
                thum = "이미지1",
                bodyPart= ["팔","다리"],
                level = "상",
                exercise = ["헬스", "맨손"],
                time = 130
            },
            {
                title = "운동4",
                thum = "이미지5",
                bodyPart= ["팔","다리"],
                level = "상",
                exercise = ["헬스", "맨손"],
                time = 130
            },
            {
                title = "운동5",
                thum = "이미지d",
                bodyPart= ["팔","다리"],
                level = "상",
                exercise = ["헬스", "맨손"],
                time = 130
            }
        ]