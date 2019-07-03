module Page.Origin.Video exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Page.Page exposing(..)
import ExpandEvent as ExEvent
import Json.Decode
import String
import Route exposing (..)


            
-- -- helperFunction
onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Json.Decode.map tagger targetValue)

targetFiles : Json.Decode.Decoder (List String)
targetFiles = 
    Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.string)
registformView exercise empty levelmodel seletmsg  partmodel partmsg titlemsg  url   model openbtn check filterResult addItem  goedit textAreaInput filterTitle pointCheck payMsg sexMsg=
    div[] [
        columnsHtml [pageTitle "유어핏영상 등록"],
        columnsHtml [
            labelWrap "기본설정" 
        ],
        div [class "searchWrap"] [
            columnsHtml [
            formInputEvent "운동 제목" "운동 제목을 입력 해 주세요." False titlemsg model.titleModel 
            ],
            columnsHtml [
            noEmptyselectForm "난이도" False levelmodel seletmsg  model.levelModel,
            noEmptyselectForm "운동 부위" False partmodel partmsg  model.partModel
            ]
            ,columnsHtml [
            noEmptyselectForm "유 / 무료" False [{code = "false", name = "무료"}, {code = "true", name = "유료"}] payMsg  model.is_pay
            , noEmptyselectForm "성별" (if model.is_pay == "false" then True else False) [{code = "", name = "구분 없음"},{code = "true", name = "남성"}, {code = "false", name = "여성"}] sexMsg model.is_sex
            ]
            , columnsHtml [
            checkBoxCustom model.pointCode (if model.is_pay == "false" then True else False) model.checkPoint pointCheck model.checkPoint
           , textAreaRegist "운동 설명" False "250자까지 입력 가능" textAreaInput
        ]
        

        ]
        ,
        columnsHtml [
             div [ class "field is-horizontal" ] [
                labelWrap "운동 조합",
                button [ class "button is-small is-primary" , disabled False, onClick openbtn ] [
                text "필터 설정"
                ]
                ,filterItem model.gofilter
            ] 
        ],
        columnsHtml [
            div [ class "field-body  customBox" ]
            [ 
                div [ class "field  is-fullwidth" ]
                [ p [ class "control" ] [
                    p [ class "title" ]
                            [   
                                div [ class "media restStyle"] [
                                i [ class "far fa-plus-square", onClick (addItem Nothing) ]
                                [],
                                div [ class "media-left" ]
                                [  figure [ class "image is-85x85" ]
                                    [ i [ class "fas fa-coffee" ]
                                        []
                                    ]
                                ]
                                , div [ class "media-content" ]
                                [ p [ class "title is-4" ]
                                    [ text "휴식" ]
                                , p [ class "subtitle is-6" ]
                                    [ text "1 분" ]
                                ]
                            ],
                            exercise
                        ]
                    ]
                    
                ]
            ]

            , div [ class "field-body customBox scrollon" ]
            [ 
                div [ class "field  is-fullwidth" ]
                [ p [ class "control" ]
                     [ 
                        empty
                     ]
                ]
            ]
        ],
        columnsHtml [
                div [ class "buttons" ] [
                    div [ class "button is-primary cursor", onClick goedit ] [text "저장"]
                    ,
                    a [ class "button is-warning", Route.href (Just url) ] [text "취소"]
                ]
        ]
        
         , 
        videoFilter model.partDetail model.levelData model.exerCode model.instrument model.openFilter openbtn check model.filter filterResult filterTitle model.filtertitle
    ]

checkBoxCustom list disabled checkV partMsg checkModel=
        div [ class "field is-horizontal" ]
            [ 
            labelWrap "운동 방향",
            div [ class "field-body customCheck" ]
                [ div [ class "field  is-fullwidth" ]
                    [ p [ class "control customCheck" ]
                        (
                            List.map (
                                \title ->
                                checkBoxReadOnly checkModel title partMsg disabled 
                                -- checkBoxReadOnly checkModel title partMsg disabled(
                                --     let 
                                --         match = 
                                --             List.head(List.filter(\f ->
                                --             f.code == title.code
                                --             )checkV  )
                                --     in
                                --     case match of
                                --         Just m ->
                                --             m
                                --         Nothing ->
                                --             { code= ""
                                --             , name= ""}
                                --     )
                                ) list
                            )
                    ]
                ]
            ]


checkBoxReadOnly checkModel title partMsg readOnly = 
    label [ class "checkbox" ]
        [ input 
        [ type_ "checkbox"
        , disabled readOnly
        -- , value checkModel
        -- , checked (val.code == title.code)
        , onClick (partMsg (title.code , title.name))
        ]
            [], text title.name 
        ]

checkBoxReadOnlyDetail checkModel title partMsg readOnly checkVal = 
    label [ class "checkbox" ]
        [ input 
        [ type_ "checkbox"
        , disabled readOnly
        , value title.code
        , checked (List.member title.code checkVal)
        , onClick (partMsg (title.code , title.name))
        ]
            [], text title.name 
        ]


checkBoxCustomDetail list disabled checkV partMsg checkModel checkVal =
        div [ class "field is-horizontal" ]
            [ 
            labelWrap "운동 방향",
            div [ class "field-body customCheck" ]
                [ div [ class "field  is-fullwidth" ]
                    [ p [ class "control customCheck" ]
                        (
                            List.map (
                                \title ->
                                checkBoxReadOnlyDetail checkModel title partMsg disabled checkVal
                                -- checkBoxReadOnly checkModel title partMsg disabled(
                                --     let 
                                --         match = 
                                --             List.head(List.filter(\f ->
                                --             f.code == title.code
                                --             )checkV  )
                                --     in
                                --     case match of
                                --         Just m ->
                                --             m
                                --         Nothing ->
                                --             { code= ""
                                --             , name= ""}
                                --     )
                                ) list
                            )
                    ]
                ]
            ]

formView dis exercise empty levelmodel seletmsg seletmodel partmodel partmsg titlemsg disabledMask url  changePage model openbtn check filterResult addItem btntitle toptitle goedit description textareaInput filterTitle payMsg sexMsg pointCheck =
    let
        textInput text =
            text
                |> String.replace "%26" "&"    
                |> String.replace "%25" "%"
    in
    
    div[] [
        columnsHtml [pageTitle toptitle],
        columnsHtml [
            labelWrap "기본설정" 
        ],
        div [class "searchWrap"] [
            columnsHtml [
            formInputEvent "운동 제목" "운동 제목을 입력 해 주세요." dis titlemsg (textInput seletmodel.title)
            ],
            columnsHtml [
            noEmptyselectForm "난이도" dis levelmodel seletmsg seletmodel.difficulty_code ,
            noEmptyselectForm "운동 부위" dis partmodel partmsg seletmodel.exercise_part_code
            ] 
            ,columnsHtml [
            noEmptyselectForm "유 / 무료" model.disabled [{code = "false", name = "무료"}, {code = "true", name = "유료"}] payMsg  model.is_pay
            , noEmptyselectForm "성별" (if model.disabled || model.is_pay =="false" then True else False) [{code = "", name = "구분 없음"},{code = "true", name = "남성"}, {code = "false", name = "여성"}] sexMsg model.is_sex
            ]
            , columnsHtml [
            checkBoxCustomDetail model.pointCode (if model.disabled || model.is_pay =="false" then True else False) model.checkPoint pointCheck model.checkPoint model.checkVal
            , textAreaEvent "운동설명" dis description textareaInput
            ]
            
        ]
        ,
        columnsHtml [
             div [ class "field is-horizontal" ] [
                labelWrap "운동 조합",
                button [ class "button is-small is-primary" , disabled dis, onClick openbtn ] [
                text "필터 설정"
                ]
                ,filterItem model.gofilter
            ] 
        ],
        columnsHtml [
            div [ class "field-body  customBox", style "background-color" (if disabledMask then "" else "#80808021")
                 ]
            [ 
                if disabledMask then
                div [ class "field  is-fullwidth" ]
                [ p [ class "control" ] [
                    p [ class "title" ]
                            [   
                                div [ class "media restStyle"] [
                                i [ class "far fa-plus-square" , onClick (addItem Nothing)]
                                [],
                                div [ class "media-left" ]
                                [ figure [ class "image is-85x85" ]
                                    [ i [ class "fas fa-coffee" ]
                                        []
                                    ]
                                ]
                                , div [ class "media-content" ]
                                [ p [ class "title is-4" ]
                                    [ text "휴식" ]
                                , p [ class "subtitle is-6" ]
                                    [ text "1 분" ]
                                ]
                            ],
                            exercise
                        ]
                    ]
                    
                ]
                else
                div [ class "field  is-fullwidth" ]
                [ p [ class "control" ] [
                    p [ class "title" ]
                            [   
                                div [ class "media restStyle"] [
                                i [ class "far fa-plus-square" ]
                                [],
                                div [ class "media-left" ]
                                [ figure [ class "image is-85x85" ]
                                    [ 
                                    i [ class "fas fa-coffee" ]
                                        []
                                    ]
                                ]
                                , div [ class "media-content" ]
                                [ p [ class "title is-4" ]
                                    [ text "휴식" ]
                                , p [ class "subtitle is-6" ]
                                    [ text "1 분" ]
                                ]
                            ],
                            exercise
                        ]
                    ]
                    
                ]
            ]

            , div [ class "field-body customBox scrollon" , style "background-color" (if disabledMask then "" else "#80808021")]
            [ 
                --   if disabledMask then
                --     div [] []
                -- else
                --     div [ class "disabledMsk"][]
                -- ,
                div [ class "field  is-fullwidth" ]
                [ p [ class "control" ]
                     [ 
                        empty
                     ]
                ]
            ]
        ],
        columnsHtml [
            if model.goEdit then
            div [ class "buttons" ] [
                    if btntitle == "수정" then
                    div [ class "button is-primary cursor", onClick changePage ] [text btntitle]
                    else
                    div [ class "button is-primary cursor", onClick goedit ] [text btntitle]
                    ,
                    a [ class "button is-warning", Route.href (Just url) ] [text "취소"]
                ]
            else
            backPageBtn url
        ]
         , 
        videoFilter model.partDetail model.levelData model.exerCode model.instrument model.openFilter openbtn check model.filter filterResult filterTitle model.filtertitle
    ]

filterItem item = 
    if item == [] then
        div [] []
    else
        div [ class "control" ]
        [ div [ class "tags has-addons" ]
                (List.map (\x -> 
                span [ class "tag is-light tagStyle" ]
                [ text x ]
                ) item)
        ]
inputBtnx btn model getFile thumb=
    div [ class "field is-horizontal" ] [
        labelWrap "썸네일",
        div [ class "file has-name is-right is-fullwidth" ]
        [ label [ class "file-label" ]
            [ input [ class "file-input", disabled model, type_ "file", multiple False, id "thumbFile", onChange getFile ]
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
                [ 
                    thumb
                ]
            ]
        ]
    ]

justString item = 
    case item of
        Just ok ->
            ok
    
        Nothing ->
            ""

justList item = 
     List.map (\x ->
        case x of
            Just ok ->
                ok
        
            Nothing ->
                ""
                    
    ) item
    

justInt item =
    case item of
        Just ok ->
            ok
    
        Nothing ->
            0

exerciseItem idx item addItem mask preview=

        div [ class "media restStyle" ] [
            if mask then
            i [ class "far fa-plus-square" , onClick (addItem (Just (justInt item.id)))]
                [  ]
            else
            i [ class "far fa-plus-square noDropCursor" ]
                [  ]
                ,
            div [ class "media-left" ]
            [ figure [ class "image is-85x85" ]
                [
                if (justString item.title) == "" then
                    i [ class "fas fa-coffee" ]
                    []
                else
                img [ src 
                    item.thembnail
                    , onClick (preview((justInt item.id), (justString 
                    item.title)))
                 ]
                    []
                ]
            ]
            , div [ class "media-content" , onClick (preview((justInt item.id), (justString item.title))) ]
            [ p [ class "textTitleTop"  ]
                [ text ((justString item.title) ++ " - " ++ "3 세트")  ]
            , p [ class "textvideoStyle" ]
               [ 
                   p [] [text ((justString item.difficulty_name) ++ " - "),
                   text ((justString item.exercise_name) ++ " - "),
                   text (justString item.instrument_name)
                   ]
               ]
            , p [class "textvideoStyle"] [
                i [ class "fas fa-stopwatch" ]
                []
                , text item.duration
            ]
            , p [class "textvideoStyle"] [
                text (String.join "  " (justList item.part_detail_name))
            ]
            ]
        ]

exerciseBackItem idx item backItem switchItem newStyle settingShow settingShowIdx rest setModel restModel pmd valueWarn mask preview=
    div [ class "restStyle"] [
        div [ class "media" ] [
            div [] [
                if mask then
                i [
                     class ("fas fa-arrows-alt-v " ++ "style"++ String.fromInt(idx) 
                     )
                     , onClick (switchItem idx)
                ]
                []
                else
                i [
                     class ("fas fa-arrows-alt-v noDropCursor " ++ "style"++ String.fromInt(idx) 
                     )
                ]
                []
            ],
            if mask then
            i [ class "far fa-minus-square" 
            , onClick( backItem idx) 
            ]
                []
            else
            i [ class "far fa-minus-square noDropCursor" 
            ]
                []
            ,
            div [ class "media-left" ]
            [ figure [ class "image is-85x85" ]
                [
                if (justString item.title) == "" then
                    i [ class "fas fa-coffee" ]
                    []
                else
                img [ src 
                    item.thembnail
                    , onClick (preview((justInt item.id), (justString 
                    item.title)))
                 ]
                    []
                ]
            ] ,
            if (justString item.title) == "" then
            div [ class "media-content" ]
            [ 
                p [ class "title is-4"  ]
                    [ 
                        -- text (
                            case item.value of
                                Just x ->
                                    div [ class "media-content" ]
                                    [ p [ class "title is-4" ]
                                        [ text "휴식" ]
                                    , p [ class "subtitle is-6" ]
                                        [ text (String.fromInt(x) ++ " 분") ]
                                    ]
                                    -- "휴식 - " ++ String.fromInt(x)  ++ " 분"
                            
                                Nothing ->
                                   div [ class "media-content" ]
                                    [ p [ class "title is-4" ]
                                        [ text "휴식" ]
                                    , p [ class "subtitle is-6" ]
                                        [ text "1 분" ]
                                    ]
                        -- )
                    ]

            ]
            else
            div [ class "media-content", onClick (preview((justInt item.id), (justString item.title))) ]
            [ 
                
                p [ class "textTitleTop"  ]
                [ 
                    text (
                        case item.value of
                            Just x ->
                                 (justString item.title) ++ " - " ++ String.fromInt(x) ++ " 세트"
                            Nothing ->
                                (justString item.title) ++ " -  3 세트"
                    )
                ]
            , p [ class "textvideoStyle" ]
               [ 
                   p [] [text ((justString item.difficulty_name) ++ " - "),
                   text ((justString item.exercise_name) ++ " - "),
                   text (justString item.instrument_name)
                   ]
               ]
             , p [class "textvideoStyle"] [
                i [ class "fas fa-stopwatch" ]
                []
                , text item.duration
            ]
            , p [class "textvideoStyle"]
               ( List.map (\x ->
                    text ((justString x) ++"  ")
                )( item.part_detail_name))
            ]
            , p [] [
                i [ 
                    class "fas fa-chevron-down"
                , onClick (settingShow idx) 
                ]
                []
            ]
        ] ,
            
            if mask then
                if String.fromInt(idx) == settingShowIdx then
                    if (justString item.title) == "" then
                    div [ class ("overlay  settingStyle " ++ newStyle) ] [
                            setRest rest restModel idx item.value pmd backItem settingShow valueWarn
                            ] 
                    else
                        div [ class ("overlay  settingStyle " ++ newStyle) ] [
                            setSetting rest restModel idx item.value pmd backItem settingShow valueWarn
                            ]
                else
                div [ class ("overlay  settingStyle ")] []
            else
            div [ class ("overlay  settingStyle noDropCursor")] []
            ]

setRest rest restModel idx val pmd backItem show valueWarn=
        div [ class ("is-small content " ) ]
        [
           div [ class "settingItem" ] [
                h4[] [text "휴식시간 설정"],
            div[ class "level"] [
                div [ class "level-item"] [
                    div [ class "button" 
                        , onClick (pmd idx "minus" -1) ] [
                        i [ class "fas fa-minus"
                        ] 
                        []
                     ]
                ],
                div [ class "level-item"] [ 
                    input 
                    [ class "input"
                    , type_ "text"
                    , placeholder "휴식시간을 설정해 주세요."
                    , onInput (rest idx)
                    , value 
                    ( case val of
                            Just n ->
                                String.fromInt (n)
                            Nothing ->
                                "" 
                        )
                    ]
                    []
                ]
                , div [ class "level-item" ] [
                    
                    text " Min "
                ],
                div [ class "level-item"] [ 
                    div [ class "button" 
                        , onClick (pmd idx "plus" 1)
                        ] [ 
                        i [ class "fas fa-plus"
                        ]
                        []
                     ]
                 ]
            ],
            div [class "setWarn"] [text valueWarn]
            , div[ class "level"] [
                div [ class "level-item"] [
                    div [class "button is-primary"
                    , onClick (show idx) 
                    ] [text "닫기"]
                ],
                div [ class "level-item"] [
                    div [class "button is-danger"
                    , onClick (backItem idx)
                    ] [text "삭제"]
                ]
            ]
           ]
        ]

setSetting rest restModel idx val pmd backItem show valueWarn=
        div [ class ("is-small content " ) ]
        [
           div [ class "settingItem" ] [
                h4[] [text "세트 설정"],
            div[ class "level"] [
                div [ class "level-item"] [
                    div [ class "button"
                        , onClick (pmd idx "minus" -1) ] [
                        i [ class "fas fa-minus"] 
                        []
                     ]
                ],
                div [ class "level-item"] [ 
                    input 
                    [ class "input"
                    , type_ "text"
                    , placeholder "운동 세트를 설정해 주세요."
                    , onInput (rest idx)
                    , value 
                       ( case val of
                            Just n ->
                                String.fromInt (n)
                            Nothing ->
                                "" 
                        )
                    ]
                    []
                ],
                div [ class "level-item" ] [
                    
                    text " SET "
                ],
                div [ class "level-item"] [ 
                    div [ class "button" 
                        , onClick (pmd idx "plus" 1)] [ 
                        i [ class "fas fa-plus"]
                        []
                     ]
                 ]
            ],
            div [class "setWarn"] [text valueWarn]
            , div[ class "level"] [
                div [ class "level-item"] [
                    div [class "button is-primary"
                    , onClick (show idx ) 
                    ] [text "닫기"]
                ],
                div [ class "level-item"] [
                    div [class "button is-danger"
                    , onClick (backItem idx)
                    ] [text "삭제"]
                ]
            ]
           ]
        ]
        

restMark item = 
    span [] [text (item ++ ",")]
hypen item = 
    span [] [text (item ++ " - ")]


itemText item check category list= 
    let
        filter = List.member item.code list
    in
    if filter then
        label [ class( "button is-light filterBtn selectBtn " )]
            [ input 
                [  type_ "checkbox"   
                , onClick (check (item.code, category, item.name))
                , value item.code]
                [] 
                , text item.name 
            ]
    else
        label [ class( "button is-light filterBtn " )]
            [ input 
                [  type_ "checkbox"   
                , onClick (check (item.code, category, item.name))
                , value item.code]
                [] 
                , text item.name 
            ]



videoFilter exermodel levelmodel exerItemmodel toolmodel show open check list filterResult filterTitle title =
    if show then
        div [class "widePop_container"][
            div [ class "widePop"] [
            popTitle "필터 설정" ,
            
            div [ class "closeBtn"][
                    i [ class "far fa-times-circle", onClick open ]
                    []
                ],
                div [class "filtertitle"] [text "운동 제목"],
                div [class "filterExerTitle"] [
                input [class "input", placeholder "운동 제목을 입력 해 주세요.", onInput filterTitle, value title] []
                
            ],
                div [ class "filtertitle"] [
                    text "운동부위"
                
            ],
            columnsHtml [
                    div [] (
                        List.map(
                            \x -> itemText x check "part_detail_code" list.part_detail_code
                        )exermodel
                    )
            ],
                div [class "filtertitle"] [
                    text "난이도"
            ],
            columnsHtml [
                 div [] (
                        List.map(
                            \x -> itemText x check "difficulty_code" list.difficulty_code
                        ) levelmodel
                    )
            ],
                div [class "filtertitle"] [
                    text "운동종류"
            ],
             columnsHtml [
                  div [] (
                        List.map(
                            \x -> itemText x check "exercise_code" list.exercise_code
                        )exerItemmodel
                    )
            ],
                div [class "filtertitle"] [
                    text "기구"
            ],
             columnsHtml [
                  div [] (
                        List.map(
                            \x -> itemText x check "instrument_code" list.instrument_code
                        )toolmodel
                    )
                
            ],
            columnsHtml [
                div [ class "layerBtn"] [
                    div [ class "button is-primary", onClick filterResult ]
                    [ text "확인" ]
                    ,  div [ class "button is-warning" , onClick open ]
                    [ text "취소" ]
                ]
            ]
        ]]
        else
        div [] []
