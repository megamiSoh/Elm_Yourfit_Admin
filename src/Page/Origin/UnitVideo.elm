module Page.Origin.UnitVideo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page as Page
import Json.Decode
import String


unitVideoForm title disabled msg detailmodel model selectLevel exerCode tool urlmsg partMsg checkModel areaMsg=
    div [] [
        Page.columnsHtml [
            Page.pageTitle title
        ],
        div [ class "searchWrap"] [
            
        Page.columnsHtml [
            Page.formInputEvent "제목" "제목을 입력 해 주세요." disabled msg model.editData.title,
            Page.noEmptyselectForm  "난이도" disabled model.levels selectLevel 
            
            detailmodel.difficulty_code
        ],
        Page.columnsHtml [
            Page.noEmptyselectForm  "운동종류" disabled model.exerCode exerCode 
            
            detailmodel.exercise_code,
            Page.noEmptyselectForm  "기구종류" disabled model.instrument tool 
            
            detailmodel.instrument_code
        ],
         Page.columnsHtml [
              Page.formInputEvent "URL" "URL 아이디를 입력 해 주세요." disabled urlmsg model.editData.video,
              (Page.normalBtn "미리보기" "is-normal")
            
        ],
        Page.columnsHtml [
            Page.textAreaEvent "운동 설명" disabled model.editData.description areaMsg
            ,checkBoxCustom model.part disabled detailmodel.part_detail_code partMsg checkModel
        ]
        ]
    ]
    
checkBoxCustom list disabled checkV partMsg checkModel=
        div [ class "field is-horizontal" ]
            [ 
            Page.labelWrap "운동 부위",
            div [ class "field-body customCheck" ]
                [ div [ class "field  is-fullwidth" ]
                    [ p [ class "control customCheck" ]
                        (
                            List.map (
                                \title ->
                                checkBoxReadOnly checkModel title partMsg disabled(
                                    let _ = Debug.log "match " match
                                        match = 
                                            List.head(List.filter(\f ->
                                            f.code == title.code
                                            )checkV  )
                                    in
                                    case match of
                                        Just m ->
                                            m
                                        Nothing ->
                                            { code= ""
                                            , name= ""}
                                    )
                                ) list
                            )
                    ]
                ]
            ]
            
checkBoxReadOnly checkModel title partMsg readOnly val = 
    label [ class "checkbox" ]
        [ input 
        [ type_ "checkbox"
        , disabled readOnly
        , value checkModel
        , checked (val.code == title.code)
        , onClick (partMsg (title.code , title.name))
        ]
            [], text title.name 
        ]

-- start
unitRegist title disabled msg model selectLevel exerCode tool urlmsg partMsg  areaMsg=
    div [] [
        Page.columnsHtml [
            Page.pageTitle title
        ],
        div [ class "searchWrap"] [
            
        Page.columnsHtml [
            Page.formInputEvent "제목" "제목을 입력 해 주세요." disabled msg model.editData.title,
            Page.noEmptyselectForm  "난이도" disabled model.levels selectLevel 
           
           model.editData.difficulty
        ],
        Page.columnsHtml [
            Page.noEmptyselectForm  "운동종류" disabled model.exerCode exerCode 
           
            model.editData.exercise
            ,
            Page.noEmptyselectForm  "기구종류" disabled model.instrument tool 
           
            model.editData.instrument
        ],
         Page.columnsHtml [
              Page.formInputEvent "URL" "URL 아이디를 입력 해 주세요." disabled urlmsg model.editData.video,
              (Page.normalBtn "미리보기" "is-normal")
            
        ],
        Page.columnsHtml [
            Page.textAreaEvent "운동 설명" disabled model.editData.description areaMsg
            ,registcheckBoxCustom model.part disabled model.editData.part_details partMsg 
        ]
        ]
    ]
    
registcheckBoxCustom list disabled checkV partMsg =
        div [ class "field is-horizontal" ]
            [ 
            Page.labelWrap "운동 부위",
            div [ class "field-body customCheck" ]
                [ div [ class "field  is-fullwidth" ]
                    [ 
                        p [ class "control customCheck" ]
                        (
                            List.map (
                                \title ->
                                registcheckBoxReadOnly  title partMsg disabled
                                ) list
                            )
                    ]
                ]
            ]
            
registcheckBoxReadOnly  title partMsg readOnly  = 
    label [ class "checkbox" ]
        [ input 
        [ type_ "checkbox"
        , disabled readOnly
        , onClick (partMsg (title.code , title.name))
        ]
            [], text title.name 
        ]



-- -- helperFunction
onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Json.Decode.map tagger targetValue)

targetFiles : Json.Decode.Decoder (List String)
targetFiles = 
    Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.string)

inputBtnx btn model getFile thumb title=
    div [ class "field is-horizontal" ] [
        Page.labelWrap title,
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