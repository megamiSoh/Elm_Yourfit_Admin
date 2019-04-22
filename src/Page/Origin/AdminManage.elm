module Page.Origin.AdminManage exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page exposing (..)


body item margin=
    div [ class ("table marginTable " ++ margin)][
        div  [class "tableRow tableNotList"] [
                div [ class "tableCell" ] [text "닉네임"],
                div [ class "tableCell" ] [
                    case item.nickname of
                        Just ok ->
                            text ok
                    
                        option2 ->
                            text "ㅡ"
                ],
                div [ class "tableCell" ] [text "ID"],
                div [ class "tableCell" ] [text item.username]
        ],
    div  [class "tableRow tableNotList" ] [
                div [ class "tableCell" ] [text "등록일"],
                div [ class "tableCell" ] [text item.joined_at],
                div [ class "tableCell" ] [text "최종 접속일"],
                div [ class "tableCell" ] [text item.connected_at]
        ]
    ]



authTable code = 
    div[ class "tableRow contentsNoWidth"]
        (List.map (\x -> div [class "tableCell"] [text x.name]) code)

helloText code =        
    label [] [
            input [type_ "checkbox", value code.code][]
            ]

test i =  
    div [] [text (String.fromInt(i.menu_id))]

tableFirstBody item =
    div  [class "tableRow tableNotList"] [
                div [ class "tableCell" ] [text "닉네임"],
                div [ class "tableCell" ] [text item.userName],
                div [ class "tableCell" ] [text "ID"],
                div [ class "tableCell" ] [text item.userId]
        ]
tableSecondBody item =
    div  [class "tableRow tableNotList" ] [
                div [ class "tableCell" ] [text "등록일"],
                div [ class "tableCell" ] [text item.createDate],
                div [ class "tableCell" ] [text "최종 접속일"],
                div [ class "tableCell" ] [text item.connectDate]
        ]
tableHeader =
    div [class "tableRow"] [
        div [class"tableCell"] [text "No"],
        div [class"tableCell"] [text "닉네임"],
        div [class"tableCell"] [text "아이디"],
        div [class"tableCell"] [text "등록일"],
        div [class"tableCell"] [text "관리자"]
    ]
tableBody (idx ,item)  choiceMsg=
    div [class "tableRow"] [
        div [class"tableCell"] [text (String.fromInt(idx + 1))],
        div [class"tableCell"] [
            case item.nickname of
                Just name ->
                    text name
                Nothing ->
                    text "ㅡ"    
        ],
        div [class"tableCell"] [text item.username],
        div [class"tableCell"] [text item.joined_at],
        div [class"tableCell"] [
            div [class "button", onClick (choiceMsg (String.fromInt(item.id)))] [text "선택"]
        ]
    ]

adminRegistPop model popEvent nickmsg idmsg nickModel idModel searchmsg resetmsg userData choiceMsg= 
    if model then 
         div [ class "adminPop" ] [
            div [class"poptitleWrap"] [
                p [ class "popTitle"] [text "관리자 등록"] ,
                i[class"fas fa-times popClose" , onClick popEvent][]
                ],
            columnsHtmlADmin [
                formInputEvent "닉네임" "닉네임 입력." False nickmsg nickModel,
                formInputEvent "아이디" "아이디 입력." False idmsg idModel,
                searchB searchmsg resetmsg
            ],
            if List.length userData > 0 then
                div [class "table marginTop"]([tableHeader] ++ (List.indexedMap (
                    \idx  x ->
                    tableBody (idx,x) choiceMsg  
                )userData ) )
            else
                div [class "emptyMsg"][text "관리자를 선택 해 주세요."]
            , div [class "button", onClick popEvent] [text "취소"]
        ]
    else
        div [] []



-- helloText code =        
--     label [] [
--             input [type_ "checkbox", value "10"][]
--             ]

    -- else 
    -- div [] [text "fuck"]
