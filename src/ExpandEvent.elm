module ExpandEvent exposing (..)

import Browser
import Route exposing (Route)
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(onClick)
import Page.Page as Page
import Debug exposing (log)

type alias Model =
    Bool
init : Model
init = False

type Msg = Collapse | Expand | Checked Bool

-- null: Maybe Model -> Bool
-- null model =
--     case model of
--         Nothing ->
--             False
    
--         Just ok ->
--             ok

update : Msg -> Model -> Model
update msg model =
    let _  = Debug.log "hello" msg
        
    in
    
        case msg of
            Collapse ->
                False
            Expand ->
                True
            Checked check ->
                if check then
                    True
                else
                    True

view : Model -> Html Msg
view model =
    if model then
        div [onClick Collapse, class "expandHeader"] [
            i [ class "fa fa-sort-up"] []
            , ul [class "hiddenMenu"] 
            [
                li [] [
                    a [href "/userInfo"] [text "내정보"]
                 ] ,
                li [] [text "로그아웃"]
            ]
        ]
    else
        div [onClick Expand,class "expandHeader"] [
            i [ class "fa fa-sort-down"] []
        ]

warningIcon : Html msg
warningIcon =
     i [ class "fas fa-exclamation-circle" ][]

triangleWarn : Html msg
triangleWarn = 
    i [ class "fas fa-exclamation-triangle" ][]


layoutPop : Model -> String -> String -> String -> Bool ->  Html Msg
layoutPop model style title phrase icon  =
    if model then
          div [] [
            button [ class ("button " ++ style) , onClick Collapse]
            [ text title ]
               , div [ class "layerWrap" , onClick Collapse] []
               , div [ class "layerPop"  ] 
               [
                   div [ class "closeBtn"][
                       i [ class "far fa-times-circle" , onClick Collapse]
                        []
                   ],
                   div [ class "iconStyle"] [
                       if icon then
                        triangleWarn
                       else
                        warningIcon
                   ]
                    ,
                   text phrase
                   , div [ class "buttons"] [
                        div [ class "button is-primary" , onClick Collapse]
                        [ text "확인" ]
                    ,  div [ class "button is-warning"  , onClick Collapse]
                        [ text "취소" ]
                   ]
               ] 
          ]

    else
        div [ class ("button " ++ style) , onClick Expand]
            [ text title ]

passwordPop : Model -> String -> String -> String -> Bool -> String-> String-> String-> String-> Bool -> Html Msg
passwordPop model style title phrase icon input inputh inputr inputrh read =
    if model then
          div [] [
            button [ class ("button " ++ style) , onClick Collapse , disabled read]
            [ text title ]
               , div [ class "layerWrap" , onClick Collapse] []
               , div [ class "layerPop"  ] 
               [
                   div [ class "closeBtn"][
                       i [ class "far fa-times-circle" , onClick Collapse]
                        []
                   ],
                   div [ class "iconStyle"] [
                       if icon then
                        triangleWarn
                       else
                        warningIcon
                   ]
                    ,
                   text phrase
                   , div [class "passwordConfirm"] [
                       Page.normalInput input inputh "normal" False
                       ,  Page.normalInput inputr inputrh "normal" False
                    ]
                   , div [ class "layerBtn"] [
                        div [ class "button is-primary" , onClick Collapse]
                        [ text "확인" ]
                    ,  div [ class "button is-warning"  , onClick Collapse]
                        [ text "취소" ]
                   ]
               ] 
          ]

    else
        button [ class ("button " ++ style) , onClick Expand, disabled read]
            [ text title ]



adminSearchPop : Model -> Bool -> Html Msg
adminSearchPop model read =
    if model then
        div [] [
            button [ class "button is-small" , href "#", onClick Collapse ,disabled read  ] [],
            div [ class "layerWrap" , onClick Collapse] [],
            div [ class "layerPop searchPop" ] [
                div [ class "closeBtn"][
                       i [ class "far fa-times-circle" , onClick Collapse]
                        []
                   ],
                    Page.popTitle "관리자 등록" ,
                   div [class "notification"] [
                        Page.normalInput "닉네임" "닉네임을 입력 해 주세요." "inline" False,
                        Page.normalInput "아이디" "아이디를 입력 해 주세요." "inline" False ,
                        Page.searchBtn
                        ],
                Page.userData (Route.AdminDetail ),
                div [ class "backArea", onClick Collapse]
                [Page.normalBtn "취소" "is-primary" ]
            ] 
        ]
    else
        button [ class "button is-small" , href "#", onClick Expand, disabled read   ] [
            text "관리자 등록"
        ]
-- test : List String -> String


-- videoFilter : Model -> Html Msg
-- videoFilter model exerpart level exer tool=
--     if model then
--         div [] [
--             -- div [ class "layerWrap" , onClick Collapse] [],
--         div [ class "widePop"] [
--             Page.popTitle "필터 설정" ,
--             div [ class "closeBtn"][
--                     i [ class "far fa-times-circle" , onClick Collapse]
--                     []
--                 ],
--             Page.columnsHtml[
--                 Page.labelWrap "운동부위"
--             ],
--             Page.columnsHtml [
--             --    div [] (List.map filterBtn exerpart )
--             ],
            
--             Page.columnsHtml[
--                 Page.labelWrap "난이도"
--             ],
--             Page.columnsHtml [
--                 -- div [] (List.map filterBtn level)
--             ],
--             Page.columnsHtml[
--                 Page.labelWrap "운동종류"
--             ],
--              Page.columnsHtml [
--                 -- div [] (List.map filterBtn exer)
--             ],
--             Page.columnsHtml[
--                 Page.labelWrap "기구"
--             ],
--              Page.columnsHtml [
--                 -- div [] (List.map filterBtn tool)
--             ],
--             Page.columnsHtml [
--                 div [ class "layerBtn"] [
--                     div [ class "button is-primary" , onClick Collapse]
--                     [ text "확인" ]
--                     ,  div [ class "button is-warning"  , onClick Collapse]
--                     [ text "취소" ]
--                 ]
--             ]
--         ]
--         ]
--     else
--         button [ class "button is-small is-primary" , href "#", onClick Expand  ] [
--             text "필터 설정"
--         ]

