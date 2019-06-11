module Page.Origin.ApiVideo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page as Page
import Json.Decode
import String

-- apiVideoLayout : String  -> Bool -> (String.String -> msg) -> Html msg
apiVideoLayout bigTitle  onlyRead link popup selectedVideoList count =
    div [] [
        Page.columnsHtml [
            Page.pageTitle bigTitle 
        ],
        Page.columnsHtml [
            Page.labelWrap "기본 설정"
        ],
        div [class "searchWrap"] [
            Page.columnsHtml [
                Page.formSelect "카테고리" onlyRead,
                Page.formInput "제목" "제목을 입력 해 주세요." onlyRead
            ]
        ],
        Page.columnsHtml [
            Page.labelWrap "영상 설정"
        ],
        Page.columnsHtml [
            label [class "fakeInput"] [
            Page.formInfoInput "키워드" "입력 예) 다이어트, 운동, 허벅지 (쉼표로 단어 구분)"  "최대 5개까지 키워드 설정이 가능합니다.",
            (goBtn onlyRead popup)
            ]
        ],
            div [class "itemCount"] [
                text ("총 "++ String.fromInt(count) ++" 개의 영상 선택")
            ],
            selectedVideoList
            ,
            link
    ]

noSelected = 
    div [class "selectedVideoArea"] [
        p[] [text "선택 된 외부영상이 없습니다."],
        p[] [text "키워드를 입력 해 주세요."]
        
    ]

searchBtn onlyRead =
    button [ class "button is-outline" , disabled onlyRead] [
               span [class "btnText"] [ text "검색"],
               span [ class "icon" ]
                [ i [ class "fas fa-search" ]
                    []
                ]
           ]
goBtn onlyRead popup= 
    button [ class "button is-outline" , disabled onlyRead, onClick popup] [
               span [class "btnText"] [ text "검색"],
               span [ class "icon" ]
                [ i [ class "fas fa-search" ]
                    []
                ]
           ]




apiVideoList onlyRead videoList popUp videoResult =
    div[class "apiVideoWrap"][
            Page.columnsHtml [
                p [class "poptitle"][text "외부 영상 리스트"]
            ],
            div [class "borderWrap"] [
            Page.columnsHtmlBtn [
                {contents = Page.formInput "키워드" "입력 예) 다이어트, 운동, 허벅지 (쉼표로 단어 구분)" onlyRead , 
                class = ""},
                 {contents = searchBtn onlyRead, class = "is-one-fifth"}
            ]
        ],
        videoList,
        div [ class "buttons"] [
            div [ class "button is-primary", onClick videoResult ]
                [ text "확인" ] ,
            div [ class "button is-warning", onClick popUp  ]
                [ text "취소" ]
        ]
    ]

videoListLayout idx item selectVideo =
    label [ class "media" ]
        [ 
            p[class "apiVideoCheckStyle"] [
                input [type_ "checkbox", onClick(selectVideo idx)] []
            ],
            figure [ class "media-left" ]
            [ p [ class "image is-64x64" ]
                [ img [ src item.thumb ]
                    []
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ p []
                    [ strong []
                    [ text item.title ]
                    , div []
                        [text item.article ] 
                    ]
                ]
            
            ]
        ]

videoResultLayout idx item delete=
    label [ class "media" ]
        [ 
            figure [ class "media-left" ]
            [ p [ class "image is-64x64" ]
                [ img [ src item.thumb ]
                    []
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content" ]
                [ p []
                    [ strong []
                    [ text item.title ]
                    , div []
                        [text item.article ] 
                    ]
                ]
            
            ],
        span [ class "btnWrap" ] [apiBtnSet "미리보기" "" ],
        span [ class "btnWrap" ] [deleteBtn  "삭제" "is-danger" (delete idx)]
        ]

apiBtnSet title style = 
    button [ class ("button is-outline " ++ style)] [
               span [class "btnText"] [ text title]
               
           ]

deleteBtn title style delete= 
    button [ class ("button is-outline " ++ style), onClick delete ] [
               span [class "btnText"] [ text title]
               
           ]

