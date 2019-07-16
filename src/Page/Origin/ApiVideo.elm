module Page.Origin.ApiVideo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page as Page
import Json.Decode
import String

-- apiVideoLayout : String  -> Bool -> (String.String -> msg) -> Html msg
apiVideoLayout bigTitle  onlyRead selectEvent selectData selectModel thisone description textareaMsg title titleMsg=
    div [] [
        div [class "youtubeTitle"][text bigTitle],
        div [class "searchWrap"] [
            Page.columnsHtml [
                Page.noEmptyselectForm "카테고리" onlyRead selectData selectEvent selectModel,
                Page.formInputEvent "제목" "제목을 입력 해 주세요." onlyRead titleMsg title 
            ]
            ,Page.columnsHtml [
                Page.textAreaEvent "영상 설명" onlyRead description textareaMsg
            ]
        , label [ class "media youtubeApi_container" ]
        [ 
            
            div[ class "field is-horizontal", style "width" "100%" ] [
           Page.labelWrap "선택 된 영상",
            div [ class "field-body inputWidth apiselectedItemStyle" ]
            [ 
                if thisone.snippet.title == "" then
                div [class "noSelectedItem"][text "선택된 영상이 없습니다."]
                else
                figure [ class "media-left" ]
                [ p [ class "image is-64x64 imageCenter" ]
                    [ img [ src thisone.snippet.thumbnails.default.url ]
                        []
                    
                    ]
                    ]
                , div [ class "media-content" ]
                    [ div [ class "content youtubeApi_content" ]
                        [ p []
                            [ strong []
                            [ text thisone.snippet.title ]
                            , div []
                                [text thisone.snippet.description ] 
                            ]
                        ]
                    
                    ]
                
            ]
            ]

            -- text "선택 된 영상"
            -- , 
        
        
        ]
        ]
        
        -- , Page.columnsHtml [
        --     Page.labelWrap "영상 설정"
        -- ]
        -- , Page.columnsHtml [
        --     label [class "fakeInput"] [
        --     Page.formInfoInput "키워드" "입력 예) 다이어트, 운동, 허벅지 (쉼표로 단어 구분)"  "최대 5개까지 키워드 설정이 가능합니다.",
        --     (goBtn onlyRead popup)
        --     ]
        -- ],
        --     div [class "itemCount"] [
        --         text ("총 "++ String.fromInt(count) ++" 개의 영상 선택")
        --     ],
        --     selectedVideoList
        --     ,
        --     link
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




apiVideoList onlyRead videoList popUp videoResult searchInput search preview is_show title =
    div[class "apiVideoWrap"][
            -- Page.columnsHtml [
            --      p [class "poptitle"][text "외부 영상 리스트"]
            div [class "youtubeTitle"][text title]
                
            -- ],
            , div [class "borderWrap", style "display" (if is_show then "none" else "flex")] [
            -- Page.columnsHtml [
                 p [class "youtubekeywordStyle"][text "키워드"]
                , div [ class "field-body" ]
                    [ div [ class "inputWidth" ]
                        [  
                            input [ class "input" , placeholder "입력 예) 다이어트, 운동, 허벅지 (쉼표로 단어 구분)", maxlength 50 , onInput searchInput, disabled is_show]
                            []
                        ]
                    ]
                , button [ class "button is-primary"  , onClick search, disabled is_show]
                    [i [ class "fas fa-search"]
                        [], text "영상 검색" 
                    ]
            -- ]
            -- , Page.columnsHtmlBtn [
            --     {contents = Page.formInput "키워드" "입력 예) 다이어트, 운동, 허벅지 (쉼표로 단어 구분)" onlyRead , 
            --     class = ""},
            --      {contents = searchBtn onlyRead, class = "is-one-fifth"}
            -- ]
        ]
        , div [class "youtubePlayerWrap" ]
        [ videoList
        , div [ class (if preview then "ypreview" else "ypreviewBefore"), id "playerHere"][
            div [id (if preview then "player" else "")][]
        ]
        ]
        
    ]

videoListLayout idx item selectVideo videopreview previewidx preview endvideo is_show =
    label [ class "media youtubeApi_container" , style "background-color" (if previewidx == String.fromInt idx then "#efefef" else "")]
        [ 
            figure [ class "media-left" ]
            [ p [ class "image is-64x64" ]
                [ img [ src item.snippet.thumbnails.default.url ]
                    []
                    , if previewidx == String.fromInt idx then
                    
                    div [class "button is-small is-light", style "margin" ".5rem", onClick endvideo][text "미리보기 종료" ]
                        else
                    div [class "button is-small is-light", style "margin" ".5rem", onClick (videopreview idx item.id.videoId)][text "미리보기"]
                ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content youtubeApi_content" ]
                [ p []
                    [ strong []
                    [ text item.snippet.title ]
                    , div []
                        [text item.snippet.description ] 
                    ]
                ]
            
            ]
        , button [class "button ", onClick (selectVideo item.id.videoId idx), disabled is_show][text "선택"]
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

