module Page exposing (..)

-- import VideoCall exposing(VidioCallPortMsg)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)
import Login as Login
import Api exposing(..)
import Session exposing (Session)
type Page 
    = Home
    | Other
    | UserManage
    | AdminManage
    | VideoUnit
    | Video
    | ApiVideo
    | FoodCalorie
    | UserPost
    | Info
    | Faq
    | UserInfo
    | UserDetail
    | AdminRegist
    | AdminDetail
    | AdminEdit
    | UnitVideoEdit
    | UnitVideoDetail
    | UnitVideoRegist
    | VideoDetail
    | VideoEdit
    | VideoRegist
    | ApiVideoRegist
    | ApiVideoDetail
    | ApiVideoEdit
    | InfoDetail
    | InfoEdit
    | InfoRegist
    | FaqDetail
    | FaqEdit
    | FaqRegist
    | VideoCall
    | Login

-- type Maybe login =
--     Just login |
--     Nothing

-- view: Page -> {title : String, content: Html msg} -> Browser.Document msg
-- view page {title, content} =
--     { title = title 
--     , body = viewHeader page:: contents content page :: [viewFooter]
--     }

view: Maybe Cred ->  Page -> {title : String, content: Html msg} -> Browser.Document msg
view maybeViewer page { title, content } =
    case maybeViewer of
        Nothing ->
            { title = title 
            , body =  contents content Login maybeViewer :: [viewFooter]
            }
        Just _ ->
            { title = title 
            , body = viewHeader page maybeViewer:: contents content page maybeViewer :: [viewFooter]
            }

login content  =
  div [ class "tile is-ancestor" ]
    [ 
        content 
    ]
-- contents : Model ->Html Msg
contents content page maybeViewer =
    case maybeViewer of
        Nothing ->
            div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [ 
                                content
                            ]
                            
                        ]
                    ]
        Just viewer ->
            div [ class "tile is-ancestor" ]
                [ div [ class "tile is-4 is-vertical is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [ viewAside page ]
                        ]
                    ]
                , div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [ content ]
                            
                        ]
                    ]
                ]





viewHeader : Page -> Maybe Cred -> Html msg
viewHeader page maybeViewer =
    case maybeViewer of
        Nothing ->
            span [] []
    
        Just viewer ->
          nav [ class "navbar" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", Route.href (Just Route.Home) ]
                    [ 
                        div [class "yf-title"]
                        [ text "YourFit" ]
                    ]
                , a [ class "navbar-burger burger"]
                    [ span []
                        []
                    , span []
                        []
                    , span []
                        []
                    ]
                ]
            , div [ id "navbarBasicExample", class "navbar-menu" ]
                [ div [ class "navbar-end" ]
                    [ i [ class "fa fa-user-circle" ]
                                [], div [ class "navbar-item" ]
                        [ div []
                            [ 
                                a [ Route.href  ( Just Route.UserInfo)] [text "FinalCompany"]
                            ]
                        ]
                    ]
                ]
            ]
navbarLink : Page -> Maybe Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


viewAside : Page -> Html msg
viewAside page =
    aside [ class "menu"] [
        ul [ class "menu-list yf-list"] <|
            navbarLink page (Just Route.UserManage) [ text "사용자 관리" ]
            :: viewMenu page
    ]
viewMenu : Page -> List (Html msg)
viewMenu page  =
    let
        linkTo =
            navbarLink page
    in
    [ linkTo (Just Route.AdminManage) [ div [] [ text "관리자 관리" ] ]
    , linkTo (Just Route.VideoUnit) [ div [] [ text "유어핏 단위 영상" ] ]
    , linkTo (Just Route.Video) [ div [] [ text "유어핏 영상" ] ]
    , linkTo (Just Route.ApiVideo) [ div [] [ text "외부 API영상" ] ]
    , linkTo (Just Route.FoodCalorie) [ div [] [ text "음식칼로리 관리" ] ]
    , linkTo (Just Route.UserPost) [ div [] [ text "사용자 게시물" ] ]
    , linkTo (Just Route.Info) [ div [] [ text "공지사항" ] ]
    , linkTo (Just Route.Faq) [ div [] [ text "1:1 문의" ] ]
  
    ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ span [ class "attribution" ]
                [ text " copyrights © 2019 finalCompany all rights reserved"]
            ]
        ]





isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        (Home, Route.Home) -> 
            True
        (UserManage, Route.UserManage) -> 
            True
        (AdminManage, Route.AdminManage) ->
            True
        (VideoUnit, Route.VideoUnit) ->
            True
        (Video, Route.Video) ->
            True
        (ApiVideo, Route.ApiVideo) ->
            True
        (FoodCalorie, Route.FoodCalorie)->
            True
        (UserPost, Route.UserPost)->
            True
        (Info, Route.Info)->
            True
        (Faq, Route.Faq)->
            True
        (UserInfo, Route.UserInfo)->
            True
        (UserDetail, Route.UserMDetail)->
            True
        (AdminRegist, Route.AdminRegist)->
            True
        (AdminDetail, Route.AdminDetail ) ->
            True
        (AdminEdit, (Route.AdminEdit "detail" "edit"))->
            True
        (UnitVideoEdit, Route.UvideoEdit)->
            True
        (UnitVideoDetail, Route.UvideoDetail)->
            True
        (UnitVideoRegist, Route.UvideoRegist)->
            True
        (VideoRegist, Route.VideoRegist)->
            True
        (VideoEdit, Route.VideoEdit)->
            True
        (VideoDetail, Route.VideoDetail)->
            True
        (ApiVideoRegist, Route.ApiVideoRegist)->    
            True
        (ApiVideoDetail, Route.ApiDetail)->
            True
        (ApiVideoEdit, Route.ApiEdit)->
            True
        (InfoRegist, Route.InfoRegist )->
            True
        (InfoDetail, Route.InfoDetail)->
            True
        (InfoEdit, Route.InfoEdit)->
            True
        (FaqDetail, Route.FaqDetail)->
            True
        (FaqRegist, Route.FaqRegist)->
            True
        (FaqEdit,Route.FaqEdit)->
            True

        _ ->
            False



viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]