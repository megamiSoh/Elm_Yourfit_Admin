module Page exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)
import Login as Login
import Api exposing(..)
import Session exposing (Session)

type alias MenusType = 
    { menu_auth_code : List String
    , menu_id : Int
    , menu_name : String }

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
    | Menus
    | C
    | CD
    | PM
    | PR
    | PD
    | BM
    | BR
    | BD


view : Maybe Cred ->  Page -> {title : String, content: Html msg, menu: Html msg} -> Browser.Document msg
view maybeViewer page { title, content, menu }  =
    case maybeViewer of
        Nothing ->
            { title = title 
            , body =  contents content Login maybeViewer menu :: [viewFooter] 
            }
        Just _ ->
            { title = title 
            , body = viewHeader page maybeViewer:: contents content page maybeViewer menu :: [viewFooter] 
            }

login : Html msg -> Html msg
login content  =
  div [ class "tile is-ancestor" ]
    [ 
        content 
    ]

contents : Html msg -> Page -> Maybe Api.Cred -> Html msg -> Html msg
contents content page maybeViewer menu=
    case maybeViewer of
        Nothing ->
            div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [content]
                        ]
                    ]
        Just viewer ->
            div [ class "tile is-ancestor" ]
                [ div [ class "tile is-4 is-vertical is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [ menu ]
                        ]
                    ]
                , div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ p []
                            [ content ]
                            , div [id "webToast"] []
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
            ]

header : String -> Html msg
header username = 
    div [class  "userprofile" ] [
            a [ Route.href  ( Just Route.UserInfo ), class "userProfileInside cursor"] [text username]
            , a [class "button", Route.href (Just Route.Logout) ] [text "로그아웃"]
        ]

navbarLink : Page -> Maybe Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]



viewMenu : MenusType -> Html msg
viewMenu item  =
    case item.menu_id of
        1 ->
            li [] [ a [ class "nav-link", Route.href (Just Route.UserManage )][text item.menu_name]]
        2 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.AdminManage) ][text item.menu_name]]
        3 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.VideoUnit) ][text item.menu_name]]
        4 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.Video) ][text item.menu_name]]
        5 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.ApiVideo) ][text item.menu_name]]
        6 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.FoodCalorie) ][text item.menu_name]]
        7 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.UserPost) ][text item.menu_name]]
        8 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.Info) ][text item.menu_name]]
        9 ->  
            li [] [ a [ class "nav-link", Route.href(Just Route.C) ][text item.menu_name]]
        10 ->  
            li [] [ a [ class "nav-link", Route.href(Just Route.Faq) ][text item.menu_name]]
        11 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.PM) ][text item.menu_name]]
        12 ->
            li [] [ a [ class "nav-link", Route.href(Just Route.BM) ][text item.menu_name]]
        _ ->
            li [] [ a [ class "nav-link", Route.href(Just Route.UserInfo) ][text item.menu_name]]

viewAside : MenusType -> Html msg
viewAside page =
    aside [ class "menu"] [
        ul [ class "menu-list yf-list"] 
             [viewMenu page]
    ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ span [ class "attribution" ]
                [ text " copyrights © 2019 finalCompany all rights reserved"]
            ]
        ]





isActive : Page -> Maybe Route -> Bool
isActive page route =
    case ( page, route ) of
        (Home, (Just Route.Home)) -> 
            True
        (UserManage, (Just Route.UserManage)) -> 
            True
        (AdminManage, (Just Route.AdminManage)) ->
            True
        (VideoUnit, (Just Route.VideoUnit)) ->
            True
        (Video, (Just Route.Video)) ->
            True
        (ApiVideo, (Just Route.ApiVideo)) ->
            True
        (FoodCalorie, (Just Route.FoodCalorie))->
            True
        (UserPost, (Just Route.UserPost))->
            True
        (Info, (Just Route.Info))->
            True
        (Faq, (Just Route.Faq))->
            True
        (UserInfo, (Just Route.UserInfo))->
            True
        (UserDetail, (Just Route.UserMDetail))->
            True
        (AdminRegist, (Just Route.AdminRegist))->
            True
        (AdminDetail, (Just Route.AdminDetail) ) ->
            True
        (AdminEdit, (Just (Route.AdminEdit "detail" "edit")) )->
            True
        (UnitVideoEdit, (Just Route.UvideoEdit))->
            True
        (UnitVideoDetail, (Just Route.UvideoDetail))->
            True
        (UnitVideoRegist, (Just Route.UvideoRegist))->
            True
        (VideoRegist, (Just Route.VideoRegist))->
            True
        (VideoEdit, (Just Route.VideoEdit))->
            True
        (VideoDetail, (Just Route.VideoDetail))->
            True
        (ApiVideoRegist, (Just Route.ApiVideoRegist))->    
            True
        (ApiVideoDetail, (Just Route.ApiDetail))->
            True
        (ApiVideoEdit, (Just Route.ApiEdit))->
            True
        (InfoRegist, (Just Route.InfoRegist) )->
            True
        (InfoDetail, (Just Route.InfoDetail))->
            True
        (InfoEdit, (Just Route.InfoEdit))->
            True
        (FaqDetail, (Just Route.FaqDetail))->
            True
        (FaqRegist, (Just Route.FaqRegist))->
            True
        (FaqEdit,(Just Route.FaqEdit))->
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