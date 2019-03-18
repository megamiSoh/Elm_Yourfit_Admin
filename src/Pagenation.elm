module Pagenation exposing (..)

import Browser

import Html exposing (..)
import Html exposing(Html, text, select, option, label, a, table, h1)
import Html.Attributes exposing( class, placeholder, disabled, title)
import Html.Events exposing(..)
pagenation: Html msg
pagenation = 
    nav [ class "pagination" ]
        [ a [ class "pagination-previous", title "This is the first page", disabled True ]
            [ text "Previous" ]
        
        , ul [ class "pagination-list" ]
            [ li []
                [ a [ class "pagination-link is-current"]
                    [ text "1" ]
                ]
            , li []
                [ a [ class "pagination-link" ]
                    [ text "2" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "3" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "4" ]
                ]
            , li []
                [ a [ class "pagination-link" ]
                    [ text "5" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "6" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "7" ]
                ]
            , li []
                [ a [ class "pagination-link" ]
                    [ text "8" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "9" ]
                ]
            , li []
                [ a [ class "pagination-link"]
                    [ text "10" ]
                ]
            ]
        , a [ class "pagination-next" ]
            [ text "Next page" ]
        ]

    
-- pagination: Int -> Html msg
pagination btn page= 
    let
        index = (page.total_count // page.per_page) + 1
    in
    
    nav [ class "pagination" ]
        [ div [ class "pagination-previous", title "This is the first page",  onClick (
            if (page.page - 1) == 0 then
                btn (1 , "prev")
            else
                btn ((page.page - 1), "prev")
        ) ]
            [ text "Previous" ]
        
        , ul [ class "pagination-list" ]
            (
                List.indexedMap (\idx x ->
                    
                    item idx x page.page btn
                ) (List.range 1 index)
            )
        , div [ class "pagination-next" , onClick (
            if (page.page + 1) > index then
                btn (index, "next")
            else
                btn ((page.page + 1), "next")
        )]
            [ text "Next page" ]
        ]


item  idx num current btn=
    li [class (
        if (idx + 1) == current then
        "pagination-link is-current"
        else
        "pagination-link"
        ), onClick (btn ((idx + 1), "go"))] [
        div [] [ text (String.fromInt (idx + 1)) ]
    ]



