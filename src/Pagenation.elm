module Pagenation exposing (..)

import Browser
import Html exposing (..)
import Html exposing(Html, text, select, option, label, a, table, h1)
import Html.Attributes exposing( class, placeholder, disabled, title, href)
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


pagination btn page initNum= 
    let
        index = 
            if (page.total_count // page.per_page) == 0 then
                1
            else
                if (page.total_count // page.per_page) * page.per_page < page.total_count then
                    (page.total_count // page.per_page) + 1
                else
                    (page.total_count // page.per_page)
    in
    
    nav [ class "pagination" ]
        [ div [ class "pagination-previous", title "This is the first page",  onClick (
             if (initNum * 10) > 10 then
                btn (
                    (initNum - 1) * 10
                    , "prev")
            else
                btn ( 0 , "" )
        ) ]
            [ text "10페이지 뒤로" ]
        
        , ul [ class "pagination-list" ]
            (
                List.indexedMap (\idx x ->
                    
                    item idx x page.page btn
                ) (List.range (
                    if initNum  == 1 then
                        1
                    else
                        ((initNum - 1) * 10 ) + 1
                ) (
                    if index > (10 * initNum) then
                        10 * initNum
                    else if index < 10 then 
                        index
                    else
                        ((initNum - 1 ) * 10 ) + ( index - ((initNum - 1) * 10 ))
                )
            )
            )
        , div [ class "pagination-next" , onClick (
            if (initNum * 10) < index then
                btn ((initNum * 10 + 1), "next")
            else
                btn ( 0 , "" )
        )]
            [ text "10페이지 앞으로" ]
        ]


item  idx num current btn=
    div [class (
        if (num) == current then
        "pagination-link is-current"
        else
        "pagination-link"
        ),  onClick (btn ((num), "go"))] [
             text (String.fromInt (num)) 
        ]

