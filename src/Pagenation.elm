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
        [ button [ class "pagination-previous button", title "This is the first page", disabled (if page.page <= 10 then True else False ),  onClick (
            if page.page <= 10 then
                btn (page.page , "prev")
            else
                if (toFloat ((page.page // 10) * 10 ) / toFloat page.page) ==  1  then
                    btn (((page.page // 10) * 10) - 10, "prev")
                else
                    btn (((page.page // 10) * 10) , "prev")
        ) ]
            [ text "Previous" ]
        
        ,
         if page.page <= 10 then
            if index <= 10 then
                ul [ class "pagination-list" ]
                (
                    List.indexedMap (\idx x ->
                        
                        item idx x page.page btn 0 index
                    ) (List.range 1 index)
                )
            else
                ul [ class "pagination-list" ]
                (
                    List.indexedMap (\idx x ->
                        
                        item idx x page.page btn 0 index
                    ) (List.range 1 10)
                )
        else 
                if (toFloat ((page.page // 10) * 10 ) / toFloat page.page) ==  1  then
                ul [ class "pagination-list" ]
                    (
                        List.indexedMap (\idx x ->
                            item idx x page.page btn (((page.page // 10 ) * 10) - 10) index
                        ) (List.range (((page.page // 10 ) * 10)) ((page.page // 10 ) * 10 + 9))
                    )
                else
                    if index < (page.page // 10 ) * 10 + 9 then
                        ul [ class "pagination-list" ]
                            (
                                List.indexedMap (\idx x ->
                                    item idx x page.page btn ((page.page // 10 ) * 10) index
                                ) (List.range (((page.page // 10 ) * 10) + 1) index )
                            )
                    else
                    ul [ class "pagination-list" ]
                        (
                            List.indexedMap (\idx x ->
                                item idx x page.page btn ((page.page // 10 ) * 10) index
                            ) (List.range (((page.page // 10 ) * 10)) ((page.page // 10 ) * 10 + 9))
                        )
        , button [ class "pagination-next  button" , disabled (if index <= 10 then True else False), onClick (
            if (page.page + 1) > index then
                btn (index, "next")
            else
                if (toFloat ((page.page // 10) * 10 ) / toFloat page.page) ==  1  then
                    btn ((((page.page // 10 ) * 10 + 10) - 9), "next")
                else
                    btn ((((page.page // 10 ) * 10 + 10) + 1), "next")
        )]  
            [ text "Next" ]
        ]


item  idx num current btn y index=
    -- if index < (current // 10 ) * 10 + 9 then
    -- li [class (
    --     if (y + idx + 1) == current then
    --     "pagination-link is-current"
    --     else
    --     "pagination-link"
    --     ), onClick (btn ((y + idx + 1 ), "go"))] [
    --     div [] [ text (String.fromInt (y + idx + 1 )) ]
    -- ]
    -- else 
    li [class (
        if (y + idx + 1) == current then
        "pagination-link is-current"
        else
        "pagination-link"
        ), onClick (btn ((y + idx + 1), "go"))] [
        div [] [ text (String.fromInt (y + idx + 1)) ]
    ]



