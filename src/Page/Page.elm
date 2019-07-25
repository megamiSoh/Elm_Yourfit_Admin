module Page.Page exposing (..)

import Date exposing (..)
import DatePicker exposing (Msg(..))
import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
-- import Page.Page exposing(..)
-- import ExpandEvent as ExEvent
import Json.Decode
import String
import Route exposing (Route)


pageTitle : String -> Html msg
pageTitle title =
    h1[ class "pageTitle" ]
    [
        text title
    ]
popTitle : String -> Html msg
popTitle title =
    h3[ class "poptitle" ]
    [
        text title
    ]


searchSelect : String -> Html msg
searchSelect title =
        div [ class "column" ]
            [ 
                label [] [ text title ],
                div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "전체" ]
                        , option []
                            [ text "사용자 지정" ]
                        ]
                    ]
            ]                    




                    
normalBtn : String -> String -> Html msg        
normalBtn title style =
    div [ class ("button " ++ style) ]
        [ text title ]

-- goNormalBtn : String -> String-> String -> Html msg       
goNormalBtn title style link =
    a [ class ("button " ++ style), Route.href link ]
        [ text title ]


registBtn : String -> Html msg
registBtn title =
        div [ class "registWrap"] 
        [
            a [ class "button is-primary" ]
            [ 
                i [ class "far fa-registered" ]
                []
                , text title 
                ]
        ]

registSetBtn : String -> Html msg
registSetBtn link =
        div [ class "buttons" ] [
            a [ class "button is-primary", href link ] [text "등록"],
            a [ class "button is-warning", href link ] [text "취소"]
        ]

detailSetBtn : String -> String-> Html msg
detailSetBtn link back =
        div [ class "buttons" ] [
            a [ class "button is-primary", href link ] [text "수정"],
            a [ class "button is-warning", href back ] [text "취소"]
        ]

routeRegist : Route -> Html msg
routeRegist link =
        div [ class "buttons" ] [
            a [ class "button is-primary", Route.href (Just link) ] [text "등록"],
            a [ class "button is-warning", Route.href (Just link) ] [text "취소"]
        ]

routeDetail : Route -> Route-> Html msg
routeDetail link back =
        div [ class "buttons" ] [
            a [ class "button is-primary", Route.href (Just link) ] [text "수정"],
            a [ class "button is-warning", Route.href (Just back) ] [text "취소"]
        ]

routeEdit : Route -> Route-> Html msg
routeEdit link back =
        div [ class "buttons" ] [
            a [ class "button is-primary", Route.href (Just link) ] [text "저장"],
            a [ class "button is-warning", Route.href (Just back) ] [text "취소"]
        ]

editSetBtn : String -> String-> Html msg
editSetBtn link back =
        div [ class "buttons" ] [
            a [ class "button is-primary", href link ] [text "저장"],
            a [ class "button is-warning", href back ] [text "취소"]
        ]

-- editEventBtn : String -> String-> Html msg
editEventBtn =
        div [ class "buttons" ] [
            div [ class "button is-primary" ] [text "저장"],
            div [ class "button is-warning" ] [text "취소"]
        ]

-- detailEventBtn : Route -> Route-> Html msg
detailEventBtn  title msg route=
        div [ class "buttons" ] [
            div [ class "button is-primary", onClick msg] [text title],
            a  [ class "button is-warning", Route.href (Just route) ] [text "취소"]
        ]

disabledBtn  title msg route=
        div [ class "buttons" ] [
            div [ class "button is-primary", onClick msg] [text title],
            a  [ class "button is-warning", Route.href (Just route) ] [text "취소"]
        ]

backPageBtn route =
        div [ class "buttons" ] [
            a [ class "button is-warning", Route.href (Just route) ] [text "취소"]
        ]

goRegist : String -> String -> Html msg
goRegist title link =
        div [ class "registWrap"] 
        [
            a [ class "button is-primary", href link ]
            [ 
                i [ class "far fa-registered" ]
                []
                , text title 
                ]
        ]

registRoute : String -> Route -> Html msg
registRoute title link =
        div [ class "registWrap"] 
        [
            a [ class "button is-primary", Route.href (Just link) ]
            [ 
                i [ class "far fa-registered" ]
                []
                , text title 
                ]
        ]

registClick title msg =
    div [ class "registWrap"] 
            [
                div [ class "button is-primary",onClick msg]
                [ 
                    i [ class "far fa-registered" ]
                    []
                    , text title 
                    ]
        ]

searchLabel : String -> Html msg
searchLabel title =  
        label []
        [text title]

searchInput : String -> String -> Html msg
searchInput title ph  =
        div [ class "column" ]
        [ 
            label []
            [ text title ],
            input [ class "input", placeholder ph, maxlength 50 ]
            []
        ]
        
normalInput : String -> String -> String-> Bool -> Html msg
normalInput title ph style read=
            div [class style] [
                label []
                [ text title ],
                input [ class "input", placeholder ph, disabled read, maxlength 50]
                []
            ]

userDataCount : Html msg
userDataCount = 
    div [ class "searchCount" ] [text "검색 결과 수 2"]
 

dataCount : String -> Html msg
dataCount count= 
    div [ class "searchCount" ] [text ("검색 결과 수 " ++ count)]

userData : Route -> Html msg
userData link=
    table [ class "table" ]
        [ thead []
            [ tr [ ]
                [ th []
                    [ abbr [ title "indexNumber" ]
                        [ text "No" ]
                    ]
                , th []
                    [ abbr [ title "nickName" ]
                        [ text "닉네임" ]
                    ]
                , th []
                    [ abbr [ title "UserId" ]
                        [ text "아이디" ]
                    ]
                , th []
                    [ abbr [ title "CreateDate" ]
                        [ text "등록일" ]
                    ]
                ]
            ]
        , tbody []
            [ tr []
                [ th [ Route.href (Just link) ]
                    [text "1"]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "2" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "3" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "4" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "5" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "6" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "7" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "8" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "9" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            , tr []
                [ th []
                    [ text "10" ]
                , td []
                    [ text "megamiSoh" ]
                , td []
                    [ text "Final" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            ]
        ]



userInfoData : Html msg
userInfoData =
    table [ class "table" ]
        [ 
        tbody []
            [ tr []
                [ th []
                    [ text "닉네임" ]
                , td []
                    [ text "megamiSoh" ]
                , th []
                    [ text "ID" ]
                , td []
                    [ text "FinalCompany" ]
                ]
                ,tr []
                [ th []
                    [ text "등록일" ]
                , td []
                    [ text "2018-12-12" ]
                , th []
                    [ text "최종 접속일" ]
                , td []
                    [ text "2019-01-01" ]
                ]
            ]
        ]

userInfo : Maybe String -> String -> String -> String -> Html msg
userInfo nickname username createDate updateDate=
    table [ class "table" ]
        [ 
        tbody []
            [ tr []
                [ th []
                    [ text "닉네임" ]
                , td []
                    [ 
                        case nickname of
                            Just str ->
                                text str
                        
                            Nothing ->
                                text "ㅡ"
                     ]
                , th []
                    [ text "ID" ]
                , td []
                    [ text username ]
                ]
                ,tr []
                [ th []
                    [ text "등록일" ]
                , td []
                    [ text (String.dropRight 10 createDate) ]
                , th []
                    [ text "최종 접속일" ]
                , td []
                    [ text (String.dropRight 10 updateDate) ]
                ]
            ]
        ]

columns:  List String -> Html msg
columns title = 
        div [ class "columns" ] 
        (List.map column title)

column: String -> Html msg
column menuTitle = 
         div [ class "column" ]
            [ text menuTitle ]
        

columnsHtmlBtn item = 
        div [ class "columns" ] 
        (List.map (
            \x -> (columnHtmlBtn x )
        ) item )
-- columnHtml: String ->String -> Html msg
columnHtmlBtn item = 
         div [class ("column " ++ item.class)] [
             item.contents
         ]



-- columnsHtml: (a -> b) -> List megami->  Html msg
columnsHtml title  = 
        div [ class "columns" ] 
        (List.map columnHtml title)
-- columnHtml: String ->String -> Html msg
columnHtml title= 
         div [class  "column"] [
             title
         ]
columnsHtmlADmin title  = 
        div [ class "columns" ] 
        (List.map columnHtmlAdmin title)
-- columnHtml: String ->String -> Html msg
columnHtmlAdmin title= 
         div [class  "column adminColumn"] [
             title
         ]


commonInput : String -> String -> Bool -> Html msg
commonInput title ph read =
    input [
        class "input" , placeholder ph , disabled read, maxlength 50
    ] []

commonLabel : String -> Html msg
commonLabel title =
    label [] [text title]



formInput : String -> String -> Bool -> Html msg
formInput title ph read=
        div [ class "field is-horizontal" ] [
            labelWrap title,
            inputWrap ph read
            ]

-- formInputEvent : String -> String -> Bool -> Html msg
formInputEvent title ph read msg model=
        div [ class "field is-horizontal" ] [
            labelWrap title,
            inputText ph read msg model
            ]

-- inputText :String -> Bool -> Html msg 
inputText ph read msg model=  
    -- div [class "field has-addons"] [
        div [ class "field-body" ]
            [ div [ class "control inputWidth" ]
                [  
                    input [ class "input" , placeholder ph , disabled read, onInput msg, value model, maxlength 50 ]
                    []
                ]
            ]


formInfoInput : String -> String  -> String ->  Html msg
formInfoInput title ph info=
        div [ class "field is-horizontal" ] [
            labelWrap title,
            inputInfoWrap ph  info
            ]

inputInfoWrap :String ->  String -> Html msg 
inputInfoWrap ph info=  
        div [ class "field-body" ]
            [ div [ class "control inputWidth" ]
                [  
                    div [ class "input fakeInput"  ]
                    [text ph],
                    p [class "inputInfoStyle"] [text info]

                ]
            ]

formSelect : String-> Bool -> Html msg
formSelect title read = 
    div [ class "field is-horizontal" ] [
            labelWrap title,
            selectWrap read
            ]
labelWrap : String -> Html msg
labelWrap title= 
    div [class "field-label is-normal"] [
        commonLabel title
            ]

selectWrap :  Bool  -> Html msg
selectWrap  read=
        div [ class "field-body" ]
            [ 
                    p [ class "control inputWidth" ]
                    [ 
                       div [ class "select inputWidth"] [
                           select [ class "inputWidth" ,disabled read ]
                        [ option []
                            [ text "전체" ]
                        , option []
                            [ text "사용자 지정" ]
                        ]
                       ]
                    ]
            ] 

-- selectForm : String-> Bool -> Html msg
selectForm title read instrument select empty selectModel= 
    div [ class "field is-horizontal" ] [
            labelWrap title,
            selectEvent read instrument select empty selectModel
            ]

noEmptyselectForm title read instrument select selectModel= 
    div [ class "field is-horizontal" ] [
            labelWrap title,
            noEmptyselectEvent read instrument select selectModel
            ]        
-- selectEvent :  Bool  -> Html msg
noEmptyselectEvent  read instrument selectmsg  selectModel=
        div [ class "field-body" ]
            [ 
                p [ class "control inputWidth" ]
                [ 
                    div [ class "select inputWidth"] [   
                        select [ class "inputWidth" ,disabled read, onInput (selectmsg) ]
                                (List.map ( \x -> 
                                selectOption x selectModel  
                                ) instrument  ) 
                ]
            ] 
            ]
selectEvent  read instrument selectmsg empty selectModel=
        div [ class "field-body" ]
            [ 
                p [ class "control inputWidth" ]
                [ 
                    div [ class "select inputWidth"] [   
                        select [ class "inputWidth" ,disabled read, onInput (selectmsg) ]
                                ( [selectAll empty] ++ List.map ( \x -> 
                                selectOption x selectModel  
                                ) instrument  ) 
                ]
            ] 
            ]
selectAll empty= 
    option [ value empty ]
        [ span [] [text "전체"] ] 
selectOption item selectModel= 
    option [ value item.code, selected (item.code == selectModel)][
        text item.name
    ]

-- normalSelect : String -> Bool  -> Html msg
-- normalSelect title read=
--         div []
--             [ 
--                 label [] [ text title ],
--                 div [ class "select" ]
--                     [ select [ disabled read ]
--                         [ option []
--                             [ text "전체" ]
--                         , option []
--                             [ text "사용자 지정" ]
--                         ]
--                     ]
--             ]  

inputWrap :String -> Bool -> Html msg 
inputWrap ph read =  
    -- div [class "field has-addons"] [
        div [ class "field-body" ]
            [ div [ class "control inputWidth" ]
                [  
                    input [ class "input" , placeholder ph , disabled read , maxlength 50]
                    []
                ]
            ]
        -- ]




    -- ]
inputBtn : String -> Bool -> String ->  Html msg
inputBtn ph read btn=
        div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control is-expanded" ]
                    [ 
                        div [ class "field has-addons" ]
                        [ div [ class "control inputBtn" ]
                            [ input [ class "input", placeholder ph , disabled read , maxlength 50]
                                []
                            ]
                        , div [ class "control" ]
                            [ input [ class "button is-info" , disabled read, type_ "file"]
                                [ text btn ]
                            ]
                        ]
                    ]
                ]
            ]


-- textAreaEvent : String -> Bool -> String -> Html msg
textAreaEvent  title read article msg =
       div[ class "field is-horizontal" ] [
           labelWrap title,
            div [ class "field-body inputWidth" ]
            [  p [ class "control inputWidth" ]
                   [ 
                      textarea [ class "textarea", placeholder "250자까지 입력 가능", disabled read, onInput msg, value article,maxlength 250] []
                    ]
            ]
       ]

textAreaRegist  title read article msg=
       div[ class "field is-horizontal" ] [
           labelWrap title,
            div [ class "field-body inputWidth" ]
            [  p [ class "control inputWidth" ]
                   [ 
                      textarea [ class "textarea", placeholder article, disabled read, onInput msg, maxlength 250] []
                    ]
            ]
       ]

textAreaWrap : String -> Bool -> Html msg
textAreaWrap  title read =
       div[ class "field is-horizontal" ] [
           labelWrap title,
            div [ class "field-body inputWidth" ]
            [  p [ class "control inputWidth" ]
                   [ 
                      textarea [ class "textarea", placeholder "Normal textarea", disabled read, maxlength 250] []
                    ]
            ]
       ]

searchBtn : Html msg
searchBtn = 
    div [ class "field has-addons has-addons-right searchBtnStyle" ] [
        div [ class "field-body " ]
            [ div [ class "field  is-grouped is-grouped-right " ]
                [ p [ class "control" ]
                [ 
                    a [ class "button is-primary" ]
                    [i [ class "fas fa-search" ]
                        [], text "검색" 
                    ]
                ] ,
                p [ class "control"] [
                    a [ class "button is-warning" ]
                    [ 
                        i [ class "fa fa-ban" ]
                        [], text "초기화" 
                    ]
                ]
                ]
            ]
    ]

-- search : Html msg
searchB msg reset= 
    div [ class "field has-addons has-addons-right searchBtnStyle" ] [
        div [ class "field-body " ]
            [ div [ class "field  is-grouped is-grouped-right " ]
                [ p [ class "control" ]
                [ 
                    div [ class "button is-primary", onClick msg ]
                    [i [ class "fas fa-search" ]
                        [], text "검색" 
                    ]
                ] ,
                p [ class "control"] [
                    div [ class "button is-warning", onClick reset ]
                    [ 
                        i [ class "fa fa-ban" ]
                        [], text "초기화" 
                    ]
                ]
                ]
            ]
    ]

searchDataSet : String -> Html msg
searchDataSet title=
        div [ class "field is-horizontal" ]
            [ 
                labelWrap title,
                p [ class "control" ]
                [ span [ class "select" ]
                    [ select []
                       [ option []
                            [ text "전체" ]
                        , option []
                            [ text "사용자 지정" ]
                        ]
                    ]
                ],
            div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control is-expanded has-icons-right" ]
                    [ input [ class "input", type_ "text", placeholder "DatePicker" ]
                        []
                    , span [ class "icon is-small is-right" ]
                        [ i [ class "fa fa-calendar-alt" ]
                            []
                        ]
                    ]
                ]
            , div [ class "field" ]
                [ p [ class "control is-expanded  has-icons-right" ]
                    [ input [ class "input is-success", type_ "email", placeholder "DatePicker" ]
                        []
                    , span [ class "icon is-small is-right" ]
                        [ i [ class "fa fa-calendar-alt" ]
                            []
                        ]
                    ]
                ]
            ]
            ]
       

-- searchDate : String -> String -> Html msg
searchDate title msg datePicker firstDate endmsg enddatepicker endDate dateVal dateModel readOnly=
        div [ class "field is-horizontal" ]
            [ 
                labelWrap title,
                p [ class "control" ]
                [ span [ class "select" ]
                    [ select [ onInput dateVal, value dateModel ]
                       [ option [value "all"]
                            [ text "전체" ]
                        , option [ value "user"]
                            [ text "사용자 지정" ]
                        ]
                    ]
                ],
                if readOnly =="readOnly" then
                div [ class "field-body" ]
                    [ div [ class "field datepickerOn" ]
                        [ p [ class ("control is-expanded has-icons-right datePickerBorder readOnly" ) ]
                            [ div []
                                [text firstDate]
                            , span [ class "icon is-small is-right" ]
                                [ i [ class "fa fa-calendar-alt" ]
                                    []
                                ]
                            ]
                            ,datePicker
                        ]
                    , div [ class "field" ]
                        [ p [ class "control is-expanded  has-icons-right datePickerBorder  readOnly" ]
                            [ div []
                                [text endDate]
                            , span [ class "icon is-small is-right" ]
                                [ i [ class "fa fa-calendar-alt" ]
                                    []
                                ]
                            ]
                            ,enddatepicker
                        ]
                    ]
                else
                    div [ class "field-body" ]
                        [ div [ class "field datepickerOn" ]
                            [ p [ class ("control is-expanded has-icons-right datePickerBorder " ), onClick msg ]
                                [ div []
                                    [text firstDate]
                                , span [ class "icon is-small is-right" ]
                                    [ i [ class "fa fa-calendar-alt" ]
                                        []
                                    ]
                                ]
                                ,datePicker
                            ]
                        , div [ class "field" ]
                            [ p [ class "control is-expanded  has-icons-right datePickerBorder", onClick endmsg ]
                                [ div []
                                    [text endDate]
                                , span [ class "icon is-small is-right" ]
                                    [ i [ class "fa fa-calendar-alt" ]
                                        []
                                    ]
                                ]
                                ,enddatepicker
                            ]
                        ]
            ]
          




customBox title=
        div [ class "field-body customBox" ]
            [ div [ class "field  is-fullwidth" ]
                [ p [ class "control" ]
                      (List.map checkBox title) 
                ]
            ]
        


checkBox : String -> Html msg
checkBox title= 
    label [ class "checkbox" ]
        [ input [ type_ "checkbox"]
            [], text title 
        ]       

-- checkBoxReadOnly : String -> Bool -> Html msg
checkBoxReadOnly title readOnly= 
    label [ class "checkbox" ]
        [ input [ type_ "checkbox", disabled readOnly]
            [], text title 
        ]  

textArea: String -> Bool -> Html msg
textArea title read= 
        div [class "control"] [
                label []
                [ text title ],
                textarea [ class "textarea", placeholder "Normal textarea", disabled read, maxlength 250]
            []
        ]


-- datepicker
endDatePicker model enddatemsg= 
    if model.endShow then
    div [class "datepickerPosition"] 
        [ div
            []
            [ DatePicker.view
                model.endDatePickerData
                getDatePickerProps
                |> Html.map enddatemsg
            ]
         ]
    else
    span [] []


datepicker model startdatemsg= 
    if model.show then
    div [class "datepickerPosition"] 
        [ div
            []
            [ DatePicker.view
                model.datePickerData
                getDatePickerProps
                |> Html.map startdatemsg
            ]
         ]
    else
    span [] []


getFormattedDate model whenday= 
    let
        year x = Date.year x
        month x= Date.month x
        day x= Date.day x
        encode d = 
            Date.fromCalendarDate (year d) (month d) (day d)
            |> Date.toIsoString
    in  
    case model of  
        Just d ->  
            encode d
        Nothing ->
            case whenday of
                Just d ->  
                    encode d
                Nothing ->
                    "날짜 로드 실패"

getDatePickerProps : DatePicker.Props
getDatePickerProps =
    let
        defaultProps =
            DatePicker.defaultProps
    in
        {defaultProps
            | okButtonText = "확인",
            cancelButtonText = "취소"}
type Day
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun

validationErr err validErrShow = 
    div [ class  (
        if validErrShow then
            "validStyle "
        else
            "validOn"
        )] [
        i [ class "fas fa-exclamation-circle" ]
        [] ,
        div [ class "validText" ] [text err] 
        ]



-- mediaShow 
videoShow title show showBtn=
    if show then
        div [ class "videoPop" ] [
            p [class "videoTitle"] [ text title 
            ,
                i[ class "fas fa-times videoClose", onClick showBtn][]
            ],
            div [ id "myElement" ] [
            ],
            div [class "button videoBtn", onClick showBtn] [text "닫기"]   
            ]
    else
        div [ id "myElement"] []

detailvideoShow title show =
    if show then
        div [ class "videoPop" ] [
            p [class "videoTitle"] [ text title 
            ,
                i[ class "fas fa-times videoClose"][]
            ],
            div [ id "myElement" ] [
            ],
            div [class "button videoBtn"] [text "닫기"]   
            ]
    else
        div [] []


yfVideoShow show close model sort sortMsg=
    if show then
        div [ class "videoPop" ] [
            p [class "videoTitle"] [ text model.title
            ,
                i[ class "fas fa-times videoClose", onClick close][]
            ],
            div [ id "myElement" ] [
            ],
            div [class ""] [
                div [class "descStyleWrap"] [
                table [ class "descStyle"] 
                    [ tr [] 
                    [ th [] [text "영상 x 세트"]
                    , th [] [text "재생시간"]
                    ]
                    , tbody [] (
                        List.map (\x ->
                            tr [onClick (sortMsg x.sort)]
                            [ td [] [text (x.title ++ " x " ++ String.fromInt(x.value))]
                            , td [] [text x.duration]
                            ]
                        ) model.exercise_items
                    )
                ]
                ]
                , p [class "descriptionVideo"] [
                    text sort
                ]
            ]
            , div [class "button videoBtn", onClick close] [text "닫기"]   
            ]
    else
        div [] []



spinner = 
    div [ class "lds-ring" ]
        [ div []
            []
        , div []
            []
        , div []
            []
        , div []
            []
        ]