module Page.FoodCalorie exposing (..)

import Browser
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (Route)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias Model =
    {
        popup : Bool,
        session: Session,
        popShow : Bool
        , menus : List Menus
        , username : String
    }
type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session = 
    ({
        popup = False,
        session = session,
        popShow = False
        , menus = []
        , username = ""
    }, Cmd.batch[Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    -- , Api.pgGo ()
    ]
    )

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession(Session.navKey model.session)

type Msg 
    = PopClose 
    | PopOpen 
    | DeletePop 
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GotSession Session

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session},
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
             )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        PopClose ->
            ({model | popup = False}, Cmd.none)

        PopOpen ->
            ({model | popup = True}, Cmd.none)
        
        DeletePop ->
            ({model | popShow = not model.popShow}, Cmd.none)
            

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model = 
    { title = "음식 칼로리 관리"
    , content = 
        div [] [text "준비 중 입니다."]
        -- div [ class "container is-fluid" ]
        -- [ 
            
        --     columnsHtml [pageTitle "음식 칼로리 관리"],
        --     div [ class "searchWrap" ] [
        --         columnsHtml [
        --             searchDataSet "등록일"
        --         ],
        --         columnsHtml [
        --             formInput "음식명" "음식 명을 입력 해 주세요." False,
        --             searchBtn
        --         ]
                
        --     ],
        --     registBtn,
        --     div [ class "table" ] ([headerTable] ++ (List.map tableLayout datatable ))
        --     , Pagenation.pagenation
        --     , foodRegist model
        --     , 
        --     if model.popShow then
        --         deleteConfirm
        --     else
        --      span [] []
        -- ]
        , menu =  
                aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }



headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "음식명"],
         div [ class "tableCell" ] [text "칼로리"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "삭제"]
     ]

--, Route.href Route.UvideoDetail
tableLayout item = 
        div [class "tableRow"] [
                 div [ class "tableCell" ] [text (String.fromInt(item.no))],
                 div [ class "tableCell" ] [text item.foodTitle],
                 div [ class "tableCell" ] [text (item.calorie ++ "Kcal")],
                 div [ class "tableCell" ] [text item.createDate],
                 div [ class "tableCell", onClick DeletePop] [
                     div [class "button"][text "삭제"]
                     ]
         ]


deleteConfirm = 
    div [ class "positionLayout" ] [
        div [ class "deleteConfirm"] [
        div [class "closeBtnDelete"] [
            i [ class "fas fa-times", onClick DeletePop ]
            []
        ],
        div [class "topTitle"] [text "음식 삭제"],
        div [class "deleteText"] [text "음식을 삭제 하시겠습니까?"],
        div [class "deleteConfirmBtn"] [
            div [class "button is-warning", onClick DeletePop] [
                text "확인"
            ],
             div [class "button is-primary", onClick DeletePop] [
                text "취소"
            ]
        ]
    ]
    ]


registBtn : Html Msg
registBtn  =
        div [ class "registWrap"] 
        [
            button [ class "button is-primary", onClick PopOpen ]
            [ 
                i [ class "far fa-registered" ]
                []
                , text "음식 등록"
                ]
        ]

registSet : Html Msg
registSet=
        div [ class "buttons" ] [
            button [ class "button is-primary", onClick PopClose ] [text "등록"],
            button [ class "button is-warning", onClick PopClose ] [text "취소"]
        ]

foodRegist model = 
    if model.popup  then
            div [class "foodRegist"] [
                div [class "closeBtnDelete"] [
                i [ class "fas fa-times", onClick DeletePop ]
                []
            ],
            div [class "topTitle"] [text "음식 칼로리 등록"],
            columnsHtmlBtn [
                {contents = formInput "음식명" "음식명 입력" False, class = ""},
                {contents = text "", class = "is-one-fifth"}
            ],
            columnsHtmlBtn [
                {contents = formInput "칼로리" "칼로리 입력" False, class = ""},
                {contents = text "Kcal", class = "is-one-fifth"}
            ],
            columnsHtml [
                registSet
            ]
        ]
    else
        p [] []


datatable 
    = [
        {
            no = 1,
            foodTitle = "Tomato",
            calorie = "300",
            createDate = "2019-12-01"
        },
        {
            no = 2,
            foodTitle = "Egg",
            calorie = "500",
            createDate = "2019-12-01"
        },
        {
            no = 3,
            foodTitle = "Meat",
            calorie = "400",
            createDate = "2019-12-01"
        },
        {
            no = 4,
            foodTitle = "Milk",
            calorie = "200",
            createDate = "2019-12-01"
        },
        {
            no = 5,
            foodTitle = "Potato Chips",
            calorie = "100",
            createDate = "2019-12-01"
        },
        {
            no = 6,
            foodTitle = "Hamburger",
            calorie = "500",
            createDate = "2019-12-01"
        },
        {
            no = 7,
            foodTitle = "Kimchi",
            calorie = "350",
            createDate = "2019-12-01"
        },
        {
            no = 8,
            foodTitle = "Tohu",
            calorie = "240",
            createDate = "2019-12-01"
        },
        {
            no = 9,
            foodTitle = "Maratang",
            calorie = "10",
            createDate = "2019-12-01"
        },
        {
            no = 10,
            foodTitle = "Sushi",
            calorie = "800",
            createDate = "2019-12-01"
        }
    ]