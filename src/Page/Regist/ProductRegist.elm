module Page.Regist.ProductRegist exposing(..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http exposing (..)
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Api as Api
import Session exposing(Session)
import Route as Route
import Page as Page
import Page.Page exposing (..)
import Date exposing (..)
import DatePicker exposing (Msg(..))


type alias Model = 
    { session : Session 
    , menus : List Menus
    , username : String
    , product_name : String
    , is_pay : String
    , selectModel : List { code : String , name : String}
    , description : String
    , price : String
    , rage_date : String
    , validationErr : String
    , validErrShow : Bool
    , errType : String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model , Cmd Msg)
init session = 
    (
    { session = session
    , menus = []
    , username = ""
    , product_name = ""
    , is_pay = "true"
    , selectModel = 
        [
        { code = "true"
        , name = "유료"} ,
        { code = "false"
        , name = "무료"}
        ]
    , description = ""
    , price = ""
    , rage_date = ""
    , validationErr = ""
    , validErrShow = False
    , errType = ""
    }
    , Cmd.batch
    [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    ]
    )
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"

registApi model = 
    let
        body =
            formUrlencoded
            [ ("name", model.product_name)
            , ("description", model.description)
            , ("day_num", model.rage_date)
            , ("price", model.price)
            , ("is_pay", model.is_pay) ]
            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post Endpoint.productRegist (Session.cred model.session) RegistComplete body (Decoder.result)

toSession : Model -> Session
toSession model = 
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

type Msg  
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | NameInput String 
    | SelectEvent String
    | TextAreaInput String
    | PriceInput String
    | DateInput String
    | SubmitProduct
    | GotSession Session
    | RegistComplete (Result Http.Error Decoder.Success)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session}, 
            if model.errType == "" then
                Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            else
                registApi model
            )
        RegistComplete (Ok ok) ->
            (model, Route.pushUrl(Session.navKey model.session) Route.PM)
        RegistComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "regist"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        SubmitProduct ->
            if String.isEmpty model.product_name then
                ({model | validationErr = "상품명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.price then 
                ({model | validationErr = "가격을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            
            else if String.isEmpty model.rage_date then
                 ({model | validationErr = "기간을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.description then
                ({model | validationErr = "상품설명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else     
            ({model | validationErr = "", validErrShow = False}, registApi model)
        DateInput date ->
            case String.toInt date of
                Just ok ->
                    ({model | rage_date = date}, Cmd.none)       
            
                Nothing ->
                    if (String.length model.rage_date) == 1 then
                        ({model | rage_date = ""}, Cmd.none)
                    else
                        (model, Cmd.none)
        PriceInput price ->
            case String.toInt price of
                Just ok ->
                    ({model | price = price}, Cmd.none)        
            
                Nothing ->
                    if (String.length model.price) == 1 then
                        ({model | price = ""}, Cmd.none)
                    else
                        (model, Cmd.none)
            
        TextAreaInput text ->
            ({model | description = text}, Cmd.none)
        SelectEvent selected ->
            ({model | is_pay = selected}, Cmd.none)
        NameInput name ->
            ({model | product_name = name}, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 5) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        ( {model |  menus = item.data.menus, username = item.data.admin.username},Cmd.none)
                    else
                        ( {model |  menus = item.data.menus, username = item.data.admin.username},Cmd.none)
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username},Cmd.none)


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "상품 등록"
    , content =
        div []
            [ columnsHtml [pageTitle "상품 등록"]
           
       , div [] 
        [ div [class "searchWrap"] [
            columnsHtml 
            [ formInputEvent "상품명" "상품명을 입력 해 주세요." False NameInput model.product_name
            , formInputEvent "가격" "가격을 입력 해 주세요. (단위 / 원)" False PriceInput model.price
            ]
            ,  columnsHtml 
            [ formInputEvent "기간" "기간을 입력 해 주세요. (단위 / 일)" False DateInput model.rage_date
            , noEmptyselectForm "유 / 무료" False model.selectModel SelectEvent model.is_pay
            ]
            , columnsHtml [
                textAreaEvent "상품 설명" False model.description TextAreaInput
            ]
        ]
        ]
        , div [ class "buttons" ] [
            div [ class "button is-primary cursur", onClick SubmitProduct ] [text "등록"],
            a [ class "button is-warning", Route.href (Just Route.PM) ] [text "취소"]
        ]
        , validationErr model.validationErr model.validErrShow
    ]
    , menu =  
        aside [ class "menu"] [
            Page.header model.username
            ,ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
            ]
    }
            