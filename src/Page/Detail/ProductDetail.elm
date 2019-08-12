module Page.Detail.ProductDetail exposing(..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http exposing (..)
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Json.Decode as Decode
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
    , detailId : String
    , is_detail : Bool
    , pageTitle : String
    , errType : String
    , auth : List String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias DetailData = 
    { data : Data }

type alias Data = 
    { day_num : Int
    , description : String
    , id : Int
    , is_pay : Bool
    , name : String
    , price : Int 
    }


detailApi session id = 
    Api.get DetailComplte (Endpoint.productDetail id) (Session.cred session) (Decoder.productDetailData DetailData Data)
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
    , detailId = ""
    , is_detail = True
    , pageTitle = "상품관리 상세"
    , errType = ""
    , auth = []
    }
    , Cmd.batch
    [ Api.getParams ()
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

editApi model = 
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
    Api.post (Endpoint.productEdit model.detailId) (Session.cred model.session) EditComplete body (Decoder.result)

toSession : Model -> Session
toSession model = 
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.params GetId
    ]

type Msg  
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | NameInput String 
    | SelectEvent String
    | TextAreaInput String
    | PriceInput String
    | DateInput String
    | SubmitProduct
    | EditComplete (Result Http.Error Decoder.Success)
    | DetailComplte (Result Http.Error DetailData)
    | GotSession Session
    | GetId Encode.Value
    | GoEdit 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoEdit ->
            ({model | is_detail = False, pageTitle = "상품관리 수정"}, Cmd.none)
        GetId id ->
            let
                decode = 
                    Decode.decodeValue Decode.string id
            in
                case decode of
                    Ok str ->
                        ({model | detailId = str}, 
                        Cmd.batch[ detailApi model.session str
                        , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)])
                
                    Err _->
                        (model , Cmd.none)
        GotSession session ->
            ({ model | session = session}, 
            if model.errType == "" then
            Cmd.batch[ detailApi model.session model.detailId
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)]
            else
            editApi model
            )
        DetailComplte (Ok ok) ->
            ({model | rage_date = String.fromInt ok.data.day_num, description = ok.data.description , is_pay = if ok.data.is_pay then "true" else "false", product_name = ok.data.name, price = String.fromInt ok.data.price}, Cmd.none)
        DetailComplte (Err err)->
            (model, Cmd.none)
        EditComplete (Ok ok) ->
            ({model | is_detail = True, pageTitle = "상품관리 상세"}, Cmd.none)
        EditComplete (Err err) ->
            ({model | errType = "edit"}, Cmd.none)
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
            ({model | validationErr = "", validErrShow = False}, editApi model)
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
                menuf = List.head (List.filter (\x -> x.menu_id == 11) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                        ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code},Cmd.none)
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username},Cmd.none)


memberAuth num model= List.member num model.auth

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "상품 관리"
    , content =
        div []
            [ columnsHtml [pageTitle model.pageTitle]
           
       , div [] 
        [ div [class "searchWrap"] [
            columnsHtml 
            [ formInputEvent "상품명" "상품명을 입력 해 주세요." model.is_detail NameInput model.product_name
            , formInputEvent "가격" "가격을 입력 해 주세요. (단위 / 원)" model.is_detail PriceInput model.price
            ]
            ,  columnsHtml 
            [ formInputEvent "기간" "기간을 입력 해 주세요. (단위 / 일)" model.is_detail DateInput model.rage_date
            , noEmptyselectForm "유 / 무료" model.is_detail model.selectModel SelectEvent model.is_pay
            ]
            , columnsHtml [
                textAreaEvent "상품 설명" model.is_detail model.description TextAreaInput
            ]
        ]
        ]
        , div [ class "buttons" ] [
            if memberAuth "30" model then
                if model.is_detail then
                div [ class "button is-primary cursur", onClick GoEdit ] [text "수정"]
                else
                div [ class "button is-primary cursur", onClick SubmitProduct ] [text "저장"]
            else
                div [][]
            ,
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
            