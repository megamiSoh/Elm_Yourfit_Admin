module Page.Detail.FaqDetail exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onCheck, onClick, onInput)
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
import Page.Page exposing(..)
import Page.Origin.Info as Info
import Session exposing (Session)
import Route exposing(..)
import Api as Api
import Json.Encode as Encode
import Json.Decode as Decode exposing (..)
import Http exposing(..)
import Api.Endpoint as Endpoint
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Page as Page
import Api.Decode as Decoder

defaultOptions =
    { softAsHardLineBreak = False
    , rawHtml = ParseUnsafe
    }

type alias Model =
    { textarea : String
    , onDemandText : String
    , options : Markdown.Config.Options
    , showToC : Bool
    , selectedTab : EditorTab
    , selectedPreviewTab : PreviewTab
    , session : Session
    , data : DataWrap
    , isEdit : Bool
    , title : String
    , noticeId : String
    , menus : List Menus
    , goEdit : Bool
    , username : String
    , errType : String
    }

type alias Data = 
    { content : String
    , id : Int
    , title : String
    }

type alias DataWrap = 
    { data : Data }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

dataWrapDecoder =
    Decode.succeed DataWrap
        |> required "data" dataDecoder

dataDecoder = 
    Decode.succeed Data
        |> required "content" string
        |> required "id" int
        |> required "title" string


init : Session -> (Model, Cmd Msg)
init session =
    ({ textarea = ""
    , onDemandText = ""
    , options = defaultOptions
    , showToC = False
    , selectedTab = Editor
    , selectedPreviewTab = RealTime
    , session = session
    , isEdit = False
    , title = ""
    , menus = []
    , username = ""
    , goEdit = False
    , noticeId =""
    , data = 
        {
            data = 
            { content = ""
            , id = 0
            , title = ""}
            }
    , errType = ""
    }, Cmd.batch[
        Api.getParams ()
        
        ])

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [  Session.changes GotSession (Session.navKey model.session)
    , Api.params GetId
    ]
    

type EditorTab
    = Editor


type PreviewTab
    = RealTime


type Msg
    = TextAreaInput String
    | GetId Encode.Value
    | GetData (Result Http.Error DataWrap)
    | TitleText String
    | IsEdit 
    | HttpResult(Result Http.Error Code)
    | GotSession Session
    | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err err) ->
            -- let
            --     error = Api.decodeErrors err
            -- in
            -- if error == "401"then
            -- ({model | errType = "GetMyInfo"}, Api.changeInterCeptor (Just error))
            -- else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 8) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        ( {model |  menus = item.data.menus, username = item.data.admin.username, goEdit = True}, Cmd.none )
                    else
                        ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "GetData" ->
                    Api.get GetData (Endpoint.faqDetail model.noticeId)(Session.cred session) dataWrapDecoder
                "HttpResult" ->
                    encodeList model session 
                _ ->
                    Cmd.none
            )
        
        TextAreaInput str ->
             ({ model | textarea = str }, Cmd.none)
        
        GetId id ->
            let
                decode = 
                    Decode.decodeValue Decode.string id
            in
                case decode of
                    Ok str ->
                        ({model | noticeId = str}, Api.get GetData (Endpoint.faqDetail str)(Session.cred model.session) dataWrapDecoder)
                
                    Err _->
                        (model , Cmd.none)

        GetData (Ok item ) ->
            ({model | data = item, textarea = item.data.content , title = item.data.title}, 
            Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

        GetData (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetData"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        TitleText str->
            ({model| title = str},Cmd.none)
        IsEdit ->
            ({model | isEdit = not model.isEdit}, 
            if model.isEdit then
            Cmd.batch [
                encodeList model model.session
            ]
            else
            Cmd.none
            )

        HttpResult (Ok item)->
            (model ,Api.get GetData (Endpoint.faqDetail model.noticeId)(Session.cred model.session) dataWrapDecoder )
        HttpResult (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "HttpResult"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

encodeList model session = 
    let
        body = 
            Encode.object
                [ ("title", Encode.string model.title)
                , ("content", Encode.string model.textarea)]
                    |> Http.jsonBody
    in
        Api.post (Endpoint.faqEdit model.noticeId) (Session.cred session) HttpResult body resultDecoder

resultDecoder = 
    Decode.succeed Code
        |>required "result" string

type alias Code = 
    { result : String}

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.isEdit then
    { title = "FAQ"
    , content = 
            div [] [
                Info.infoDetail 
                model
                TextAreaInput 
                False
                "FAQ 수정"
                (routeDetail Route.Faq Route.Faq)
                TitleText
                model.title
                model.data.data.content
                ,
                div [ class "buttons" ] [
                    div [ class "button is-primary cursur", onClick IsEdit ] [text "저장"],
                    a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
            ]
         , menu =  
            aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
    }
    else
    { title = "FAQ"
    , content = 
            div [] [
                Info.infoDetail 
                model
                TextAreaInput 
                True
                "FAQ 상세"
                (routeDetail Route.Faq Route.Faq)
                TitleText
                model.data.data.title
                model.data.data.content
                ,
                div [ class "buttons" ] [
                    div [style "margin-right" "5px"][
                        if model.goEdit then
                        div [ class "button is-primary cursur", onClick IsEdit ] [text "수정"]
                        else
                        div [] []
                    ]
                    , a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
            ]
         , menu =  
            aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
    }
   