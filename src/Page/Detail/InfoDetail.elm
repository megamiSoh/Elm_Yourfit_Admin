module Page.Detail.InfoDetail exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
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
    , goEdit = False
    , noticeId =""
    , data = 
        {
            data = 
            { content = ""
            , id = 0
            , title = ""}
            }
    }, Cmd.batch[
        Api.getInfo()
        , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
        ])

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.onSucceesSession SessionCheck
    , Session.changes GotSession (Session.navKey model.session)
    , Api.getInfoParams GetId
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
    | SessionCheck Encode.Value
    | GotSession Session
    | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

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
                        ( {model |  menus = item.data.menus, goEdit = True}, Cmd.none )
                    else
                        ( {model |  menus = item.data.menus}, Cmd.none )
                Nothing ->
                    ( {model |  menus = item.data.menus}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, Cmd.batch [
                             Api.get GetData (Endpoint.infoDetail model.noticeId)(Session.cred model.session) dataWrapDecoder
                        ])
                    Err _ ->
                        (model, Cmd.none)
        TextAreaInput str ->
             ({ model | textarea = str }, Cmd.none)
        
        GetId id ->
            let
                decode = 
                    Decode.decodeValue Decode.string id
            in
                case decode of
                    Ok str ->
                        ({model | noticeId = str}, Api.get GetData (Endpoint.infoDetail str)(Session.cred model.session) dataWrapDecoder)
                
                    Err _->
                        (model , Cmd.none)

        GetData (Ok item ) ->
            ({model | data = item, textarea = item.data.content , title = item.data.title}, Cmd.none)

        GetData (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        TitleText str->
            ({model| title = str},Cmd.none)
        IsEdit ->
            ({model | isEdit = not model.isEdit}, 
            if model.isEdit then
            Cmd.batch [
                encodeList model
            ]
            else
            Cmd.none
            )

        HttpResult (Ok item)->
            (model ,Api.get GetData (Endpoint.infoDetail model.noticeId)(Session.cred model.session) dataWrapDecoder )
        HttpResult (Err err) ->
            let
                error = Api.decodeErrors err
            in
            (model,Session.changeInterCeptor (Just error))

encodeList model  = 
    let
        body = 
            Encode.object
                [ ("title", Encode.string model.title)
                , ("content", Encode.string model.textarea)]
                    |> Http.jsonBody
    in
        Api.post (Endpoint.infoEdit model.noticeId) (Session.cred model.session) HttpResult body resultDecoder

resultDecoder = 
    Decode.succeed Code
        |>required "result" string

type alias Code = 
    { result : String}

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "공지사항"
    , content = 
        if model.isEdit then
            div [] [
                Info.infoDetail 
                model
                TextAreaInput 
                False
                "공지사항 수정"
                (routeDetail Route.InfoEdit Route.Info)
                TitleText
                model.title
                model.data.data.content
                ,
                div [ class "buttons" ] [
                    div [ class "button is-primary cursur", onClick IsEdit ] [text "저장"],
                    a [ class "button is-warning", Route.href (Just Route.Info) ] [text "취소"]
                ]
            ]
        else 
            div [] [
                Info.infoDetail 
                model
                TextAreaInput 
                True
                "공지사항 상세"
                (routeDetail Route.InfoEdit Route.Info)
                TitleText
                model.data.data.title
                model.data.data.content
                ,
                div [ class "buttons" ] [
                    div [][
                        if model.goEdit then
                        div [ class "button is-primary cursur", onClick IsEdit ] [text "수정"]
                        else
                        div [] []
                    ]
                    , a [ class "button is-warning", Route.href (Just Route.Info) ] [text "취소"]
                ]
            ]
         , menu =  
            aside [ class "menu"] [
                ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
            ]
    }
   