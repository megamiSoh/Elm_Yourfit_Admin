module Page.Regist.FaqRegist exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
import Page.Page exposing(..)
import Page.Origin.Info as Info
import Session exposing (Session)
import Route exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Api.Endpoint as Endpoint
import Api as Api
import Http exposing (..)
import Json.Encode as Encode
import Page as Page
import Api.Decode as Decoder

defaultOptions =
    { softAsHardLineBreak = False
    , rawHtml = ParseUnsafe
    }

type EditorTab
    = Editor

type PreviewTab
    = RealTime

type alias Model =
    { textarea : String
    , onDemandText : String
    , options : Markdown.Config.Options
    , showToC : Bool
    , selectedTab : EditorTab
    , selectedPreviewTab : PreviewTab
    , session : Session
    , title : String
    , content : String
    , validErrShow : Bool
    , validationErr : String
    , menus : List Menus
    , username : String
    , errType : String
    }

type alias Menus =
    { menu_auth_code: List String
    , menu_id : Int
    , menu_name : String
    }

type alias ResultForm =
    { result : String }

init : Session -> (Model, Cmd Msg)
init session=
    ({ textarea = ""
    , onDemandText = ""
    , menus = []
    , options = defaultOptions
    , showToC = False
    , selectedTab = Editor
    , selectedPreviewTab = RealTime
    , session = session
    , title = ""
    , content = ""
    , validErrShow = False
    , validationErr = ""
    , username = ""
    , errType = ""
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

faqRegist : Model -> Session -> Cmd Msg
faqRegist model session =
    let
        list = 
            Encode.object
                [ ("title", Encode.string model.title)
                , ("content" , Encode.string model.textarea)
                ]
        body = 
            list 
                |> Http.jsonBody     
    in
        Api.post Endpoint.faqRegist (Session.cred session) GetList body resultFormDecoder
    

resultFormDecoder : Decoder ResultForm
resultFormDecoder = 
    Decode.succeed ResultForm
        |> required "result" string

toSession : Model -> Session
toSession model =
    model.session

type Msg
    = TextAreaInput String
    | GetList (Result Http.Error ResultForm)
    | SubmitInfo
    | Title String
    | GotSession Session
    | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetMyInfo"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "GetMyInfo" ->
                     Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            
                "GetList" ->
                    faqRegist model session
                _ ->
                    Cmd.none
            )
        Title str ->    
            ({model | title = str}, Cmd.none)
        TextAreaInput str ->
             ({ model | textarea = str }, Cmd.none)
        GetList (Ok item)->
            (model , Route.pushUrl(Session.navKey model.session) Route.Faq  )
        GetList (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetList"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        SubmitInfo ->
            if String.isEmpty model.title then
                ({model | validErrShow = True, validationErr = "제목을 입력 해 주세요."}, Cmd.none)
            else if String.isEmpty model.textarea then
                ({model | validErrShow = True, validationErr = "내용을 입력 해 주세요."}, Cmd.none)
            else
                ({model | validErrShow = False}, faqRegist model model.session)


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "FAQ 등록"
    , content = 
        div [] [
            Info.infoArticle 
                model
                TextAreaInput
                False
                "FAQ 등록"
                (routeRegist Route.Info)
                Title
                ,
            div [ class "buttons" ] [
                    div [ class "button is-primary cursur", onClick SubmitInfo ] [text "등록"],
                    a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
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
   
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
    