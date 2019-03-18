module Page.Regist.InfoRegist exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
-- import Json.Encode as JE
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
-- import Regex exposing (Regex)
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
    , title : String
    , content : String
    , validErrShow : Bool
    , validationErr : String
    }


type alias ResultForm =
    { result : String }

init : Session -> (Model, Cmd Msg)
init session=
    ({ textarea = ""
    , onDemandText = ""
    , options = defaultOptions
    , showToC = False
    , selectedTab = Editor
    , selectedPreviewTab = RealTime
    , session = session
    , title = ""
    , content = ""
    , validErrShow = False
    , validationErr = ""
    }, Cmd.none)

infoRegist model =
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
        Api.post Endpoint.infoRegist (Session.cred model.session) GetList body resultFormDecoder
    

resultFormDecoder = 
    Decode.succeed ResultForm
        |> required "result" string

toSession : Model -> Session
toSession model =
    model.session

type EditorTab
    = Editor


type PreviewTab
    = RealTime


type Msg
    = TextAreaInput String
    | GetList (Result Http.Error ResultForm)
    | SubmitInfo
    | Title String
    | SessionCheck Encode.Value
    | GotSession Session

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
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
                            infoRegist model
                        ])
                    Err _ ->
                        (model, Cmd.none)
        Title str ->    
            ({model | title = str}, Cmd.none)
        TextAreaInput str ->
             ({ model | textarea = str }, Cmd.none)
        GetList (Ok item)->
            (model , Route.pushUrl(Session.navKey model.session) Route.Info  )
        GetList (Err error) ->
            let _ = Debug.log "item" error
                serverErrors =
                    Api.decodeErrors error
            in
            (model, Session.changeInterCeptor (Just serverErrors) )
        SubmitInfo ->
            if String.isEmpty model.title then
                ({model | validErrShow = True, validationErr = "제목을 입력 해 주세요."}, Cmd.none)
            else if String.isEmpty model.textarea then
                ({model | validErrShow = True, validationErr = "내용을 입력 해 주세요."}, Cmd.none)
            else
                ({model | validErrShow = False}, infoRegist model)


view : Model -> {title : String, content : Html Msg}
view model =
    { title = "공지사항 등록"
    , content = 
        div [] [
            Info.infoArticle 
                model
                TextAreaInput
                False
                "공지사항 등록"
                (routeRegist Route.Info)
                Title
                ,
            div [ class "buttons" ] [
                    div [ class "button is-primary cursur", onClick SubmitInfo ] [text "등록"],
                    a [ class "button is-warning", Route.href (Just Route.Info) ] [text "취소"]
                ]
            , validationErr model.validationErr model.validErrShow
        ]
        
    }
   
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.onSucceesSession SessionCheck
    , Session.changes GotSession (Session.navKey model.session)
    ]