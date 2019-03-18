module Page.Edit.InfoEdit exposing (..)


import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
import Regex exposing (Regex)
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
    , session: Session
    , data : DataWrap
    }

type alias Data = 
    { content : String
    , id : Int
    , title : String
    }

type alias DataWrap = 
    { data : Data }

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
    , data = 
            {
                data = 
                { content = ""
                , id = 0
                , title = ""}
                }
        }, Api.getInfo())

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Api.getInfoParams GetId


type EditorTab
    = Editor


type PreviewTab
    = RealTime


type Msg
    = TextAreaInput String
    | GetId Encode.Value
    | GetData (Result Http.Error DataWrap)
    | NoOp String


update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        TextAreaInput str ->
             ({ model | textarea = str }, Cmd.none)
        
        GetId id ->
            let
                decode = 
                    Decode.decodeValue Decode.string id
            in
                case decode of
                    Ok str ->
                        (model, Api.get GetData (Endpoint.infoDetail str)(Session.cred model.session) dataWrapDecoder)
                
                    Err _->
                        (model , Cmd.none)

        GetData (Ok item ) ->
            ({model | data = item, textarea = model.data.data.content}, Cmd.none)

        GetData (Err err) ->
            (model, Cmd.none)
        NoOp str->
            (model, Cmd.none)


view : Model -> {title : String, content : Html Msg}
view model =
    { title = "공지사항 수정"
    , content = 
       Info.infoDetail 
        -- model.data.data.content
        model
        TextAreaInput 
        True
        "공지사항 상세"
        (routeDetail Route.InfoEdit Route.Info)
        NoOp
        model.data.data.title
        model.data.data.content
    }
   
