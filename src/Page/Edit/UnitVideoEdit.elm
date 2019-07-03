module Page.Edit.UnitVideoEdit exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page as Page
import Json.Decode
import String
import Page.Origin.UnitVideo as Unit
import Session exposing (Session)
import Route exposing(..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias Model =
    {
        disabled : Bool,
        fileName: String,
        session: Session
        , menus : List Menus
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init: Session -> (Model, Cmd Msg)
init session = 
    ({disabled = False, fileName = "", session = session , menus = []},
     Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

type Msg 
    = GetFile String 
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GotSession Session

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GotSession session ->
            ({ model | session = session }, 
             Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
             )
        GetFile filename ->
           ({model | fileName = filename}, Cmd.none)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )

inputBtnx btn model thumb title =
    Unit.inputBtnx btn model.disabled GetFile thumb title

            



view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "유어핏 단위 영상 수정"
    , content = 
    div [] []
        -- Unit.unitVideoForm
        -- (Unit.checkBoxCustom bodyPart model.disabled)
        -- (Page.routeEdit Route.UvideoDetail Route.UvideoDetail )
        -- (inputBtnx "찾아보기" model (thumbInput model "썸네일을 선택 해 주세요.") "썸네일")
        -- "유어핏 단위 영상 수정"
        -- model.disabled
    , menu =  
    aside [ class "menu"] [
        ul [ class "menu-list yf-list"] 
            (List.map Page.viewMenu model.menus)
    ]
    }

thumbInput model ph= 
    if model.fileName == "" then
        text ph
    else 
        text model.fileName

bodyPart = ["가슴" ,"허벅지", "팔", "엉덩이", "등", "종아리", "복부"]

