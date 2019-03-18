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

type alias Model =
    {
        disabled : Bool,
        fileName: String,
        session: Session
    }

init: Session -> (Model, Cmd Msg)
init session = 
    ({disabled = False, fileName = "", session = session}, Cmd.none)

toSession : Model -> Session
toSession model =
    model.session


type Msg = GetFile String 

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        GetFile filename ->
           ({model | fileName = filename}, Cmd.none)

inputBtnx btn model thumb title =
    Unit.inputBtnx btn model.disabled GetFile thumb title

            



view : Model -> {title : String , content : Html Msg}
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
    }

thumbInput model ph= 
    if model.fileName == "" then
        text ph
    else 
        text model.fileName

bodyPart = ["가슴" ,"허벅지", "팔", "엉덩이", "등", "종아리", "복부"]

