module Page.UserInfo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.Page exposing (..)
import ExpandEvent as Exevent
import Session exposing (Session)
import Http exposing (..)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as JsonDecodePipeline exposing (..)
import Api as Api


type alias Model =
    {
        popEvent : Exevent.Model,
        passwordEvent : Exevent.Model,
        session: Session,
        data : Data,
        problems : String
    }
type Problem
    = InvalidEntry String
    | ServerError String

type alias Data = 
    {
        admin : Admin,
        menus : List Menus
    }
type alias Admin = 
    {
        connected_at : String,
        id : Int,
        joined_at : String,
        nickname : Maybe String,
        username : String
    }
type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }


init : Session -> (Model, Cmd Msg)
init session = 
    (
    {popEvent = Exevent.init,
    passwordEvent = Exevent.init,
    data = {
        admin = {
            connected_at = "",
            id = 0,
            joined_at = "",
            nickname = Nothing,
            username = "" 
        } ,
        menus = [{
            menu_auth_code = [],
            menu_id = 0,
            menu_name = ""
        }]
    },
    problems = "" ,
    session = session}, managelist session
    )   



toSession : Model -> Session
toSession model = 
    model.session


managelist session =
    Api.post Endpoint.myInfo (Session.cred session) GetList Http.emptyBody userDecoder 


userDecoder = 
    map2 Data
        (field "admin" decoderAdmin)
        (field "menu" (list decoderMenu))

decoderAdmin = 
    map5 Admin
        (field "connected_at" string) 
        (field "id" int) 
        (field "joined_at" string) 
        (field "nickname" (nullable string)) 
        (field "username" string) 

decoderMenu = 
    map3 Menus
        (field "menu_auth_code" (list string) )
        (field "menu_id" int)
        (field "menu_name" string)

-- decoderAuthCode =
--     decodeString (list string) 


type Msg = PopEvent Exevent.Msg | PasswordEvent Exevent.Msg | GetList (Result Http.Error Data)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetList (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            ( { model | problems = "Err" }
            , Cmd.none
            )

        GetList (Ok item) ->
            ( {model | data = item}, Cmd.none )
        PopEvent subMsg ->
            let
                updateEventModel =
                    Exevent.update subMsg model.popEvent    
            in
                ({model | popEvent = updateEventModel }, Cmd.none)
        
        PasswordEvent subMsg ->
            let
                updateEventModel =
                    Exevent.update subMsg model.passwordEvent
            in
                ({model | passwordEvent = updateEventModel}, Cmd.none)
            

view : Model -> {title : String , content : Html Msg}
view model =
    { title = "내 정보"
    , content =
        div [ class "box" ]
        [ article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle "내 정보",
                    figure [ class "image is-64x64" ]
                    [ 
                        i [ class "fas fa-user-circle" ]
                        []
                    ]
                     , popEvent model "is-small" "사진삭제" "사진을 삭제 하시겠습니까?" False
                ]
            , div [ class "media-content" ]
                [ 
                    passwordEvent model "is-info is-small"  "비밀번호 변경" "비밀번호를 변경하시겠습니까?" True "비밀번호 입력" "새로운 비밀번호를 입력 해 주세요." "비밀번호 확인" "비밀번호를 한번 더 입력 해 주세요." False,
                    p [ class "adminId" ] [
                        text "어드민 아이디 : FinalCompany"
                    ],
                    div [ class "content" ]
                    [ 
                         userInfoData
                    ]
                ]
            ]
            ,
            div [ class " menuAuth" ] [
                pageTitle "메뉴 권한",
            article [ class "media" ]
                [ 
                    columns ["사용자 관리", "관리자 관리", "유어핏 단위 영상" , "유어핏 영상", "외부 API 영상"]
                ]
            ,article [ class "media"]
            [
                columns ["음식 칼로리 관리", "사용자 게시물", "공지사항" , "1:1 문의"]
            ]
            ]
        ]
    }

popEvent : Model -> String -> String -> String-> Bool -> Html Msg
popEvent model style title phrase icon= 
    Html.map PopEvent ( Exevent.layoutPop model.popEvent style title phrase icon)

passwordEvent : Model -> String -> String -> String -> Bool -> String -> String -> String -> String -> Bool -> Html Msg
passwordEvent model style title phrase icon input inputh inputr inputrh disabled= 
    Html.map PasswordEvent ( Exevent.passwordPop model.passwordEvent style title phrase icon input inputh inputr inputrh disabled)