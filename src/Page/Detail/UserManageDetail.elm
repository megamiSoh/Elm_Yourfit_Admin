port module Page.Detail.UserManageDetail exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(class, src)
import Html.Events exposing(..)
import Page.Page exposing(..)
import ExpandEvent as Exevent
import Session exposing (Session)
import Api as Api
import Json.Encode as E
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Http exposing(..)
import Api.Endpoint as Endpoint
import Task exposing(..)
import Route exposing(..)
import Api.Decode as D
import Page as Page

type alias Model = {
    session: Session
    , getData : Data
    , menus : List Menus
    , getId : String
    , goEdit : Bool
    }


type alias Data = 
    {data : User}

type alias User = 
    {user : UserData}

type alias UserData = 
    { connected_at : String
    , id: Int
    , joined_at : String
    , nickname : Maybe String
    , username : String
    , profile : Maybe String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

-- userdataDecoder = 
--     D.userdataDecoder Data User UserData


init : Session -> (Model, Cmd Msg)
init session = ({
        session = session
        , menus = []
        , getId = ""
        , goEdit = False
        , getData =  {
         data = {
            user = { connected_at = ""
                    , id = 0 
                    , joined_at = ""
                    , nickname = Nothing
                    , username = ""
                    , profile = Nothing}
                }  
                  }
            
    } 
    , Cmd.batch[ 
        Api.getParams()
        , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo) 
    ]
    )



type Msg 
    = NoOp 
    | GetId E.Value
    | GetData (Result Http.Error Data)
    | GotSession Session
    | SessionCheck E.Value
    | GetMyInfo (Result Http.Error D.DataWrap)
    | ResetPwd
    | ResetComplete (Result Http.Error D.Success)



toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetComplete (Ok ok) ->
            (model, Api.showToast (E.string "임시 비밀번호가 발송되었습니다."))
        ResetComplete (Err err) ->
            (model, Cmd.none)
        ResetPwd ->
            (model, Api.get ResetComplete (Endpoint.resetpwd model.getId) (Session.cred model.session) D.result)
        GetId id ->
            let
                decodeId = Decode.decodeValue Decode.string id
            in
                case decodeId of
                    Ok strId ->
                        ({model | getId = strId} , Api.get  GetData (Endpoint.userDetail strId) (Session.cred model.session) (D.userdataDecoder Data User UserData) ) 
                
                    Err _  ->
                        (model,Cmd.none)
        GetData (Ok item) ->
            ({model | getData = item}, Cmd.none)
        GetData (Err error) ->
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            , Cmd.batch 
            [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            , Api.get  GetData (Endpoint.userDetail model.getId) (Session.cred session) (D.userdataDecoder Data User UserData) ]
            )
        NoOp ->
            ( model, Cmd.none)
        SessionCheck str ->
            let
                decodeCheck = Decode.decodeValue Decode.string str
            in
                case decodeCheck of
                    Ok continue ->
                        (model, Cmd.none)
                    Err _ ->
                        (model, Cmd.none)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err  
            in
            (model, Session.changeInterCeptor (Just error) )
        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 1) item.data.menus)
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
                        
  
view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.goEdit then
    { title = "사용자 관리 상세"
    , content = 
        div [ class "box" ]
        [   
            article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle "사용자 관리 상세",
                    figure [ class "image is-64x64 userimage" ]
                    [ 
                       case model.getData.data.user.profile of
                           Just image ->
                               img [src image] []
                       
                           Nothing ->
                                i [ class "fas fa-user-circle" ]
                                []
                    ]
                ]
            , div [ class "media-content" ]
                    [
                    div [] [
                            div [ class "button is-danger btnpositionRight", onClick ResetPwd ] [text "비밀번호 초기화"]
                    ]
                    , div [ class "content marginTop" ]
                    [ 
                        userInfo
                        model.getData.data.user.nickname model.getData.data.user.username
                        model.getData.data.user.joined_at model.getData.data.user.connected_at
                    ]
                ]
            ]
            , div [ class "backArea"] [
                goNormalBtn "리스트로 돌아가기" " is-primary"  (Just Route.UserManage)
            ]
        ]
        , menu =  
        aside [ class "menu"] [
            ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
        ]
    }
    else
    { title = "사용자 관리 상세"
    , content = 
        div [ class "box" ]
        [   
            article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle "사용자 관리 상세",
                    figure [ class "image is-64x64 userimage" ]
                    [ 
                       case model.getData.data.user.profile of
                           Just image ->
                               img [src image] []
                       
                           Nothing ->
                                i [ class "fas fa-user-circle" ]
                                []
                    ]
                ]
            , div [ class "media-content" ]
                    [
                    div [] [
                            div [class "emptyheight"][]
                    ]
                    , div [ class "content marginTop" ]
                    [ 
                        userInfo
                        model.getData.data.user.nickname model.getData.data.user.username
                        model.getData.data.user.joined_at model.getData.data.user.connected_at
                    ]
                ]
            ]
            , div [ class "backArea"] [
                goNormalBtn "리스트로 돌아가기" " is-primary"  (Just Route.UserManage)
            ]
        ]
        , menu =  
        aside [ class "menu"] [
            ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Api.params  GetId, 
    Session.changes GotSession (Session.navKey model.session)
    , Api.onSucceesSession SessionCheck
    
    ]