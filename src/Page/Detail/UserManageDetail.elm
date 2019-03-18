port module Page.Detail.UserManageDetail exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(class)
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

type alias Model = {
    session: Session
    , getData : Data
    -- , user : User
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
    }


-- userdataDecoder = 
--     D.userdataDecoder Data User UserData


init : Session -> (Model, Cmd Msg)
init session = ({
        session = session
        , getData =  {
         data = {
            user = { connected_at = ""
                    , id = 0 
                    , joined_at = ""
                    , nickname = Nothing
                    , username = ""}
                }  
                  }
            
    } 
    , Cmd.batch[ 
        Api.getParams() 
    ]
    )



type Msg 
    = NoOp 
    | GetId E.Value
    | GetData (Result Http.Error Data)
    | GotSession Session
    | SessionCheck E.Value



toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetId id ->
            let _ = Debug.log "decode" id
                decodeId = Decode.decodeValue Decode.string id
            in
                case decodeId of
                    Ok strId ->
                        let _ = Debug.log "decode" strId
                        in
                        (model , Api.get  GetData (Endpoint.userDetail strId) (Session.cred model.session) (D.userdataDecoder Data User UserData) ) 
                
                    Err _  ->
                        let _ = Debug.log "decode" decodeId
                        in
                        (model,Cmd.none)
        GetData (Ok item) ->
            ({model | getData = item}, Cmd.none)
        GetData (Err error) ->
            let _ = Debug.log "err" error
                
            in
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            , Cmd.none
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
                        
  
view : Model -> {title : String , content : Html Msg}
view model =
    { title = "사용자 관리 상세"
    , content = 
        div [ class "box" ]
        [   
            article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle "사용자 관리 상세",
                    figure [ class "image is-64x64" ]
                    [ 
                        i [ class "fas fa-user-circle" ]
                        []
                    ]
                ]
            , div [ class "media-content" ]
                [ 
                    div [ class "content marginTop" ]
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
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Api.params  GetId, 
    Session.changes GotSession (Session.navKey model.session)
    , Api.onSucceesSession SessionCheck
    
    ]