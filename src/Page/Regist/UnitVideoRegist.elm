module Page.Regist.UnitVideoRegist exposing (..)

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
import Api as Api
import Api.Decode as D
import Api.Endpoint as Endpoint
import Api as Api
import Http exposing(..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode
import Page as Page

type alias Model =
    { session : Session
    , levels : List Level
    , instrument : List Level
    , part : List Level
    , exerCode : List Level
    , editData : EditData
    , checkModel : String
    , loading : Bool
    , validationErr : String
    , validErrShow : Bool
    , menus : List Menus
    , username : String
    , errType : String
    }

type alias EditData = 
    { title : String 
    , difficulty : String
    , exercise : String
    , instrument : String
    , video : String
    , description : String
    , part_details : List String
    }

type alias Menus =
    { menu_auth_code : List String
    , menu_id : Int
    , menu_name : String
    }

type alias PartDetail = 
    { code : String
    , name : String
    }

type alias ListData = 
    { data : List Level}

type alias Level = 
    { code : String
    , name : String }


type alias ResultDecoder = 
    { result : String }


init: Session -> (Model, Cmd Msg)
init session = 
    ({session = session
    , levels = []
    , instrument = []
    , part = []
    , exerCode = []
    , loading = True
    , checkModel = ""
    , validationErr = ""
    , validErrShow = False
    , menus = []
    , username = ""
    , editData =
        { title = "" 
        , difficulty = "H1"
        , exercise = "10"
        , instrument = "10"
        , video = ""
        , description = ""
        , part_details =  []
        } 
    , errType = ""
    }, 
    Cmd.batch 
    [ Api.getParams ()
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
    ]
    )
editEncoder : EditData -> Session -> Cmd Msg
editEncoder editData session = 
    let
        newinput text=
            text 
                |> String.replace "&" "%26"
                |> String.replace "%" "%25"
        list =
            ("title="
                ++ (newinput editData.title)
                ++ "&difficulty="
                ++ editData.difficulty
                ++ "&exercise="
                ++ editData.exercise
                ++ "&instrument="
                ++ editData.instrument
                ++ "&video="
                ++ editData.video
                ++ "&description=" ++ (newinput editData.description)
                ++ "&part_details=[\"" ++ 
                    String.join "\",\"" editData.part_details
                ++"\"]" 
            )
        body =
            list
            |> Http.stringBody "application/x-www-form-urlencoded"
                
    in
    Api.post Endpoint.unitRegist (Session.cred session) SucceesEdit body (D.resultDecoder ResultDecoder)

toSession : Model -> Session
toSession model =
    model.session

type Msg 
    =  
     TitleText String
    | GetLevel (Result Http.Error ListData)
    | GetTool (Result Http.Error ListData)
    | GetPart (Result Http.Error ListData)
    | ExerCode (Result Http.Error ListData)
    | SelectTool String
    | SelectPart (String, String)
    | SelectLevel String
    | SelectExerCode String
    | VideoId String
    | SucceesEdit (Result Http.Error ResultDecoder)
    | GoEdit
    | AreaMsg String
    | GotSession Session
    | GetMyInfo (Result Http.Error D.DataWrap)



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
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, 
            Cmd.batch
            [ Api.post Endpoint.unitLevel (Session.cred model.session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
            , Api.post Endpoint.instrument (Session.cred model.session) GetTool Http.emptyBody (D.unitLevelsDecoder ListData Level)
            , Api.post Endpoint.part (Session.cred model.session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
            , Api.post Endpoint.exerCode (Session.cred model.session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData Level)
            ] )
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "GetMyInfo" ->
                    Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            
                "SucceesEdit" ->
                    editEncoder model.editData session
                _ ->
                    Cmd.none
            )
        AreaMsg str ->
            let
                old = model.editData
                new = {old | description = str}
            in
            ({model | editData = new} , Cmd.none)
        GoEdit ->
            if String.isEmpty model.editData.title then
                ({model | validationErr = "제목을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.editData.video  then
                ({model |validationErr = "비디오 Id를 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.editData.description then
                ({model | validationErr = "운동 설명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if List.isEmpty model.editData.part_details then
                ({model | validationErr = "운동 부위를 선택 해 주세요.", validErrShow = True}, Cmd.none)
            else
                ({model | validErrShow = False, loading = True},editEncoder model.editData model.session)
        SucceesEdit (Ok ok )->
            let
                textEncode = Encode.string "등록이 완료 되었습니다."
            in
            
            (model, Cmd.batch[Route.pushUrl (Session.navKey model.session)Route.VideoUnit, Api.showToast textEncode])
        SucceesEdit (Err err)->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "SucceesEdit"}, Api.changeInterCeptor (Just error))
            else if error == "500" then
                 ({model | validationErr = "등록할 수 없습니다.", validErrShow = True}, Cmd.none)
            else
                (model, Cmd.none)
        VideoId str ->
            let
                old = model.editData
                new = {old | video = str}
            in
            ({model | editData = new} , Cmd.none)
        SelectTool str ->
            let
                old = model.editData
                new =  {old | instrument = str}
            in
            ({model | editData = new},Cmd.none)
        SelectPart (code, title) ->
            let
                old = model.editData
                new = {old | part_details = old.part_details ++ [code]}
                sameData = 
                    List.member code old.part_details
                sameNewData = 
                    List.filter (\x -> x /= code) old.part_details
                sameResult = 
                    {old | part_details = sameNewData}
                
            in  
                if sameData then
                ({model | editData =  sameResult}, Cmd.none)
                else
                ({model | editData = new},Cmd.none)
        SelectLevel str ->
            let
                old = model.editData
                new =  {old | difficulty = str}
            in
             ({model | editData = new},Cmd.none)
        SelectExerCode str ->
            let
                old = model.editData
                new =  {old | exercise = str}
            in
             ({model | editData = new},Cmd.none)   
        ExerCode (Ok ok) -> 
            ({model | exerCode = ok.data} ,Cmd.none)
        ExerCode (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "ExerCode"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetPart (Ok ok) -> 
            ({model | part = ok.data} ,Cmd.none)
        GetPart (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetPart"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
               let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetTool"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
            
        GetLevel (Ok ok ) ->
            ({model | levels = ok.data, loading = False}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetLevel"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        TitleText str ->
            let
                old = model.editData
                new = {old | title = str}
            in
            
            ({model | editData = new},Cmd.none)


view : Model -> {title : String ,content : Html Msg, menu : Html Msg}
view model =
    if model.loading then
    { title = "유어핏 단위 영상 상세"
    , content = 
            div [] [
                div [class "adminloadingMask"][Page.spinner]
            ]
            , menu =  
            aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
    }
    else
    { title = "유어핏 단위 영상 상세"
    , content = 
            div [] [
                div [][] ,
                Unit.unitRegist
                    "유어핏 단위 영상 등록"
                    False
                    TitleText 
                    model
                    SelectLevel
                    SelectExerCode
                    SelectTool
                    VideoId
                    SelectPart
                    AreaMsg
                ,
                Page.detailEventBtn "등록" GoEdit Route.VideoUnit
                , Page.validationErr model.validationErr model.validErrShow
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
