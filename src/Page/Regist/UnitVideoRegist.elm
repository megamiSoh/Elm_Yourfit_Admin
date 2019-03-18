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

type alias Model =
    { session : Session
    , levels : List Level
    , instrument : List Level
    , part : List Level
    , exerCode : List Level
    , editData : EditData
    , checkModel : String
    , validationErr : String
    , validErrShow : Bool
    }

type alias EditData = 
    { title:String 
    , difficulty:String
    , exercise:String
    , instrument:String
    , video:String
    , description:String
    , part_details:List String
    }

type alias PartDetail = 
    { code: String
    , name: String
    }

type alias ListData = 
    { data : List Level}

type alias Level = 
    { code : String
    , name : String }


type alias ResultDecoder = 
    {result : String}


init: Session -> (Model, Cmd Msg)
init session = 
    ({session = session
    , levels = []
    , instrument = []
    , part = []
    , exerCode = []
    , checkModel = ""
    , validationErr = ""
    , validErrShow = False
    , editData =
        { title = "" 
        , difficulty = "H1"
        , exercise = "10"
        , instrument = "10"
        , video = ""
        , description = ""
        , part_details =  []
        } 
    }, 
    Cmd.batch 
    [ Api.getParams ()
    , Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
     , Api.post Endpoint.instrument (Session.cred session) GetTool
      Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Api.post Endpoint.part (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
    , Api.post Endpoint.exerCode (Session.cred session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData Level)
    ]
    )

editEncoder editData session = 
    let
        list =
            ("title="
                ++ editData.title
                ++ "&difficulty="
                ++ editData.difficulty
                ++ "&exercise="
                ++ editData.exercise
                ++ "&instrument="
                ++ editData.instrument
                ++ "&video="
                ++ editData.video
                ++ "&description=" ++editData.description
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
                            Api.post Endpoint.unitLevel (Session.cred model.session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
                            , Api.post Endpoint.instrument (Session.cred model.session) GetTool
                            Http.emptyBody (D.unitLevelsDecoder ListData Level)
                            , Api.post Endpoint.part (Session.cred model.session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
                            , Api.post Endpoint.exerCode (Session.cred model.session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData Level)
                        ])
                    Err _ ->
                        (model, Cmd.none)
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
                ({model | validErrShow = False},editEncoder model.editData model.session)
            -- (model , editEncoder model.editData model.session)
        SucceesEdit (Ok ok )->
            (model, Route.pushUrl (Session.navKey model.session)Route.VideoUnit)
        SucceesEdit (Err err)->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
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
            let _ = Debug.log "code" code
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
            
            (model, Session.changeInterCeptor(Just error))
        GetPart (Ok ok) -> 
            ({model | part = ok.data} ,Cmd.none)
        GetPart (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model, Session.changeInterCeptor(Just error))
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
            let
                error = Api.decodeErrors err
            in
                (model, Session.changeInterCeptor (Just error))
            
        GetLevel (Ok ok ) ->
            ({model | levels = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        TitleText str ->
            let
                old = model.editData
                new = {old | title = str}
            in
            
            ({model | editData = new},Cmd.none)


view : Model -> {title : String ,content : Html Msg}
view model =
    { title = "유어핏 단위 영상 상세"
    , content = 
            div [] [
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


        
    }
  
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.onSucceesSession SessionCheck
    , Session.changes GotSession (Session.navKey model.session)
    ]

validtitle model = 
    if String.isEmpty model.title then
        []
    else
        []
