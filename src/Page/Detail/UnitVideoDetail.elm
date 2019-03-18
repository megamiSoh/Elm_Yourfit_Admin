module Page.Detail.UnitVideoDetail exposing (..)

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
    , detailList : DetailList
    , levels : List Level
    , instrument : List Level
    , part : List Level
    , exerCode : List Level
    , edit : Bool
    , contentsId: String
    , editData : EditData
    , checkModel : String
    }

type alias EditData = 
    { action_id: Int
    , title:String 
    , difficulty:String
    , exercise:String
    , instrument:String
    , video:String
    , description:String
    , part_details:List String
    }

type alias Data = 
    { data : DetailList}

type alias DetailList = 
    { description: String
    , difficulty_code: String
    , exercise_code: String
    , id: Int
    , instrument_code: String
    , part_detail_code: List PartDetail
    , title: String
    , video: String }

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
    ({
     edit = False
    , contentsId = ""
    , session = session
    , levels = []
    , instrument = []
    , part = []
    , exerCode = []
    , checkModel = ""
    , editData =
        { action_id =  0
        , title = "" 
        , difficulty = ""
        , exercise = ""
        , instrument = ""
        , video = ""
        , description = ""
        , part_details =  []
        } 
    , detailList = 
        { description= ""
        , difficulty_code= ""
        , exercise_code= ""
        , id= 0
        , instrument_code= ""
        , part_detail_code= []
        , title= ""
        , video= "" }
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


editEncoder editData session id= 
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
    Api.post (Endpoint.unitEdit id) (Session.cred session) SucceesEdit body (D.resultDecoder ResultDecoder)

toSession : Model -> Session
toSession model =
    model.session

type Msg 
    =  GetId Encode.Value
    | GetData (Result Http.Error Data)
    | TitleText String
    | GetLevel (Result Http.Error ListData)
    | GetTool (Result Http.Error ListData)
    | GetPart (Result Http.Error ListData)
    | ExerCode (Result Http.Error ListData)
    | SelectTool String
    | SelectPart (String, String)
    | SelectLevel String
    | SelectExerCode String
    | VideoId String
    | DetailOrEdit
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
            let
                old = model.editData
                new = 
                    { }
            in
            
            ({model | edit = not model.edit}
            , editEncoder model.editData model.session model.contentsId)
        SucceesEdit (Ok ok )->
            (model, Route.pushUrl (Session.navKey model.session)Route.VideoUnit)
        SucceesEdit (Err err)->
            let
                error = Api.decodeErrors err
            in
            
            (model,Session.changeInterCeptor (Just error))
        DetailOrEdit ->
            ({model | edit = not model.edit}, Cmd.none)
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
                old = model.detailList
                new =  {old | part_detail_code = old.part_detail_code ++ [{code = code, name = title}]}
                notMatch = List.filter ( \x ->
                        x.code /= code
                    ) old.part_detail_code
                matchTest =   
                    List.filter(\x ->
                        x.code == code
                    ) old.part_detail_code
                nomatchnew = {old | part_detail_code = notMatch}
                editData = 
                    List.map (\x ->
                        x.code
                    ) old.part_detail_code
                editnomatch = 
                    List.filter (\x ->
                        x /= code
                        ) editData
                edit = model.editData
                editNew = {edit | part_details = editData ++ [code]}
                editNotNew = {edit | part_details = editnomatch}
            in
            if  List.length(matchTest) >0 then
                ({model | detailList = nomatchnew, editData = editNotNew},Cmd.none)
            else
                ({model | detailList = new, editData = editNew},Cmd.none)
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
        GetData (Ok ok)->
            let
                old = model.editData
                new = 
                    { old | title = ok.data.title
                    , difficulty = ok.data.difficulty_code
                    , exercise = ok.data.exercise_code
                    , instrument = ok.data.instrument_code
                    , video = ok.data.video
                    , description = ok.data.description
                    , action_id = 
                        case String.toInt(model.contentsId) of
                            Just int ->
                                int
                        
                            Nothing ->
                                0
                    , part_details = 
                        List.map(\x ->
                            x.code 
                            ) ok.data.part_detail_code
                    }
            in
            
            ({model | detailList = ok.data, editData = new}, Cmd.none)
        GetData (Err err) ->
            let
                error = Api.decodeErrors err  
            in
            (model, Session.changeInterCeptor (Just error) )

        GetId id ->
            let
                data = Decode.decodeValue Decode.string id
            in
            case data of
                Ok ok ->
                    ({model | contentsId = ok}, Api.get GetData (Endpoint.unitDetail ok)(Session.cred model.session) (D.unitDetailDecoder Data DetailList PartDetail) )
            
                Err _ ->
                    (model, Cmd.none)

-- inputBtnx btn model thumb title =
--     Unit.inputBtnx btn model.disabled GetFile thumb title

            



view : Model -> {title : String ,content : Html Msg}
view model =
    { title = "유어핏 단위 영상 상세"
    , content = 
        if model.edit then
            div [] [
                Unit.unitVideoForm
                    "유어핏 단위 영상 수정"
                    False
                    TitleText 
                    model.detailList
                    model
                    SelectLevel
                    SelectExerCode
                    SelectTool
                    VideoId
                    SelectPart
                    model.checkModel
                    AreaMsg
                ,
                Page.detailEventBtn "저장" GoEdit Route.VideoUnit
            ]

       else 
            div [] [
                Unit.unitVideoForm
                "유어핏 단위 영상 상세"
                True
                TitleText 
                model.detailList
                model
                SelectLevel
                SelectExerCode
                SelectTool
                VideoId
                SelectPart
                model.checkModel
                AreaMsg
                , 
                Page.detailEventBtn "수정" DetailOrEdit Route.VideoUnit
            ]
       

        
    }
  


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.onSucceesSession SessionCheck
    , Session.changes GotSession (Session.navKey model.session)
    , Api.params GetId
    ]
    