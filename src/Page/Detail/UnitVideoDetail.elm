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
import Page as Page

type alias Model =
    { session : Session
    , detailList : DetailList
    , levels : List Level
    , instrument : List Level
    , part : List Level
    , exerCode : List Level
    , edit : Bool
    , contentsId: String
    , validationErr : String
    , loading : Bool
    , editData : EditData
    , checkModel : String
    , menus : List Menus
    , validErrShow : Bool
    , videoShow : Bool
    , goEdit : Bool
    }

type alias PreviewWrap =  
    { data : DataPreview }
type alias DataPreview = 
    { file : String
    , image : String }

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


type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init: Session -> (Model, Cmd Msg)
init session = 
    ({
     edit = False
    , contentsId = ""
    , session = session
    , levels = []
    , instrument = []
    , menus = []
    , part = []
    , loading = True
    , exerCode = []
    , checkModel = ""
    , validationErr = ""
    , validErrShow = False
    , goEdit = False
    , videoShow = False
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
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
    ]
    )


editEncoder editData session id= 
    let
        newInput text = 
            text
                |> String.replace "&" "%26"
                |> String.replace "%" "%25"
        list =
            ("title="
                ++ newInput (editData.title)
                ++ "&difficulty="
                ++ editData.difficulty
                ++ "&exercise="
                ++ editData.exercise
                ++ "&instrument="
                ++ editData.instrument
                ++ "&video="
                ++ editData.video
                ++ "&description=" ++ (newInput editData.description)
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
    | RetryRequest Session
    | GotSession Session
    | GetMyInfo (Result Http.Error D.DataWrap)
    | GetPreview 
    | PreviewComplete (Result Http.Error PreviewWrap)
    | VideoClose

update : Msg -> Model ->  (Model, Cmd Msg)
update msg model =
    case msg of
        VideoClose ->
                ({model | videoShow = not model.videoShow}, Api.heightControll (not model.videoShow))
        PreviewComplete (Ok ok) ->
            let
                data = 
                    Encode.object
                        [ ("file", Encode.string ok.data.file)
                        , ("image", Encode.string ok.data.image)]
            in
            (model, Api.sendData data)
        PreviewComplete (Err err) ->
            (model, Cmd.none)
        GetPreview ->
            ({model | videoShow = not model.videoShow}, Cmd.batch[Api.get PreviewComplete (Endpoint.unitVideoShow model.contentsId) (Session.cred model.session) (D.videoData PreviewWrap DataPreview)
            , Api.heightControll (not model.videoShow)]
            )
        RetryRequest session ->
            ({model | session = session}, editEncoder model.editData session model.contentsId)
        GotSession session ->
            ({model | session = session}
            , Cmd.batch [
                Api.post Endpoint.unitLevel (Session.cred session) GetLevel Http.emptyBody (D.unitLevelsDecoder ListData Level)
                , Api.post Endpoint.instrument (Session.cred session) GetTool
                Http.emptyBody (D.unitLevelsDecoder ListData Level)
                , Api.post Endpoint.part (Session.cred session) GetPart Http.emptyBody (D.unitLevelsDecoder ListData Level)
                , Api.post Endpoint.exerCode (Session.cred session) ExerCode Http.emptyBody (D.unitLevelsDecoder ListData Level)
                , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
                , Api.get GetData (Endpoint.unitDetail model.contentsId)(Session.cred session) (D.unitDetailDecoder Data DetailList PartDetail)
            ]
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
            ({model | edit = not model.edit, validErrShow = False, loading = True}
            , editEncoder model.editData model.session model.contentsId)
        SucceesEdit (Ok ok )->
            let
                textEncoder = Encode.string "수정이 완료 되었습니다."
            in
            
            (model, Cmd.batch[Route.pushUrl (Session.navKey model.session)Route.VideoUnit, Api.showToast textEncoder])
        SucceesEdit (Err err)->
            let
                error = Api.decodeErrors err
            in
            if error == "401" then
                (model, Api.thirdRefreshFetch ())
            else 
                (model, Cmd.none)
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
            (model, Cmd.none)
        GetPart (Ok ok) -> 
            ({model | part = ok.data} ,Cmd.none)
        GetPart (Err err) ->
            (model, Cmd.none)
        GetTool (Ok ok) ->
            ({model | instrument =ok.data}, Cmd.none)
        GetTool (Err err) ->
            (model, Cmd.none)
        GetLevel (Ok ok ) ->
            ({model | levels = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            (model, Cmd.none)
        TitleText str ->
            let
                old = model.editData
                new = {old | title = str}
            in
            
            ({model | editData = new},Cmd.none)
        GetData (Ok ok)->
            let
                newInput text = 
                    text
                        |> String.replace "%26" "&" 
                        |> String.replace "%25" "%" 

                old = model.editData
                new = 
                    { old | title = (newInput ok.data.title)
                    , difficulty = ok.data.difficulty_code
                    , exercise = ok.data.exercise_code
                    , instrument = ok.data.instrument_code
                    , video = ok.data.video
                    , description = (newInput ok.data.description)
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
            
            ({model | detailList = ok.data, editData = new, loading = False}, Cmd.none)
        GetData (Err err) ->
            (model, Cmd.none)

        GetId id ->
            let
                data = Decode.decodeValue Decode.string id
            in
            case data of
                Ok ok ->
                    ({model | contentsId = ok}, Api.get GetData (Endpoint.unitDetail ok)(Session.cred model.session) (D.unitDetailDecoder Data DetailList PartDetail) )
            
                Err _ ->
                    (model, Cmd.none)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err  
            in
            (model, Session.changeInterCeptor (Just error) )

        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 3) item.data.menus)
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
            



view : Model -> {title : String ,content : Html Msg, menu : Html Msg}
view model =
    if model.edit then
        if model.loading then
            { title = "유어핏 단위 영상 상세"
            , content = 
                    div [] [
                        div [class "adminloadingMask"][Page.spinner]
                    ]
            , menu =  
                aside [ class "menu"] [
                    ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
            }
        else
            if model.videoShow then
            { title = "유어핏 단위 영상 상세"
            , content = 
                    div [] [
                        div [class "adminAuthMask"] []
                        , Unit.unitVideoForm
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
                            GetPreview
                        ,
                        Page.detailEventBtn "저장" GoEdit Route.VideoUnit
                        , Page.validationErr model.validationErr model.validErrShow
                        , Page.videoShow "영상 미리보기" model.videoShow VideoClose
                    ]
            , menu =  
                aside [ class "menu"] [
                    ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
            }
            else
            { title = "유어핏 단위 영상 상세"
            , content = 
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
                            GetPreview
                        ,
                        Page.detailEventBtn "저장" GoEdit Route.VideoUnit
                        , Page.validationErr model.validationErr model.validErrShow
                        , Page.videoShow "영상 미리보기" model.videoShow VideoClose
                    ]
            , menu =  
                aside [ class "menu"] [
                    ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
            }
    else
        if model.loading then
        { title = "유어핏 단위 영상 상세"
        , content = 
                div [] [
                    div [class "adminloadingMask"][Page.spinner]
                ]
        , menu =  
            aside [ class "menu"] [
                ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
            ]

            
        }
        else
            if model.goEdit then
            { title = "유어핏 단위 영상 상세"
            , content = 
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
                        GetPreview
                        , 
                        div [] [
                            Page.detailEventBtn "수정" DetailOrEdit Route.VideoUnit
                        ]
                        , Page.videoShow "영상 미리보기" model.videoShow VideoClose
                    ]
            , menu =  
                aside [ class "menu"] [
                    ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]

                
            }
            else
            { title = "유어핏 단위 영상 상세"
            , content = 
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
                        GetPreview
                        , 
                        div [] [
                            Page.backPageBtn Route.VideoUnit
                        ]
                        , Page.videoShow "영상 미리보기" model.videoShow VideoClose
                    ]
            , menu =  
                aside [ class "menu"] [
                    ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]

                
            }
  


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.params GetId
    , Session.retryChange RetryRequest (Session.navKey model.session)
    ]
    