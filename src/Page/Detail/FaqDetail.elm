module Page.Detail.FaqDetail exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Page.Page exposing(..)
import Route exposing(..)
import Html.Events exposing(..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Json.Decode as Decode

type alias Model = {
    session: Session,
    question: String,
    onlyRead: Bool
    , menus : List Menus
    , username : String 
    , id : String
    , detail : Detail
    , validMsg : String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias DetailData = 
    { data : Detail }

type alias Detail = 
    { answer : Maybe String
    , asked_id : Int
    , content : String
    , id : Int
    , is_answer : Bool
    , title : String
    , username : String
    }

answerEncode answer session id=
    let
        newInput = 
            answer
                |> String.replace "&" "%26"
                |> String.replace "%" "%25"

        body = ("answer=" ++ newInput)
            |> Http.stringBody "application/x-www-form-urlencoded"

    in
    Api.post (Endpoint.faqanswer id) (Session.cred session) AnswerComplete body Decoder.result

init : Session -> (Model, Cmd Msg)
init session = ({
        session = session,
        detail = 
            { answer = Nothing
            , asked_id = 0
            , content = ""
            , id = 0
            , is_answer = False
            , title = ""
            , username = ""}
        , question = "",
        onlyRead = False
        , menus = []
        , username = ""
        , id = ""
        , validMsg = ""
    }, Cmd.batch [
        Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
        , Api.getParams ()
    ])

type Msg 
    = Disabled
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | GetId Encode.Value
    | GetDetail (Result Http.Error DetailData)
    | AnswerComplete (Result Http.Error Decoder.Success)
    | Answer String
    | GoAnswer
    | GotSession Session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.params GetId
    , Session.changes GotSession (Session.navKey model.session)]

toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session}, Api.get GetDetail (Endpoint.faqDetail model.id) (Session.cred session) (Decoder.faqDetail DetailData Detail))
        GoAnswer ->
            if String.isEmpty model.question then
            ({model | validMsg = "답변을 입력 해 주세요."}, Cmd.none)
            else if String.length model.question > 250 then
            ({model | validMsg = "250자 이상 입력할 수 없습니다."}, Cmd.none)
            else
            (model, answerEncode model.question model.session model.id)
        Answer str->
            ({model | question = str}, Cmd.none)
        AnswerComplete (Ok ok) ->
            (model,Cmd.batch[Api.get GetDetail (Endpoint.faqDetail model.id) (Session.cred model.session) (Decoder.faqDetail DetailData Detail)
            , Api.showToast (Encode.string "답변이 등록 되었습니다.")])
        AnswerComplete (Err err) ->
            (model,Cmd.none)
        GetDetail (Ok ok) ->
            ({model | detail = ok.data, question = 
                case ok.data.answer of
                    Just answer ->
                        answer
                            |> String.replace "%26" "&"
                            |> String.replace "%25" "%"
                    option2 ->
                        ""
            }, Cmd.none)
        GetDetail (Err err) ->
            let _ = Debug.log "err" err
                error = Api.decodeErrors err
            in
            case error of
                "401" ->
                    (model, Api.thirdRefreshFetch ())
            
                _ ->
                    (model, Cmd.none)   
        GetId id ->
            let
                result = Decode.decodeValue Decode.string id
            in
            case result of
                Ok ok ->
                    ({model | id = ok}, Api.get GetDetail (Endpoint.faqDetail ok) (Session.cred model.session) (Decoder.faqDetail DetailData Detail))   
            
                Err err ->
                    (model, Cmd.none)
        Disabled ->
            let
                old = model.detail
                new = {old | is_answer = not old.is_answer}
            in
            
             ({model | detail = new}, Cmd.none)
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )

view: Model -> {title: String, content : Html Msg, menu : Html Msg}
view model =
    if model.detail.is_answer then
    { title = ""
    , content = 
        div [] [
            pageTitle "1 : 1 문의상세",
            div [class "faqtitle"] [
                span [] [text "제목 : "] ,
                span [] [text model.detail.title],
                span [class "faqDate"] [text "" ]
            ],
            div [class "questionContentTitle"] [
                text "내용 "
            ] ,
            div [class "questionUser"] [ text model.detail.content ],
            div [class "questionTitle"] [text "답변"],
            textarea [ class "questionStyle", disabled model.detail.is_answer, value model.question, placeholder "답변을 등록 해 주세요."] [],
                div [ class "buttons" ] [
                div [ class "button is-primary", onClick Disabled] [text "수정"],
                a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
        ]
        , menu =
            aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }
    else
    { title = ""
    , content = 
        div [] [
            pageTitle "1 : 1 문의상세",
            div [class "faqtitle"] [
                span [] [text "제목 : "] ,
                span [] [text model.detail.title]
            ],
             div [class "questionContentTitle"] [
                text "내용 "
            ] ,
            div [class "questionUser"] [ text model.detail.content ],
            div [class "questionTitle"] [text "답변"],
            div [class "validstyle"][text model.validMsg],
            textarea [ class "questionStyle", disabled model.detail.is_answer, onInput Answer , value model.question, placeholder "답변을 등록 해 주세요."] [],
                div [ class "buttons" ] [
                div [ class "button is-primary",  onClick GoAnswer] [text "저장"],
                a [ class "button is-warning", Route.href (Just Route.Faq) ] [text "취소"]
                ]
        ]
        , menu =
            aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }

