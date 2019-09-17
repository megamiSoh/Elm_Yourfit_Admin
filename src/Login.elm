port module Login exposing (..)
import Api as Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, value, type_)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Api.Endpoint as Endpoint
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Time
import Task

-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , err: String
    , form : Form
    , token : String
    , tstr : Str
    }


type Problem
    = InvalidEntry ValidatedField String


type alias Form =
    { email : String
    , password : String
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , err =""
      , token =""
      , form =
            { email = ""
            , password = ""
            }
      , tstr = 
            { token = ""}
      }
    , Cmd.none
    )



-- VIEW

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "로그인"
    , content = 
            viewForm model.form model
    , menu = div [] []    
    } 

viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str
    in
    li [] [ text errorMessage ]


viewForm : Form -> Model -> Html Msg
viewForm form model =
             div [ class "loginWrap"] [
           h1 [ class "loginText"] [text "Yourfit Admin"],
           div [ class "field" ]
        [ div [class "loginErr"] [
                    text model.err
                ]
            , p [ class "control has-icons-left has-icons-right" ]
            [
                 input [ class "input", type_ "id", placeholder "E-mail", onInput EnteredEmail, value form.email  ]
                []
            , span [ class "icon is-small is-left" ]
                [ i [ class "fas fa-user" ]
                    []
                ]
            , span [ class "icon is-small is-right" ]
                [ i [ class "fas fa-check" ]
                    []
                ]
            ]
        ]
    ,div [ class "field" ]
        [ p [ class "control has-icons-left" ]
            [ input [onKeyDown KeyDown, class "input", type_ "password", placeholder "Password" , onInput EnteredPassword, value form.password  ]
                []
            , span [ class "icon is-small is-left" ]
                [ i [ class "fas fa-lock" ]
                    []
                ]
            ]
        ]
    ,div [ class "field" ]
        [ p [ class "control" ]
            [ button [ class "button is-success is-fullwidth", onClick SubmittedForm ]
                [ text "Login" ]
            ]
        ]
       ]
            


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Cred)
    | GotSession Session
    | KeyDown Int


type alias Str =  {
    token : String
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            if key == 13 then
                update SubmittedForm model
            else
                (model, Cmd.none)

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( model
                    , login validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            if serverErrors == "badbody" then
            ({model | err = "아이디와 비밀번호를 확인 해 주세요."}, Cmd.none)
            else
            ( { model | err = serverErrors }
            , Cmd.none
            )

        CompletedLogin (Ok viewer) ->
            ( model, Cmd.batch[Api.storeCredWith (viewer)] )



        GotSession session ->
            ( { model | session = session }
            , 
            Cmd.batch[Api.refreshFetchData (),  Route.replaceUrl (Session.navKey model.session) Route.Home]
            )

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

    

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Session.changes GotSession (Session.navKey model.session)]


-- FORM



type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []



trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



login : TrimmedForm -> Cmd Msg
login (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "username", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            user
                |> Http.jsonBody
    in
    Api.login body CompletedLogin tokenDecoder 

tokenDecoder : Decoder Api.Cred
tokenDecoder =
    Decode.succeed Api.Cred
        |> required "token" Decode.string

toSession : Model -> Session
toSession model =
    model.session



