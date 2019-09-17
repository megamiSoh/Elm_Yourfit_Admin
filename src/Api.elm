port module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode as Decode exposing (Decoder, Value, list, decodeString, field, string )
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)
import Task exposing (Task)


type Cred
    = Cred String

credHeader (Cred str) =
    [ Http.header "authorization" ("bearer " ++ str)
    ]

credFormHeader (Cred str) =
    [ Http.header "authorization" ("bearer " ++ str)
    , Http.header "Content-Type" "application/x-www-form-urlencoded"
    ]

credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "token" Decode.string


credInDecoder (Cred token) =
    Decode.succeed Cred
        |> required "token" Decode.string


-- PERSISTENCE

port receiveData : (Value -> msg) -> Sub msg
port onStoreChange : (Value -> msg) -> Sub msg
port retryR : (Value -> msg) -> Sub msg
port params : (Value -> msg) -> Sub msg
port infoCheck : (Value -> msg) -> Sub msg
port saveCheck : (Value -> msg) -> Sub msg
port getInfoParams : (Value -> msg) -> Sub msg
port onSucceesSession : (Value -> msg) -> Sub msg
port onfourChange : (Value -> msg) -> Sub msg 
port sendPageNum : (Value -> msg) -> Sub msg
port next : (Value -> msg) -> Sub msg


port storeCache : Maybe Value -> Cmd msg
port refreshFetchData : () -> Cmd msg
port secRefreshFetch : () -> Cmd msg
port getRefreshToken :  () -> Cmd msg
port getParams : () -> Cmd msg
port saveData : Value -> Cmd msg
port infodata : Value -> Cmd msg
port getInfo : () -> Cmd msg
port thirdRefreshFetch : () -> Cmd msg
port fourRefreshFetch : () -> Cmd msg
port sendData : Value -> Cmd msg
port heightControll : Bool -> Cmd msg
port validationHeight : Bool -> Cmd msg
port showToast : Value -> Cmd msg
port pageNum : Value -> Cmd msg
port youtubeVideo : Value -> Cmd msg
port youtubeControl : () -> Cmd msg
port pgGo : () -> Cmd msg

viewerChanges : (Maybe viewer -> msg) -> Decoder viewer -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))

retryRequest : (Maybe viewer -> msg) -> Decoder viewer -> Sub msg
retryRequest toMsg decoder= 
    retryR (\value -> toMsg (decodeFromChange decoder value))

decodeFromChange : Decoder viewer -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    Decode.decodeValue(viewerDecoder)val
        |> Result.toMaybe

secRetryRequest : (Maybe viewer -> msg) -> Decoder viewer -> Sub msg
secRetryRequest toMsg decoder = 
    onfourChange (\value -> toMsg (decodeFromChange decoder value))

storeCredWith : Cred -> Cmd msg
storeCredWith (Cred token)  =
    let 
        json =
            Encode.object
            [ 
            ( "token", Encode.string token )
            ]
    in
    storeCache (Just json)

logout : Cmd msg
logout =
    storeCache Nothing


application :
        { init : Maybe Cred -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (credDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }

storageDecoder : Decoder Cred
storageDecoder  =
    Decode.field "token" credDecoder

get : (Result Http.Error a -> msg)-> Endpoint -> Maybe Cred -> Decoder a -> Cmd msg
get msg url maybeCred decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson msg decoder
        , headers =
            case maybeCred of
                Just cred ->
                    credHeader cred 

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> Maybe Cred -> (Result Http.Error a -> msg) -> Http.Body -> Decoder a -> Cmd msg
post url maybeCred tagger body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =
            case maybeCred of
                Just cred ->
                     credHeader cred 

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }

formpost : Endpoint -> Maybe Cred -> (Result Http.Error a -> msg) -> Http.Body -> Decoder a -> Cmd msg
formpost url maybeCred tagger body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =
            case maybeCred of
                Just cred ->
                     credFormHeader cred 

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }

login : Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
login body msg decoder =
    post Endpoint.login Nothing msg body decoder


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


-- ERRORS


decodeErrors : Http.Error -> String
decodeErrors error =
    case error of
        Http.BadStatus response ->
                 String.fromInt(response)
        Http.BadUrl response ->
                 response
        Http.Timeout  ->
                 "time out"  
        Http.NetworkError ->
                "networkErr"
        Http.BadBody _ ->
                 "badbody"

newdecodeErrors : Http.Error -> Cmd msg
newdecodeErrors error =
    case error of
        Http.BadStatus response ->
                changeInterCeptor ( Just( String.fromInt(response))) 
        Http.BadUrl response ->
                changeInterCeptor (Just response)
        Http.Timeout  ->
                changeInterCeptor (Just "time out"  ) 
        Http.NetworkError ->
               changeInterCeptor (Just "networkErr")
        Http.BadBody _ ->
                changeInterCeptor (Just "badbody")

changeInterCeptor : Maybe String -> Cmd msg
changeInterCeptor error =
    case error of
        Just err ->
            if err == "401" then
            Cmd.batch[
                refreshFetchData ()
            ]
            else
                Cmd.none
    
        Nothing ->
                Cmd.none

errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors

