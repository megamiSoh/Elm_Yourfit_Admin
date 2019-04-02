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
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))



retryRequest toMsg decoder= 
    retryR (\value -> toMsg (decodeFromChange decoder value))




decodeFromChange viewerDecoder val =
    Decode.decodeValue(viewerDecoder)val
        |> Result.toMaybe

secRetryRequest toMsg decoder = 
    onfourChange (\value -> toMsg (decodeFromChange decoder value))

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
-- application :
    -- Decoder (Cred -> viewer)
    -- ->
    --     { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
    --     , onUrlChange : Url -> msg
    --     , onUrlRequest : Browser.UrlRequest -> msg
    --     , subscriptions : model -> Sub msg
    --     , update : msg -> model -> ( model, Cmd msg )
    --     , view : model -> Browser.Document msg
    --     }
    -- -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (credDecoder))
                        -- |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
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


-- storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder  =
    Decode.field "token" credDecoder





-- get : tag ->Endpoint -> Maybe Cred -> Decoder success -> Cmd msg
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


-- put : Endpoint -> Cred -> (WebData success -> msg ) -> Http.Body -> Decoder success -> Cmd msg
put url cred  tagger body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =  credHeader cred 
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }





-- post : Endpoint -> Maybe Cred ->(WebData success -> msg)-> Http.Body -> Decoder success -> Cmd msg
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

    


-- delete : (WebData success ->msg ) -> Endpoint -> Cred -> Http.Body -> Decoder success -> Cmd msg
delete tagger url cred body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =  credHeader cred 
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }



login body msg decoder =
    post Endpoint.login Nothing msg body decoder




decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


-- testDecode : Decoder (Cred -> a) -> Decoder a
testDecode decoder =
    Decode.map (\fromCred cred -> fromCred cred)
        decoder


-- ERRORS




-- decodeErrors : Http.Error -> String
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


changeInterCeptor error=
    case error of
        Just err ->
            if err == "401" then
            Cmd.batch[
                -- Api.getRefreshToken (), 
                secRefreshFetch ()
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

