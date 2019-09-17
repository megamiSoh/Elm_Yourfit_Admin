module Session exposing (..)

import Api exposing (..)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time
import Http exposing (..)

type Session
    = LoggedIn Nav.Key Cred
    |  Guest Nav.Key


viewer : Session -> Maybe Cred
viewer session =
    case session of
        LoggedIn _ val ->
            Just val
        Guest _ ->
            Nothing

cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (val)

        Guest _ ->
            Nothing

navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key
        LoggedIn key _ ->
            key

changeInterCeptor : Maybe String -> Cmd msg
changeInterCeptor error=
    case error of
        Just err ->
            if err == "401" then
            Cmd.batch[
                Api.secRefreshFetch ()
            ]
            else
                Cmd.none
    
        Nothing ->
                Cmd.none

retryChange : (Session -> msg) -> Nav.Key -> Sub msg
retryChange toMsg key =
    Api.retryRequest (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder

secRetryChange : (Session -> msg) -> Nav.Key -> Sub msg
secRetryChange toMsg key = 
    Api.secRetryRequest (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder

fromViewer : Nav.Key -> Maybe Cred -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
    