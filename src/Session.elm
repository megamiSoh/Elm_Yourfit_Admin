module Session exposing (..)

import Api exposing (..)
-- import Avatar exposing (Avatar)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
-- import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
-- import Profile exposing (Profile)
import Time
-- import Viewer exposing (Viewer)
import Http exposing (..)
-- TYPES

type Session
    = LoggedIn Nav.Key Cred
    |  Guest Nav.Key


-- INFO


viewer : Session -> Maybe Cred
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            let _ = Debug.log "guest" session
                
            in
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



-- interceptor : Http.Error -> List String
changeInterCeptor error=
    case error of
        Just err ->
            if err == "401" then
            Cmd.batch[
                -- Api.getRefreshToken (), 
                Api.secRefreshFetch ()
            ]
            else
                Cmd.none
    
        Nothing ->
                Cmd.none


-- interceptor toMsg key err msg =
--     if err == "401" then
--         Api.receiveRefreshToken msg
--     else 
--         tokenChange toMsg key
retryChange toMsg key =
    Api.retryRequest (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder



changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder

fromViewer : Nav.Key -> Maybe Cred -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
    