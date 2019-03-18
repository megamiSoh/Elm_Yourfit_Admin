module Viewer exposing (..)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred)
-- import Email exposing (Email)
import Json.Decode as Decode exposing (Decoder, string, at)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


-- TYPES


type Viewer
    = Viewer Cred


-- INFO


cred : Viewer -> Cred
cred (Viewer val) =
    val

-- cred : Cred -> Viewer
-- cred (Cred val) =
--     val
-- username : Viewer -> Username
-- username (Viewer val) =
--     Api.username val


-- avatar : Viewer -> Avatar
-- avatar (Viewer val _) =
--     val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION


decoder =
    Decode.succeed Viewer
        -- |>  custom (at cred (Viewer val) )
        -- |> custom (Decode.field "username")
        -- |> custom (Decode.field "token" Avatar.decoder)
        -- |> custom (Decode.field "token" Avatar.decoder)
        -- |> required "token" string


store : Viewer -> Cmd msg
store (Viewer credVal) =
            Api.storeCredWith credVal
        
        
        -- avatarVal