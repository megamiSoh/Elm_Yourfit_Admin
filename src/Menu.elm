-- module Menu exposing (..)

-- import Html exposing (..)
-- import Html.Attributes as Attr exposing (class, value, checked, type_, disabled)
-- import Api.Decode as Decoder
-- import Http exposing (..)
-- import Session exposing (Session)
-- import Api as Api
-- import Api.Endpoint as Endpoint
-- type alias Model =
--     {menus : List Authmenu
--     , session : Session}

-- type Menu -=
-- init : Session -> (Model, Cmd Msg)
-- init session = 
--     (
--     {
--         menus = []
--     , session = session}, 
--     Cmd.batch[ Api.post Endpoint.authMenu (Session.cred session) GetMenus Http.emptyBody (Decoder.authMenusDecoder Authmenus Authmenu )
--         ]
 
--     ) 

-- toSession : Model -> Session
-- toSession model = 
--     model.session

-- type Msg = NoOp
--     | GetMenus (Result Http.Error Authmenus)

-- type alias Authmenus = 
--     { data : List Authmenu}

-- type alias Authmenu =
--     { id : Int
--     , name : String}

-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         NoOp ->
--             ( model, Cmd.none )
--         GetMenus (Ok menu) ->
--             ({model| menus = menu.data }, Cmd.none)
--         GetMenus (Err err) ->
--             (model,  Cmd.none)

            
-- view : Model ->  { title : String, content : Html msg }
-- view model =
--     { title = ""
--     , content = 
--         div [] [text "baka"]
--     }