module Page.UserInfo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, value, checked, type_, disabled, src)
import Page.Page exposing (..)
import Html.Events exposing (..)
import Session exposing (Session)
import Http exposing (..)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as JsonDecodePipeline exposing (..)
import Api as Api
import Api.Decode as Decoder
import Page.Origin.AdminManage as AdminManage
import Page as Page



type alias Model =
    {
        session: Session
        , data : Data
        , problems : String
        , authMenus : List Authmenu
        , authCode : List AuthCode
        , menus : List Menus
        , menuss : List Menuss
        , username : String
    }
type Problem
    = InvalidEntry String
    | ServerError String

type alias Menuss =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias DataWrap = 
    { data : Data }

type alias Data = 
    {
        admin : Admin,
        menus : List Menus
    }
type alias Admin = 
    {
        connected_at : String,
        id : Int,
        joined_at : String,
        nickname : Maybe String,
        username : String,
        profile : Maybe String
    }
type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int
        -- menu_name : String
    }

type alias Authmenus = 
    { data : List Authmenu}

type alias Authmenu =
    { id : Int
    , name : String}

type alias AuthCodes =  
    { data: List AuthCode}

type alias AuthCode = 
    { code : String
    , name : String
    }


init : Session -> (Model, Cmd Msg)
init session = 
    (
    {
     menus = []
    , menuss = []
    , authMenus = []
    , authCode = []
    , username = ""
    , data = {
        admin = {
            connected_at = "",
            id = 0,
            joined_at = "",
            nickname = Nothing,
            username = "" ,
            profile = Nothing
        } ,
        menus = []
    },
    problems = "" ,
    session = session}, 
    Cmd.batch[ managelist session
    , Api.post Endpoint.authCode (Session.cred session)
        GetCode Http.emptyBody (Decoder.authCodeDecoder AuthCodes AuthCode)
    , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
        ]
    )   
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

toSession : Model -> Session
toSession model = 
    model.session


managelist session =
    Api.post Endpoint.myInfo (Session.cred session) GetList Http.emptyBody (Decoder.userInfo DataWrap Data Admin Menus) 


type Msg
    = GetList (Result Http.Error DataWrap)
    | GetMenus (Result Http.Error Authmenus)
    | GetCode (Result Http.Error AuthCodes)
    | GotSession Session
    | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            ( { model | problems = "Err" }
            , Session.changeInterCeptor (Just serverErrors)
            )

        GetMyInfo (Ok item) -> 
            ( {model |  menuss = item.data.menus, username = item.data.admin.username}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , Cmd.batch [
            managelist session
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.post Endpoint.authCode (Session.cred session)
                GetCode Http.emptyBody (Decoder.authCodeDecoder AuthCodes AuthCode)
            , Api.post Endpoint.authMenu (Session.cred session) GetMenus Http.emptyBody (Decoder.authMenusDecoder Authmenus Authmenu)
            ]
            )
        GetMenus (Ok menu) ->
            ({model| authMenus = menu.data }, Cmd.none)
        GetMenus (Err err) ->
            (model,  Cmd.none)
        GetCode (Ok menu) ->
            ({model| authCode =[{code = "메뉴", name = "메뉴"}] ++ menu.data}, Cmd.none)
        GetCode (Err err) ->
            (model,  Cmd.none)
        GetList (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
            ( { model | problems = "Err" }
            , Session.changeInterCeptor (Just serverErrors)
            )

        GetList (Ok item) ->
            let
                old = model.data
                new = {old | admin = item.data.admin, menus = item.data.menus}
            in
            
            ( {model | data = new}, Api.post Endpoint.authMenu (Session.cred model.session) GetMenus Http.emptyBody (Decoder.authMenusDecoder Authmenus Authmenu) )


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "내 정보"
    , content =
        div [ class "box" ]
        [ article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle "내 정보",
                    figure [ class "image is-64x64" ]
                    [ 
                        case model.data.admin.profile of
                            Just image ->
                                img [src image] []
                        
                            Nothing ->
                                i [ class "fas fa-user-circle" ]
                                []
                    ]
                ]
            , div [ class "media-content" ]
                [ 
                    div [ class "content marginTop" ]
                    [ 
                         userInfo model.data.admin.nickname model.data.admin.username model.data.admin.joined_at model.data.admin.connected_at
                    ]
                ]
            ]
            ,
            div [ class " menuAuth" ] [
               adminLayout
                True 
                model.authMenus
                model.authCode
                model.data.menus
                model            ]
        ]
        , menu =  
        aside [ class "menu"] [
        Page.header model.username
        ,ul [ class "menu-list yf-list"] 
            (List.map Page.viewMenu model.menuss)
    ]
    }

adminLayout disabled menu code menuId model=
        div [ class "box" ]
        [ 
            div [] [
                pageTitle "나의 메뉴 권한",
            article [ class "media adminMediaWrap" ]
                [  
                div [class "table"] [
                        thead [] [AdminManage.authTable code],
                        tbody [] ( 
                            List.map (\i ->
                                authTableContent i disabled (
                                    let
                                        get = List.head (List.filter (\x ->
                                                x.menu_id == i.id 
                                                    ) menuId)
                                    in
                                    case get of
                                        Just g ->
                                            g
                                        Nothing ->
                                            { menu_id = 0, menu_auth_code = [] }
                                ) 
                            ) menu 
                        )
                    ]
                        
                ]
            ]
        ]

authTableContent authMenu d x= 
    let
        checkFilter check = List.member check x.menu_auth_code
    in
    
    div[ class "tableRow"][
        div [class "tableCell"] [text authMenu.name ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "10")
                    , disabled d 
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "20")
                    , disabled d 
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "30")
                    , disabled d 
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "40")
                    , disabled d 
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "50")
                    , disabled d 
                    ][]
            ]
        ]
    ]