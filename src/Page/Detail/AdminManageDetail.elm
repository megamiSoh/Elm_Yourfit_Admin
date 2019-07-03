module Page.Detail.AdminManageDetail exposing (..)



import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, value, src, checked, type_, disabled, style)
import Html.Events exposing (..)
import Page.Page exposing (..)
import Session exposing (Session)
import Route exposing (Route)
import Page.Origin.AdminManage as AdminManage
import Api as Api
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Json.Encode as Encode
import Api.Endpoint as Endpoint
import Api.Decode as D
import Page as Page

type alias User =
    {
        no: Int,
        userName : String,
        userId : String,
        createDate: String,
        connectDate : String
    }

type alias AuthCodes =  
    { data: List AuthCode}

type alias AuthCode = 
    { code : String
    , name : String
    }

type alias Model =
    { pop : Bool
    , session : Session
    , admin : Admin
    , menus: List Menus
    , sendAuth : SendAuth
    , authMenus : List Authmenu
    , authCode : List AuthCode
    , isEdit : Bool
    , plz : Menus
    , closeOpen : Bool
    , menuAuthEdit : List MenuAuthEdit
    , userId : String
    , userIntId : Int
    , menuAuth : List MenuAuthCodeList
    , menuss : List Menuss
    , goEdit : Bool
    , goDelete : Bool
    , username : String
    , errType : String
    }
type alias Menuss =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias SendAuth = 
    { menu_auth : List Menus}

type alias Menus =
    { menu_auth_code : List String
    , menu_id : Int
    -- , menu_name : String
    }

type alias MenuAuthCodeList = 
    {codelist : List String}
type alias MenuAuthEditWrap = 
    { menu_auth : List MenuAuthEdit }

type alias MenuAuthEdit = 
    { menu_id : Int
    , menu_auth_code: List String}

type alias Testst = 
    { id : Int
    , name : String
    , auth_code : List String
    }
type alias DataWrap = 
    {data : Data}

type alias ResultDecoder = 
    {result : String}
    
type alias Data = 
    { admin : Admin
    , menus : List Menus}

type alias Admin = 
    { connected_at : String
    , id : Int
    , joined_at : String
    , nickname : Maybe String
    , username : String
    , profile : Maybe String}


type alias Authmenus = 
    { data : List Authmenu}

type alias Authmenu =
    { id : Int
    , name : String}

init : Session -> (Model, Cmd Msg)
init session = 
        (
        { session = session
        , pop = False
        , authMenus = []
        , authCode = []
        , userIntId = 0
        , isEdit = False
        , menuAuth = []
        , menuss = []
        , username = ""
        , closeOpen= False
        , goEdit = False
        , goDelete = False
        , userId = ""
        , plz = {
            menu_auth_code = []
            , menu_id = 0
        }
        , sendAuth = {
           menu_auth = []
        }
        , admin = 
            { connected_at = ""
            , id = 0
            , joined_at = ""
            , nickname = Nothing
            , username = ""
            , profile = Nothing}
        , menus = 
            [
                {
                    menu_auth_code = []
                    , menu_id = 0
                }
            ]
        , menuAuthEdit = 
            []
        , errType = ""
        }, 
        Cmd.batch [
            Api.getParams (),
            Api.post Endpoint.authCode (Session.cred session)
            GetCode Http.emptyBody (D.authCodeDecoder AuthCodes AuthCode)
            , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
        ]
        )

menuAuthEncoder model =
    Encode.object
    [ ("menu_auth_code", (Encode.list Encode.string)model.menu_auth_code )
    , ("menu_id", Encode.int model.menu_id)]

menuAuthEncode form model session= 
    let
        result =
            Encode.object
                [("menu_auth", (Encode.list menuAuthEncoder ) form.menu_auth)
                ]
        body = 
            result    
                |>Http.jsonBody

    in
        Api.post (Endpoint.adminEdit model.userId) (Session.cred model.session) EditAdmin body (D.resultDecoder ResultDecoder)

deleteEncode model session =
    let
        result = 
            Encode.object
                [("user_id", Encode.int model.userIntId) ]
        body = 
            result 
                |> Http.jsonBody
    in
     Api.post Endpoint.adminDelete (Session.cred session) DeleteComplete body (D.resultDecoder ResultDecoder)

toSession : Model -> Session
toSession model =
    model.session


type Msg = PopEvent 
        | ChoiceItem Int
        | GetId Encode.Value
        | GetData (Result Http.Error DataWrap)
        | GetMenus (Result Http.Error Authmenus)
        | GetCode (Result Http.Error AuthCodes)
        | GotSession Session
        | DetailOrEdit String
        | EditAdmin (Result Http.Error ResultDecoder)
        | NoOp (String, Int, List String)
        | DeleteComplete (Result Http.Error ResultDecoder)
        | AuthMenu
        | DeleteAdmin
        | CloseOpen 
        | GetMyInfo (Result Http.Error D.DataWrap)
        -- | RetryRequest Session
        -- | VideoRetry Session

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- RetryRequest session ->
        --     ({model | session = session}, menuAuthEncode model.sendAuth model session)
        -- VideoRetry session ->
        --     ({model | session = session}, deleteEncode model session)
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "myInfo"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 2) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        if auth "40" then
                        ( {model |  menuss = item.data.menus, goEdit = True, goDelete = True, username = item.data.admin.username}, Cmd.none )
                        else
                        ( {model |  menuss = item.data.menus, goEdit = True, username = item.data.admin.username}, Cmd.none )
                    else if auth "40" then
                        ( {model |  menuss = item.data.menus, goDelete = True, username = item.data.admin.username}, Cmd.none )
                    else
                        ( {model |  menuss = item.data.menus, username = item.data.admin.username}, Cmd.none )
                Nothing ->
                    ( {model |  menuss = item.data.menus, username = item.data.admin.username}, Cmd.none )
        CloseOpen ->
            ({model | closeOpen = not model.closeOpen}, Cmd.none)
        DeleteAdmin ->
            (model, Cmd.batch[deleteEncode model model.session])
        AuthMenu ->
            (model, Cmd.none)
        NoOp (str,id , list) ->
            let
                idFilter = 
                    List.filter (\x ->
                        x.menu_id == id
                    )model.menus
                least =
                    List.filter(\x ->
                        x.menu_id /= id
                        )model.menus
                be_id = 
                    List.map (\x ->
                        let
                            valFilter = List.member str x.menu_auth_code
                            new = List.filter(\i -> i/= str) x.menu_auth_code
                        in
                            if valFilter then
                            {x|menu_auth_code = new }
                            else

                            {x|menu_auth_code = x.menu_auth_code ++ [str]}
                    )idFilter
                empty = 
                    List.filter (\x ->
                    x.menu_auth_code /= []
                    ) be_id
                result = empty ++ least
                old = model.plz
                secResult = {old | menu_id = id, menu_auth_code = [str] }
                senda = model.sendAuth
                sendResult = {senda | menu_auth = List.sortBy .menu_id result}
                secSendResult = {senda | menu_auth = List.sortBy .menu_id (least ++ [secResult])}
            in
                if List.length (idFilter) > 0 then
                    ({model | menus = List.sortBy .menu_id result, sendAuth = sendResult},Cmd.none)
                else
                    ({model | menus = List.sortBy .menu_id model.menus ++ [secResult], sendAuth = secSendResult},Cmd.none)
        DetailOrEdit str ->
            if str == "edit" then
            ({model | isEdit = not model.isEdit}, menuAuthEncode model.sendAuth model model.session)
            else
            ({model | isEdit = not model.isEdit}, Cmd.none)
        PopEvent ->
            (model, Cmd.none)
        
        ChoiceItem idx ->
            (model, Cmd.none)    
        
        GetId id ->
            let
                idDecode = 
                    Decode.decodeValue Decode.string id
            in
            case idDecode of
                Ok str ->
                    let
                         
                        intStr = case String.toInt str of
                            Just i ->
                                i
                            Nothing ->
                                0
                    in
                    
                    ({model | userId = str, userIntId = intStr}, Api.get GetData (Endpoint.adminDetail str) (Session.cred model.session) (D.decoder DataWrap Data  Menus Admin))
            
                Err _ ->
                    (model, Cmd.none)
        GetData (Ok item) ->
            ({model | admin = item.data.admin
            , menus = item.data.menus}
            , Api.post Endpoint.authMenu (Session.cred model.session) GetMenus Http.emptyBody (D.authMenusDecoder Authmenus Authmenu))
        GetData (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "getdata"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMenus (Ok menu) ->
           
            ({model| authMenus = menu.data }, Cmd.none)
        GetMenus (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "getmenus"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetCode (Ok menu) ->
            ({model| authCode =[{code = "메뉴", name = "메뉴"}] ++ menu.data}, Cmd.none)
        GetCode (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "getcode"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            ,  case model.errType of
                "myInfo" ->
                    Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            
                "getdata" ->
                    Api.get GetData (Endpoint.adminDetail model.userId) (Session.cred session) (D.decoder DataWrap Data  Menus Admin)
                "getmenus" ->
                    Api.post Endpoint.authMenu (Session.cred session) GetMenus Http.emptyBody (D.authMenusDecoder Authmenus Authmenu)
                "getcode" ->
                    Api.post Endpoint.authCode (Session.cred session)
                    GetCode Http.emptyBody (D.authCodeDecoder AuthCodes AuthCode)
                "editadmin" ->
                    menuAuthEncode model.sendAuth model session
                "delete" ->
                    deleteEncode model session
                _ ->
                     Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
           )
        EditAdmin (Ok item) ->
            (model, Cmd.none)
        EditAdmin (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "editadmin"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        DeleteComplete (Ok item) ->
            (model, Route.pushUrl (Session.navKey model.session) Route.AdminManage)
        DeleteComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "delete"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)


filter model idx = 
    List.filter (\x -> x.id == idx) model.authMenus
    

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    if model.isEdit then
        { title = "관리자 관리"
        , content = 
            div [] [
                adminLayout 
                PopEvent 
                model.admin
                "관리자 수정"
                False
                model.authMenus
                model.authCode
                model.menus
                model,
                
                div [ class "buttons" ] [
                div [ class "button is-primary cursor", onClick (DetailOrEdit "edit") ] [text "저장"],
                a [ class "button is-warning", Route.href (Just Route.AdminManage) ] [text "취소"]
            ]
        ]
        , menu =  
        aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menuss)
                ]
        }
    else
    { title = "관리자 관리"
    , content = 
         div [] [
            adminLayout 
            PopEvent 
            model.admin
            "관리자 상세"
            True
            model.authMenus
            model.authCode
            model.menus
            model,
            div [ class "buttons" ] [
            div [style "margin-right" "5px"] [
                if model.goEdit then
                div [ class "button is-primary cursor", onClick (DetailOrEdit "detail") ] [text "수정"]
                else 
                div [] []
            ]
            , a [ class "button is-warning", Route.href (Just Route.AdminManage) ] [text "취소"]
        ]
       ]
       , menu =  
        aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menuss)
                ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.batch[ Api.params GetId
    , Session.changes GotSession (Session.navKey model.session)
    -- , Session.retryChange RetryRequest (Session.navKey model.session)
    -- , Session.secRetryChange VideoRetry (Session.navKey model.session)
   ]

adminLayout popEvent userData title disabled menu code menuId model=
        div [ class "box" ]
        [ article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle title,
                    figure [ class "image is-64x64 adminImg" ]
                    [ 
                        case userData.profile of
                            Just image ->
                                img [src image] []
                        
                            Nothing ->
                                i [ class "fas fa-user-circle" ]
                                []
                    ]
                ]
            , div [ class "media-content" ]
                [
                    div [] [
                        if model.goDelete then
                        div [class "button is-danger cursor",onClick CloseOpen] [text "관리자 삭제"] 
                        else 
                        div [class "emptyheight"] []
                    ]
                    , AdminManage.body userData "marginTable2"
                    
                ],
                if model.closeOpen then
                            div [] [
                                    button [ class "button is-small"  ] [],
                                    div [ class "layerWrap" ] [],
                                    div [ class "layerPop searchPop" ] [
                                        div [ class "closeBtn", onClick CloseOpen][
                                            i [ class "far fa-times-circle" ]
                                                []
                                        ],
                                            popTitle "관리자를 삭제 하시겠습니까?" ,
                                        
                                        div [ class "buttons" ] [
                                        div [ class "button is-primary cursor", onClick DeleteAdmin ] [text "삭제"],
                                        div [ class "button is-warning cursor", onClick CloseOpen] [text "취소"]
                                    ]
                                    ] 
                                ]
                        else
                            span [] []
            ]
            ,
            div [ class " menuAuth" ] [
                pageTitle "권한 관리",
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
                    , onClick (NoOp ("10", authMenu.id, x.menu_auth_code))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "20")
                    , disabled d 
                    , onClick (NoOp ("20", authMenu.id, x.menu_auth_code))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "30")
                    , disabled d 
                    , onClick (NoOp ("30", authMenu.id, x.menu_auth_code))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "40")
                    , disabled d 
                    , onClick (NoOp ("40", authMenu.id, x.menu_auth_code))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , checked (checkFilter "50")
                    , disabled d 
                    , onClick (NoOp ("50", authMenu.id, x.menu_auth_code))
                    ][]
            ]
        ]
    ]