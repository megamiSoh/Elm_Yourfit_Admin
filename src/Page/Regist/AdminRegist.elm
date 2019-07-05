module Page.Regist.AdminRegist exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_ , checked, disabled, src)
import Html.Events exposing (..)
import Page.Page exposing (..)
import Session exposing (Session)
import Route exposing (Route)
import Page.Origin.AdminManage as AdminManage
import Json.Encode as Encode
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Api.Endpoint as Endpoint
import Api as Api
import Api.Decode as D
import Page as Page

type alias Model =
    { pop : Bool
    , session : Session
    , sendBody : SendBody
    , userData : ResultForm
    , choiceData: GetBody
    , authCode : List AuthCode
    , authMenus : List Authmenu
    , registData : List RegistData
    , registItem : RegistData
    , checkModel : Bool
    , menus : List Menus
    , username : String
    , errType : String
    , choidId : String
    }

type alias RegistData = 
    { menu_id : Int
    , menu_auth_code : List String}

type alias AuthCodes =  
    { data: List AuthCode}

type alias AuthCode = 
    { code : String
    , name : String
    }

type alias SendBody=
    { page : Int
    , per_page : Int
    , username : String
    , nickname : String }  

type alias ResultForm = 
    { data : List GetBody
    , paginate : Paginate}

type alias Paginate = 
    { nickname: String
    , page: Int
    , per_page: Int
    , total_count: Int
    , username : String
    }

type alias Data = 
    {data : User}

type alias User = 
    {user : GetBody}

type alias GetBody =
    { connected_at : String
    , id : Int
    , joined_at : String
    , nickname : Maybe String
    , username : String
    , profile : Maybe String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias Authmenus = 
    { data : List Authmenu}

type alias Authmenu =
    { id : Int
    , name : String}
type alias Success = 
    { result : String}

init : Session -> (Model, Cmd Msg)
init session = 
        (
        { session = session
        , pop = False
        , checkModel = False
        , username = ""
        , registItem = 
            { menu_id = 0
            , menu_auth_code = []}
        , registData = []
        , authCode = [] 
        , menus = []
        , authMenus = []
        , userData = 
            { data = []
            , paginate  = 
                { nickname = ""
                , page = 0
                , per_page = 0
                , total_count = 0
                , username  = ""
                }
            }
        , choiceData = 
            { connected_at = ""
            , id = 0
            , joined_at = ""
            , nickname = Nothing
            , username = ""
            , profile = Nothing
            }
        , sendBody = 
            { page = 1
            , per_page = 10
            , username = ""
            , nickname = ""
            }
        , errType = ""
        , choidId = ""
        }
        , Cmd.batch[ 
       Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)]
        )

encoderSendBody model session= 
    let
        list = 
            Encode.object
                [ ("page", Encode.int model.page)
                , ("per_page", Encode.int model.per_page)
                , ("username", Encode.string model.username)
                , ("nickname", Encode.string model.nickname) ]    
        body= 
            list
                |> Http.jsonBody
    in
    Api.post Endpoint.adminSearch (Session.cred session) GetData body (D.decoderBody ResultForm GetBody Paginate)

authEncoder model=
    Encode.object 
        [ ("menu_id", Encode.int model.menu_id)
        , ("menu_auth_code", (Encode.list Encode.string model.menu_auth_code))]

registEncoder form model session =
    let
        list = Encode.object
            [ ("user_id", Encode.int model.choiceData.id) 
            , ("menu_auth", (Encode.list authEncoder) form) ] 
        body = 
            list    
                |> Http.jsonBody
    in
    Api.post Endpoint.adminR (Session.cred session) RegistSuccess body (D.resultDecoder Success)
    
toSession : Model -> Session
toSession model =
    model.session


type Msg 
    = PopEvent 
    | NickName String
    | Id String
    | Search
    | Reset
    | GetData (Result Http.Error ResultForm)
    | ChoiceItem String
    | GetUser (Result Http.Error Data)
    | GetCode (Result Http.Error AuthCodes)
    | GetMenu (Result Http.Error Authmenus)
    | AdminRegist (Int, String)
    | RegistSuccess (Result Http.Error Success)
    | Regist
    | SessionCheck Encode.Value
    | GotSession Session
    | GetMyInfo (Result Http.Error D.DataWrap)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetMyInfo"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus, username = item.data.admin.username}, 
            Cmd.batch 
            [  Api.post Endpoint.authMenu (Session.cred model.session) GetMenu Http.emptyBody (D.authMenusDecoder Authmenus Authmenu)
            , Api.post Endpoint.authCode (Session.cred model.session) GetCode Http.emptyBody (D.authCodeDecoder AuthCodes AuthCode)
            ] )
        GotSession session ->
            let
                sort =  List.sortBy .menu_id model.registData
            in
            ({model | session = session}
            , case model.errType of
                "GetMyInfo" ->
                    Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (D.muserInfo)
            
                "RegistSuccess" ->
                    registEncoder sort model session
                "GetData" ->
                    encoderSendBody model.sendBody session
                "GetUser" ->
                    Api.get  GetUser (Endpoint.userDetail model.choidId) (Session.cred model.session) (D.userdataDecoder Data User GetBody)
                _ ->
                    Cmd.none
            
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model,Cmd.batch[ 
                        Api.post Endpoint.authCode (Session.cred model.session) GetCode Http.emptyBody (D.authCodeDecoder AuthCodes AuthCode)
                        , Api.post Endpoint.authMenu (Session.cred model.session) GetMenu Http.emptyBody (D.authMenusDecoder Authmenus Authmenu)])
                    Err _ ->
                        (model, Cmd.none)
        Regist ->
            let
                sort =  List.sortBy .menu_id model.registData
            in
            (model, Cmd.batch[registEncoder sort model model.session] )
        RegistSuccess (Ok ok)->
            (model,  Route.pushUrl (Session.navKey model.session) Route.AdminManage)
        RegistSuccess (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "RegistSuccess"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        AdminRegist(menu, code) -> 
            let
                old = model.registItem
                match= List.filter(\x ->
                        x.menu_id == menu 
                    ) model.registData

                notMatch = List.filter(\x ->
                        x.menu_id /= menu
                    ) model.registData
                
                matchItem = 
                    List.map (\x -> 
                        let
                            matchCode = List.member code x.menu_auth_code 
                            newCode = List.filter(\i ->
                                    i /= code
                                )x.menu_auth_code
                        in
                        if matchCode then
                            {x|menu_auth_code = newCode }
                        else
                            {x|menu_auth_code = x.menu_auth_code ++ [code]}
                        )match

                resultFilter = 
                    List.filter (\i ->
                        i.menu_auth_code /= []
                    )matchItem
                result = 
                      resultFilter ++ notMatch

                secResult = {old | menu_id = menu , menu_auth_code = [code]}
                
            in
                if List.length (match) > 0 then
                    ({model | registData = result}, Cmd.none)
                else
                    ({model | registData = model.registData ++ [secResult]} , Cmd.none)
        GetMenu (Ok menu) ->
            let
               old = 
                List.concatMap 
            in
            ({model| authMenus = menu.data }, Cmd.none)
        GetMenu (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetMenu"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetCode (Ok menu) ->
            ({model| authCode =[{code = "메뉴", name = "메뉴"}] ++ menu.data}, Cmd.none)
        GetCode (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetCode"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        PopEvent ->
            ({model | pop = not model.pop}, Cmd.none)   
        NickName str ->
            let
                old = model.sendBody
                new = {old | nickname = str}
            in
            
            ({model | sendBody = new} , Cmd.none)
        Id str ->  
            let
                old = model.sendBody
                new = {old| username = str}
            in
             
            ({model | sendBody = new}, Cmd.none)
        Search ->
            (model , encoderSendBody model.sendBody model.session)
        Reset ->
            (model, Cmd.none)
        GetUser (Ok ok)->
            ({model | choiceData = ok.data.user, pop = False}, Cmd.none)
        GetUser (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetUser"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetData (Ok ok)->
            ({model | userData = ok}, Cmd.none)
        GetData (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "GetData"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        ChoiceItem id ->
            ({model | choidId = id} , Api.get  GetUser (Endpoint.userDetail id) (Session.cred model.session) (D.userdataDecoder Data User GetBody) )

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "관리자 등록"
    , content = 
       div [] [
            adminLayout 
            PopEvent 
            "관리자 등록"
            False
            model.pop
            model.sendBody.nickname
            model.sendBody.username
            model.userData
            ChoiceItem
            model.choiceData
            model.authCode
            model.authMenus,
            div [ class "buttons" ] [
            div [ class "button is-primary", onClick Regist ] [text "등록"],
            a [ class "button is-warning", Route.href (Just Route.AdminManage) ] [text "취소"]
        ]
       ]
       , menu =  
        aside [ class "menu"] [
                    Page.header model.username
                    ,ul [ class "menu-list yf-list"] 
                        (List.map Page.viewMenu model.menus)
                ]
    }
-- menu codemenuId
adminLayout popEvent title disabled popModel nickModel idModel userData choiceMsg choiceData code menus=
        div [ class "box" ]
        [ article [ class "media" ]
            [ div [ class "media-left" ]            
                [   pageTitle title,
                    figure [ class "image is-64x64 adminImg" ]
                    [ 
                        case choiceData.profile of
                            Just image ->
                                img [src image] []
                        
                            Nothing ->
                                i [ class "fas fa-user-circle" ]
                                []
                    ]
                    , div [class "button is-small", onClick PopEvent ] [text "관리자 검색"]
                    , AdminManage.adminRegistPop popModel PopEvent NickName Id nickModel idModel Search Reset userData.data choiceMsg
                ]
            , div [ class "media-content" ]
                [  
                    if choiceData.id == 0 then
                        div [class "emptyMsg"] [text "관리자를 등록 해 주세요."]
                    else
                        AdminManage.body choiceData ""
                    
                ]
            ]
            ,
            div [ class " menuAuth" ] [
                pageTitle "권한 관리",
            article [ class "media adminMediaWrap" ]
                [ 
                    if choiceData.username == "" then
                        div [ class "disabledWrap"] [ text "관리자를 등록 해 주세요."]
                    else
                        span [] []
                    ,div [class "table"] [
                            thead [] [AdminManage.authTable code]
                           ,tbody []
                            -- [authTableContent disabled]
                           ( 
                            List.map (\x-> 
                                    authTableContent x  disabled 
                                ) menus

                            )
                    
                        
                ]
               
           
            ]
        ]
        ]

authTableContent menu d= 
    div[ class "tableRow"][
        div [class "tableCell"] [text menu.name],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , disabled d 
                    , onClick (AdminRegist (menu.id, "10"))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , disabled d 
                    , onClick (AdminRegist (menu.id, "20"))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , disabled d 
                    , onClick (AdminRegist (menu.id, "30"))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , disabled d 
                    , onClick (AdminRegist (menu.id, "40"))
                    ][]
            ]
        ],
        div [class "tableCell"] [
            label [] [
                input 
                    [ type_ "checkbox"
                    , disabled d 
                    , onClick (AdminRegist (menu.id, "50"))
                    ][]
            ]
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Api.onSucceesSession SessionCheck
    , Session.changes GotSession (Session.navKey model.session)
    ]