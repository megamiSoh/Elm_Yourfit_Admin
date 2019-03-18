module Route exposing (..)

-- import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)

-- ROUTING


type Route
    = Home
    | Root
    | Logout
    | Other
    | UserManage
    | AdminManage
    | VideoUnit
    | Video
    | ApiVideo
    | FoodCalorie
    | UserPost
    | Info
    | Faq
    | UserInfo
    | UserMDetail
    | AdminRegist
    | AdminDetail
    | AdminEdit String String
    | UvideoEdit
    | UvideoDetail
    | UvideoRegist
    | VideoRegist
    | VideoEdit
    | VideoDetail
    | ApiVideoRegist
    | ApiDetail
    | ApiEdit
    | InfoRegist
    | InfoDetail
    | InfoEdit
    | FaqDetail
    | FaqRegist
    | FaqEdit
    | Login

-- decoder : Decoder (Cred -> Page)
-- decoder =
--     Decode.succeed Viewer
--         |> custom (Decode.field "username" Avatar.decoder)

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "userManage")
        , Parser.map UserManage (s "userManage")
        , Parser.map AdminManage (s "adminManage")
        , Parser.map VideoUnit (s "videoUnit")
        , Parser.map Video (s "video")
        , Parser.map ApiVideo (s "apiVideo")
        , Parser.map FoodCalorie (s "foodCalorie")
        , Parser.map UserPost (s "userPost")
        , Parser.map Info (s "info")
        , Parser.map Faq (s "faq")
        , Parser.map UserInfo (s "userInfo")
        , Parser.map UserMDetail (s "userManageDetail")
        , Parser.map AdminRegist (s "adminManageRegist")
        , Parser.map AdminEdit (s "adminManage"</>string</>string)
        , Parser.map AdminDetail (s "adminDetail")
        , Parser.map UvideoEdit (s "unitVideoEdit")
        , Parser.map UvideoDetail (s "unitVideoDetail")
        , Parser.map UvideoRegist (s "unitVideoRegist")
        , Parser.map VideoRegist (s "videoRegist")
        , Parser.map VideoEdit (s "videoEdit")
        , Parser.map VideoDetail (s "videoDetail")
        , Parser.map ApiVideoRegist (s "apiVideoRegist")
        , Parser.map ApiDetail (s "apiVideoDetail")
        , Parser.map ApiEdit (s "apiVideoEdit")
        , Parser.map InfoRegist (s "infoRegist")
        , Parser.map InfoDetail (s "infoDetail")
        , Parser.map InfoEdit (s "infoEdit")
        , Parser.map FaqRegist (s "faqRegist")
        , Parser.map FaqDetail (s "faqDetail")
        , Parser.map FaqEdit (s "faqEdit")
        , Parser.map Other (s "other")
        , Parser.map Logout (s "logout")
        , Parser.map Login (s "login")
        ]


-- PUBLIC HELPERS


href : Maybe Route -> Attribute msg
href targetRoute =
    case targetRoute of
        Nothing ->
            Attr.href ("#/login")
        Just r ->
            Attr.href (routeToString r)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)

pushUrl : Nav.Key -> Route ->Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)

reload =
    Nav.reload

fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

-- modifyUrl : Route -> Cmd msg
-- modifyUrl =
--     routeToString >> modifyUrl

-- INTERNAL


routeToString :Route -> String
routeToString page =
    let
        pages =
            case page of
                Home ->
                    []
                Root ->
                    []
                Logout -> 
                    ["logout"]
                UserManage ->
                    ["userManage"]
                AdminManage ->
                    ["adminManage"]
                VideoUnit ->
                    ["videoUnit"]
                Video ->
                    ["video"]
                ApiVideo ->
                    ["apiVideo"]
                FoodCalorie ->
                    ["foodCalorie"]
                UserPost ->
                    ["userPost"]
                Info ->
                    ["info"]
                Faq ->
                    ["faq"]
                UserInfo ->
                    ["userInfo"]
                UserMDetail ->
                    ["userManageDetail"]
                AdminRegist ->
                    ["adminManageRegist"]
                AdminDetail  ->
                    ["adminDetail"]
                AdminEdit detail edit->
                    ["adminManage",detail , edit]
                UvideoEdit ->
                    ["unitVideoEdit"]
                UvideoDetail ->
                    ["unitVideoDetail"]
                UvideoRegist ->
                    ["unitVideoRegist"]
                VideoDetail ->
                    ["videoDetail"]
                VideoEdit ->
                    ["videoEdit"]
                VideoRegist ->
                    ["videoRegist"]
                ApiVideoRegist ->
                    ["apiVideoRegist"]
                ApiDetail ->
                    ["apiVideoDetail"]
                ApiEdit ->
                    ["apiVideoEdit"]
                InfoRegist ->
                    ["infoRegist"]
                InfoDetail ->
                    ["infoDetail"]
                InfoEdit ->
                    ["infoEdit"]
                FaqDetail ->
                    ["faqDetail"]
                FaqRegist ->
                    ["faqRegist"]
                FaqEdit ->
                    ["faqEdit"]
                Other ->
                    ["other"]
                Login ->
                    ["login"]
    in
    "#/" ++ String.join "/" pages