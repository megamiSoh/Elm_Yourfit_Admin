module Page.Detail.ApiVideoDetail exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Page.Origin.ApiVideo as ApiVideo
import Session exposing (Session)
import Route exposing(..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder

type alias VideoItem = {
    check : Bool,
    thumb : String,
    title : String,
    article : String
    }

type alias Model = 
    {
        popup : Bool,
        videoSelected : List VideoItem,
        -- originVideo : List VideoItem,
        videoShow : List VideoItem,
        session : Session
        , menus : List Menus
        , detailId : String
    }

type alias Data = 
    { content : String
    , id : Int
    , media_id : String
    , snippet : Snippet
    , title : String
    , video_code : Int }

type alias Snippet = 
    { etag : String
    , items : List Items
    , kind : String
    , pageInfo : PageInfo}

type alias Items =
    { etag : String
    , id : String
    , kind : String
    , snippet : ItemSnippet
    , title : String
     }

type alias PageInfo = 
    { resultsPerPage : Int
    , totalResults : Int}

type alias ItemSnippet = 
    { categoryId : String
    , channelId : String
    , channelTitle : String
    , defaultAudoiLanguage : String
    , description : String
    , liveBroadCastContent : String
    , localized : Local
    , publishedAt : String
    , tags : List String
    , thumbnails : Thumb
    }

type alias Thumb = 
    {default : ThumbItem}

type alias ThumbItem = 
    { height : Int
    , url : String
    , width: Int}

type alias Local = 
    { description: String
    , title : String}


type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

detailApi session id = 
    Api.post (Endpoint.apiDetail id) (Session.cred session) DetailComplete Http.emptyBody  (Decoder.apiDetailData Data Snippet Items PageInfo ItemSnippet Thumb ThumbItem Local)
    

init : Session -> (Model, Cmd Msg)
init session = 
    -- let
        -- initMapVideo =
        --     List.map (
        --         \item -> 
        --          {
        --              check = item.check,
        --              thumb = item.thumb,
        --              title = item.title,
        --              article = item.article
        --          }
        --         ) videoList
    -- in
    
    ({
        popup = False,
        videoSelected = [],
        -- originVideo = initMapVideo,
        videoShow = [],
        session = session 
        , menus = []
        , detailId = ""
    }, Cmd.batch
    [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    , Api.getParams ()])

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.params ReceiveId]

type Msg 
    = PopUpOpen 
    | PopUpClose 
    | SelectVideo Int 
    | VideoResult 
    | DeleteItem Int 
    | GotSession Session
    | GetMyInfo (Result Http.Error Decoder.DataWrap)
    | DetailComplete (Result Http.Error Data)
    | ReceiveId Encode.Value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveId id -> 
            case Decode.decodeValue Decode.string id of
                Ok ok ->
                     ({model | detailId = ok},  detailApi model.session ok)
            
                Err _ ->
                    (model, Cmd.none)
           
        DetailComplete (Ok ok) ->
            (model, Cmd.none)
        DetailComplete (Err err) ->
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session},
                 Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            )
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )
        PopUpOpen ->
            (model, Cmd.none)
        PopUpClose ->
            (model, Cmd.none)
        SelectVideo idx ->
            (model, Cmd.none)
        VideoResult ->
            (model, Cmd.none)
        DeleteItem idx ->
            (model, Cmd.none)


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "외부 API 영상 상세"
    , content = 
        div [ class "apiVideoRegistStyle"] [
        div [] [
            -- ApiVideo.apiVideoLayout
            -- "외부 API 영상 상세"
            -- True
            -- (routeDetail Route.ApiEdit Route.ApiVideo)
            -- PopUpOpen
            -- (selectedVideoList model)
            -- (List.length model.videoShow)
        -- ],
        -- div [] [
        --     layerPop model
        ]
        ]
        , menu =  
        aside [ class "menu"] [
            ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
        ]
    }
    

-- selectedVideoList model=
--     if List.length model.videoShow > 0 then
--         videoResultLayout model
--     else
--         ApiVideo.noSelected

-- videoListLayout =
--     div [class "apiVideoItem"] (
--         List.indexedMap (
--         \idx item -> ApiVideo.videoListLayout idx item SelectVideo
--     ) videoList
--     )

-- videoResultLayout model=
--     div [class "apiVideoItem"] (
--         List.indexedMap (
--         \idx item ->ApiVideo.videoResultLayout idx item DeleteItem
--     ) model.videoShow 
--     )


-- layerPop model=
--     if model.popup then
--         ApiVideo.apiVideoList False (videoListLayout ) PopUpClose VideoResult
--     else
--         text ""


-- videoList =
--     [
--         {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상1",
--             article = "운동은 몸에 좋습니다."
--         },
--         {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상2",
--             article = "운동은 몸에 좋습니다."
--         },
--          {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상3",
--             article = "운동은 몸에 좋습니다."
--         },
--         {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상4",
--             article = "운동은 몸에 좋습니다."
--         },
--         {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상5",
--             article = "운동은 몸에 좋습니다."
--         },
--         {   check = False,
--             thumb = "https://bulma.io/images/placeholders/128x128.png" ,
--             title = "운동 영상6",
--             article = "운동은 몸에 좋습니다."
--         }
--     ]