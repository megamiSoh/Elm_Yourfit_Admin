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
    { popup : Bool
    , videoSelected : List VideoItem
    -- , originVideo : List VideoItem
    , videoShow : List VideoItem
    , session: Session
    , menus : List Menus
    , videoCode : List VideoCode
    , selectVideoCode: String
    , page_token : String
    , per_page : Int
    , keyword : String
    , videoData : VideoData
    , preview : Bool
    , previewtitle : String
    , selectedVideo : VideoDataInfo
    , selectVideo : String
    , description : String
    , title : String
    , detailData : Data
    , detailId : String
    , isShow : Bool
    , is_edit : Bool
    , videoId : String
    , username : String
    , goEdit : Bool
    , errType : String
    }
    

type alias VideoData = 
    { data : List VideoDataInfo 
    , paginate : Paginate}

type alias VideoDataInfo = 
    { etag : String
    , id : VideoId 
    , kind : String
    , snippet : VideoSnippet
    }

type alias VideoId = 
    { kind : String
    , videoId : String}

type alias VideoSnippet = 
    { channelId : String
    , channelTitle : String
    , description: String
    , liveBroadcastContent : String
    , publishedAt : String
    , thumbnails : Thumbnail
    , title : String
    }
type alias Paginate = 
    { next_token : String
    , page_token : String
    , per_page : Int
    , prev_token: String
    , search_word: String
    , total_count : Int }

type alias Thumbnail = 
    { default : ThumbnailItem
    }
type alias ThumbnailItem =
    { height: Int
    , url: String
    , width : Int }


type alias VideoCodeData = 
    { data : List VideoCode}

type alias VideoCode = 
    { code : String
    , name : String }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias DataWrap = 
    { data : Data }

type alias Data = 
    { content : String
    , id : Int
    , media_id : String
    , snippet : Snippet
    , title : String
    , video_code : String
     }


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
     }

type alias PageInfo = 
    { resultsPerPage : Int
    , totalResults : Int}

type alias ItemSnippet = 
    { categoryId : String
    , channelId : String
    , channelTitle : String
    -- , defaultAudoiLanguage : String
    , description : String
    , liveBroadcastContent : String
    , localized : Local
    , publishedAt : String
    -- , tags : List String
    , thumbnails : Thumb
    , title : String
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


formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"
--  "\"" ++++ "\""
editForm model session =
    let
        list=
            formUrlencoded
            [ ("video_code", model.selectVideoCode )
            , ("title", model.title)
            , ("media_id", model.selectedVideo.id.videoId)
            , ("content", model.description)]
            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.apiedit model.detailId) (Session.cred session) EditComplete list (Decoder.resultDecoder Decoder.Success)

detailApi session id = 
    Api.get DetailComplete (Endpoint.apiDetail id) (Session.cred session)  (Decoder.apiDetailDataWrap DataWrap Data Snippet Items PageInfo ItemSnippet Thumb ThumbItem Local)   

videoDataApi session page_token per_page keyword = 
    let
        body = 
            Encode.object
                [ ("page_token", Encode.string page_token)
                , ("per_page", Encode.int per_page)
                , ("keyword", Encode.string keyword)]
                |> Http.jsonBody
    in
    Api.post Endpoint.youtubeVideoApi (Session.cred session) VideoDataComplete body (Decoder.youtubeVideoData VideoData VideoDataInfo VideoId VideoSnippet Paginate Thumbnail ThumbnailItem )

videoCodeApi session = 
    Api.post Endpoint.videoCode (Session.cred session) VideoCodeComplete Http.emptyBody (Decoder.videoCodeData VideoCodeData VideoCode)

init : Session -> (Model, Cmd Msg)
init session = 
    ({
        popup = False,
        videoSelected = [],
        -- originVideo = initMapVideo,
        videoShow = []
        , menus = []
        , videoCode = []
        , session = session
        , selectVideoCode = "10"
        , page_token = ""
        , per_page = 10
        , keyword = ""
        , videoData = 
            { data = []
            , paginate = 
                { next_token = ""
                , page_token = ""
                , per_page = 0
                , prev_token = ""
                , search_word = ""
                , total_count = 0}
            }
        , preview = False
        , previewtitle = ""
        , selectedVideo = 
            { etag = ""
            , id = 
                { kind = ""
                , videoId = ""
                }
            , kind = ""
            , snippet = 
                { channelId = ""
                , channelTitle = ""
                , description= ""
                , liveBroadcastContent = ""
                , publishedAt = ""
                , thumbnails = 
                    { default  = 
                        { height= 0
                        , url= ""
                        , width = 0 }
                    }
                , title = ""}
            }
        , selectVideo = "Search"
        , description = ""
        , title = ""
        , detailData = 
            { content = ""
            , id = 0
            , media_id = ""
            , snippet = 
                { etag = ""
                , items = []
                , kind = ""
                , pageInfo = 
                    { resultsPerPage = 0
                    , totalResults = 0}
                    }
            , title = ""
            , video_code = ""
            }
        , detailId = ""
        , isShow = True
        , is_edit = False
        , videoId = ""
        , username = ""
        , goEdit = False
        , errType = ""
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
    , Api.next GoNextPage
    , Api.params ReceiveId]

type Msg 
    = PopUpOpen 
    | PopUpClose 
    | SelectVideo String Int
    | VideoResult 
    | DeleteItem Int 
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | GotSession Session
    | VideoCodeComplete (Result Http.Error VideoCodeData)
    | CategoryEvent String
    | VideoDataComplete (Result Http.Error VideoData)
    | VideoSearchInput String
    | VideoSearch
    | VideoPreview Int String
    | EndofVideo
    | GoNextPage Encode.Value
    | TextAreaMsg String
    | TitleInput String
    | EditGo 
    | EditComplete (Result Http.Error Decoder.Success)
    | DetailComplete (Result Http.Error DataWrap)
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
            case List.head ok.data.snippet.items of
                Just get ->
                    let
                        name = model.selectedVideo
                    in
                    
                    ({model | detailData = ok.data, selectVideoCode = ok.data.video_code, title = ok.data.title, description = ok.data.content
                    , keyword = ok.data.media_id
                    , selectVideo = "selectNSearch"
                    }, videoDataApi model.session "" 1 ok.data.media_id)
                Nothing ->
                    (model, Cmd.none)
            
        DetailComplete (Err err) ->
            (model, Cmd.none)
        EditComplete (Ok ok) ->
            ({model | selectVideo = "Search", errType = ""},  videoDataApi model.session "" model.per_page model.videoId)
        EditComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "edit"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        EditGo ->
            if model.is_edit then
            ({model | is_edit = not model.is_edit, isShow = not model.isShow}, editForm model model.session)
            else
            ({model | is_edit = not model.is_edit, isShow = not model.isShow}, Cmd.none)
        TitleInput title ->
            ({model| title = title }, Cmd.none)
        TextAreaMsg article ->
            ({model | description = article}, Cmd.none)
        GoNextPage next ->
            if List.length model.videoData.data >= model.videoData.paginate.total_count || model.selectVideo == "next" then
            (model, Cmd.none)
            else
            ({model | selectVideo = "next"}, videoDataApi model.session model.page_token model.per_page model.keyword)
        EndofVideo ->
            ({model | preview = False, previewtitle = ""}, Api.youtubeControl ())
        VideoPreview idx videoId ->
            ({model | preview = True, previewtitle = String.fromInt idx}, Api.youtubeVideo (Encode.string videoId))
        VideoSearchInput keyword ->
            ({model | keyword = keyword}, Cmd.none)
        VideoSearch ->
            ({model | selectVideo = "Search"},  videoDataApi model.session "" model.per_page model.keyword)
        VideoDataComplete (Ok ok) ->
            case model.selectVideo of
                "next" ->  
                    let
                        old = model.videoData
                        new = 
                            { old | data = old.data ++ ok.data
                            , paginate = ok.paginate}
                    in
                    
                    ({model | videoData = new, page_token = ok.paginate.next_token, selectVideo = ""},Cmd.none) 
                "Search" ->
                    ({model | videoData = ok, page_token = ok.paginate.next_token},Cmd.none)        
            
                "select" ->
                    let
                        data = 
                            List.head ok.data
                    in
                    
                    ({model | selectedVideo = 
                        case data of
                            Just a ->
                                a
                        
                            Nothing ->
                                model.selectedVideo
                    },Cmd.none)
                "selectNSearch" ->
                    let
                        data = 
                            List.head ok.data
                    in
                    
                    ({model | selectedVideo = 
                        case data of
                            Just a ->
                                a
                        
                            Nothing ->
                                model.selectedVideo
                    , videoData = ok, page_token = ok.paginate.next_token
                    },Cmd.none)
                _ ->
                    ({model | videoData = ok},Cmd.none)
            
        VideoDataComplete (Err err) ->
            (model, Cmd.none)
        CategoryEvent category ->
            ({model | selectVideoCode = category}, Cmd.none)
        VideoCodeComplete (Ok ok) ->
            ({model | videoCode = ok.data}, Cmd.none)
        VideoCodeComplete (Err err) ->
            (model, Cmd.none)
        GotSession session ->
            if model.errType == "" then
            ({model | session = session},
                 Cmd.batch[Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
                 , videoCodeApi session]
            )
            else
            update EditGo { model | session = session}
        GetMyInfo (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)

        GetMyInfo (Ok item) -> 
            let
                menuf = List.head (List.filter (\x -> x.menu_id == 5) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        ( {model |  menus = item.data.menus, goEdit = True, username = item.data.admin.username}, videoCodeApi model.session )
                    else
                        ( {model |  menus = item.data.menus, username = item.data.admin.username}, videoCodeApi model.session )
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username}, videoCodeApi model.session )
            -- ( {model |  menus = item.data.menus}, 
            -- Cmd.batch [
            -- ] )
        PopUpOpen ->
            ({model | popup = True}, 
            videoDataApi model.session model.page_token model.per_page model.keyword
            )
        PopUpClose ->
            ({model | popup = False}, Cmd.none)
        SelectVideo id idx->
            -- let
            --     after =
            --         List.take (idx + 1) model.originVideo 
            --     before =
            --         List.drop ( List.length after - 1 ) after
            -- in
            --     ({model | videoSelected = before ++ model.videoSelected}, Cmd.none)
            ({model | selectVideo = "select", previewtitle = String.fromInt idx, videoId = id}, videoDataApi model.session "" model.per_page id)
        VideoResult ->
            ({model | videoShow = model.videoSelected, popup = False}, Cmd.none)

        DeleteItem idx ->
            let
                after =
                    List.take idx model.videoSelected 
                before =
                    List.drop ( idx + 1 ) model.videoSelected
            in
                ({model | videoShow = after ++ before, videoSelected = after++before}, Cmd.none)


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "외부 API 영상 정보"
    , content = 
        div [ class "apiVideoRegistStyle"] [
        div [] 
            [ columnsHtml [
            -- pageTitle "외부 API 영상 등록" 
            ]
            , layerPop model
            , ApiVideo.apiVideoLayout
            "외부 Api 영상 정보"
            model.isShow
            CategoryEvent
            model.videoCode
            model.selectVideoCode
            model.selectedVideo
            model.description
            TextAreaMsg
            model.title
            TitleInput
            ]
            , div [ class "buttons" ] [
                    div [ class "button is-primary cursur", onClick EditGo] [text (if model.is_edit then "등록" else "수정")],
                    a [ class "button is-warning", Route.href (Just Route.ApiVideo) ] [text "취소"]
                ]
        ]
        , menu =  
            aside [ class "menu"] [
                Page.header model.username
                ,ul [ class "menu-list yf-list"] 
                    (List.map Page.viewMenu model.menus)
                ]
    }
    

selectedVideoList model=
    if List.length model.videoShow > 0 then
        videoResultLayout model
    else
        ApiVideo.noSelected

videoListLayout model =
    div [class "apiVideoItem" ] [
        if List.isEmpty model.videoData.data then
        div [][text "키워드 검색을 통해, 영상을 선택 해 주세요."]
        else 
        div [style "overflow-y""scroll", style "height" "95%", style "width" "100%", id "searchHeight"](
        List.indexedMap (
        \idx item -> ApiVideo.videoListLayout idx item SelectVideo VideoPreview model.previewtitle model.preview EndofVideo  model.isShow
    ) model.videoData.data 
    )]

videoResultLayout model=
    div [class "apiVideoItem"] (
        List.indexedMap (
        \idx item ->ApiVideo.videoResultLayout idx item DeleteItem
    ) model.videoShow 
    )


layerPop model=
        ApiVideo.apiVideoList False (videoListLayout model ) PopUpClose VideoResult VideoSearchInput VideoSearch model.preview model.isShow (if model.isShow then "선택 영상 미리보기" else "외부 영상 선택")
