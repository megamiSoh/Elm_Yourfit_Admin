module Page.Regist.ApiVideoRegist exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page exposing (..)
import Json.Decode
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
        originVideo : List VideoItem,
        videoShow : List VideoItem,
        session: Session
        , menus : List Menus
    }


type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

init : Session -> (Model, Cmd Msg)
init session = 
    let
        initMapVideo =
            List.map (
                \item -> 
                 {
                     check = item.check,
                     thumb = item.thumb,
                     title = item.title,
                     article = item.article
                 }
                ) videoList
    in
    
    ({
        popup = False,
        videoSelected = [],
        originVideo = initMapVideo,
        videoShow = []
        , menus = []
        , session = session
    }, Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo))

toSession : Model -> Session
toSession model =
    model.session


type Msg = PopUpOpen | PopUpClose | SelectVideo Int | VideoResult | DeleteItem Int | GetMyInfo (Result Http.Error Decoder.DataWrap)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetMyInfo (Err error) ->
            ( model, Cmd.none )

        GetMyInfo (Ok item) -> 
            ( {model |  menus = item.data.menus}, Cmd.none )
        PopUpOpen ->
            ({model | popup = True}, Cmd.none)
        PopUpClose ->
            ({model | popup = False}, Cmd.none)
        SelectVideo idx ->
            let
                after =
                    List.take (idx + 1) model.originVideo 
                before =
                    List.drop ( List.length after - 1 ) after
            in
                ({model | videoSelected = before ++ model.videoSelected}, Cmd.none)
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
    { title = "외부 API 영상 등록"
    , content = 
        div [ class "apiVideoRegistStyle"] [
        div [] [
            ApiVideo.apiVideoLayout
            "외부 API 영상 등록"
            False
            (routeRegist Route.ApiVideo)
            PopUpOpen
            (selectedVideoList model)
            (List.length model.videoShow)
        ],
        div [] [
            layerPop model
        ]
        ]
        , menu =  
    aside [ class "menu"] [
        ul [ class "menu-list yf-list"] 
            (List.map Page.viewMenu model.menus)
    ]
    }
    

selectedVideoList model=
    if List.length model.videoShow > 0 then
        videoResultLayout model
    else
        ApiVideo.noSelected

videoListLayout =
    div [class "apiVideoItem"] (
        List.indexedMap (
        \idx item -> ApiVideo.videoListLayout idx item SelectVideo
    ) videoList
    )

videoResultLayout model=
    div [class "apiVideoItem"] (
        List.indexedMap (
        \idx item ->ApiVideo.videoResultLayout idx item DeleteItem
    ) model.videoShow 
    )


layerPop model=
    if model.popup then
        ApiVideo.apiVideoList False (videoListLayout ) PopUpClose VideoResult
    else
        text ""


videoList =
    [
        {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상1",
            article = "운동은 몸에 좋습니다."
        },
        {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상2",
            article = "운동은 몸에 좋습니다."
        },
         {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상3",
            article = "운동은 몸에 좋습니다."
        },
        {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상4",
            article = "운동은 몸에 좋습니다."
        },
        {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상5",
            article = "운동은 몸에 좋습니다."
        },
        {   check = False,
            thumb = "https://bulma.io/images/placeholders/128x128.png" ,
            title = "운동 영상6",
            article = "운동은 몸에 좋습니다."
        }
    ]