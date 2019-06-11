module Page.Edit.ApiVideoEdit exposing (..)

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
        session : Session
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
        videoShow = [],
        session = session
    }, Cmd.none)

toSession : Model -> Session
toSession model =
    model.session


type Msg = PopUpOpen | PopUpClose | SelectVideo Int | VideoResult | DeleteItem Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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


view : Model -> {title : String ,content : Html Msg, menu : Html Msg}
view model =
    { title = "외부 API 영상 수정"
    , content = 
        div [ class "apiVideoRegistStyle"] [
            div [] [
                ApiVideo.apiVideoLayout
                "외부 API 영상 수정"
                False
                (routeEdit Route.ApiVideo Route.ApiVideo)
                PopUpOpen
                (selectedVideoList model)
                (List.length model.videoShow)
            ],
            div [] [
                layerPop model
            ]
        ]
        , menu = div [] []
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