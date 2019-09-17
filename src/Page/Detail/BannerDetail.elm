module Page.Detail.BannerDetail exposing(..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http exposing (..)
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Json.Decode as Decode
import Api as Api
import Session exposing(Session)
import Route as Route
import Page as Page
import Page.Page exposing (..)
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Page.BannerManage as BM
import Pagenation exposing(..)

type alias Model = 
    { session : Session 
    , menus : List Menus
    , username : String
    , bannerTitle : String
    , target : String
    , description : String
    , link : String
    , bannerPath : String
    , validationErr : String
    , validErrShow : Bool
    , imageData : ImageDataList
    , path : String
    , bannerShow : Bool
    , page : Int
    , per_page : Int
    , pageNum : Int
    , is_detail : Bool
    , detailId : String
    , errType : String
    , auth : List String
    , bg_color : String
    , is_vertical : String
    , verticalList : List Code
    }

type alias Code =
    { code : String
    , name : String}

type alias DetailData = 
    { data : Detail }

type alias Detail = 
    { description : String
    , id : Int
    , link : Maybe String
    , src : String
    , target : Maybe String
    , title : String 
    , backcolor : Maybe String
    , is_vertical : Bool }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

type alias ImageDataList = 
    { data : List ImageData
    , paginate : ImagePaginate }

type alias ImageData = 
    { file_id : Int
    , inserted_at : String
    , path : String
    , title : String}

type alias ImagePaginate = 
    { end_date : String
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    }

detailApi session id = 
    Api.get DetailComplete (Endpoint.bannerDetail id) (Session.cred session) (Decoder.bannerDetailData DetailData Detail)


imagelistApi page per_page title start_date end_date session = 
    let
       body =
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page)
                , ("title", Encode.string title)
                , ("start_date", Encode.string start_date)
                , ("end_date", Encode.string end_date) ]
                |> Http.jsonBody 
    in
    Api.post Endpoint.bannerImagelist (Session.cred session) ImageListComplete body (Decoder.bannerimageList ImageDataList ImageData ImagePaginate)

init : Session -> (Model , Cmd Msg)
init session = 
    (
    { session = session
    , menus = []
    , username = ""
    , bannerTitle = ""
    , target = ""
    , description = ""
    , link = ""
    , bannerPath = ""
    , validationErr = ""
    , validErrShow = False
    , imageData = 
        { data = []
        , paginate = 
            { end_date = ""
            , title = ""
            , page = 1
            , per_page = 10
            , start_date = ""
            , total_count = 0}
        }
    , path = ""
    , bannerShow = False
    , page = 1
    , per_page = 10
    , pageNum = 1
    , is_detail = True
    , detailId = ""
    , errType = ""
    , auth = []
    , bg_color = ""
    , is_vertical = ""
    , verticalList = 
        [ { code = "true", name = "vertical" }
        , { code = "false", name = "horizontal" }]
    }
    , Cmd.batch
    [ Api.getParams ()
    ]
    )
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                name
                    ++ "="
                    ++ value
            )
        |> String.join "&"

editApi model = 
    let
        body =
            formUrlencoded
            [ ("title", model.bannerTitle)
            , ("description", model.description)
            , ("src", model.bannerPath)
            , ("link", model.link)
            , ("target", model.target) 
            , ("backcolor", model.bg_color)
            , ("is_vertical", model.is_vertical)]
            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.bannerEdit model.detailId) (Session.cred model.session) EditComplete body (Decoder.result)

toSession : Model -> Session
toSession model = 
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[ Api.params ReceiveId
    , Session.changes GotSession (Session.navKey model.session)
    ]

type Msg  
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | NameInput String 
    | TargetEvent String
    | TextAreaInput String
    | BannerLink String
    | BannerUrl String
    | SubmitProduct
    | EditComplete (Result Http.Error Decoder.Success)
    | ImageListComplete (Result Http.Error ImageDataList)
    | ImagePreview String
    | FindBanner
    | PageBtn (Int, String)
    | PageChange
    | SelectUrl String
    | DetailComplete (Result Http.Error DetailData)
    | ReceiveId Encode.Value
    | EditOrDetail
    | GotSession Session
    | BgColorInput String
    | VerticalEvent String

caseString item = 
    case item of
        Just ok ->
            ok
        Nothing ->
            ""

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerticalEvent vertical ->
            ({model | is_vertical = vertical}, Cmd.none)
        BgColorInput color ->
            ({model | bg_color = color}, Cmd.none)
        GotSession session ->
            case model.errType of
            "findBanner" ->
                update FindBanner { model | session = session}
            "edit" ->
                update SubmitProduct {model | session = session }
            _ ->
                ({ model | session = session}, 
                Cmd.batch [detailApi session model.detailId
                     , Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)]
                )
        EditOrDetail ->
            ({model | is_detail = False}, Cmd.none)
        ReceiveId id ->
            case Decode.decodeValue Decode.string id of
                Ok ok ->
                     ({model | detailId = ok},  
                     Cmd.batch [detailApi model.session ok
                     , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)])
            
                Err _ ->
                    (model, Cmd.none)
        DetailComplete (Ok ok) ->
            ({model | description = ok.data.description, target = caseString ok.data.target, bannerTitle = ok.data.title, link = caseString ok.data.link, bannerPath = ok.data.src, bg_color = caseString ok.data.backcolor, is_vertical = (if ok.data.is_vertical then "true" else "false")}, Cmd.none)
        DetailComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            (model, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        SelectUrl url ->
            ({model | bannerPath = url, bannerShow = False , page = 1}, Cmd.none)
        PageChange ->   
            (model, 
            Cmd.batch 
            [ imagelistApi model.page model.per_page "" "" "" model.session
            ,  Api.pageNum (Encode.int model.page)])
        PageBtn (idx, str) ->
            let 
                listAll num = 
                    { model | page = idx,
                    per_page = 10,
                    pageNum = num}
            in
                case str of
                    "prev" ->
                        update PageChange (listAll (model.pageNum - 1))
                    "next" ->
                        update PageChange  (listAll (model.pageNum + 1))
                    "go" -> 
                        update PageChange  (listAll model.pageNum)
                    _ ->
                        (model, Cmd.none)
        FindBanner ->
            ({model | bannerShow = not model.bannerShow, page = 1}, imagelistApi model.page 10 "" 
            "" "" model.session)
        ImagePreview path ->
            ({model | path = path}, Cmd.none)
        ImageListComplete (Ok ok) ->
            ({model | imageData = ok, errType = ""}, Cmd.none)
        ImageListComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "findBanner"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        EditComplete (Ok ok) ->
            ({model | is_detail = True, errType = ""}, Api.showToast (Encode.string "수정 되었습니다."))
        EditComplete (Err err) ->
            let
                error = Api.decodeErrors err
            in
            if error == "401"then
            ({model | errType = "edit"}, Api.changeInterCeptor (Just error))
            else 
            (model, Cmd.none)
        SubmitProduct ->
            if String.isEmpty model.bannerTitle then
                ({model | validationErr = "배너 명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.bannerPath then
                 ({model | validationErr = "배너 Url을 입력 또는 선택해 주세요.", validErrShow = True}, Cmd.none)
            else if String.isEmpty model.description then
                ({model | validationErr = "상품설명을 입력 해 주세요.", validErrShow = True}, Cmd.none)
            else     
            ({model | validationErr = "", validErrShow = False}, editApi model)
        BannerUrl date ->
            ({model | bannerPath = date}, Cmd.none)       
        BannerLink link ->
            ({model | link = link}, Cmd.none)        
            
        TextAreaInput text ->
            ({model | description = text}, Cmd.none)
        TargetEvent selected ->
            ({model | target = selected}, Cmd.none)
        NameInput name ->
            ({model | bannerTitle = name}, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
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
                menuf = List.head (List.filter (\x -> x.menu_id == 12) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                        ( {model |  menus = item.data.menus, username = item.data.admin.username, auth = a.menu_auth_code},Cmd.none)
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username},Cmd.none)

memberAuth num model= List.member num model.auth

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "배너 상세"
    , content =
        div []
            [ columnsHtml [pageTitle "배너 상세"]
           
       , div [] 
        [ div [class "searchWrap"] [
            columnsHtml 
            [ formInputEvent "배너 명" "배너명 입력 해 주세요." model.is_detail NameInput model.bannerTitle
            , formInputEvent "배너 링크" "링크를 입력 해 주세요." model.is_detail BannerLink model.link
            ]
            ,  columnsHtml 
            [ formInputEventBtn "배너 URL" "배너를 선택 해 주세요." model.is_detail BannerUrl model.bannerPath (if model.bannerShow then "찾기창 닫기" else "배너 찾기") FindBanner
            , formInputEvent "배너 Target" "타겟을 입력 해 주세요. " model.is_detail  TargetEvent model.target
            ]
            , columnsHtml [
                bannerList model
            ]
            , columnsHtml [
                textAreaEvent "배너 설명" model.is_detail model.description TextAreaInput
            ]
            , columnsHtml [
                formInputEvent "배경 컬러" "배경 컬러를 입력 해 주세요." model.is_detail BgColorInput model.bg_color
                , noEmptyselectForm "Vertical" model.is_detail model.verticalList VerticalEvent model.is_vertical
            ]
        ]
        ]
        , div [ class "buttons" ] [
            if memberAuth "30" model then
                if model.is_detail then
                div [ class "button is-primary cursur", onClick EditOrDetail ] [text "수정" ]
                else
                div [ class "button is-primary cursur", onClick SubmitProduct ] [text  "저장"]
            else 
            div [][]
            ,
            a [ class "button is-warning", Route.href (Just Route.BM) ] [text "취소"]
        ]
        , validationErr model.validationErr model.validErrShow
        , imagePreview model.path
       
    ]
    , menu =  
        aside [ class "menu"] [
            Page.header model.username
            ,ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
            ]
    }
            
bannerList model = 
    div [ class "bannerListPop", style "height" (if model.bannerShow then "100%" else "")][
        div [class "table"]
        ( [imageheaderTable] ++
        (List.indexedMap (\idx item -> imagetableLayout idx item model NoOp ImagePreview) model.imageData.data)
        )
        , pagination 
            PageBtn
            model.imageData.paginate
            model.pageNum 
    ]

imagePreview path= 
    div [class "previewWrap", style "display" (if String.isEmpty path then "none" else "flex" )][
        div [class "preview_contents"]
        [ div [class "preview_image_container"][
            img [src path][]
        ] 
        , div [class "button is-danger", style "width" "100%", onClick (ImagePreview "")][text "닫기"]]
    ]
imageheaderTable = 
    div [ class "tableRow headerStyle"] [
        div [ class "tableCell" ] [text "No"],
        div [ class "tableCell" ] [text "제목"],
        div [ class "tableCell" ] [text "path"],
        div [ class "tableCell" ] [text "등록일"],
        div [ class "tableCell" ] [text "미리보기"], 
        div [ class "tableCell" ] [text "선택"]
    ]

imagetableLayout idx item model msg previewMsg= 
    div [class "tableRow"] [
            div [ class "tableCell", onClick msg] [
                text ( String.fromInt(model.imageData.paginate.total_count - ((model.imageData.paginate.page - 1) * 10) - (idx)
                )) 
            ],
            div [ class "tableCell", onClick msg] [text item.title],
            div [ class "tableCell", style "width" "50%" , onClick msg] [text item.path],
            div [ class "tableCell", onClick msg] [text (String.dropRight 10 item.inserted_at)],
            div [ class "tableCell", onClick msg] 
            [button [class "button is-small", onClick (previewMsg item.path)][text "미리보기"]],
            div [class "tableCell"][
                button [class "button is-small is-dark ", onClick (SelectUrl item.path)][text "선택"]
            ]
            
        ]