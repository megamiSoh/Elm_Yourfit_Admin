module Page.BannerManage exposing(..)

import Browser exposing (..)
import Html exposing (..)
-- import Html.Attributes exposing (..)
import Http exposing (..)
import Html.Events exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
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
import File exposing(File)
import Task
import Pagenation exposing(..)

type alias Model = 
    { session : Session 
    , menus : List Menus
    , username : String
    , firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , show : Bool
    , start_date : String
    , end_date : String
    , today : Maybe Date
    , endday : Maybe Date
    , endShow : Bool
    , todaySave : String
    , dateModel : String
    , listData : ProductList
    , page : Int
    , per_page : Int
    , name : String
    , is_pay : String
    , selectModel : List { code : String , name : String}
    , pageNum : Int
    , is_use : Bool
    , selected_item : String
    , title : String
    , imageData : ImageDataList
    , path : String
    , showRegist : Bool
    , filename : String
    , registTitle : String
    , previewUrl : String
    , getFile : List File.File
    }
type alias ProductList = 
    { data : List Data
    , paginate : Paginate}

type alias Data = 
    { day_name : Int
    , id : Int
    , inserted_at : String
    , is_pay : Bool
    , is_use: Bool
    , name : String
    , price : Int
    , product_code_name : String }

type alias Paginate = 
    { end_date : String
    , is_use : Maybe Bool
    , name : String
    , page : Int
    , per_page : Int
    , product_code : String
    , start_date : String
    , total_count : Int}

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

type alias ImgRegist = 
    { data : ImgRegistData }
type alias ImgRegistData =
    { path : String}


type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

listApi page per_page name is_pay start_date end_date session = 
    let
        body =
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page)
                , ("name", Encode.string name)
                , ("is_pay", 
                    if is_pay == "true" then Encode.bool True
                    else if is_pay == "false" then Encode.bool False
                    else Encode.string is_pay)
                , ("start_date", Encode.string start_date)
                , ("end_date", Encode.string end_date) ]
                |> Http.jsonBody
    in
    Api.post Endpoint.productList (Session.cred session) ListComplete body 
    (Decoder.productList ProductList Data Paginate)

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
imageregistApi title session getFile = 
    let
    
        body = 
            (List.map (Http.filePart "image") getFile) ++
                -- Http.filePart "file" filename
            (List.map (Http.stringPart "title") [title])
            
            -- (List.map (Http.filePart "file") getFile)
             |> Http.multipartBody  
    in
    Api.post Endpoint.bannerimgregist (Session.cred session) ImageRegistComplete body (Decoder.imgRegist ImgRegist ImgRegistData)

init : Session -> (Model , Cmd Msg)
init session = 
    let
        
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    (
    { session = session
    , menus = []
    , username = ""
    , datePickerData = datePickerData
    , endDatePickerData = endDatePickerData
    , firstSelectedDate = Nothing
    , secondSelectedDate = Nothing
    , start_date = ""
    , end_date = ""
    , endday = Nothing
    , today = Nothing
    , endShow = False
    , todaySave = ""
    , show = False
    , dateModel = "all"
    , listData = 
        { data = []
        , paginate = 
            { end_date = ""
            -- , is_pay = Nothing
            , is_use = Nothing
            , name = ""
            , page = 1
            , per_page = 10
            , product_code = ""
            , start_date = ""
            , total_count = 0}
        }
    , page = 1
    , per_page = 10
    , name = ""
    , is_pay = "null"
    , selectModel = 
        [
            { code = "null"
            , name = "전체"} ,
            { code = "true"
            , name = "유료"} ,
            { code = "false"
            , name = "무료"}
        ]
    , pageNum = 1
    , is_use = False
    , selected_item = "banner"
    , title = ""
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
    , showRegist = False
    , filename = ""
    , registTitle = ""
    , previewUrl = ""
    , getFile = []
    }
    , Cmd.batch
    [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    ]
    )

toSession : Model -> Session
toSession model = 
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.saveCheck GoDetailComplete]

type Msg  
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.DataWrap) 
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | ListComplete (Result Http.Error ProductList)
    | NameInput String
    | Search
    | Reset
    | IsPaySelect String
    | IsUse Bool Int
    | IsActiveComplete (Result Http.Error Decoder.Success)
    | GoDetail Int
    | GoDetailComplete Encode.Value
    | GotSession Session
    | TabSelected String
    | ImageListComplete (Result Http.Error ImageDataList)
    | ImagePreview String
    | ImageRegistPop
    | GetFile (List File.File)
    | GotPreviews (List String)
    | RegistTitle String
    | GoRegist
    | ImageRegistComplete (Result Http.Error ImgRegist)
    | ImageSearch
    | ImageReset
    | PageBtn (Int, String)
    | PageChange

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageChange ->   
            (model, 
            Cmd.batch 
            [ imagelistApi model.page model.per_page model.title model.start_date model.end_date model.session
            ,  Api.pageNum (Encode.int model.page)])
        PageBtn (idx, str) ->
            let 
                list num = 
                    { model | page = idx,
                    per_page = 10,
                    title= model.title,
                    start_date = model.start_date,
                    end_date = model.end_date,
                    pageNum = num}
                listAll num = 
                    { model | page = idx,
                    per_page = 10,
                    title= model.title,
                    start_date = "",
                    end_date = "",
                    pageNum = num}
            in
            if model.dateModel == "all" then
                case str of
                    "prev" ->
                        update PageChange (listAll (model.pageNum - 1))
                    "next" ->
                        update PageChange  (listAll (model.pageNum + 1))
                    "go" -> 
                        update PageChange  (listAll model.pageNum)
                    _ ->
                        (model, Cmd.none)
            else
                case str of
                    "prev" ->
                        update PageChange (list (model.pageNum - 1))
                    "next" ->
                        update PageChange (list (model.pageNum + 1))
                    "go" -> 
                        update PageChange (list model.pageNum)
                    _ ->
                        (model, Cmd.none)
        ImageSearch ->
            let
                old = model.listData.paginate
                date = {old | page = 1}
            in
            if model.dateModel == "all" then
            ({model | pageNum = 1, page = 1, start_date = "", end_date = ""}, 
            Cmd.batch
            [ Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
            else
            ({model | page = 1, pageNum = 1}, 
            Cmd.batch 
            [ Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
        ImageReset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
            ({model | page = 1, title = "",  datePickerData = datePickerData, endDatePickerData = endDatePickerData, dateModel ="all"}, 
            Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        ImageRegistComplete (Ok ok) ->
            ({model | getFile = [], registTitle = "", filename = "", showRegist = False, previewUrl = ""}
            , imagelistApi model.page model.per_page model.title model.start_date model.end_date model.session) 
        ImageRegistComplete (Err err) ->
            (model, Cmd.none) 
        GoRegist ->
            (model, 
            imageregistApi model.registTitle model.session model.getFile
            )
        RegistTitle name ->
            ({model | registTitle = name}, Cmd.none)
        GotPreviews url ->
            let
                getUrl =
                    List.head (url)
            in
            case getUrl of
                Just ok ->
                    ({model | previewUrl = ok}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)
        GetFile filename ->
            let
                title =
                    List.head (filename)
            in
            case title of
                Just a ->
                   
                    ({model | filename = File.name a, getFile = filename}
                    -- , Cmd.none
                   , Task.perform GotPreviews <| Task.sequence <|
                    List.map File.toUrl filename
                    )
            
                Nothing ->
                    (model, Cmd.none)
          
        ImageRegistPop ->
            ({model | showRegist = not model.showRegist}, Cmd.none)
        ImagePreview path ->
            ({model | path = path}, Cmd.none)
        ImageListComplete (Ok ok) ->
            ({model | imageData = ok}, Cmd.none)
        ImageListComplete (Err err) ->
            (model, Cmd.none)
        TabSelected tab ->
            ({model | selected_item = tab}, Cmd.none)
        GotSession session ->
            ({ model | session = session}, 
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            )
        GoDetail id ->
            (model, Cmd.batch[Api.saveData (Encode.string (String.fromInt id))
            ])
        GoDetailComplete go ->
            (model,Route.pushUrl (Session.navKey model.session) Route.PD)
        IsActiveComplete (Ok ok) ->
            if model.dateModel == "all" then
                (model,  listApi model.page model.per_page model.name model.is_pay "" "" model.session)
            else
                (model, listApi model.page model.per_page model.name model.is_pay model.start_date model.end_date model.session)
        IsActiveComplete (Err err) ->
            (model, Cmd.none)
        IsUse use id ->
            let _ = Debug.log "use" use
                body = Encode.object 
                    [ ("is_use", Encode.bool use) ]
                    |> Http.jsonBody
            in
            (model, Api.post (Endpoint.productActive (String.fromInt id)) (Session.cred model.session) IsActiveComplete body Decoder.result)
        IsPaySelect pay ->
            ({model | is_pay = pay}, Cmd.none)
        Search ->
            let
                old = model.listData.paginate
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, 
            Cmd.batch
            [ listApi 1 model.per_page model.name model.is_pay "" "" model.session
            ,Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
            else
            ({model | page = 1, pageNum = 1}, 
            Cmd.batch 
            [ listApi 1 model.per_page model.name model.is_pay  model.start_date model.end_date model.session
            ,Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.muserInfo)
            , Api.pageNum (Encode.int 1)])
        Reset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
            ({model | page = 1, name = "", is_pay = "null",  datePickerData = datePickerData, endDatePickerData = endDatePickerData, dateModel ="all"}, 
            Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        NameInput name ->
            ({model | title = name}, Cmd.none)
        ListComplete (Ok ok) ->
            ({model | listData = ok}, Cmd.none)
        ListComplete (Err err) ->
            (model, Cmd.none)
        DateValue str->
            ({model | dateModel = str},Cmd.none)
        EndShow ->
            ( {model | endShow = not model.endShow, show = False}, Cmd.none )
        Show ->
            ( {model | show = not model.show, endShow = False }, Cmd.none )
        EndDatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.endDatePickerData
                |> (\( data, cmd ) ->
                        ( { model | endDatePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                        case datePickerMsg of    
                            CancelClicked ->
                                ({newModel | endShow = False}, cmd)
                                                        
                            SubmitClicked currentSelectedDate ->
                                let
                                    new = getFormattedDate (Just currentSelectedDate) model.endday
                                in
                                
                                ( { newModel | secondSelectedDate = Just currentSelectedDate, 
                                end_date = new, 
                                endShow = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new =  getFormattedDate (Just todaydate) model.endday
                                in
                                
                                ( { newModel | endday = Just todaydate, secondSelectedDate = Just todaydate, 
                                end_date = new , 
                                todaySave = getFormattedDate (Just todaydate) model.endday}
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        DatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.datePickerData
                |> (\( data, cmd ) ->
                        ( { model | datePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )

                |> (\( newModel, cmd ) ->
                        case datePickerMsg of         
                            CancelClicked ->
                                ({newModel | show = False}, cmd)                   
                            SubmitClicked currentSelectedDate ->
                                let
                                     new =  getFormattedDate (Just currentSelectedDate) model.today
                                in
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, start_date = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new =getFormattedDate (Just todaydate) model.today
                                in
                                
                                ( { newModel | today = Just todaydate, start_date = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
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
                menuf = List.head (List.filter (\x -> x.menu_id == 5) item.data.menus)
            in
            case menuf of
                Just a ->
                    let
                        auth num = List.member num a.menu_auth_code
                    in
                    if auth "30" then
                        ( {model |  menus = item.data.menus, username = item.data.admin.username},
                        imagelistApi model.page model.per_page model.title model.start_date model.end_date model.session)
                    else
                        ( {model |  menus = item.data.menus, username = item.data.admin.username},
                        imagelistApi model.page model.per_page model.title model.start_date model.end_date model.session)
                Nothing ->
                    ( {model |  menus = item.data.menus, username = item.data.admin.username},
                    imagelistApi model.page model.per_page model.title model.start_date model.end_date model.session)


view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "배너 관리"
    , content =
        div [class "bannerManage_container"]
            [   --tab
                div [class "selectedTab"]
                [ p [class (if model.selected_item == "banner" then "selectedTab_selected" else ""), onClick (TabSelected "banner")][text "배너관리"]
                , p [class (if model.selected_item == "image" then "selectedTab_selected" else ""), onClick (TabSelected "image")][text "이미지관리"]
                ] ,
                -- 배너관리
                case model.selected_item of
                    "banner" ->
                       div [ class "banner_container"][
                            columnsHtml [pageTitle "배너 관리"]
                                , div [ class "searchWrap" ] [
                                    columnsHtml [
                                        searchDate 
                                            "등록일"
                                            Show 
                                            (datepicker model DatePickerMsg) 
                                            (getFormattedDate model.firstSelectedDate model.today) 
                                            EndShow (endDatePicker model EndDatePickerMsg) 
                                            (getFormattedDate model.secondSelectedDate model.endday) DateValue model.dateModel
                                            (if model.dateModel == "all" then
                                                "readOnly"
                                            else
                                                ""
                                            )
                                    ]
                                    , columnsHtml [
                                        formInputEvent "제목명" "제목 명을 입력 해 주세요." False NameInput model.name,
                                        noEmptyselectForm "유 / 무료" False model.selectModel IsPaySelect model.is_pay,
                                        searchB Search Reset 
                                    ]
                            ]
                            , registRoute "상품 등록" Route.PR
                            , dataCount (String.fromInt model.imageData.paginate.total_count)
                            , if List.length model.listData.data > 0 then
                            div [class "table"]
                            ( [headerTable] ++
                            (List.indexedMap (\idx item -> tableLayout idx item model) model.listData.data)
                            )
                            else
                            div [class "table"] [
                                headerTable 
                                , tr[][
                                td [colspan 8, class "noSearch"] [text "검색 결과가 없습니다."]
                                ]
                            ]
                        ] 
                
                    "image" ->
                        div [ class "image_container"][
                        columnsHtml [pageTitle "이미지 관리"]
                            , div [ class "searchWrap" ] [
                                columnsHtml [
                                    searchDate 
                                        "등록일"
                                        Show 
                                        (datepicker model DatePickerMsg) 
                                        (getFormattedDate model.firstSelectedDate model.today) 
                                        EndShow (endDatePicker model EndDatePickerMsg) 
                                        (getFormattedDate model.secondSelectedDate model.endday) DateValue model.dateModel
                                        (if model.dateModel == "all" then
                                            "readOnly"
                                        else
                                            ""
                                        )
                                ]
                                , columnsHtml [
                                    formInputEvent "제목명" "제목 명을 입력 해 주세요." False NameInput model.title,
                                    searchB ImageSearch ImageReset 
                                ]
                            ]
                            , registClick "이미지 등록" ImageRegistPop
                            , dataCount (String.fromInt model.imageData.paginate.total_count)
                            , if List.length model.imageData.data > 0 then
                            div [class "table"]
                            ( [imageheaderTable] ++
                            (List.indexedMap (\idx item -> imagetableLayout idx item model) model.imageData.data)
                            )
                            else
                            div [class "table"] [
                                imageheaderTable 
                                , tr[][
                                td [colspan 8, class "noSearch"] [text "검색 결과가 없습니다."]
                                ]
                            ]
                            , pagination 
                                PageBtn
                                model.imageData.paginate
                                model.pageNum 
                            , imagePreview model.path
                            , imageRegist model.showRegist model.registTitle model.filename model.previewUrl
                        ]
                    _ ->
                        div [][
                            text "목록 불러오기 실패"
                        ]
            ]
    , menu =  
        aside [ class "menu"] [
            Page.header model.username
            ,ul [ class "menu-list yf-list"] 
                (List.map Page.viewMenu model.menus)
            ]
    }
            

headerTable = 
      div [ class "tableRow headerStyle"] [
         div [ class "tableCell" ] [text "No"],
         div [ class "tableCell" ] [text "제목"],
         div [ class "tableCell" ] [text "유/무료"],
         div [ class "tableCell" ] [text "기간", span [class "productSmall"][text "(단위/일)"]],
         div [ class "tableCell" ] [text "가격", span [class "productSmall"][text "(단위/원)"]],
         div [ class "tableCell" ] [text "상품 분류"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "게시"]
     ]

tableLayout idx item model = 
        div [class "tableRow"] [
                div [ class "tableCell", onClick (GoDetail item.id)] [
                    text ( String.fromInt(model.listData.paginate.total_count - ((model.listData.paginate.page - 1) * 10) - (idx)
                    )) 
                ],
                div [ class "tableCell", onClick (GoDetail item.id)] [text item.name],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (if item.is_pay then "유료" else "무료")],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.fromInt item.day_name)],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.fromInt item.price)],
                div [ class "tableCell", onClick (GoDetail item.id)] [text item.product_code_name],
                div [ class "tableCell", onClick (GoDetail item.id)] [text (String.dropRight 10 item.inserted_at)],
                div [ class "tableCell"] [
                    if item.is_use then
                        div [class "button is-small is-success", onClick (IsUse (not item.is_use) item.id)][text "게시 중"]
                    else
                        div [class "button is-small", onClick (IsUse (not item.is_use) item.id)][text "게시하기"]
                ]
         ]

imageheaderTable = 
    div [ class "tableRow headerStyle"] [
        div [ class "tableCell" ] [text "No"],
        div [ class "tableCell" ] [text "제목"],
        div [ class "tableCell" ] [text "path"],
        div [ class "tableCell" ] [text "등록일"],
        div [ class "tableCell" ] [text "미리보기"]
    ]

imagetableLayout idx item model = 
    div [class "tableRow"] [
            div [ class "tableCell", onClick (GoDetail item.file_id)] [
                text ( String.fromInt(model.imageData.paginate.total_count - ((model.imageData.paginate.page - 1) * 10) - (idx)
                )) 
            ],
            div [ class "tableCell", onClick (GoDetail item.file_id)] [text item.title],
            div [ class "tableCell", style "width" "50%" ,onClick (GoDetail item.file_id)] [text item.path],
            div [ class "tableCell", onClick (GoDetail item.file_id)] [text (String.dropRight 10 item.inserted_at)],
            div [ class "tableCell"] 
            [button [class "button is-small", onClick (ImagePreview item.path)][text "미리보기"]]
        ]

imagePreview path= 
    div [class "previewWrap", style "display" (if String.isEmpty path then "none" else "flex" )][
        div [class "preview_contents"]
        [ div [class "preview_image_container"][
            img [src path][]
        ] 
        , div [class "button is-danger", style "width" "100%", onClick (ImagePreview "")][text "닫기"]]
    ]

imageRegist showRegist registTitle filename previewUrl= 
    div [class "previewWrap", style "display" (if showRegist then "flex" else "none" )][
        div [class "regist_container"]
        [ div [ class "contents_container_banner"]
        [ div [class "banner_title"] [text "이미지 등록"]
        , formInputEvent "제목명" "제목 명을 입력 해 주세요." False RegistTitle registTitle
        , div [ class "file has-name is-right is-fullwidth" ]
        [ label [ class "file-label" ]
            [ input [ class "file-input",  type_ "file", multiple False, id "thumbFile", on "change" (Decode.map GetFile targetFiles)  ]
                []
            , span [ class "file-cta" ]
                [ span [ class "file-icon" ]
                    [ i [ class "fas fa-upload" ]
                        []
                    ]
                , span [ class "file-label" ]
                    [ text "파일선택 "]
                ]
            , span [ class "file-name" ]
                [  text filename ]
            ]
        ]
        , div [class "bannerpreviewWrap"]
        [ p [][text "미리보기"]
        , if previewUrl == "" then
        p [class "bannernoImg"][text "선택 된 배너이미지가 없습니다."]
        else 
        img [src previewUrl ][]
        ]
        ]
        , div [ class "buttons previewBtn" ] [
            button [ class "button is-primary", onClick GoRegist, disabled (if registTitle == "" || filename == ""  then True else False)] [text "등록"],
            div [ class "button is-warning", onClick ImageRegistPop] [text "취소"]
        ]
        ]
    ]


targetFiles = 
    Decode.at ["target", "files"] (Decode.list File.decoder)