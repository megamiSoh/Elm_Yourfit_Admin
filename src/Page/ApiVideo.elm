module Page.ApiVideo exposing (..)

import Browser
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing( class, style )
import Html.Events exposing(..)
import Pagenation exposing(..)
import Page.Page exposing(..)
import Session exposing (Session)
import Route exposing (..)
import Page as Page
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decode as Decoder
import Json.Encode as Encode
import Date exposing (..)
import DatePicker exposing (Msg(..))

type alias Model = {
    firstSelectedDate : Maybe Date
    , secondSelectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    , endDatePickerData :DatePicker.Model
    , show : Bool
    , session: Session
    , menus : List Menus
    , username : String
    , page : Int
    , per_page : Int
    , title : String
    , video_code : String
    , start_date : String
    , end_date : String
    , data : Data
    , today : Maybe Date
    , endday : Maybe Date
    , endShow : Bool
    , todaySave : String
    , dateModel : String
    , pageNum : Int
    , videoCode : List VideoCode
    , shareShow : Bool
    }

type alias VideoCodeData = 
    { data : List VideoCode}

type alias VideoCode = 
    { code : String
    , name : String }

type alias Data = 
    { data : List ApiData
    , paginate : Paginate }

type alias ApiData = 
    { category : String
    , id : Int
    , inserted_at : String
    , is_use : Bool
    , title : String }

type alias Paginate = 
    { end_date : String
    , is_use : Maybe Bool
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    , video_code : String }



type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }

dataApi session page per_page title video_code start_date end_date = 
    let
        body = 
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page)
                , ("title", Encode.string title)
                , ("video_code" , Encode.string video_code)
                , ("start_date", Encode.string start_date)
                , ("end_date", Encode.string end_date) 
                ] |> Http.jsonBody
    in
    Api.post Endpoint.apilist (Session.cred session) ApiListComplete body (Decoder.apivideolist Data ApiData Paginate)

videoCodeApi session = 
    Api.post Endpoint.videoCode (Session.cred session) VideoCodeComplete Http.emptyBody (Decoder.videoCodeData VideoCodeData VideoCode)



init : Session -> (Model, Cmd Msg)
init session = 
    let
        
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
        ( endDatePickerData, enddatePickerCmd) = 
            DatePicker.init "my-datepicker"
    in
    ({  datePickerData = datePickerData
        , endDatePickerData = endDatePickerData
        , firstSelectedDate = Nothing
        , secondSelectedDate = Nothing
        , session = session
        , menus = []
        , username =""
        , videoCode = []
        , page = 1
        , per_page = 10
        , title = ""
        , video_code = ""
        , start_date = ""
        , end_date = ""
        , data =    
            { data = []
            , paginate = 
                { end_date = ""
                , is_use = Nothing
                , page = 1
                , per_page = 10
                , start_date = ""
                , title = ""
                , total_count = 0
                , video_code = ""
                }
            }
        , endday = Nothing
        , today = Nothing
        , endShow = False
        , todaySave = ""
        , show = False
        , dateModel = "all"
        , pageNum = 1
        , shareShow = False
    }, Cmd.batch
    [ Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo)
    , Cmd.map DatePickerMsg datePickerCmd
    , Cmd.map EndDatePickerMsg enddatePickerCmd
    ] )

type Msg 
    = NoOp
    | GetMyInfo (Result Http.Error Decoder.Profile)
    | GotSession Session
    | VideoCodeComplete (Result Http.Error VideoCodeData)
    | ApiListComplete (Result Http.Error Data)
    | EndDatePickerMsg DatePicker.Msg
    | DatePickerMsg DatePicker.Msg
    | Show
    | EndShow
    | DateValue String
    | PageBtn (Int, String)
    | CategoryEvent String
    | TitleSearch String
    | Search
    | Reset
    | IsActive
    | GoDetail String
    | GoDetailComplete Encode.Value
    -- | Search
    -- | Reset

toSession : Model -> Session
toSession model =
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            let
                old = model.data.paginate
                date = {old | page = 1}
            in
            
            if model.dateModel == "all" then
            ({model | pageNum = 1}, 
            Cmd.batch
            [ dataApi model.session 1 model.per_page model.title model.video_code "" ""
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo)
            , Api.pageNum (Encode.int 1)])
            else
            ({model | page = 1, pageNum = 1}, 
            Cmd.batch 
            [ dataApi model.session 1 model.per_page model.title model.video_code model.start_date model.end_date
            , Api.post Endpoint.myInfo (Session.cred model.session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo)
            , Api.pageNum (Encode.int 1)])
        Reset ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init "my-datepicker"
                ( endDatePickerData, enddatePickerCmd) = 
                    DatePicker.init "my-datepicker"
            in
            
            ({model | page = 1, title = "", video_code = "",  datePickerData = datePickerData, endDatePickerData = endDatePickerData, dateModel ="all"}, 
            Cmd.batch
                [ Cmd.map DatePickerMsg datePickerCmd
                , Cmd.map EndDatePickerMsg enddatePickerCmd])
        IsActive ->
            ({model | shareShow = not model.shareShow}, Cmd.none)
        TitleSearch title ->
            ({model | title = title}, Cmd.none)
        CategoryEvent code ->
            ({model | video_code = code}, Cmd.none)
        PageBtn (idx, pageCategory) ->
            if model.dateModel == "all" then
                case pageCategory of
                    "prev" ->
                        ({model | page = idx, pageNum = model.pageNum - 1},
                        Cmd.batch[
                        dataApi model.session idx model.per_page model.title model.video_code "" ""
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | page = idx, pageNum = model.pageNum + 1}, 
                        Cmd.batch[dataApi model.session idx model.per_page model.title model.video_code "" ""
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | page = idx},
                        Cmd.batch[dataApi model.session idx model.per_page model.title model.video_code "" ""
                        , Api.pageNum (Encode.int idx)])
                    _ ->
                        (model, Cmd.none)
            else 
                case pageCategory of
                    "prev" ->
                        ({model | page = idx, pageNum = model.pageNum - 1}, Cmd.batch[dataApi model.session idx model.per_page model.title model.video_code model.start_date model.end_date
                        , Api.pageNum (Encode.int idx)])
                    "next" ->
                        ({model | page = idx, pageNum = model.pageNum + 1}, Cmd.batch[dataApi model.session idx model.per_page model.title model.video_code model.start_date model.end_date
                        , Api.pageNum (Encode.int idx)])
                    "go" -> 
                        ({model | page = idx}, Cmd.batch[dataApi model.session idx model.per_page model.title model.video_code model.start_date model.end_date
                        , Api.pageNum (Encode.int idx)])
                    _ ->
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
                                ( { newModel | firstSelectedDate = Just currentSelectedDate, end_date = new, show = False }
                                , cmd
                                )
                            GetToday todaydate ->
                                let
                                    new =getFormattedDate (Just todaydate) model.today
                                in
                                
                                ( { newModel | today = Just todaydate, end_date = new, firstSelectedDate = Just todaydate }
                                , cmd
                                )
                            _ ->
                                ( newModel, cmd )
                   )
        ApiListComplete (Ok ok) ->
            ({model | data = ok}, Cmd.none)
        ApiListComplete (Err err) ->
            (model, Cmd.none)
        VideoCodeComplete (Ok ok) ->
            ({model | videoCode = ok.data}, Cmd.none)
        VideoCodeComplete (Err err) ->
            (model, Cmd.none)
        GotSession session ->
            ({ model | session = session}, 
            Api.post Endpoint.myInfo (Session.cred session) GetMyInfo Http.emptyBody (Decoder.myProfileInfo)
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
            ( {model |  menus = Decoder.mymenu item, username = Decoder.myname item}, 
            Cmd.batch [dataApi model.session model.page model.per_page model.title model.video_code model.start_date model.end_date
            , videoCodeApi model.session] )
        GoDetail id ->
            (model, Cmd.batch[Api.saveData (Encode.string id)
            ])
        GoDetailComplete go ->
            (model,Route.pushUrl (Session.navKey model.session) Route.ApiDetail)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Session.changes GotSession (Session.navKey model.session)
    , Api.saveCheck GoDetailComplete]

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "외부 API 영상"
    , content = 
        div [ class "is-fluid" ]
        [ 
            
            columnsHtml [pageTitle "외부 API 영상"],
            div [ class "searchWrap" ] [
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
                ],
                columnsHtml [
                    selectForm "카테고리" False model.videoCode CategoryEvent "" model.video_code
                    , formInputEvent "제목" "제목을 입력 해 주세요." False TitleSearch model.title,
                    searchB Search Reset
                ]
                
            ],
            (registRoute "영상 등록" Route.ApiVideoRegist)
            , dataCount (String.fromInt(model.data.paginate.total_count))
            , div [class "table"] 
            ([headerTable] ++ List.indexedMap(\idx i -> tableLayout idx i model) model.data.data)
            , pagination 
                    PageBtn
                    model.data.paginate
                    model.pageNum
            , shareLayout model
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
         div [ class "tableCell" ] [text "카테고리"],
         div [ class "tableCell" ] [text "등록일"],
         div [ class "tableCell" ] [text "공유건수"],
         div [ class "tableCell" ] [text "공유"]
     ]

--, Route.href (Just Route.UvideoDetail)
tableLayout idx item model = 
        div [class "tableRow"] [
                div [ class "tableCell" , Route.href (Just Route.ApiDetail) , onClick (GoDetail (String.fromInt item.id))] [text (
                    String.fromInt(model.data.paginate.total_count - ((model.data.paginate.page - 1) * 10) - (idx)
                    ))]
                , div [ class "tableCell" , Route.href (Just Route.ApiDetail) , onClick (GoDetail (String.fromInt item.id))] [text item.title]
                , div [ class "tableCell" , Route.href (Just Route.ApiDetail) , onClick (GoDetail (String.fromInt item.id))] [text item.category]
                , div [ class "tableCell" , Route.href (Just Route.ApiDetail) , onClick (GoDetail (String.fromInt item.id))] [ text (String.dropRight 10 item.inserted_at)]
                , div [ class "tableCell" , onClick (GoDetail (String.fromInt item.id))] [ text "1 회 (예시)" ]
                , div [ class "tableCell" ] [
                            button [class "button is-small", onClick IsActive ] [text "공유 하기"]
                    ]
                
         ]          


shareLayout model = 
    div [style "display" (if model.shareShow then "flex" else "none")][
        text "new"
    ]