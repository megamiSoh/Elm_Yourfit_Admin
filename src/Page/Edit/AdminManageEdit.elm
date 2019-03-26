module Page.Edit.AdminManageEdit exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Page exposing (..)
import Session exposing (Session)
import Route exposing (Route)
import Page.Origin.AdminManage as AdminManage

type alias User =
    {
        no: Int,
        userName : String,
        userId : String,
        createDate: String,
        connectDate : String
    }


type alias Model =
    {
        pop : Bool,
        session : Session,
        userData : List User,
        dataList : List User
    }


init : Session ->  (Model, Cmd Msg)
init session  = 
    let
        data =
            List.indexedMap(
                \idx item ->
                    {no = item.no,
                    userName = item.userName,
                    userId = item.userId,
                    createDate = item.createDate,
                    connectDate = item.connectDate
                     }
                )  adminList
        user = 
            [{
                no = 1,
                userName = "파이널2",
                userId = "finalcomnapy",
                createDate = "2019-02-02",
                connectDate ="2019-05-03"
            }]
    in
    
        (
        { session = session, pop = False, userData = user ,dataList = data}, Cmd.none
        )

toSession : Model -> Session
toSession model =
    model.session


type Msg = PopEvent | ChoiceItem Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PopEvent ->
            ({model | pop = not model.pop}, Cmd.none)
        
        ChoiceItem idx ->
            let 
                filterItem =
                    List.filter(\i -> i.no  == idx) model.dataList  
            in
            
            ({model | userData = filterItem, pop = not model.pop}, Cmd.none)    

view : Model -> {title : String , content : Html Msg, menu : Html Msg}
view model =
    { title = "관리자 수정"
    , content = 
       div [] [
            -- AdminManage.adminLayout 
            -- PopEvent 
            -- (List.length (model.userData) )
            -- (routeEdit Route.AdminManage  (Route.AdminDetail "detail")) 
            -- model
            -- ChoiceItem
            -- "관리자 수정"
            -- ""
       ]
       , menu = div [] []
    }


adminList 
    = [
        {
            no = 1,
            userName = "파이널2",
            userId = "finalcomnapy",
            createDate = "2019-02-02",
            connectDate ="2019-05-03"
        },
        {
            no = 2,
            userName = "파이널3",
            userId = "finalcomnapy",
            createDate = "2019-02-02",
            connectDate ="2019-05-03"
        },
        {
            no = 3,
            userName = "파이널4",
            userId = "finalcomnapy",
            createDate = "2019-02-02",
            connectDate ="2019-05-03"
        },
        {
            no = 4,
            userName = "파이널5",
            userId = "finalcomnapy",
            createDate = "2019-02-02",
            connectDate ="2019-05-03"
        },
        {
            no = 5,
            userName = "파이널",
            userId = "finalcomnapy",
            createDate = "2019-02-02",
            connectDate ="2019-05-03"
        }
    ]