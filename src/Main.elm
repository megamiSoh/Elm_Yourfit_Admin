
port module Main exposing (..)
import Browser
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Url exposing (Url)
import Html.Attributes as Attr
import Url.Parser as UrlParser
import Browser.Navigation as Nav
import SearchPage as Spage
import Page.AdminManage as AdminM
import Page.UserManage as UserM
import Page.VideoUnit as VideoU
import Page.Video as Video
import Page.ApiVideo as ApiV
import Page.UserPost as UserP
import Page.Info as Info
import Page.Faq as Faq
import Page.FoodCalorie as FoodC
import Page.UserInfo as UserI
import ExpandEvent as Exevent
import Route exposing (Route)
import Page.Detail.UserManageDetail as UserMDetail
import Page.Regist.AdminRegist as AdminRegist
import Page.Detail.AdminManageDetail as AdminDetail
import Page.Edit.AdminManageEdit as AdminEdit
import Page.Edit.UnitVideoEdit as UvideoEdit
import Page.Detail.UnitVideoDetail as UvideoDetail
import Page.Regist.UnitVideoRegist as UvideoRegist
import Page.Detail.VideoDetail as VideoDetail
import Page.Regist.VideoRegist as VideoRegist
import Page.Edit.VideoEdit as VideoEdit
import Page.Regist.ApiVideoRegist as ApiRegist
import Page.Detail.ApiVideoDetail as ApiDetail
import Page.Edit.ApiVideoEdit as ApiEdit
import Page.Regist.InfoRegist as InfoR
import Page.Detail.InfoDetail as InfoD
import Page.Edit.InfoEdit as InfoE
import Page.Regist.FaqRegist as FaqR
import Page.Edit.FaqEdit as FaqE
import Page.Detail.FaqDetail as FaqD
import Page.Blank as Blank
import Session exposing (Session)
import Page exposing(Page)
import Login as Login
import Api exposing(..)
import Login exposing(..)
import Json.Decode as Decode exposing (Value)
import Viewer exposing (Viewer)
type alias Flags =
    {}



type Model 
     = Redirect Session
     | Home UserM.Model
     | AdminMmodel AdminM.Model
     | UserMmodel UserM.Model
     | VideoUnitmodel VideoU.Model
     | VideoModel Video.Model
     | ApiModel ApiV.Model
     | UserPmodel UserP.Model
     | InfoModel Info.Model
     | FaqModel Faq.Model
     | FoodCmodel FoodC.Model
     | UserImodel UserI.Model
     | UsermDmodel UserMDetail.Model
     | AdminRmodel AdminRegist.Model
     | AdminmDmodel AdminDetail.Model
     | AdminEmodel AdminEdit.Model
     | UvideoEmodel UvideoEdit.Model
     | UvideoDmodel UvideoDetail.Model
     | UvideoRmodel UvideoRegist.Model
     | VideoDmodel VideoDetail.Model
     | VideoRmodel VideoRegist.Model
     | VideoEmodel VideoEdit.Model
     | ApiRmodel ApiRegist.Model
     | ApiDmodel ApiDetail.Model
     | ApiEmodel ApiEdit.Model
     | InfoRmodel InfoR.Model
     | InfoDmodel InfoD.Model
     | InfoEmodel InfoE.Model
     | FaqRmodel FaqR.Model
     | FaqEmodel FaqE.Model
     | FaqDmodel FaqD.Model
     | LoginModel Login.Model
     | NotFound Session
   
main : Program Value Model Msg
main =
    Api.application Api.storageDecoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | AdminMmsg AdminM.Msg
    | UserMmsg UserM.Msg
    | VideoUnitmsg VideoU.Msg
    | Videomsg Video.Msg
    | Apimsg ApiV.Msg
    | UserPmsg UserP.Msg
    | Infomsg Info.Msg
    | Faqmsg Faq.Msg
    | FoodCmsg FoodC.Msg
    | UserImsg UserI.Msg
    | UsermDmsg UserMDetail.Msg
    | AdminRmsg AdminRegist.Msg
    | AdminmDmsg AdminDetail.Msg
    | AdminEmsg AdminEdit.Msg
    | UvideoEmsg UvideoEdit.Msg
    | UvideoDmsg UvideoDetail.Msg
    | UvideoRmsg UvideoRegist.Msg
    | VideoDmsg VideoDetail.Msg
    | VideoRmsg VideoRegist.Msg
    | VideoEmsg VideoEdit.Msg
    | ApiRmsg ApiRegist.Msg
    | ApiDmsg ApiDetail.Msg
    | ApiEmsg ApiEdit.Msg
    | InfoRmsg InfoR.Msg
    | InfoDmsg InfoD.Msg
    | InfoEmsg InfoE.Msg
    | FaqRmsg FaqR.Msg
    | FaqEmsg FaqE.Msg
    | FaqDmsg FaqD.Msg
    | LoginMsg Login.Msg
    | GotSession Session

subscriptions model = 
    case model of
        NotFound _ ->
            Sub.none
        Redirect session ->
            Session.changes GotSession (Session.navKey (toSession model))
        Home user ->
            Sub.map UserMmsg (UserM.subscriptions user)
        AdminMmodel adminm ->
            Sub.map AdminMmsg (AdminM.subscriptions adminm)
        UserMmodel user ->
            Sub.map UserMmsg (UserM.subscriptions user)
        UserPmodel home ->
            Sub.none
        VideoUnitmodel video ->
            Sub.map VideoUnitmsg (VideoU.subscriptions video)
        VideoModel video ->
            Sub.map Videomsg (Video.subscriptions video)
        ApiModel api ->
            Sub.none
        InfoModel info ->
            Sub.map Infomsg (Info.subscriptions info)
        FaqModel faq ->
            Sub.none
        FoodCmodel food ->
            Sub.none
        UserImodel useri ->
            Sub.map UserImsg (UserI.subscriptions useri)
        UsermDmodel usermd->
            Sub.map UsermDmsg (UserMDetail.subscriptions usermd)
        AdminRmodel adminr ->
            Sub.none
        AdminmDmodel adminmd ->
            Sub.map AdminmDmsg (AdminDetail.subscriptions adminmd)
        AdminEmodel admine ->
            Sub.none
        UvideoEmodel uvideo ->
            Sub.none
        UvideoDmodel uvideod ->
            Sub.map UvideoDmsg (UvideoDetail.subscriptions uvideod)
        UvideoRmodel uvideoR ->
            Sub.map UvideoRmsg (UvideoRegist.subscriptions uvideoR)
        VideoDmodel videod ->
            Sub.map VideoDmsg (VideoDetail.subscriptions videod)
        VideoRmodel videor ->
            Sub.map VideoRmsg (VideoRegist.subscriptions videor)
        VideoEmodel videoe ->
            Sub.none
        ApiRmodel apir ->
            Sub.none
        ApiDmodel apid ->
            Sub.none
        ApiEmodel apie ->
            Sub.none
        InfoRmodel infor ->
            Sub.map InfoRmsg (InfoR.subscriptions infor)
        InfoDmodel infod ->
            Sub.map InfoDmsg (InfoD.subscriptions infod)
        InfoEmodel infoe ->
            Sub.map InfoEmsg (InfoE.subscriptions infoe)
        FaqRmodel faqr ->
            Sub.none
        FaqEmodel faqe ->
            Sub.none
        FaqDmodel faqd ->
            Sub.none
        LoginModel login ->
            Sub.map LoginMsg (Login.subscriptions login)

init : Maybe Cred -> Url -> Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    case maybeViewer of
        Just _ ->
            changeRouteTo (Route.fromUrl url) (Redirect (Session.fromViewer navKey maybeViewer))
        Nothing ->
            changeRouteTo (Just Route.Login) (Redirect (Session.fromViewer navKey maybeViewer))

changeRouteTo : Maybe Route -> Model -> (Model , Cmd Msg)
changeRouteTo maybeRoute model =
    let
        session = toSession model
    in
    case maybeRoute of
        Nothing ->
             ( model, Route.replaceUrl (Session.navKey session) Route.Login)
        Just Route.Login ->
            Login.init session  
                |> updateWith LoginModel LoginMsg model    

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )
        Just Route.Other ->
            (Redirect session , Cmd.none)
        Just Route.Home -> 
            UserM.init session
                |> updateWith UserMmodel UserMmsg model
        Just Route.UserManage ->
            UserM.init  session
                |> updateWith UserMmodel UserMmsg model
        Just Route.AdminManage ->
            AdminM.init session 
                |>updateWith AdminMmodel AdminMmsg model
        Just Route.VideoUnit ->
            VideoU.init session 
                |> updateWith VideoUnitmodel VideoUnitmsg model
        Just Route.Video ->
            Video.init session 
                |> updateWith VideoModel Videomsg model
        Just Route.ApiVideo ->
            ApiV.init session 
                |> updateWith ApiModel Apimsg model 
        Just Route.FoodCalorie ->
            FoodC.init session 
                |> updateWith FoodCmodel FoodCmsg model
        Just Route.UserPost ->
            UserP.init session
                |> updateWith UserPmodel UserPmsg model
        Just Route.Info ->
            Info.init session 
                |> updateWith InfoModel Infomsg model
        Just Route.Faq ->
            Faq.init session 
                |> updateWith FaqModel Faqmsg model
        Just Route.UserInfo ->
            UserI.init session 
                |> updateWith UserImodel UserImsg model
        Just Route.UserMDetail ->
            UserMDetail.init session 
                |> updateWith UsermDmodel UsermDmsg model
        Just Route.AdminRegist ->
            AdminRegist.init session 
                |> updateWith AdminRmodel AdminRmsg model
        Just (Route.AdminDetail )  ->
            AdminDetail.init session 
                |> updateWith AdminmDmodel AdminmDmsg model
        Just Route.VideoEdit ->
            VideoEdit.init session 
                |> updateWith VideoEmodel VideoEmsg model
        Just (Route.AdminEdit detail edit) ->
            AdminEdit.init session 
                |> updateWith AdminEmodel AdminEmsg model
        Just Route.UvideoEdit ->
            UvideoEdit.init session 
                |> updateWith UvideoEmodel UvideoEmsg model
        Just Route.UvideoDetail ->
            UvideoDetail.init session 
                |> updateWith UvideoDmodel UvideoDmsg model
        Just Route.UvideoRegist ->
            UvideoRegist.init session 
                |> updateWith UvideoRmodel UvideoRmsg model
        Just Route.VideoRegist ->
            VideoRegist.init session 
                |> updateWith VideoRmodel VideoRmsg model
        Just Route.VideoDetail ->
            VideoDetail.init session 
                |> updateWith VideoDmodel VideoDmsg model
        Just Route.ApiDetail ->
            ApiDetail.init session 
                |> updateWith ApiDmodel ApiDmsg model
        Just Route.ApiEdit ->
            ApiEdit.init session 
                |> updateWith ApiEmodel ApiEmsg model
        Just Route.ApiVideoRegist ->
            ApiRegist.init session 
                |> updateWith ApiRmodel ApiRmsg model        
        Just Route.InfoRegist ->
            InfoR.init session 
                |> updateWith InfoRmodel InfoRmsg model
        Just Route.InfoDetail ->
            InfoD.init session 
                |> updateWith InfoDmodel InfoDmsg model
        Just Route.InfoEdit ->
            InfoE.init session 
                |> updateWith InfoEmodel InfoEmsg model
        Just Route.FaqDetail ->
            FaqD.init session 
                |> updateWith FaqDmodel FaqDmsg model
        Just Route.FaqRegist ->
            FaqR.init session 
                |> updateWith FaqRmodel FaqRmsg model
        Just Route.FaqEdit ->
            FaqE.init session 
                |> updateWith FaqEmodel FaqEmsg model
        -- Just Route.Login ->
        --     Login.init session  
        --         |> updateWith LoginModel LoginMsg model
            

toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session
        NotFound session ->
            session
        Home user ->
            UserM.toSession user
        AdminMmodel adminm ->
            AdminM.toSession adminm
        UserMmodel user ->
            UserM.toSession user
        UserPmodel home ->
            UserP.toSession home
        VideoUnitmodel video ->
            VideoU.toSession video
        VideoModel video ->
            Video.toSession video 
        ApiModel api ->
            ApiV.toSession api
        InfoModel info ->
            Info.toSession info
        FaqModel faq ->
            Faq.toSession faq
        FoodCmodel food ->
            FoodC.toSession food
        UserImodel useri ->
            UserI.toSession useri
        UsermDmodel usermd->
            UserMDetail.toSession usermd
        AdminRmodel adminr ->
            AdminRegist.toSession adminr
        AdminmDmodel adminmd ->
            AdminDetail.toSession adminmd
        AdminEmodel admine ->
            AdminEdit.toSession admine
        UvideoEmodel uvideo ->
            UvideoEdit.toSession uvideo
        UvideoDmodel uvideod ->
            UvideoDetail.toSession uvideod
        UvideoRmodel uvideoR ->
            UvideoRegist.toSession uvideoR
        VideoDmodel videod ->
            VideoDetail.toSession videod
        VideoRmodel videor ->
            VideoRegist.toSession videor
        VideoEmodel videoe ->
            VideoEdit.toSession videoe
        ApiRmodel apir ->
            ApiRegist.toSession apir
        ApiDmodel apid ->
            ApiDetail.toSession apid
        ApiEmodel apie ->
            ApiEdit.toSession apie
        InfoRmodel infor ->
            InfoR.toSession infor
        InfoDmodel infod ->
            InfoD.toSession infod
        InfoEmodel infoe ->
            InfoE.toSession infoe
        FaqRmodel faqr ->
            FaqR.toSession faqr
        FaqEmodel faqe ->
            FaqE.toSession faqe
        FaqDmodel faqd ->
            FaqD.toSession faqd
        LoginModel login ->
            Login.toSession login

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Route.replaceUrl (Session.navKey (toSession model)) Route.Login )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )
                            

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
    
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model
        
        (LoginMsg subMsg , LoginModel lomodel) ->
            Login.update subMsg lomodel
                |> updateWith LoginModel LoginMsg model

        (AdminMmsg subMsg , AdminMmodel adminmodel) ->
            AdminM.update subMsg adminmodel
                |> updateWith AdminMmodel AdminMmsg model

        (UserMmsg subMsg , UserMmodel usermodel) ->
            UserM.update subMsg usermodel
                |> updateWith UserMmodel UserMmsg model

        (VideoUnitmsg subMsg , VideoUnitmodel videomodel) ->
            VideoU.update subMsg videomodel
                |> updateWith VideoUnitmodel VideoUnitmsg model

        (Videomsg subMsg , VideoModel videomodel) ->
            Video.update subMsg videomodel
                |> updateWith VideoModel Videomsg model

        (Apimsg subMsg , ApiModel apimodel) ->
            ApiV.update subMsg apimodel
                |> updateWith ApiModel Apimsg model

        (UserPmsg subMsg , UserPmodel userpmodel) ->
            UserP.update subMsg userpmodel
                |> updateWith UserPmodel UserPmsg model

        (Infomsg subMsg , InfoModel infomodel) ->
            Info.update subMsg infomodel
                |> updateWith InfoModel Infomsg model

        (Faqmsg subMsg , FaqModel faqmodel) ->
            Faq.update subMsg faqmodel
                |> updateWith FaqModel Faqmsg model

        (FoodCmsg subMsg , FoodCmodel foodmodel) ->
            FoodC.update subMsg foodmodel
                |> updateWith FoodCmodel FoodCmsg model

        (UserImsg subMsg , UserImodel userImodel) ->
            UserI.update subMsg userImodel
                |> updateWith UserImodel UserImsg model

        (UsermDmsg subMsg , UsermDmodel itemmodel) ->
            UserMDetail.update subMsg itemmodel
                |> updateWith UsermDmodel UsermDmsg  model
        
        (AdminRmsg subMsg , AdminRmodel itemModel) ->
            AdminRegist.update subMsg itemModel
                |> updateWith AdminRmodel AdminRmsg model
        
           
        (AdminmDmsg subMsg , AdminmDmodel itemModel) ->
            AdminDetail.update subMsg itemModel
                |> updateWith AdminmDmodel AdminmDmsg model
            

        (AdminEmsg subMsg , AdminEmodel itemModel) ->
            AdminEdit.update subMsg itemModel
                |> updateWith AdminEmodel AdminEmsg model
     
        (UvideoEmsg subMsg, UvideoEmodel itemModel) ->
            UvideoEdit.update subMsg itemModel
                |> updateWith UvideoEmodel UvideoEmsg model  

        (UvideoDmsg subMsg, UvideoDmodel itemModel) ->
            UvideoDetail.update subMsg itemModel
                |> updateWith UvideoDmodel UvideoDmsg model  
        
        (UvideoRmsg subMsg, UvideoRmodel itemModel) ->
            UvideoRegist.update subMsg itemModel
                |> updateWith UvideoRmodel UvideoRmsg model
          

        (VideoEmsg subMsg, VideoEmodel itemModel) ->
            VideoEdit.update subMsg itemModel
                |> updateWith VideoEmodel VideoEmsg model  

        (VideoDmsg subMsg, VideoDmodel itemModel) ->
            VideoDetail.update subMsg itemModel
                |> updateWith VideoDmodel VideoDmsg model  

        (VideoRmsg subMsg, VideoRmodel itemModel) ->
            VideoRegist.update subMsg itemModel
                |> updateWith VideoRmodel VideoRmsg model   
       
        (ApiRmsg subMsg, ApiRmodel itemModel) ->
            ApiRegist.update subMsg itemModel
                |> updateWith ApiRmodel ApiRmsg model
          
            
        (ApiDmsg subMsg, ApiDmodel itemModel) ->
            ApiDetail.update subMsg itemModel
                |> updateWith ApiDmodel ApiDmsg model
           
        
        (ApiEmsg subMsg, ApiEmodel itemModel) ->
            ApiEdit.update subMsg itemModel
                |> updateWith ApiEmodel ApiEmsg model
          
        (InfoRmsg subMsg, InfoRmodel itemModel) ->
            InfoR.update subMsg itemModel
                |> updateWith InfoRmodel InfoRmsg model
         
         
        (InfoDmsg subMsg, InfoDmodel itemModel) ->
            InfoD.update subMsg itemModel
                |> updateWith InfoDmodel InfoDmsg model
        
        (InfoEmsg subMsg, InfoEmodel itemModel) ->
            InfoE.update subMsg itemModel
                |> updateWith InfoEmodel InfoEmsg model
           
        (FaqDmsg subMsg, FaqDmodel itemModel) ->
            FaqD.update subMsg itemModel
                |> updateWith FaqDmodel FaqDmsg model
           
            
        (FaqEmsg subMsg, FaqEmodel itemModel) ->
            FaqE.update subMsg itemModel
                |> updateWith FaqEmodel FaqEmsg model
            
        (FaqRmsg subMsg, FaqRmodel faqRegist ) ->
            FaqR.update subMsg faqRegist
                |> updateWith FaqRmodel FaqRmsg model
        ( GotSession session, Redirect _) ->
            (Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home)
        ( _, _ ) ->
            ( model, Cmd.none )  
        
            
updateWith toModel toMsg model (subModel, subCmd) =
    (toModel subModel
    , Cmd.map toMsg subCmd)
            
            
       
                    
view :Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.cred (toSession model)) page config 
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case Session.cred (toSession model) of
            Nothing->
                case model of
                    LoginModel itemModel ->
                        viewPage Page.Login LoginMsg (Login.view itemModel)
                    Redirect _ ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    NotFound _ ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view

                    Home home ->
                        viewPage Page.Home UserMmsg (UserM.view home)
                    AdminMmodel _ ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view

                    UserMmodel usermanage ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view

                    VideoUnitmodel videounit ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    
                    VideoModel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    ApiModel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UserPmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    InfoModel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    FaqModel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    FoodCmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UserImodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UsermDmodel itemModel -> 
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    AdminRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    AdminmDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    AdminEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UvideoEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UvideoDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    UvideoRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    VideoDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    VideoRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    VideoEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    ApiRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    ApiDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    ApiEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    InfoRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    InfoDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    InfoEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    FaqRmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    FaqEmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
                    FaqDmodel itemModel ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view
            Just _ ->
                case model of
                    Redirect _ ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view

                    NotFound _ ->
                        viewPage Page.Other (\_ -> Ignored) Blank.view

                    Home home ->
                        viewPage Page.Home UserMmsg (UserM.view home)

                    AdminMmodel itemModel ->
                        viewPage Page.AdminManage AdminMmsg (AdminM.view itemModel)

                    UserMmodel usermanage ->
                        viewPage Page.UserManage UserMmsg (UserM.view usermanage)

                    VideoUnitmodel videounit ->
                        viewPage Page.VideoUnit VideoUnitmsg (VideoU.view videounit)
                    
                    VideoModel itemModel ->
                        viewPage Page.Video Videomsg (Video.view itemModel)
                    ApiModel itemModel ->
                        viewPage Page.ApiVideo Apimsg (ApiV.view itemModel)
                    UserPmodel itemModel ->
                        viewPage Page.UserPost UserPmsg (UserP.view itemModel)
                    InfoModel itemModel ->
                        viewPage Page.Info Infomsg (Info.view itemModel)
                    FaqModel itemModel ->
                        viewPage Page.Faq Faqmsg (Faq.view itemModel)
                    FoodCmodel itemModel ->
                        viewPage Page.FoodCalorie FoodCmsg (FoodC.view itemModel)
                    UserImodel itemModel ->
                        viewPage Page.UserInfo UserImsg (UserI.view itemModel)
                    UsermDmodel itemModel -> 
                        viewPage Page.UserDetail UsermDmsg (UserMDetail.view itemModel)
                    AdminRmodel itemModel ->
                        viewPage Page.AdminRegist AdminRmsg (AdminRegist.view itemModel)
                    AdminmDmodel itemModel ->
                        viewPage Page.AdminDetail AdminmDmsg (AdminDetail.view itemModel)
                    AdminEmodel itemModel ->
                        viewPage Page.AdminEdit AdminEmsg (AdminEdit.view itemModel)
                    UvideoEmodel itemModel ->
                        viewPage Page.UnitVideoEdit UvideoEmsg (UvideoEdit.view itemModel)
                    UvideoDmodel itemModel ->
                        viewPage Page.UnitVideoDetail UvideoDmsg (UvideoDetail.view itemModel)
                    UvideoRmodel itemModel ->
                        viewPage Page.UnitVideoRegist UvideoRmsg (UvideoRegist.view itemModel)
                    VideoDmodel itemModel ->
                        viewPage Page.VideoDetail VideoDmsg (VideoDetail.view itemModel)
                    VideoRmodel itemModel ->
                        viewPage Page.VideoRegist VideoRmsg (VideoRegist.view itemModel)
                    VideoEmodel itemModel ->
                        viewPage Page.VideoEdit VideoEmsg (VideoEdit.view itemModel)
                    ApiRmodel itemModel ->
                        viewPage Page.ApiVideoRegist ApiRmsg (ApiRegist.view itemModel)
                    ApiDmodel itemModel ->
                        viewPage Page.ApiVideoDetail ApiDmsg (ApiDetail.view itemModel)
                    ApiEmodel itemModel ->
                        viewPage Page.ApiVideoEdit ApiEmsg (ApiEdit.view itemModel)
                    InfoRmodel itemModel ->
                        viewPage Page.InfoRegist InfoRmsg (InfoR.view itemModel)
                    InfoDmodel itemModel ->
                        viewPage Page.InfoDetail InfoDmsg (InfoD.view itemModel)
                    InfoEmodel itemModel ->
                        viewPage Page.InfoEdit InfoEmsg (InfoE.view itemModel)
                    FaqRmodel itemModel ->
                        viewPage Page.FaqRegist FaqRmsg (FaqR.view itemModel)
                    FaqEmodel itemModel ->
                        viewPage Page.FaqEdit FaqEmsg (FaqE.view itemModel)
                    FaqDmodel itemModel ->
                        viewPage Page.FaqDetail FaqDmsg (FaqD.view itemModel)
                    LoginModel itemModel ->
                        viewPage Page.Login LoginMsg (Login.view itemModel)
                    