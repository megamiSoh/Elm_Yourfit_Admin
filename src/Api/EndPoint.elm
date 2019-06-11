module Api.Endpoint exposing 
    (Endpoint
    , login
    , request
    , user
    , usermanageList
    , adminList
    , myInfo
    , refresh
    , unwrap
    , usermanageStr
    , info
    , userDetail
    , infoDetail
    , infoEdit
    , infoActive
    , infoRegist
    , adminDetail
    , authMenu
    , authCode
    , adminEdit
    , adminSearch
    , adminR
    , adminDelete
    , unitLevel
    , exerCode
    , instrument
    , part
    , unitList
    , unitDetail
    , unitEdit
    , unitRegist
    , videoRegist
    , videoDetail
    , exerPartCode
    , videoFilterResult
    , videoEdit
    , videoRegistRegist
    , videoActive
    , unitVideoShow
    , yourfitVideoShow
    , resetpwd
    , contactList
    , contactDetail
    , faqanswer
    , faqList
    , faqUse
    , faqRegist
    , faqDetail
    , faqEdit
    , pointCode
    )

import Http
import Url.Builder exposing (QueryParameter)


request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }



-- TYPES

type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str



url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- *****develope Api
    Url.Builder.crossOrigin "http://13.209.49.169:4000/api"
    -- *****production Api
    -- Url.Builder.crossOrigin "https://api.yfit.co.kr/api"
        ("v1" :: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


login : Endpoint
login =
    url [ "auth","admin", "sign_in" ] []


user : Endpoint
user =
    url [ "auth", "admin", "show" ] []


refresh : Endpoint
refresh =
    url [ "auth", "admin", "refresh"] []
-- users : Endpoint
-- users =
--     url [ "users" ] []

usermanageList : Endpoint
usermanageList =
    url ["admin","users"] []

usermanageStr = 
    {url = "admin/users",
    method = "POST"}

userDetail userId = 
    url ["admin", "users", userId][]


-- notice
info = 
    url ["admin", "notices"] []
    

infoDetail noticeId = 
    url ["admin" ,"notices", noticeId][]

infoEdit noticeId = 
    url ["admin", "notices" , noticeId , "edit"] []

infoActive noticeId = 
    url ["admin", "notices" , noticeId , "use"] []

infoRegist =
    url ["admin", "notices", "new"][]

-- admin Manage

adminList : Endpoint
adminList =
    url ["admin","admins"] []

adminDetail userId= 
    url ["admin", "admins", userId] []

adminEdit userId=
    url ["admin", "admins",userId, "edit"] []

adminSearch = 
    url ["admin", "admins", "users"] []

adminDelete = 
    url ["admin", "admins","delete"][]

myInfo : Endpoint
myInfo = 
    url ["admin", "my"][]


-- menuAuth
authMenu = 
    url ["admin", "admins", "menus"] []

authCode = 
    url ["admin" , "admins" , "menu_auth_code"] []




-- adminRegst 
adminR =
    url ["admin", "admins", "new"] []

-- unitvideo

unitLevel = 
    url ["admin", "actions", "difficulty_code"] []
exerCode = 
    url ["admin", "actions" , "exercise_code"] []
instrument = 
    url ["admin", "actions", "instrument_code"] []
part =  
    url ["admin", "actions", "part_detail_code"] []
unitList = 
    url ["admin", "actions"] []
unitDetail id= 
    url ["admin", "actions" , id][]

unitEdit id = 
    url ["admin", "actions", id, "edit"] []
unitRegist = 
    url ["admin", "actions", "new"] []
-- video
videoRegist = 
    url ["admin", "exercises"] []
videoDetail id=
    url ["admin", "exercises", id] []
exerPartCode = 
    url ["admin", "exercises","exercise_part_code"][]

videoFilterResult =     
    url ["admin","actions", "filter"] []
videoEdit id= 
    url ["admin" ,"exercises", id , "edit"] []
videoRegistRegist = 
    url ["admin", "exercises", "new"] []

videoActive id =
    url ["admin", "exercises" , id, "use"] []
    

-- videoShow 
unitVideoShow id = 
    url ["admin","actions","preview",id] []


yourfitVideoShow id = 
    url ["admin", "exercises", "preview", id] []

-- resetpwd
resetpwd id= 
    url ["admin" , "users", id , "init"] []


-- faq
contactList = 
    url ["admin", "inquiries"] []
    
contactDetail id =
    url ["admin", "inquiries", id] []

faqanswer id = 
    url ["admin", "inquiries", id, "answer"][]

faqList = 
    url ["admin", "faqs"][]
faqUse id = 
    url ["admin" , "faqs" , id , "use"][]

faqRegist = 
    url ["admin", "faqs", "new"][]
faqDetail id = 
    url ["admin" , "faqs" , id] []

faqEdit id = 
    url ["admin", "faqs", id, "edit"][]

pointCode = 
    url ["admin", "exercises", "exercise_point_code"][]