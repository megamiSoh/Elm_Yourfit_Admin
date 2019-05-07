module Api.Decode exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
type alias Success = 
    {result : String}

result =
    Decode.succeed Success
        |> required "result" string


-- admin
userformDecoder resultForm dataform page= 
    Decode.succeed resultForm
        |> required "data" (list (userListData dataform))
        |> required "paginate" (userPage page)

userPage page= 
    Decode.succeed page
        |> required "end_date" string
        |> required "nickname" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "total_count" int
        |> required "username" string
        -- |> hardcoded 1.0

userListData dataform= 
    Decode.succeed dataform
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string

-- user



userdataDecoder data user userdata=
     Decode.succeed data
        |> required "data" (userWrap user userdata)
userWrap user userdata= 
    Decode.succeed user
        |> required "user" (userDecoder userdata)

userDecoder userdata = 
    Decode.succeed userdata
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string 
        |> optional "profile" (Decode.map Just string ) Nothing
-- auth

authMenusDecoder authmenus authmenu=
    Decode.succeed authmenus
        |> required "data" (Decode.list (authMenuDecoder authmenu))

authMenuDecoder authmenu =
    Decode.succeed authmenu
        |> required "id" int
        |> required "name" string

authCodeDecoder authCodes acode= 
    Decode.succeed authCodes
        |>required "data" (Decode.list (authCode acode))

authCode acode= 
    Decode.succeed acode
        |> required "code" string
        |> required "name" string
decoder datawrap data menus admin= 
    Decode.succeed datawrap
        |> required "data" (detailDecoder data menus admin)

detailDecoder data menus admin=
    Decode.succeed  data
        |> required "admin" (adminDecoder admin)
        |> required "menus" (Decode.list (menuDecoder menus))

menuDecoder menus= 
    Decode.succeed menus
        |> required "menu_auth_code" (Decode.list string)
        |> required "menu_id" int
        -- |> required "menu_name" string

adminDecoder admin=
    Decode.succeed admin
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
        |> optional "profile" (Decode.map Just string) Nothing

-- adminREgist
decoderBody resultForm getBody page= 
    Decode.succeed resultForm 
        |> required "data" (list (decodeGetBody getBody))
        |> required "paginate" (decodePaginate page)
        
decodeGetBody getBody= 
    Decode.succeed getBody
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
        |> optional "profile" (Decode.map Just string) Nothing

decodePaginate page =
    Decode.succeed page
        |> required "nickname" string
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "username" string


resultDecoder resultD= 
    Decode.succeed resultD
        |> required "result" string

-- YourfitUnitVideo

unitLevelsDecoder data level =
    Decode.succeed data
        |> required "data" (Decode.list (unitLevel level))

unitLevel level =
    Decode.succeed level
        |> required "code" string
        |> required "name" string

unitListDecoder dataList unit page = 
    Decode.succeed dataList
        |> required "data" (Decode.list (unitList unit))
        |> required "paginate" (pagination page)

unitList unit= 
    Decode.succeed unit
        |> required "difficulty_name" string
        |> required "exercise_name" string
        |> required "id" int
        |> required "inserted_at" string
        |> required "instrument_name" string
        |> required "part_detail_name" (Decode.list string)
        |> required "title" string

pagination page =
    Decode.succeed page
        |> required "difficulty_code" string
        |> required "end_date" string
        |> required "exercise_code" string
        |> required "instrument_code" string
        |> required "part_detail_code" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

-- unitDetail

unitDetailDecoder data unit part= 
    Decode.succeed data
        |> required "data" (unitData unit part)

unitData unit part= 
    Decode.succeed unit
        |> required "description" string
        |> required "difficulty_code" string
        |> required "exercise_code" string
        |> required "id" int
        |> required "instrument_code" string
        |> required "part_detail_code" (Decode.list (partDetail part))
        |> required "title" string
        |> required "video" string
partDetail part = 
    Decode.succeed part
        |> required "code" string
        |> required "name" string
        
-- video
videoDecoder data getbody page= 
    Decode.succeed data
        |> required "data" (Decode.list (videoGetBody getbody))
        |> required "paginate" (videoPage page)

videoGetBody getbody=
    Decode.succeed getbody
        |> required "difficulty_name" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "inserted_at"string
        |> required "is_use" bool
        |> required "title" string
        |> required "duration" string
    
videoPage page=
    Decode.succeed page
        |> required "difficulty_code" string
        |> required "end_date" string
        |> required "exercise_part_code" string
        |> required "make_code" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int
    
videoDetailDecoder detaildata data item =
    Decode.succeed detaildata
        |> required "data" (videodetail data item )
videoFilterDecoder detaildata item page=
    Decode.succeed detaildata
        |> required "data" (Decode.list (videoFilterItem item) )
        |> required "paginate" (videoPaginate page)

videoPaginate page = 
    Decode.succeed page
        |> required "difficulty_code" (Decode.list string)
        |> required "exercise_code" (Decode.list string)
        |> required "instrument_code" (Decode.list string)
        |> required "page" int
        |> required "part_detail_code" (Decode.list string)
        |> required "per_page" int
        |> required "title" string
        |> required "total_count" int

-- videoFilter data item =
--     Decode.succeed data
--         |> required "difficulty_code" string
--         |> required "difficulty_name" string
--         |> required "exercise_part_code" string
--         |> required "exercise_part_name" string
--         |> required "id" int
--         |> required "title" string

videoFilterItem item=
    Decode.succeed item
        |> required "difficulty_name" string
        |> required "exercise_name"  string
        |> required "id" int
        |> required "instrument_name"  string
        |> required "part_detail_name" (Decode.list string)
        |> required "title"  string
        |> optional "value" (Decode.map Just int) Nothing
        |> optional "is_rest" (Decode.map Just bool) (Just False)
        |> required "thembnail" string
        |> required "duration" string


videodetail data item =
    Decode.succeed data
        |> required "difficulty_code" string
        |> required "difficulty_name" string
        |> required "exercise_items" (Decode.list (videoExerItem item ))
        |> required "exercise_part_code" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "title" string
        |> optional "description" (Decode.map Just string) Nothing
        

videoExerItem item=
    Decode.succeed item
        -- |> required "action_id" int
        |> required "difficulty_name" string
        |> required "exercise_id"  int
        |> required "exercise_name" string
        |> required "instrument_name" string
        |> optional "is_rest" (Decode.map Just bool)Nothing
        |> required "part_detail_name" (Decode.list string)
        -- |> required "sort" int 
        |> required "title"  string
        |> optional "value" (Decode.map Just int) Nothing
        |> required "thembnail" string
        |> required "duration" string

-- usermanage

videoData data datalist=
    Decode.succeed data 
        |> required "data" (videoDataList datalist)
        
videoDataList datalist= 
    Decode.succeed datalist
        |> required "file" string
        |> required "image" string

yfVideo videodata data items fairing =
    Decode.succeed videodata 
        |> required "data" (yfVideoData data items fairing)

yfVideoData data items fairing= 
    Decode.succeed data 
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_items" (Decode.list (yfvideoExercise items))
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "pairing" (Decode.list (yfFairing fairing))
        |> required "title" string

yfvideoExercise items = 
    Decode.succeed items
        |> optional "description" (Decode.map Just string)Nothing
        |> required "duration" string
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int

yfFairing fairing = 
    Decode.succeed fairing
        |> required "file" string
        |> required "image" string
        |> required "title" string

userInfo dataWrap data admin menus = 
    Decode.succeed dataWrap 
        |> required "data" (userInfoData data admin menus)

userInfoData data admin menus= 
    Decode.succeed data
        |> required "admin" (userInfoAdmin admin)
        |> required "menus" (Decode.list (userInfoMenus menus))

userInfoAdmin admin= 
    Decode.succeed admin
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
        |> optional "profile" (Decode.map Just string) Nothing
userInfoMenus menus= 
    Decode.succeed menus
        |> required "menu_auth_code" (Decode.list string) 
        |> required "menu_id" int
        -- |> required "menu_name" string

muserInfo  = 
    Decode.succeed DataWrap 
        |> required "data" muserInfoData

muserInfoData= 
    Decode.succeed Data
        |> required "admin" muserInfoAdmin 
        |> required "menus" (Decode.list muserInfoMenus)

muserInfoAdmin = 
    Decode.succeed Admin
        |> required "connected_at" string
        |> required "id" int
        |> required "joined_at" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
muserInfoMenus = 
    Decode.succeed Menus
        |> required "menu_auth_code" (Decode.list string) 
        |> required "menu_id" int
        |> required "menu_name" string


type alias DataWrap = 
    { data : Data }


type alias Data = 
    {
        admin : Admin,
        menus : List Menus
    }

type alias Admin = 
    {
        connected_at : String,
        id : Int,
        joined_at : String,
        nickname : Maybe String,
        username : String
    }

type alias Menus =
    {
        menu_auth_code: List String,
        menu_id : Int,
        menu_name : String
    }



faqlist faq data page=
    Decode.succeed faq
        |> required "data" (Decode.list (faqlistdata data))
        |> required "paginate" (faqlistpage page)

faqlistdata data = 
    Decode.succeed data
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_answer" bool
        |> required "title"string

faqlistpage page = 
    Decode.succeed page
        |> optional "asked_id" (Decode.map Just int) Nothing
        |> required "end_date" string
        |> optional "is_answer" (Decode.map Just bool) Nothing
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int
        |> required "username" string