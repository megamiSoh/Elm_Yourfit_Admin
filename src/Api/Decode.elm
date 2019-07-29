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
    
videoDetailDecoder detaildata data item point age =
    Decode.succeed detaildata
        |> required "data" (videodetail data item point age)
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

videoFilterItem item =
    Decode.succeed item
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "exercise_name"  (Decode.map Just string) Nothing
        |> optional "id" (Decode.map Just int) Nothing
        |> optional "instrument_name"  (Decode.map Just string) Nothing
        |> required "part_detail_name" (Decode.list (Decode.nullable string))
        |> optional "title"  (Decode.map Just string) Nothing
        |> optional "value" (Decode.map Just int) Nothing
        |> optional "is_rest" (Decode.map Just bool) (Just False)
        |> required "thembnail" string
        |> required "duration" string


videodetail data item point age =
    Decode.succeed data
        |> required "difficulty_code" string
        |> required "difficulty_name" string
        |> required "exercise_items" (Decode.list (videoExerItem item ))
        |> required "exercise_part_code" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "title" string
        |> optional "description" (Decode.map Just string) Nothing
        |> optional "is_male" (Decode.map Just bool) Nothing
        |> required "is_pay" bool
        |> optional "exercise_points" (Decode.map Just ((Decode.list (exercise_points point)))) Nothing
        |> optional "age_ranges" (Decode.map Just ((Decode.list (exercise_points age)))) Nothing
exercise_points point = 
    Decode.succeed point
        |> required "code" string
        |> required "name" string

videoExerItem item=
    Decode.succeed item
        -- |> required "action_id" int
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "action_id"  (Decode.map Just int) Nothing
        |> optional "exercise_name" (Decode.map Just string) Nothing
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> optional "is_rest" (Decode.map Just bool)Nothing
        |> required "part_detail_name"(Decode.list (Decode.nullable string)) 
        -- |> required "sort" int 
        |> optional "title"  (Decode.map Just string) Nothing
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
type Profile 
    = Profile DataWrap

myProfileInfo  = 
    Decode.succeed DataWrap 
        |> required "data" muserInfoData
        |> Decode.map Profile

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

myname ( Profile info ) = 
    info.data.admin.username
mymenu (Profile info) = 
    info.data.menus

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

faqNewList faq data page = 
    Decode.succeed faq
        |> required "data" (Decode.list (faqNewListData data))
        |> required "paginate" (faqNewPage page)

faqNewListData data = 
    Decode.succeed data 
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string

faqNewPage page = 
    Decode.succeed page
        |> required "end_date" string
        |> optional "is_use" (Decode.map Just bool) Nothing
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

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
        |> optional "username" (Decode.map Just string)Nothing

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

faqDetail data detail =
    Decode.succeed data
        |> required "data" (faqdetailList detail)

faqdetailList detail = 
    Decode.succeed detail
        |> optional "answer" (Decode.map Just string) Nothing
        |> required "asked_id" int
        |> required "content" string
        |> required "id" int
        |> required "is_answer" bool
        |> required "title" string
        |> optional "username" (Decode.map Just string) Nothing


pointCode data item = 
    Decode.succeed data
        |> required "data" (Decode.list (pointCodeData item))


pointCodeData item = 
    Decode.succeed item
        |> required "code" string
        |> required "name" string

videoCodeData data item = 
    Decode.succeed data
        |> required "data" (Decode.list (videoCode item))

videoCode item =
    Decode.succeed item
        |> required "code" string
        |> required "name" string

youtubeVideoData data info id snippet page thumb thumbItem = 
    Decode.succeed data 
        |> required "data" (Decode.list (videoDataInfo info id snippet thumb thumbItem))
        |> required "paginate" (videoDataPage  page )

videoDataInfo info id snippet thumb thumbItem= 
    Decode.succeed info
        |> required "etag" string
        |> required "id" (videoId id)
        |> required "kind" string
        |> required "snippet" (videoSnippet snippet thumb thumbItem)


videoId id = 
    Decode.succeed id
        |> required "kind" string
        |> required "videoId" string

videoSnippet snippet thumb thumbItem = 
    Decode.succeed snippet 
        |> required "channelId" string
        |> required "channelTitle" string
        |> required "description" string
        |> required "liveBroadcastContent" string
        |> required "publishedAt" string
        |> required "thumbnails" (thumbnail thumb thumbItem)
        |> required "title" string

thumbnail thumb thumbItem = 
    Decode.succeed thumb
        |> required "default" (thumbnailItem thumbItem)
thumbnailItem thumbItem = 
    Decode.succeed thumbItem
        |> required "height" int
        |> required "url"  string
        |> required "width" int

videoDataPage page = 
    Decode.succeed page
        |> required "next_token" string
        |> required "page_token" string
        |> required "per_page" int
        |> required "prev_token" string
        |> required "search_word" string
        |> required "total_count" int
-- yfAgeData = 
--     Decode.succeed data
--         |> required "data" (Decode.list (yfAge item))

-- yfAge item = 
--     Decode.succeed item
--         |> required "code" string
--         |> required "name" string

apivideolist data api page=
    Decode.succeed data
        |> required "data" (Decode.list (apivideo api))
        |> required "paginate" (apipaginate page)

apivideo api = 
    Decode.succeed api
        |> required "category" string
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string

apipaginate page =
    Decode.succeed page
        |> required "end_date" string
        |> optional "is_use" (Decode.map Just bool) Nothing
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int
        |> required "video_code" string

apiDetailDataWrap dataWrap data snippet items pageinfo itemsnippet thumb thumbItem local = 
    Decode.succeed dataWrap 
        |> required "data" (apiDetailData data snippet items pageinfo itemsnippet thumb thumbItem local) 

apiDetailData data snippet items pageinfo itemsnippet thumb thumbItem local= 
    Decode.succeed data 
        |> required "content" string
        |> required "id" int
        |> required "media_id" string
        |> required "snippet" (apidetailSnippet snippet items pageinfo itemsnippet thumb thumbItem local)
        |> required "title" string
        |> required "video_code" string

apidetailSnippet snippet items pageinfo itemsnippet thumb thumbItem local= 
    Decode.succeed snippet
        |> required "etag" string
        |> required "items" (Decode.list (apiDetailItems items itemsnippet local thumb thumbItem))
        |> required "kind" string
        |> required "pageInfo" (apidetailPageInfo pageinfo)

apiDetailItems items itemSnippet local thumb thumbItem= 
    Decode.succeed items 
        |> required "etag" string
        |> required "id" string
        |> required "kind" string
        |> required "snippet" (apidetailItemSnippet itemSnippet local thumb thumbItem)
        -- |> required "title" string
        

apidetailPageInfo pageInfo = 
    Decode.succeed pageInfo 
        |> required "resultsPerPage" int
        |> required "totalResults" int


apidetailItemSnippet itemSnippet local thumb thumbItem= 
    Decode.succeed itemSnippet 
        |> required "categoryId" string
        |> required "channelId" string
        |> required "channelTitle" string
        -- |> required "defaultAudoiLanguage" string
        |> required "description" string
        |> required "liveBroadcastContent" string
        |> required "localized" (apidetailLocal local)
        |> required "publishedAt" string
        -- |> required "tags" (Decode.list string)
        |> required "thumbnails" (apidetailThumb thumb thumbItem)
        |> required "title" string
    
apidetailThumb thumb thumbItem=
    Decode.succeed thumb 
        |> required "default"  (apidetailThumbItem thumbItem)

apidetailThumbItem thumbItem = 
    Decode.succeed thumbItem 
        |> required "height"  int
        |> required "url"  string
        |> required "width" int
    
apidetailLocal local = 
    Decode.succeed local 
        |> required "description" string
        |> required "title" string


productList list data page = 
    Decode.succeed list
        |> required "data" (Decode.list (productListData data))
        |> required "paginate" (productListPaginate page)

productListData data = 
    Decode.succeed data
        |> required "day_name" int
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_pay" bool
        |> required "is_use" bool
        |> required "name" string
        |> required "price" int
        |> required "product_code_name" string 

productListPaginate page = 
    Decode.succeed page
        |> required "end_date" string
        |> required "is_use" (Decode.nullable bool)
        |> required "name" string
        |> required "page" int
        |> required "per_page" int
        |> required "product_code" string
        |> required "start_date" string
        |> required "total_count" int


productDetailData data detail = 
    Decode.succeed data
        |> required "data" (productDetail detail)

productDetail detail = 
    Decode.succeed detail
        |> required "day_num" int
        |> required "description" string
        |> required "id" int
        |> required "is_pay" bool
        |> required "name" string
        |> required "price" int

bannerimageList list data page = 
    Decode.succeed list 
        |> required "data" (Decode.list (bannerimageData data))
        |> required "paginate" (bannerimagePage page)

bannerimageData data = 
    Decode.succeed data
        |> required "file_id" int
        |> required "inserted_at" string
        |> required "path" string
        |> required "title" string

bannerimagePage page = 
    Decode.succeed page
        |> required "end_date" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

imgRegist data list =
    Decode.succeed data 
        |> required "data" (imageregistdata list)

imageregistdata list = 
    Decode.succeed list 
        |> required "path" string

bannerList data list page = 
    Decode.succeed data
        |> required "data" (Decode.list (bannerListData list))
        |> required "paginate" (bannerListPage page)

bannerListData list = 
    Decode.succeed list
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "link" (Decode.nullable string)
        |> required "src" string 
        |> required "title" string

bannerListPage page = 
    Decode.succeed page
        |> required "end_date" string
        |> required "is_use" (Decode.nullable string)
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int