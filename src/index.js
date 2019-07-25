import './bulma.css';
import './main.css';
import './css/all.min.css'
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
// *****develope Api
const url ='http://13.209.49.169:4000/api/v1/'
// *****production Api
// const url ='https://api.yfit.co.kr/api/v1/'
var flags = 
  localStorage.getItem("token")

var something = (function() {
  var executed = false;
  return function() {
      if (!executed) {
          executed = true;
          alert ("로그아웃되었습니다.")
      }
  };
})();

var app = Elm.Main.init({

  node: document.getElementById('root'),
  flags: flags
});


// app.ports.pgGo.subscribe(function () {
//   IMP.init('imp85569385')
//   IMP.request_pay({
//   pg : 'html5_inicis',
//   pay_method : 'card',
//   merchant_uid : 'merchant_' + new Date().getTime(),
//   name : '주문명:결제테스트',
//   amount : 100,
//   buyer_email : 'iamport@siot.do',
//   buyer_name : '구매자이름',
//   buyer_tel : '010-1234-5678',
//   buyer_addr : '서울특별시 강남구 삼성동',
//   buyer_postcode : '123-456'
// }, function(rsp) {
//   if ( rsp.success ) {
//       var msg = '결제가 완료되었습니다.';
//       msg += '고유ID : ' + rsp.imp_uid;
//       msg += '상점 거래ID : ' + rsp.merchant_uid;
//       msg += '결제 금액 : ' + rsp.paid_amount;
//       msg += '카드 승인번호 : ' + rsp.apply_num;
//   } else {
//       var msg = '결제에 실패하였습니다.';
//       msg += '에러내용 : ' + rsp.error_msg;
//   }

//   alert(msg);
// });
// })


app.ports.getInfo.subscribe (function() {
  var sd = localStorage.getItem ("info")
    var parse = JSON.parse(sd)
    if (sd !== undefined ) {app.ports.getInfoParams.send(parse);}
      else { location.reload(); 
      } 
})



app.ports.infodata.subscribe(function (data) {
  var str = JSON.stringify(data);
  localStorage.setItem("info" , str);
  console.log(data)
  app.ports.infoCheck.send ("complete");  
 
})



app.ports.saveData.subscribe(function (data) {
  var string = JSON.stringify(data)
  localStorage.setItem ("setData" , string)
   app.ports.saveCheck.send ("complete");
})


app.ports.getParams.subscribe (function() {
  var sd = localStorage.getItem ("setData")
    var parse = JSON.parse(sd)
    app.ports.params.send(parse);
    // localStorage.removeItem ("setData")
    return 
})


function refreshFetch () {
  let token = localStorage.getItem("token")
  var freshParse = JSON.parse(token)
  var refreshTokenHeader =  new Headers({
    "Content-Type": "application/json",
    "authorization": ("bearer " + freshParse.token)
  });

var tokenInit = 
{ method: 'GET',
headers: refreshTokenHeader,
mode: 'cors',
cache: 'default' };

fetch(url + 'auth/admin/refresh',tokenInit)
  .then(response => {
    if(response.status == 401) {
      localStorage.removeItem("token")
      localStorage.removeItem("refresh")
      something();
      return location.replace("/")
    } else {
    return  response.json()
  }
  })
  .then(data => {
    var token = JSON.stringify(data)
    localStorage.setItem("refresh", token)
  
  })
}

app.ports.refreshFetchData.subscribe(function() {
    if (new Boolean (localStorage.getItem("refresh")) == true){
    let retoken =  localStorage.getItem ("refresh")
      let freshParse = JSON.parse(retoken)
        let refreshTokenHeader =  new Headers({
        "Content-Type": "application/json",
        "authorization": ("bearer " + freshParse.token)
      });

    let tokenInit = 
      { method: 'GET',
      headers: refreshTokenHeader,
      mode: 'cors',
      cache: 'default' };

      fetch(url + 'auth/admin/token',tokenInit)
        .then(response => {
          if(response.status == 401) {
            localStorage.removeItem("token")
            localStorage.removeItem("refresh")
            something();
            return location.replace("/")
          } else {
          return  response.json()
        }
        })
        .then(data => {
          let token = JSON.stringify(data)
          localStorage.setItem ("token", token)
          app.ports.onStoreChange.send(data); 
          refreshFetch ();
        
        })
      }
      else {
        return;
      }
})



app.ports.sendData.subscribe(function(data) {
  app.ports.receiveData.send("true");
  if (data.pairing == "undefined" || ! Array.isArray (data.pairing))
  	{
      jwplayer("myElement").setup(
          data
      )
    }
  else {
    {
      jwplayer("myElement").setup(
        {"playlist" : data.pairing}
    )
    }
  }
});



app.ports.storeCache.subscribe(function(token) {
var t = JSON.stringify(token)
if (token === null) {
  localStorage.removeItem("token")
  localStorage.removeItem("refresh")
  alert ("로그아웃 되었습니다.")
  location.reload();
} else {
  localStorage.setItem("token", t)
  setTimeout(() => {
    refreshFetch ()
  }, 1000);
}
app.ports.onStoreChange.send(token);


});

app.ports.heightControll.subscribe(function(data) {
  if (data) 
 {
   document.documentElement.style.overflow = "hidden" ;
  }
  else
  {
    document.documentElement.style.overflow = "scroll";
  }
})

app.ports.showToast.subscribe(function (text) {
  var x = document.getElementById("webToast") 
  x.className = "show";
  x.textContent = text
  setTimeout(function(){ x.className = x.className.replace("show", ""); }, 3000);
})

app.ports.pageNum.subscribe(function(num) {
  
  if (num == 0) {
    var pathcheck = document.cookie.match('(^|;) ?' + "pathCheck" + '=([^;]*)(;|$)');
    if(pathcheck == null ) {
      app.ports.sendPageNum.send(1)
      document.cookie = "pageNum" + '=' + 1 + ';expires=' + ';path=/';
    document.cookie = "pathCheck" + '=' + window.location.hash + ';expires=' + ';path=/';

    } else {
      
      if (pathcheck[2] == window.location.hash) {
        var val = document.cookie.match('(^|;) ?' + "pageNum" + '=([^;]*)(;|$)');
        app.ports.sendPageNum.send(parseInt(val[2]))
      }
      else {
        app.ports.sendPageNum.send(1)
        document.cookie = "pageNum" + '=' + 1 + ';expires=' + ';path=/';
      }
    }
  } else {
  document.cookie = "pageNum" + '=' + num + ';expires=' + ';path=/';
  document.cookie = "pathCheck" + '=' + window.location.hash + ';expires=' + ';path=/';

}
})

app.ports.youtubeControl.subscribe(function () {
  var iframes = document.querySelectorAll('iframe');
    for (var i = 0; i < iframes.length; i++) {
        iframes[i].parentNode.removeChild(iframes[i]);
    }
  var innerDiv = document.createElement('div');
  innerDiv.id = 'player';
  document.getElementById("playerHere").appendChild(innerDiv)
})

app.ports.youtubeVideo.subscribe(function (videoId) {
  console.log(videoId)
  var iframes = document.querySelectorAll('iframe');
    for (var i = 0; i < iframes.length; i++) {
        iframes[i].parentNode.removeChild(iframes[i]);
    }
  var innerDiv = document.createElement('div');
  innerDiv.id = 'player';
  document.getElementById("playerHere").appendChild(innerDiv)
  var player;
    player = new YT.Player('player', {
      height: '270',
      width: '640',
      videoId: videoId,
      events: {
        'onReady': onPlayerReady,
        'onStateChange': onPlayerStateChange
      }
    });
  function onPlayerReady(event) {
    event.target.playVideo();
  }
  var done = false;
  function onPlayerStateChange(event) {
    if (event.data == YT.PlayerState.PLAYING && !done) {
      setTimeout(stopVideo, 6000);
      done = true;
    }
  }
  function stopVideo() {
    player.stopVideo();
  }
})
document.addEventListener('scroll', function(e) {
  if (document.getElementById("searchHeight"))  {
    
   var scrTop = document.getElementById("searchHeight").scrollTop
   var scrH = document.getElementById("searchHeight").scrollHeight
   var scrofh = document.getElementById("searchHeight").offsetHeight
   var total = scrTop + scrofh + 1 >= scrH
   if (total) {
       app.ports.next.send(scrH)
       
   }
 }
 else {
   // console.log ("get outout")
   return;
 }
}, 
{ capture: true });



registerServiceWorker();

