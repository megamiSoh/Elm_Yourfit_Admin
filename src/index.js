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


setInterval(async function() {
  if (localStorage.getItem ("refresh") ==  undefined && localStorage.getItem ("token") !== null && localStorage.getItem("token") !== "undefined")
    {
      var fetchSuccess = function () {
        var parse = JSON.parse(localStorage.getItem("token"))
        var myHeaders =  new Headers({
          "Content-Type": "application/json",
          "authorization": ("bearer " + parse.token)
        });
        var myInit = 
          { method: 'GET',
          headers: myHeaders,
          mode: 'cors',
          cache: 'default' };
          fetch(url + 'auth/admin/refresh',myInit)
          .then(response => {
            if(response.status == 401) {
              localStorage.removeItem("token")
              localStorage.removeItem("refresh")
              return location.reload()
            } else  {
            return  response.json()}
          })
          .then(data => {
            parse = data.token
            var refresh = JSON.stringify(data)
            localStorage.setItem ("refresh", refresh)
          })
          .catch(error => 
          console.log(error)
          
            )
        }
        if(localStorage.getItem("refresh") == null) {
          console.log(3)
          return await fetchSuccess();
        } else {
        }
      
        
    } else {
      return;
    }
}, 10000)


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


app.ports.refreshFetchData.subscribe(
function () {
}
)



app.ports.secRefreshFetch.subscribe(function() {
  var retoken = localStorage.getItem ("refresh")
  if (retoken ==undefined) {
    localStorage.removeItem("token")
    location.reload()
  }    
  var freshParse = JSON.parse(retoken)
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
        return location.reload()
      } else {
      return  response.json()
    }
    })
    .then(data => {
      var token = JSON.stringify(data)
      localStorage.setItem ("token", token)
      app.ports.onStoreChange.send(data); 
      app.ports.onSucceesSession.send("complete")
      localStorage.removeItem("refresh")
    
    })
    
})


app.ports.thirdRefreshFetch.subscribe(function() {

  var retoken = localStorage.getItem ("refresh")
  if (retoken ==undefined) {
    localStorage.removeItem("token")
    location.reload()
  }  
  var freshParse = JSON.parse(retoken)
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
        return location.reload()
      } else {
      return  response.json()
    }
    })
    .then(data => {
      var token = JSON.stringify(data)
      localStorage.setItem ("token", token)
      app.ports.retryR.send(data); 
      app.ports.onSucceesSession.send("complete")
      localStorage.removeItem("refresh")


    })
})

app.ports.fourRefreshFetch.subscribe(function() {

  var retoken = localStorage.getItem ("refresh")
  if (retoken ==undefined) {
    localStorage.removeItem("token")
    location.reload()
  }  
  var freshParse = JSON.parse(retoken)
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
        return location.reload()
      } else {
      return  response.json()
    }
    })
    .then(data => {
      var token = JSON.stringify(data)
      localStorage.setItem ("token", token)
      app.ports.onfourChange.send(data); 
      app.ports.onSucceesSession.send("complete")
      localStorage.removeItem("refresh")


    })
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
  alert ("로그아웃 되었습니다.")
  location.reload();
} else {
  localStorage.setItem("token", t)
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
      return ;
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

registerServiceWorker();

