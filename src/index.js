import './bulma.css';
import './main.css';
import './css/all.min.css'
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
const url ='http://13.209.49.169:4000/api/v1/'




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

app.ports.getInfo.subscribe (function() {
  var sd = localStorage.getItem ("info")
    var parse = JSON.parse(sd)
    if (sd !== undefined ) {app.ports.getInfoParams.send(parse);}
      else { location.reload(); } 
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
    localStorage.removeItem ("setData")
    return 
})


app.ports.refreshFetchData.subscribe(
function () {
  
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

    app.ports.onStoreChange.send(parse);
 setTimeout(function() {
    fetch(url + 'auth/admin/refresh',myInit)
    .then(response => {
      return  response.json()
    })
    .then(data => {
      var refresh = JSON.stringify(data)
     localStorage.setItem ("refresh", refresh)
    
    })
    .catch(error => 
     alert(error)
     
      )
  }, 30000)

}
)



app.ports.secRefreshFetch.subscribe(function() {

  var retoken = localStorage.getItem ("refresh")
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
      var tokenReceive = localStorage.getItem ("token")
      app.ports.onStoreChange.send(JSON.parse(tokenReceive)); 
      app.ports.onSucceesSession.send("complete")
    })

  
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

    app.ports.onStoreChange.send(parse);
 setTimeout(function() {
    fetch(url + 'auth/admin/refresh',myInit)
    .then(response => {
      return  response.json()
    })
    .then(data => {
      var refresh = JSON.stringify(data)
     localStorage.setItem ("refresh", refresh)
    
    })
    .catch(error => 
     alert(error)
     
      )
  }, 30000)


    
})


app.ports.thirdRefreshFetch.subscribe(function() {

  var retoken = localStorage.getItem ("refresh")
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
      var tokenReceive = localStorage.getItem ("token")
      app.ports.retryR.send(JSON.parse(tokenReceive)); 
      app.ports.onSucceesSession.send("complete")
      
    })

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

    app.ports.onStoreChange.send(parse);
 setTimeout(function() {
    fetch(url + 'auth/admin/refresh',myInit)
    .then(response => {
      return  response.json()
    })
    .then(data => {
      var refresh = JSON.stringify(data)
     localStorage.setItem ("refresh", refresh)
    
    })
    .catch(error => 
     alert(error)
     
      )
  }, 30000)

    
})


app.ports.sendData.subscribe(function(num) {
	app.ports.receiveData.send("true");
  	jwplayer("myElement").setup({
                "file" : num
            })
});



app.ports.storeCache.subscribe(function(token) {
  console.log(2)
var t = JSON.stringify(token)
if (token === null) {
  console.log("null this?")
  localStorage.removeItem("token")
} else {
  console.log("plz remove!! hello?")
  localStorage.setItem("token", t)
}
app.ports.onStoreChange.send(token);


});



registerServiceWorker();

