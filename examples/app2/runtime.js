// runtime.js

// Global
function inServer() {
  return (typeof window)=="undefined";
}
if (!inServer()) window.global=window;

global.error=function (MSG) {
  if (!inServer()) console.log(MSG);
              else alert(MSG);
  throw new Error(MSG);
}

// Some useful stuff
global.type=function (O) {
  try {
    return O.constructor;
  }
  catch (E) {
    return undefined;
  }
}
function _proto(X) {
  if (isDefined(X) && !isNull(X)) return X.constructor.prototype;
                             else return null;
}
global.isUndefined=function (O) { return O===undefined; }
global.isDefined=function (O) { return !isUndefined(O); }
global.isNull=function (X) { return X==null; }
global.isBoolean=function (X) { return _proto(X)==_proto(true); }
global.toBoolean=function (X) { return Boolean(X); }
global.isNumber=function (X) { return _proto(X)==_proto(1); }
global.toNumber=function (X) { return Number(X); }
global.isString=function (X) { return _proto(X)==_proto("A"); }
global.toString=function (X) { return String(X); }
global.isArray=function (X) { return _proto(X)==_proto([]); } // TODO: boxed atoms, objects & functions

global.setprop=function (O,NAME,GET,SET,E,C) {
  if (!isString(NAME)) error("setprop");
  if (isUndefined(E)) E=false;
  if (isUndefined(C)) C=false;
  var PARMS={
    "enumerable": E,
    "configurable": C
  };
  if (GET) PARMS["get"]=GET;
  if (SET) PARMS["set"]=SET;
  Object.defineProperty(O,NAME,PARMS);
}

if (inServer()) {
  global.util=require('util');
}

// DOM
global.dom={};
global.dom.fetch=function (ID) {
  if (!inServer()) return document.getElementById(ID);
}

// Out() et al.
global._OUTFOCUS=null;
global.out=function (S) {
  if (inServer()) process.stdout.write(util.inspect(S));
  else {
    var E=document.createElement("span");
    if (!isString(S)) S=JSON.stringify(S);
    E.innerHTML=S;
    (_OUTFOCUS==null?document.body:_OUTFOCUS).appendChild(E);
  }
}
global.cr=function () {
  if (inServer()) console.log("");
             else out("<br>");
}

global.out.focus=function (ID) {
  if (!inServer()) _OUTFOCUS=isString(ID)?dom.fetch(ID):ID;
}

// JSON improved
global.jsonParse=function (S) {
  if (S=="undefined") return undefined;
                 else return JSON.parse(S);
}

// HTTP
global.httpSend=function (METHOD,HREF,PARMS,DATA,CALLBACK) {
  var REQ,RES=null,
      ASYNC=isDefined(CALLBACK);
  function ret(RES) {
    if (RES!=null && REQ.getResponseHeader("Content-Type")=="application/json") RES=jsonParse(RES);
    return RES;
  }
  if (METHOD!="GET" && !isUndefined(DATA)) DATA=JSON.stringify(DATA);
                                      else DATA=null;
  REQ=new XMLHttpRequest();
  REQ.open(METHOD,HREF+(PARMS!=null && PARMS!=""?"?"+PARMS:""),ASYNC);
  REQ.setRequestHeader("Content-Type","application/x-www-form-urlencoded");
  REQ.setRequestHeader("Accept","application/json"); // Les formats qu'on _accepte_ en reponse
  REQ.onreadystatechange=function() {
    if (REQ.readyState==4) {
      if (REQ.status==200) {
        RES=REQ.responseText;
        if (ASYNC) CALLBACK(ret(RES));
      }
    }
  }
  REQ.send(DATA);
  if (ASYNC) return null;
        else return ret(RES);
}

// CLI
var HRES=[];
function cli_conn(URL) {
  var CONN={ URL:URL };
  ; // TODO: implement initial handshake, login(), etc.
  return CONN;
}
function cli_msg(CONN,O) {
  function send(O) {
    return new Promise((RET,REJ) => {
      httpSend("POST",CONN.URL,"",O,function (X) {
        RET(X);
      //HRES.push(X);
      });
    });
  }
  function exp(O) {
    function exp1(O) {
      if (isArray(O)) {
        var TAG=O.shift();
        return { _:TAG, PARM:O };
      }
      else return O;
    };
    var PARMS=O;
    if (isArray(O)) if (isArray(O[0])) PARMS=O.map(exp1);
                                  else PARMS=exp1(O);
    return PARMS;
  }
  return send(exp(O));
}
