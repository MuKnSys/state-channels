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
global.isArray=function (X) { return _proto(X)==_proto([]); }
global.isObject=function (X) { return _proto(X)==_proto({}); } // TODO: boxed atoms, objects & functions

global.arrayContains=function (a,o) {
  for (var i=0;i<a.length;i++) if (a[i]==o) return true;
  return false;
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
var _COLOR=null;
global.color=function (C) {
  if (!isString(C)) _COLOR=null;
               else _COLOR=C;
}
global.out=function (S) {
  if (inServer()) process.stdout.write(util.inspect(S));
  else {
    var E=document.createElement("span");
    if (!isNull(_COLOR)) E.style="color: "+_COLOR+";";
    if (!isString(S)) S=JSON.stringify(S);
    E.innerHTML=S;
    (_OUTFOCUS==null?document.body:_OUTFOCUS).appendChild(E);
    E.scrollIntoView();
  }
}
var _OUTINDENT=0;
global.indentInc=function (INC) {
  _OUTINDENT+=INC;
  if (_OUTINDENT<0) _OUTINDENT=0;
}
global.spc=function (N) {
  if (isUndefined(N)) N=1;
  while (N--) {
    if (inServer()) console.log("");
               else out("&nbsp;");
  }
}
global.indent=function () {
  var N=_OUTINDENT;
  while (N--) spc();
}
global.cr=function () {
  if (inServer()) console.log("");
             else out("<br>");
  indent();
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
global.cgiParms=function (URL) {
  var RES={},
      L=URL.split("?"),L2;
  if (L.length>1) {
    L=L[1].split("&");
    for (var S of L) {
      L2=S.split("=");
      RES[L2[0]]=L2[1];
    }
  }
  return RES;
}
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
async function cr_srv(CR) {
  if (isUndefined(CR)) await cli_msg(CONN,["cr"]);
  cr();
}

// Generate UIDs
function gen_uid() {
  return crypto.randomUUID();
}
