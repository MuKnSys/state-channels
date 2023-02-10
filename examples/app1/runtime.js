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
  return X.constructor.prototype;
}
global.isUndefined=function (O) { return O===undefined; }
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
