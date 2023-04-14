// com_srv1.js

// Inputs
var _TOPICS=[];
function com_subscribed(TOPIC) {
  return arrayContains(_TOPICS,TOPIC);
}
function com_subscribe(TOPIC) {
  if (!arrayContains(_TOPICS,TOPIC)) _TOPICS.push(TOPIC);
}

var _COM_MSG=[];
async function com_poll() {
  var L=await cli_msg(com_conn() ,["poll", com_peerid()]);
  for (var MSG of L) {
    if (isArray(MSG)) {
      if (MSG[1][0]==com_peerid()) continue;
      if (!com_subscribed(MSG[1][1])) continue;
    }
    _COM_MSG.push(MSG);
  }
}
function com_isPolling() {
  return true;
}
var _COM_POLLP=2000;
function com_pollPeriod() {
  return _COM_POLLP=2000;
}
function net_poll() {
  var L=_COM_MSG;
  _COM_MSG=[];
  return L;
}

// Outputs
function net_host() {
  cli_msg(com_conn() ,["enter-host", com_peerid()]);
}
function net_enter(UIDP) {
  cli_msg(com_conn() ,["net-enter", UIDP, com_peerid()]);
  com_subscribe(UIDP);
}

function net_serialize(MSG) {
  function _tyname(TY) {
    return "@call";
  }
  function val(V) {
    var RES=V;
    if (V===undefined) RES="#!void";
    return RES;
  }
  var RES={};
  for (var AV of MSG) {
    if (AV[0]==":TYPE" && isArray(AV[1])) RES[":TYPE"]=_tyname(AV[1]);
                                     else RES[AV[0]]=val(AV[1]);
  }
  return RES;
}
function net_send(MSG) {
  cli_msg(com_conn() ,["net-send", net_serialize(MSG)]);
}

function net_broadcast(TOPIC,MSG) {
  cli_msg(com_conn() ,["broadcast", [MSG[0], [com_peerid(),TOPIC], ...MSG.slice(1,MSG.length)]]);
}

function net_out(UID,MSG) {
  if (isArray(MSG[0]))
    net_send(MSG);
  else
  if (isString(MSG[0])) {
    if (MSG[0]=="'enter")
      net_enter(MSG[1]);
    else
    if (MSG[0]=="'enter-group")
      net_broadcast("ALL",MSG);
  }
}

// Init
var _CONN,_PEERID;
function com_conn() {
  return _CONN;
}
function com_peerid() {
  return _PEERID;
}
function com_connect(URL) {
  _CONN=cli_conn(URL);
  _PEERID=gen_uid();
  com_subscribe("ALL");
  return _CONN;
}
