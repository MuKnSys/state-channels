// com_srv0.js

// Basic API
function net_host() {
  cli_msg(com_conn() ,["enter-host", com_peerid()]);
}
function net_enter(PR) {
  cli_msg(com_conn() ,["net-enter", PR.UIDP, com_peerid()]);
}
function net_serialize(MSG) {
  function _tyname(TY) {
    return "@call";
  }
  function val(V) {
    var RES=V;
    if (V===undefined) RES="#!void";
  /*else
    if (V===true) RES="#t";
    else
    if (V===false) RES="#f";
    else
    if (isNumber(V)) RES=(V).toString();
    else
    if (isString(V)) RES='"'+V+'"';*/
    return RES;
  }
  var RES={};
  for (var AV of MSG) {
    if (AV[0]==":TYPE" && isArray(AV[1])) RES[":TYPE"]=_tyname(AV[1]);
                                     else RES[AV[0]]=val(AV[1]);
  }
  return RES;
}
var _MSG,_MSG0,_MSGG;
function net_out(UID,MSG) { // TODO: reunify with net_enter()
_MSG0=MSG;
  if (isArray(MSG[0]))
    cli_msg(com_conn() ,["net-send", _MSG=net_serialize(MSG)]);
  else
  if (isString(MSG[0])) {
    if (MSG[0]=="'enter")
      cli_msg(com_conn() ,["net-enter", MSG[1], com_peerid()]);
    else
    if (MSG[0]=="'enter-group")
      cli_msg(com_conn() ,["broadcast", [MSG[0], com_peerid(), ...MSG.slice(1,MSG.length)]]);
  }
}
function net_send(MSG) {
  cli_msg(com_conn() ,["net-send", MSG]);
}
var _COM_MSG=[];
async function com_poll() {
  var L=await cli_msg(com_conn() ,["poll", com_peerid()]);
  for (var MSG of L) {
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
  return _CONN;
}
