// com_srv0.js

// Basic API
function net_host() {
  cli_msg(com_conn() ,["enter-host", com_peerid()]);
}
function net_enter(PR) {
  cli_msg(com_conn() ,["net-enter", PR.UIDP, com_peerid()]);
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
