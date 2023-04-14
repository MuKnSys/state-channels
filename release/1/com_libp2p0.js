// com_libp2p0.js

// Inputs
var _TOPICS=[];
function com_subscribed(TOPIC) {
  return arrayContains(_TOPICS,TOPIC);
}
function com_subscribe(TOPIC) {
  if (!arrayContains(_TOPICS,TOPIC)) _TOPICS.push(TOPIC);
}

var _COM_RECVH;
function com_handler(F) {
  _COM_RECVH=F;
}
async function com_poll() {
  var L=await cli_msg(com_conn() ,["poll", com_peerid()]);
  _COM_RECVH(L);
}
function com_isPolling() {
  return true;
}
var _COM_POLLP=2000;
function com_pollPeriod() {
  return _COM_POLLP;
}

// Outputs
function net_host() {
  cli_msg(com_conn() ,["enter-host", com_peerid()]);
}
function net_broadcast(TOPIC,MSG) {
  cli_msg(com_conn() ,["broadcast", [MSG[0], [com_peerid(),TOPIC], ...MSG.slice(1,MSG.length)]]);
}
