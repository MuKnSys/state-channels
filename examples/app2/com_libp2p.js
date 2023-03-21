// com_libp2p.js

// Basic API
function net_host() { // Not necessary (declare the host to the server)
}
function net_enter(PR) { // Not necessary
}
function net_send(MSG) { // Post MSG to TO (UID of the recipient process), taken as a libp2p pubsub topic
}
var _COM_MSG=[];
async function com_poll() { // Not necessary ; each hosted process should subscribe to a topic that is its own UID
/*
 *  NOTE: the subscribe handlers should fill _COM_MSG
 *  =>
 *  foreach received message MSG {
 *    _COM_MSG.push(MSG);
 *  }
 */
}
function com_isPolling() {
  return false;
}
function com_pollPeriod() {
  return -1;
}
function net_poll() { // Same in all com layer implementations
  var L=_COM_MSG;
  _COM_MSG=[];
  return L;
}

// Init
var _CONN,_PEERID;
function com_conn() { // Unused ; same in all implementations
  return _CONN;       // in libp2p-based implementation, _CONN is a libp2p peer object (?)
}
function com_peerid() { // Ask libp2p for this (unused outside of com.js as well)
  return _PEERID;
}
function com_connect(URL) {
//_CONN=cli_conn(URL); // Create a peer by means of an URL or of a list of URLs
//_PEERID=gen_uid();   // Generate the peer's UID
  return _CONN;
}
