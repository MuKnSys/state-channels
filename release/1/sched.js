// sched.js [potpourrifying procs.js+cli.js]

// Log
var _CLIPROMPT="$";
function cli_prompt(CR) {
  if (isUndefined(CR)) CR=1;
  if (CR) {
    indentInc(-2),cr();
  }
  out(_CLIPROMPT),out(" ");
}
function cli_log(CMD,...PARM) {
  out(CMD);
  for (var I=0;I<PARM.length;I++)
    if (!isNull(PARM[I])) {
      out(" "),out(PARM[I]);
    }
}
function cli_out(S) {
  indentInc(+2),cr();
  out(S);
}

// Help
function h() {
  cli_log("h");
  cli_prompt();
}

// Host
function hid() {
  cli_log("hid");
  cli_out(com_peerid());
  cli_prompt();
}

// Server
function srv() {
  cli_log("srv");
  cli_msg(com_conn() ,["status"]);
  cli_prompt();
}

// Poll
function poll(FETCH) {
  FETCH=isDefined(FETCH) && FETCH;
  if (FETCH) return com_poll();
        else return _COM_MSG;
}

// Scheduler
async function sched_recv(L) {
  for (var MSG of L) if (isArray(MSG)) {
    if (MSG[1][0]!=com_peerid()) { // TODO: fix com_srv0.js (broadcast)
    //console.log(MSG);
      recvg(MSG[2],...MSG[3]); // TODO: test MSG[0], to dispatch to different commands
    }
  }
  else {
    MSG["TYPE"]="'@call";
    var S=JSON.stringify(MSG);
    await net_post(S);
  }
}

function run(START) {
  cli_log("run");
  START=isUndefined(START) || START;
  cli_out("sched "+(START?"started":"stopped"));
  idle_start(START);
  cli_prompt();
}

function autorun(AUTO) {
  return sc_cmd("autorun",[AUTO]);
}
function sched_step() {
  sc_cmd("run",[]);
}

// Main loop
var _IDLEH;
function idle_handler(FN) {
  _IDLEH=FN;
}
var _IDLE_STATUS=0;
function idle_start(B) {
  var OSTAT=_IDLE_STATUS;
  _IDLE_STATUS=B;
  if (!OSTAT && B) idle_main();
}
function idle_main() {
  if (isDefined(_IDLEH)) _IDLEH();
  if (_IDLE_STATUS) requestIdleCallback(idle_main);
}

// Init host
function host() {
  var PROM=net_host();
  if (com_isPolling())
    window.setInterval(function () {
      if (_IDLE_STATUS) com_poll(); // TODO: improve this _IDLE_STATUS thing
    },
    com_pollPeriod());
  return PROM;
}

// App init
async function init(URL) {
  com_connect(URL);
  await host();
  idle_main();
  idle_handler(function () {
    sched_step();
    var L=net_poll();
    if (L.length>0) sched_recv(L);
  });
}
