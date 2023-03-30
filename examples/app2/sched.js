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

// Procs
function send(TO,FUNC,PARM) {
/*var PR=proc_get(_CURPROC),
      PR2=proc_get(TO);*/
  cli_log("send",TO,FUNC,PARM);
/*if (isUndefined(PR)) cli_out("No current proc");
  else {
  //if (isUndefined(PR2)) cli_out("Proc "+TO+" doesn't exists");
    proc_send(PR,TO,{FUNC:FUNC,PARM:[PARM]});
  }*/
  cli_prompt();
}

// Host procs
function host() {
  var PROM=net_host();
  if (com_isPolling())
    window.setInterval(function () {
      if (_IDLE_STATUS) com_poll(); // TODO: improve this _IDLE_STATUS thing
    },
    com_pollPeriod());
  return PROM;
}

// Scheduler
function poll(FETCH) {
  FETCH=isDefined(FETCH) && FETCH;
  if (FETCH) return com_poll();
        else return _COM_MSG;
}

var _MSGL;
async function sched_recv(L) {
  _MSGL=L;
  for (var MSG of L) {
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
