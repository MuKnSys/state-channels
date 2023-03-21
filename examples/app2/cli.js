// cli.js
/*
 * Examples (to issue in the browser's "Inspect" pane):
 *
 * => srv()                    : server status (display on the server) ;
 * => lp()                     : proc list (only IDs) ;
 * => lp(1)                    : proc list (only IDs) ;
 * => iam("PR1")               : current proc becomes PR1 ;
 * => send("PR2","say","blih") : send a message "say" with parameter "blih"
 *                               to the process having the UID "PR2" ;
 * => idle_start(1)            : start the main loop ; once this is done, the
 *                               peer (i.e. the browser tab) polls the server,
 *                               and runs the scheduler in the background ;
 * NOTES:
 * => to change the polling interval, modify the parameter
 *    of window.setInterval() in host() [in procs.js] ;
 */

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
function pr(UID) {
  cli_log("pr",UID);
  var PR=proc(UID);
  cli_out(PR.UIDP);
  cli_prompt();
  return PR;
}
function lp(ALL) {
  cli_log("lp");
  cli_out("");
  ALL=isDefined(ALL) && ALL;
  listprocs(ALL);
  cli_prompt();
}

var _CURPROC;
function iam(UID) {
  var DISP=1;
  cli_log("iam",UID);
  if (isDefined(UID))
    if (isDefined(proc_get(UID))) _CURPROC=UID;
                             else cli_out("Proc "+UID+" doesn't exists"),DISP=0;
  if (DISP) cli_out(isDefined(_CURPROC)?_CURPROC:"No current proc");
  cli_prompt();
}
function send(TO,FUNC,PARM) {
  var PR=proc_get(_CURPROC),
      PR2=proc_get(TO);
  cli_log("send",TO,FUNC,PARM);
  if (isUndefined(PR)) cli_out("No current proc");
  else {
  //if (isUndefined(PR2)) cli_out("Proc "+TO+" doesn't exists");
    proc_send(PR,TO,{FUNC:FUNC,PARM:[PARM]});
  }
  cli_prompt();
}
function poll(FETCH) {
  FETCH=isDefined(FETCH) && FETCH;
  if (FETCH) com_poll();
        else return _COM_MSG;
}

function say(PR,MSG) {
  out(_SENDER),out(" says "),out(MSG);
  out(" to "),out(PR.UIDP),cr();
}
func_decl("say",say);

// Scheduler
function run(START) {
  cli_log("run");
  START=isUndefined(START) || START;
  cli_out("sched "+(START?"started":"stopped"));
  idle_start(START);
  cli_prompt();
}
