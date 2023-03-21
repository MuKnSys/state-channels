// procs.js

// Messages
function proc_msg(FROM,TO,MSG) {
  return { FROM:FROM, TO: TO, MSG: MSG };
}

// Funcs
var _FUNC={};
function func_decl(NAME,FN) {
  _FUNC[NAME]=FN;
}
function func_call(NAME,PARM) {
  return _FUNC[NAME].apply(null,PARM);
}

// Procs (constructor)
var _PROC={};
function proc_get(UIDP) {
  return _PROC[UIDP];
}
function proc_set(PR) {
  if (isDefined(proc_get(PR.UIDP))) error("proc_set");
  _PROC[PR.UIDP]=PR;
}

var _PROCNO=1;
function proc(UID) {
  if (isUndefined(UID)) UID="PR"+_PROCNO++;
  var RES={ UIDP:UID, IN:{PTR:0,BUF:[]}, OUT:{PTR:0,BUF:[]} };
  proc_set(RES);
  net_enter(RES);
  return RES;
}

// Procs (send/recv)
function q_post(Q,MSG) {
  Q.BUF.push(MSG);
}
function proc_send(PR,TO,MSG) {
  var MSG=proc_msg(PR.UIDP,TO,MSG);
  q_post(PR.OUT,MSG);
}
function proc_recv(PR,MSG) {
  q_post(PR.IN,MSG);
}

// Procs (step)
var _SENDER;
function proc_exec(PR,MSG) {
  var RES,
      FUNC=MSG.MSG,PARM=[];
  if (isObject(MSG.MSG)) {
    FUNC=MSG.MSG.FUNC;
    PARM=MSG.MSG.PARM;
  }
  else
  if (isString(MSG.MSG)) {
    FUNC=MSG.MSG;
    PARM=[];
  }
  else error("proc_exec");
  _SENDER=MSG.FROM;
  RES=func_call(FUNC,[PR,...PARM]);
  _SENDER=undefined;
  return RES;
}
function proc_step_in(PR) {
  var Q=PR.IN;
  while (Q.PTR<Q.BUF.length) {
    proc_exec(PR,Q.BUF[Q.PTR]);
    Q.PTR++;
  }
}
function proc_step_out(PR) {
  var Q=PR.OUT;
  while (Q.PTR<Q.BUF.length) {
    var MSG=Q.BUF[Q.PTR],
        PR2=proc_get(MSG.TO);
    if (isDefined(PR2)) {
      proc_recv(PR2,MSG);
    }
    else {
      net_send(MSG);
    }
    Q.PTR++;
  }
}
function proc_step(PR,IN) {
  (IN?proc_step_in:proc_step_out)(PR);
}

function sched_step() {
  for (var UID in _PROC) {
    proc_step(_PROC[UID],1);
  }
  for (var UID in _PROC) {
    proc_step(_PROC[UID],0);
  }
}
function sched_recv(L) {
  for (var MSG of L) {
    proc_recv(proc_get(MSG.TO),MSG);
  }
}

// Procs (pretty-print)
function listmsgs(Q) {
  var FIRST=true;
  indentInc(+2);
  for (var M of Q.BUF) {
    if (!FIRST) cr();
           else FIRST=false;
    out("[");
    out(M.FROM),out("=>");
    out(M.TO),out("]::<");
    out(M.MSG),out(">");
  }
  indentInc(-2);
}
function listproc(PR,CR) {
  out(PR.UIDP);
  cr(),out("I"),out(PR.IN.PTR),listmsgs(PR.IN);
  cr(),out("O"),out(PR.OUT.PTR),listmsgs(PR.OUT);
  if (isDefined(CR)) cr();
}
function listprocs(FULL) {
  var FIRST=true;
  if (isUndefined(FULL)) FULL=1;
  for (var UIDP in _PROC) {
    if (!FIRST) (FULL?cr:spc)(); 
           else FIRST=false;
    if (FULL) {
      listproc(_PROC[UIDP]);
    }
    else out(UIDP);
  }
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

// Procs (init)
function procs_init() {
  func_decl("list",listproc);
}
procs_init();
