// sc_js.js

// Objs
function objc(NAME,CLASS,...PARM) {
  return sc_cmd(NAME,["!",CLASS,...PARM]);
}
function objp(...NAME) {
  return sc_cmd("print",[...NAME]);
}
function lsto(NAME) {
  return sc_cmd(NAME,["^","lst"]);
}

// Hosts
function proch(USER,UID) {
  return sc_cmd("proch",[USER,UID]);
}
function chost(UID) {
  return sc_cmd("chost",[UID]);
}

// Procs
function proc(USER,UID,SELF) {
  return sc_cmd("proc",[USER,UID,SELF]);
}
function procm(UID,USER) {
  return sc_cmd("procm",[UID,USER]);
}
function iam(UID) {
  return sc_cmd("iam",isUndefined(UID)?[]:[UID]);
}

function prcall(UID,FNAME,...PARM) {
  return sc_cmd(UID,["^",FNAME,...PARM]);
}
function step(UID) {
  return sc_cmd("step",[UID]);
}

function state(UID) {
  return sc_cmd("state",[UID]);
}
function netlist(UID) {
  return sc_cmd("netlist",isUndefined(UID)?[]:[UID]);
}

// Proc groups
function join(UIDG,UIDM,...UIDs) {
  return sc_cmd("join",[UIDG,isUndefined(UIDM)?"_":UIDM,...UIDs]);
}

// Sched
function autorun(AUTO) {
  return sc_cmd("autorun",[AUTO]);
}
function sched_step() {
  sc_cmd("run",[]);
}
