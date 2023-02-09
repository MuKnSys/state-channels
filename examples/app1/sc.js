// sc.js

// Accounts
var _ACCOUNT={};
global.account=function (NAME,PASS,BALANCE,PRIVKEY) {
  if (new.target) {
    this.NAME=NAME;
    this.PASS=PASS;
    this.BALANCE=BALANCE;
    this.PRIVKEY=PRIVKEY;
    _ACCOUNT[NAME]=this;
  }
  else return new account(NAME,PASS,BALANCE,PRIVKEY);
}
global.isAccount=function (ACC) {
  return type(ACC)==account;
}
global.toAccount=function (ACC) {
  if (isString(ACC)) ACC=_ACCOUNT[ACC];
  if (!isAccount(ACC)) ACC=undefined;
  return ACC;
}
global.login=function (NAME,PASS) {
  var ACC=_ACCOUNT[NAME];
  if (ACC!==undefined && _OUTFOCUS!=null) { // TODO: check pass
    _OUTFOCUS.USER=ACC;
  }
}
global.whoami=function () {
  if (_OUTFOCUS!=null) return _OUTFOCUS.USER;
}

// State channels
global.schannel=function (PARM) {
  var BALANCE={};
  if (new.target) {
    this.BALANCE={};
    for (var NAME in PARM) this.BALANCE[NAME]=0;
    this.STATE="Started";
    for (var NAME in PARM) {
      login(NAME); // FIXME: improve this
      this.deposit(PARM[NAME]);
    }
  }
  else return new schannel(PARM);
}
schannel.prototype.deposit=function(AMOUNT) {
  if (this.STATE!="Started") return;
  var ME=whoami();
  if (!isNumber(AMOUNT)) error("sc.deposit : AMOUNT should be a number");
  if (ME!=null && ME.BALANCE>=AMOUNT) {
    ME.BALANCE-=AMOUNT;
    this.BALANCE[ME.NAME]+=AMOUNT;
  }
}
schannel.prototype.transfer=function(AMOUNT,ACC) {
  if (this.STATE!="Started") return;
  var SRC=whoami();
  if (!isAccount(ACC)) error("sc.transfer : ACC should be an account");
  if (!isNumber(AMOUNT)) error("sc.transfer : AMOUNT should be a number");
  if (ACC!=null && SRC!=null && the_sc().BALANCE[SRC.NAME]>=AMOUNT) {
    this.BALANCE[SRC.NAME]-=AMOUNT;
    this.BALANCE[ACC.NAME]+=AMOUNT;
  }
}
schannel.prototype.withdraw=function(AMOUNT) {
  if (this.STATE!="Started") return;
  var ME=whoami();
  if (!isNumber(AMOUNT)) error("sc.withdraw : AMOUNT should be a number");
  if (ME!=null && this.BALANCE[ME.NAME]>=AMOUNT) {
    this.BALANCE[ME.NAME]-=AMOUNT;
    ME.BALANCE+=AMOUNT;
  }
}
schannel.prototype.close=function() {
  if (this.STATE!="Started") return;
  for (var NAME in this.BALANCE) {
    login(NAME); // FIXME: improve this
    this.withdraw(this.BALANCE[NAME]);
  }
  this.STATE="Closed";
}
schannel.prototype.abort=function() { // close() & abort() are the same here ; in a real state channel, close() is multisig
  this.close();
}

// Init
account("bob","1234",1000);
account("carol","5678",1000);

out.focus({}); // NOTE: to enable login() ; TODO: improve this.
var _THE_SC;
global.the_sc=function () {
  return _THE_SC;
}
_THE_SC=schannel({"bob":10,"carol":10});
