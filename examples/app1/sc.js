// sc.js

// Accounts
var _ACCOUNT={};
global.Account=function (NAME,PASS,BALANCE,PRIVKEY) {
  if (new.target) {
    this.NAME=NAME;
    this.PASS=PASS;
    this.BALANCE=BALANCE;
    this.PRIVKEY=PRIVKEY;
    _ACCOUNT[NAME]=this;
  }
  else return new Account(NAME,PASS,BALANCE,PRIVKEY);
}
global.isAccount=function (ACC) {
  return type(ACC)==Account;
}
global.toAccount=function (ACC) {
  if (isString(ACC)) ACC=_ACCOUNT[ACC];
  if (!isAccount(ACC)) ACC=undefined;
  return ACC;
}
var _LOGIN_CREDENTIALS=[];
global.login=function (NAME,PASS) {
  var ACC=_ACCOUNT[NAME];
  if (ACC!==undefined && _OUTFOCUS!=null) {
    if (ACC.PASS==PASS) {
      if (!_LOGIN_CREDENTIALS.includes(ACC)) _LOGIN_CREDENTIALS.push(ACC);
      iam(ACC);
    }
  }
}
global.iam=function (ACC) {
  if (isString(ACC)) ACC=_ACCOUNT[ACC];
  if (!isAccount(ACC)) return;
  if (_OUTFOCUS!=null && _LOGIN_CREDENTIALS.includes(ACC)) _OUTFOCUS.USER=ACC;
}
global.whoami=function () {
  if (_OUTFOCUS!=null) return _OUTFOCUS.USER;
}

// State channels
global.StateChannel=function (PARM) {
  var BALANCE={};
  if (new.target) {
    this.BALANCE={};
    for (var NAME in PARM) this.BALANCE[NAME]=0;
    this.STATE="Started";
    for (var NAME in PARM) {
      iam(NAME);
      this.deposit(PARM[NAME]);
    }
    setprop(this,"state",function() {
      return this.STATE;
    },null);
    setprop(this,"balance",function() {
      var ME=whoami();
      if (ME!=null) return this.BALANCE[ME.NAME];
               else return null;
    },null);
  }
  else return new StateChannel(PARM);
}
StateChannel.create=function (PARM) {
  return StateChannel(PARM);
}
StateChannel.prototype.detailedStatus=function() {
  return { state:this.state, balance:this.balance };
}
StateChannel.prototype.deposit=function(AMOUNT) {
  if (this.STATE!="Started") return;
  var ME=whoami();
  if (!isNumber(AMOUNT)) error("sc.deposit : AMOUNT should be a number");
  if (ME!=null && ME.BALANCE>=AMOUNT) {
    ME.BALANCE-=AMOUNT;
    this.BALANCE[ME.NAME]+=AMOUNT;
  }
}
StateChannel.prototype.send=function(AMOUNT,ACC) {
  if (this.STATE!="Started") return;
  var SRC=whoami();
  if (!isAccount(ACC)) error("sc.send : ACC should be an account");
  if (!isNumber(AMOUNT)) error("sc.send : AMOUNT should be a number");
  if (ACC!=null && SRC!=null && this.BALANCE[SRC.NAME]>=AMOUNT) {
    this.BALANCE[SRC.NAME]-=AMOUNT;
    this.BALANCE[ACC.NAME]+=AMOUNT;
  }
}
StateChannel.prototype.withdraw=function(AMOUNT) {
  if (this.STATE!="Started") return;
  var ME=whoami();
  if (!isNumber(AMOUNT)) error("sc.withdraw : AMOUNT should be a number");
  if (ME!=null && this.BALANCE[ME.NAME]>=AMOUNT) {
    this.BALANCE[ME.NAME]-=AMOUNT;
    ME.BALANCE+=AMOUNT;
  }
}
StateChannel.prototype.withdrawAndOrClose=function(AMOUNT) {
  if (isUndefined(AMOUNT)) this.close;
                      else this.withdraw(AMOUNT);
}
StateChannel.prototype.close=function() {
  if (this.STATE!="Started") return;
  for (var NAME in this.BALANCE) {
    iam(NAME);
    this.withdraw(this.BALANCE[NAME]);
  }
  this.STATE="Closed";
}
StateChannel.prototype.abort=function() { // close() & abort() are the same here ; in a real state channel, close() is multisig
  this.close();
}

// Init
Account("bob","1234",1000);
Account("carol","5678",1000);

out.focus({}); // NOTE: to enable login() ; TODO: improve this.
var _THE_SC;
global.the_sc=function (SC) {
  if (SC) _THE_SC=SC;
  return _THE_SC;
}
