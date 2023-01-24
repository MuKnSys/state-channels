eth.defaultAccount=eth.accounts[0]
function login(ID,PASS) {
  eth.defaultAccount=eth.accounts[ID]
  personal.unlockAccount(eth.coinbase,PASS,ID)
  miner.setEtherbase(eth.accounts[ID])
  miner.stop()
  miner.start()
}

function createInstanceHashOf(ABI,CODE) {
  login(0,"1234");
  var C=eth.contract(ABI),
      PI=C.new("Bliblablo",{from:eth.coinbase, data:CODE, gas: 2000000});
  return PI.transactionHash;
}

function getInstance(ABI,ADDR) {
  var C=eth.contract(ABI);
  return C.at(ADDR);
}
function doCall(OBJ,FNAME,PARM,ID,PASS) {
  if (ID!=undefined) login(ID,PASS);
  return OBJ[FNAME](...PARM);
}
function callMethod(ABI,ADDR,FNAME,PARM,ID,PASS) {
  var OBJ=getInstance(ABI,ADDR),
      FINISHED=false,
      RES;
  while (!FINISHED) { // FIXME: crappy polling ... but there's no async in geth's console Javascript ...
    try {
      FINISHED=true;
      RES=doCall(OBJ,FNAME,PARM,ID,PASS);
    }
    catch (E) {
      FINISHED=false;
    }
  }
  return RES;
}
