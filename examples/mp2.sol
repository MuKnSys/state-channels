pragma solidity >=0.4.0 <0.7.0;

contract Micropay {
  uint storedData=1234;

// get/set
  function get() public view returns (uint) {
    return storedData;
  }

  event Set(bytes32 fname, address indexed _from, uint x);
     // TODO: remove fnames from everywhere, this information can be obtained by means of decoding the event's keccak
  function set(uint x) public {
    storedData=x;
    emit Set("set", msg.sender,x);
  }

  event Set2(bytes32 fname, address indexed _from, uint x, bytes32 from, bytes32 nonce);
  function set2(uint x, bytes32 uidFrom, bytes32 nonce) public {
    storedData=x;
    emit Set2("set2", msg.sender, x, uidFrom, nonce);
  }

// Users' addresses & balances
  uint STATE = 0; // 0=Uninitialized ; 1=Started ; 2=Finished
  address[] ACCOUNT;
  mapping(address => uint) BALANCE;

  event State(bytes32 FNAME, address indexed _FROM, uint STATE, bytes32 FROM, bytes32 NONCE);
  function init(address[] calldata ACC, bytes32 UIDFrom, bytes32 NONCE) external {
  //if (STATE != 0) return;
    ACCOUNT = new address[](0); // FIXME: remove this ; for testing purposes only
    for (uint I=0; I<ACC.length; I++) {
      ACCOUNT.push(ACC[I]);
    }
    for (uint I=0; I<ACCOUNT.length; I++) {
      BALANCE[ACCOUNT[I]] = 0;
    }
    STATE = 1;
    emit State("state", msg.sender, STATE, UIDFrom, NONCE);
  }

  function state() public view returns (uint) {
    return STATE;
  }

  function accounts() public view returns (address[] memory) {
    return ACCOUNT;
  }

  function balances() public view returns (uint[] memory) {
    uint[] memory RES = new uint[](ACCOUNT.length);
    for (uint I=0; I<ACCOUNT.length; I++) {
      RES[I] = BALANCE[ACCOUNT[I]];
    }
    return RES;
  }

// deposit ; TODO: make this function payable
  event Deposit(bytes32 FNAME, address indexed _FROM, uint AMOUNT, bytes32 FROM, bytes32 NONCE);
  function deposit(uint AMOUNT, bytes32 UIDFrom, bytes32 NONCE) public {
    BALANCE[msg.sender] += AMOUNT;
    emit Deposit("deposit", msg.sender, AMOUNT, UIDFrom, NONCE); // FIXME: how to return false when _FROM's balance is too low ?
  }

// withdraw ; TODO: verify that the balance is more than AMOUNT, & do the transfer to _FROM
  event Withdraw(bytes32 FNAME, address indexed _FROM, uint AMOUNT, bytes32 FROM, bytes32 NONCE);
  function withdraw(uint AMOUNT, bytes32 UIDFrom, bytes32 NONCE) public {
    bool RES = true;
    if (BALANCE[msg.sender] > AMOUNT) BALANCE[msg.sender] -= AMOUNT;
                                 else RES = false;
    emit Withdraw("withdraw", msg.sender, AMOUNT, UIDFrom, NONCE); // TODO: store RES inside the event
  }
}
