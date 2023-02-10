/*
 * State channels API
 */

// Accounts
interface Account {
  name: string;
  pass: string | undefined;
  balance: number;
  public_key: string;

  constructor(NAME: string, PASS: string|undefined, BALANCE:number, PUBKEY: string);
  isAccount(): boolean;
}

function login(NAME: string, PASS: string): boolean;

function iam(ACC: Account): boolean;
function whoami(): Account|null;


// State channels
interface LedgerElementDescrList {
  [index: string]: number;
}

interface StateChannel {
  readonly state: string;
  readonly balance: number;

  constructor(PARM: LedgerElementDescrList);
  isStateChannel(): boolean;

  status(): { state: string, balance: number };

  send(AMOUNT: number, ACC: Account): boolean;
  deposit(AMOUNT: number): boolean;
  withdraw(AMOUNT: number): boolean;

  withdrawAndOrClose(AMOUNT: number): boolean;
  close(): boolean;
}
