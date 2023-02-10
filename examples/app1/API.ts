/*
 * State channels API
 */

// Accounts
interface Account {
  readonly name: string;
  readonly pass: string | undefined;
  readonly balance: number;
  readonly public_key: string;

  constructor(NAME: string, PASS: string|undefined, BALANCE:number, PUBKEY: string);
  isAccount(): boolean;
}

async function login(NAME: string, PASS: string): boolean;

function iam(ACC: Account): boolean;
function whoami(): Account|null;


// State channels
interface LedgerElementDescrList {
  [index: string]: number;
}

interface StateChannel {
  readonly state: string;
  readonly balance: number;

  async constructor(PARM: LedgerElementDescrList);
  isStateChannel(): boolean;

  status(): { state: string, balance: number };

  async send(AMOUNT: number, ACC: Account): boolean;
  async deposit(AMOUNT: number): boolean;
  async withdraw(AMOUNT: number): boolean;

  async withdrawAndOrClose(AMOUNT: number): boolean;
  async close(): boolean;
}
