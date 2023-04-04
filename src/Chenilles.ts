/*
 * State channels API
 */

// Petname, locally-assigned unique name for account, *never* shared externally, see
// https://spritelyproject.org/news/petname-systems.html
// https://spritely.institute/news/two-petnames-papers-are-released.html
type Petname = string; // e.g. "bob", "auntMay", "peter", "spiderman"...

// Network identifier. To Be Specified
type Network = string; // e.g. "ethereum", "cosmos", "laconic"...

// Address on the specified network
type Address = string; // e.g. on Ethereum, "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"

// Accounts
interface Account {
  readonly petname: Petname;
  readonly network: Network|null;
  readonly address: Address;

  constructor(petname: Petname, network: Network|null, address: Address);
  isAccount(): boolean;
}

// Login to an account, allowing us to use it in an active role.
// TODO: integrate with wallet or use its existing facility.
async function login(petname: Petname, passphrase: string): boolean;

// Choose current account to use in active role.
function iam(account: Account): boolean;

// Display current account
function whoami(): Account|null;

// Asset name
type AssetName = string; // e.g. "ETH", "LNT", "USDC"...

// Vector of assets
type Assets = [assetName: AssetName]: BigInt

// Assignment of assets to Petname-identified accounts
// used to specify deposits to or withdrawal from state channels, etc.
// for the initial deposit, specify an account with an empty vector of assets
// for participants who will be part of the state channel yet won't be depositing at first
interface PetLedger {
  [index: Petname]: Assets;
}

// Condition driving the release of table stakes in case of challenge.
// Table stakes are all assets in the channel that are assigned to a participant
type ChenilleCondition = string;

type ChenilleState = {
  sequence: BigInt, // sequence number of latest consensually signed state
  assets: Assets, // total assets under management
  balances: PetLedger, // assets fully assigned to each participant
  condition: ChenilleCondition, // associated condition for table stakes
  cumulatedDeposits: PetLedger, // cumulated deposits of each participant since creation
  cumulatedWithdrawals: PetLedger, // cumulated withdrawals of each participant since creation
};

// Our State Channel interface
interface Chenille {
  readonly network : Network, // blockchain network on which the state channel is managed
  readonly parent : Address|null, // address for the contract that manages the channel
  readonly nonce : string, // nonce identifying the state channel within the network or contract

  async constructor(initialDeposit: PetLedger);
  isChenille(): boolean;

  status(): ChenilleStatus;

  async currentState(): ChenilleState;
  async send(assets: Assets, recipient: Petname): boolean;
  async deposit(assets: Assets): boolean;
  async withdraw(assets: Assets): boolean;
  async close(): boolean;
}
