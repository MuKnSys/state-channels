import {Participant} from './Chenilles.base';
import {Petname, Petnaming} from './Chenilles.names';

// UI Context for Chenille interaction
export interface ChenilleContext {
  user: Petname;
  // TODO: add cryptographic context information about known private keys
  petnames: Petnaming;
  peers: Participant;
   // peers: For each supported AddressContextName, the contact point URL,
   // contract address, RPC endpoint, etc., to interact with Chenille on that address context.
   // The entry may be an empty byte array or an all zeroes address if Chenille is directly supported
   // without any such data necessary; e.g. this might be the case for Bitcoin or Cardano
   // state channels, that work without a Chenille contract.
   // Absence of such an entry means do not connect to that protocol or network
   // within that ChenilleContext.
}

// Login to an account, allowing us to use it in an active role.
// The first argument is a ChenilleContext.
// The second argument is a Petname (string).
// If needed, a UI request for passphrase, 2FA access, etc., will be requested from the user,
// but not if the user or a super-user of it was already logged in.
export function login (ctx: ChenilleContext, petname: Petname) : boolean;

// Return the current participant petname
export function whoami (ctx: ChenilleContext) : Petname | null;
