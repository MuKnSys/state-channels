///// Types for naming entity when presenting them to the user.
// The types are essential for the UI, but
// the smart contract logic only happens on the underlying entities
// and an external mapping is maintained between the two.

export as namespace ChenilleNames;

import type { Table, StringTable, Digest, ContentAddressed } from './Chenilles.utils';
import type { AddressContextId, AddressBytes, AddressInContext, Participant, AssetId, Assets, Balances } from './Chenilles.base';


// Petname is a locally-assigned unique name for a participant, *never* shared externally, see
// https://spritelyproject.org/news/petname-systems.html
// https://spritely.institute/news/two-petnames-papers-are-released.html
// Self proposed name: names given to you by yourself, shared with others but not trusted by them.
// Petname: private name given to someone, trusted by the name giver, not shared with anyone else.
// Edgename: names given to someone with the intent of sharing them with others.
// Hubname: edgename registered by a naming hub, e.g. twitter, ENS, email, etc.
export type SelfProposedName = string;
export type Petname = string;
export type Edgename = string;
export type Hubname = string;

// A petnaming is a user-specific private mapping from petname to participant,
// never to be shared or trusted on the network.
export type Petnaming = StringTable<Participant>;

// AddressContextName is a Network or Protocol identifier.
// e.g. "Ethereum", "Sepolia", "Bitcoin", "BitcoinTestnet", "Cosmos", "Laconic", "libp2p", ...
// TODO: determine a proper global database to serve as reference, possibly from Filecoin?
export type AddressContextName = string;

// Asset name; e.g. "ETH", "LNT", "USDC"...
export type AssetName = string;

// NamedAssets for use in user-level frontends rather than contract-level backends.
export type NamedAssets = StringTable<BigInt>;

