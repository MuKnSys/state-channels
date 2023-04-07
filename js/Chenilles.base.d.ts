///// Base entities for Chenilles

import type { Table, StringTable, Digest, ContentAddressed } from './Chenilles.utils';

// Bytes identifying the network name and chenille peer address on that network,
// e.g. "Ethereum" followed by the address of the Ethereum Chenilles contract,
// or digest of the above, etc.
export type AddressContextId = Uint8Array;

// AddressBytes is a context-specific array of bytes representing
// an address to which to make a payment, to be displayed in context-specific ways,
// e.g. 0xabcdef... for Ethereum vs 1abcdef... for Bitcoin.
export type AddressBytes = Uint8Array;
// IMPORTANT: Note that in the context of a Chenille interaction,
// the AddressBytes can also be the digest of some ChenilleTerms defined below.

// An AddressInContext contains both the AddressContextName and
// the bytes of an address within the specified network.
export type AddressInContext = [addressContext: AddressContextId, addressBytes: AddressBytes];

// A participant has one address on each of several contexts,
// A chenille participant MUST include at least:
// * Under the AddressContextName "SelfProposedName", a SelfProposedName
// * Under the AddressContextName "ChenilleSecp256k1Communication",
//   a permanent ECDSA secp256k1 public key that allows other participants
//   to privately communicate with the participant, presumably on top of libp2p.
//   It is NOT advised to use this public key as the basis for an Ethereum or Bitcoin address,
//   though it IS advised to somehow generate this communication key from a HD wallet master key
//   that is also used to generate Ethereum and Bitcoin addresses.
// A chenille participant will also typically include at least one of the below:
// * Under the AddressContextName "EthereumMainnet", an Ethereum address on the Ethereum main net.
// * Under the AddressContextName "BitcoinMainnet", a Bitcoin address on the Bitcoin main net.
// * Under the AddressContextName "BIP32", a HD wallet public key from which to derive other keys.
// A same address could be used for both "EthereumMainnet", "EthereumSepolia",
// as well as "LaconicTestnet", etc. But it is probably safer to have separate addresses,
// if not separate participants, on test networks vs production networks.
export type Participant = Table<AddressContextId, AddressBytes>;

// AssetId is a byte array identifying some asset, relative to a specific address context.
// e.g. on the Ethereum Chenilles contract, an empty byte array might be ETH,
// a 33-byte array starting with byte 20 might be a ERC-20 token,
// a 66-byte array starting with 721 might be an ERC-721 token,
// a 66-byte array starting with 1155 might be an ERC-1155 token, etc.
// Or we might have a table of pre-registered ERC-20, ERC-721 and ERC-1155 contracts
// to compress the AssetId representation, since it is especially expensive to use gas-wise.
export type AssetId = Uint8Array;

// Assets are a bag of assets relative a given Chenilles peer, mapping each asset id to a quantity.
// For an NFT, the quantity will be 0 or 1. Represented as an array of key-value array pairs.
// keys must not repeat.
export type Assets = Table<AssetId, BigInt>;

// Balances are a table mapping addresses to assets
// Note that in the context of Chenilles, the AddressBytes could be the digest of some ChenilleTerms
export type Balances = Table<AddressBytes, Assets>;
