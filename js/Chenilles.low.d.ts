// ChenilleIdentificationUint8Array are what a ChenilleId is a digest of. Its design is necessary
// to ensure security, so you cannot be rugpulled into signing a message that ends up being
// interpreted in a state channel that was not initialized to what you believed it would be.
export type ChenilleIdentificationBytes = [
  nonce: Digest, // for security, participants hash the parent-id with a mutually random number
  initialBalances: Balances, // the assets to be deposited upon channel creation.
  initialState: Digest]; // digest of the initial state

// A ChenilleId is computed as a digest of the ChenilleIdentificationUint8Array,
// as checked by the Chenilles contract on Ethereum, which will also check that
// the initial-balances corresponds to the assets deposited into the state channel during creation.
// On Bitcoin it might be just the hash of the UTXO of state channel.
// State Channels involve a lot of pre-signing transactions. You must only pre-sign transactions
// as part of state channels that you agreed upon, for which you properly contributed
// a random number to the nonce.
type ChenilleId = Digest;

// LOW-LEVEL CHENILLE API FOR EUTXO, etc.

// Terms driving the release of some assets, in an encoding relative
// to the context of a specific Chenilles peer.
// Analogous to a Bitcoin lock script or an Ethereum smart contract.
// E.g. on our Ethereum Chenilles contract, it will encode parameters for
// a StaticCall to some contract validating the terms of the agreement.
// Term validators will be called with length-prefix serializations of:
// (1) the term parameters, (2) a ChenilleTransaction to validate including data and witnesses,
// (3) a list for each of the inputs and outputs that the validator is directly associated with
// of pointers to (a) the transaction, (b) the element of data (or revealed nested witness data)
// corresponding to that witness, and (c) the sub-element of the witness that corresponds to
// this validator. A term validator will either fail, or succeed while outputing a set of
// "spending witnesses" that must be disjoint from those of other validators and sub-validators.
// Typical spending witnesses are digests of an input (plus byte 0 as input tag) or
// an output (same with byte 1) marking said input or output as not to be accounted multiple times.
export type ChenilleTerms =
  ChenilleTermsNew |
  ChenilleTermsEcdsa |
  ChenilleTermsOutputs |
  ChenilleTermsDisjunction |
  ChenilleTermsConjunction |
  ChenilleTermsChallenge
  // ChenilleTermsEthSchnorr |
  // ChenilleTermsLnHtlc |
  // ChenilleTermsStaticCall |
  // ChenilleTermsGroth16 |
  // ChenilleTermsPlonk |

export interface ChenilleTermsNew {};
   // Default terms of a Chenille, only valid if the ChenillesId is uninitialized.
   // Never valid as input.
   // Valid as output if the witness is a list of the inputs claimed,
   // (that will be claimed as spending witnesses) together with a validation of the
   // ChenilleId as being correctly digested from some revealed ChenilleIdentificationUint8Array
   // that match the claimed inputs and initial state.
   // Spending witnesses for the claimed inputs and outputs are issued.

export interface ChenilleTermsEcdsa {
  address: AddressBytes;
}
   // Valid on input if transaction authorized by standard EOA ECDSA signature.
   // Witness: the signature by that address of the entire ChenilleTransaction.
   // Spending Witnesses: digest of the UTXOs being claimed by the transaction.
   // Always valid on output.

export interface ChenilleTermsOutputs {
  outputs: ContentAddressed<Table<ChenilleId, ChenilleState>>;
}
   // Valid on inputs if the outputs exactly matches the digested chenilles and states.
   // Witness: empty.
   // Spending Witnesses: digest of this UTXOs being accounted for.
   // Always valid on output.
   // Notably used to implement proposed settlements and Nested state channels, etc.

export interface ChenilleTermsDisjunction {
  terms: List<ContentAddressed<ChenilleTerms>>; // A or B or C...
}
   // Valid if one of the terms matches
   // witness: the UInt16 index of the sub-term,
   // the length-prefixed bytes of the chosen term, and
   // the length-prefixed bytes of its witness.
   // Notably used to implement multisig as taking priority over challenge
   // Always valid on output.

export interface ChenilleTermsConjunction {
  terms: List<ContentAddressed<ChenilleTerms>>; // A and B and C...
}
   // valid if all of the terms match
   // witness: a sequence of length-prefixed bytes of each of the terms' witnesses.
   // Notably used to implement simple n-of-n multisig.

export interface ChenilleTermsChallenge {
  : (ContentAddressed
               (Record
                control: [ChenilleTerms] // who can post a challenge update or 
                proposed: [ProposedState]))
   // Valid on input if one of either
   // (a) the witness starts with a challenge update prefix (and the deadline hasn't passed?),
   // followed by a witness for the data being digest of timestamp followed by previous data
   // followed by a witness for transaction from previous data to agreed state.
   // for the control of a proposed state from this ChenilleId
   // with the previous state and some more recent proposed state, or
   // (b) the deadline has passed and the witness starts with a deadline passed prefix
   // followed by a witness for the proposed state's terms.
   // Always valid on output, except that the but consumes all the data and produces
   // a deadline followers by the previous data as output.
   // Usually used in disjunction with a multisig for settlemetnsl

   #| TO BE IMPLEMENTED LATER:
   EthSchnorr: Address
   // valid if authorized by Ethereum-modified Schnorr signature as per chainlink code.
   // witness: the signature by that address of the entire ChenilleTransaction
   // input data consumed: none. output data produced: none. -- but checked by the signature

   LnHtlc: Uint8Array // parameters for a HTLC compatible with the Bitcoin Lightning Network
   StaticCall: (Tuple Address Uint8Array) // parameters to a STATICCALL to some validating contract.
   // Groth16: ... // validating contract interactions via a Groth16 zk SNARK ?
   // Plonk: ... // validating contract interactions via a Plonk zk SNARK ?
   |#))

interface ProposedState = {
  (Record
   timestamp: [UInt256] // sequence number to compare which state is more recent
   balances: [(ContentAddressed Balances)])) // terms for distribution if this state is confirmed after deadline

// A Chenille is essentially a EUTXO (UTXO with multiple assets and a lock script)
// with a persistent id (its ChenilleId) and additional data that can be queried.
(define-type ChenilleState
  (Record
   balances: [Balances]
   control: [(ContentAddressed ChenilleTerms)]
   data: Uint8Array32)) // optional data synchronously updated by the control program,
   // typically a mutually agreed random nonce to prevent replay attacks on multi-session chenilles,
   // or the hash of such a nonce and some data to be used as input for the term
   // and verified as output for more elaborate chenilles.
   // For instance, it could include a digest of the cumulated-deposits and
   // cumulated-withdrawals balances for state channels with asynchronous deposits and withdrawals, etc.

// Additional data, interaction-specific, associated to a Chenille
// that can be queried asynchronously by other contracts.
// That's where e.g. the state of the Laconic Bridge will be stored,
// or messages sent by oracles and bridges, etc.
// In particular, some state channels that allow for asynchronous deposits and withdrawals
// will have in their heap entries as follow
// cumulatedDeposits: [Balances] // cumulated deposits of each participant since creation
// cumulatedWithdrawals: [Balances] // cumulated withdrawals of each participant since creation
(define-type Heap
  (HashTable Uint8Array32 <- Uint8Array32))

// Type of the data in the overall Ethereum Chenilles contract
(define-type ChenillesContractState
  (Record
   states: (HashTable ChenilleState <- ChenilleId)
   heaps: (HashTable Heap <- ChenilleId)))

// a ChenilleTransaction is very similar to a Bitcoin transaction with UTXOs,
// except using persistent ChenilleId instead of ever-changing content-addressed UTXO hash.
// A ChenilleId may appear in multiple inputs and outputs, as long as there initial state is
// the first input, and each successive input state is the state from the previous output,
// and at most one output state doesn't match an input, at which point it is the final state
// of the Chenille. If no such state is specified, that Chenille is deleted
// at the end of the transaction.
// Note: for external deposits from outside the Chenilles contract, we do all ERC-20 transfer calls
// with given input addresses, but for raw ETH we only validate the sum and trust the poster
// not to credit any address he did not otherwise receive funds for.
// Thus, it is always safer to first deposit into a Chenille of yours, then
// do a regular transfer without external deposits and withdrawals. In some cases this may be required.
(declare-type ChenilleTransaction
  (Record
    inputs: (List (Tuple ChenilleId ChenilleState)) // the participating Chenilles in their current state
    outputs: (List (Tuple ChenilleId ChenilleState)) // the new Chenilles in their new states.
    deposits: Balances
    withdrawals: Balances))

// Have the validation witnesses be a separate object.
(declare-type ChenilleTransactionWitness
  Uint8Array)

(declare-type post-chenille-transaction
  (Fun Bool <- ChenilleTransaction))

