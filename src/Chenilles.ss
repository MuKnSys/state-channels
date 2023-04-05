;;;;; State channels API in Scheme

(import
  (only-in :std/sugar defrule)
  (only-in :clan/poo/mop define-type Any)
  (only-in :clan/poo/type String Sum Record Unit List Tuple)
  (only-in :clan/persist/content-addressing ContentAddressed)
  (only-in :mukn/ethereum/types Bytes32 UInt256 Bytes)
  (only-in :mukn/ethereum/ethereum Address Digest))

(defrule (declare-type id type) (void)) ;; Type declarations for functions to be implemented :-/
(defrule (HashTable Value <- Key) Any) ;; Type to be properly defined in :clan/poo/type


;; Petname is a locally-assigned unique name for a participant, *never* shared externally, see
;; https://spritelyproject.org/news/petname-systems.html
;; https://spritely.institute/news/two-petnames-papers-are-released.html
;; Self proposed name: names given to you by yourself, shared with others but not trusted by them.
;; Petname: private name given to someone, trusted by the name giver, not shared with anyone else.
;; Edgename: names given to someone with the intent of sharing them with others.
;; Hubname: edgename registered by a naming hub, e.g. twitter, ENS, email, etc.
(define-type SelfProposedName String)
(define-type Petname String)
(define-type Edgename String)
(define-type Hubname String)

;; AddressContextName is a Network or Protocol identifier.
;; e.g. "Ethereum", "Sepolia", "Bitcoin", "BitcoinTestnet", "Cosmos", "Laconic", "libp2p", ...
;; TODO: determine a proper global database to serve as reference, possibly from Filecoin?
(define-type AddressContextName String)

;; AddressBytes is a context-specific array of bytes representing
;; an address to which to make a payment, to be displayed in context-specific ways,
;; e.g. 0xabcdef... for Ethereum vs 1abcdef... for Bitcoin.
(define-type AddressBytes Bytes)
;; IMPORTANT: Note that in the context of a Chenille interaction,
;; the AddressBytes can also be the digest of some ChenilleTerms defined below.

;; An AddressInContext contains both the AddressContextName and
;; the bytes of an address within the specified network.
(define-type AddressInContext (Tuple AddressContextName AddressBytes))

;; A participant has one address on each of several contexts,
;; A chenille participant MUST include at least:
;; * Under the AddressContextName "SelfProposedName", a SelfProposedName
;; * Under the AddressContextName "ChenilleSecp256k1Communication",
;;   a permanent ECDSA secp256k1 public key that allows other participants
;;   to privately communicate with the participant, presumably on top of libp2p.
;;   It is NOT advised to use this public key as the basis for an Ethereum or Bitcoin address,
;;   though it IS advised to somehow generate this communication key from a HD wallet master key
;;   that is also used to generate Ethereum and Bitcoin addresses.
;; A chenille participant will also typically include at least one of the below:
;; * Under the AddressContextName "EthereumMainnet", an Ethereum address on the Ethereum main net.
;; * Under the AddressContextName "BitcoinMainnet", a Bitcoin address on the Bitcoin main net.
;; * Under the AddressContextName "BIP32", a HD wallet public key from which to derive other keys.
;; A same address could be used for both "EthereumMainnet", "EthereumSepolia",
;; as well as "LaconicTestnet", etc. But it is probably safer to have separate addresses,
;; if not separate participants, on test networks vs production networks.
(define-type Participant
  (HashTable NetworkAddressBytes <- AddressContextName))

;; A petnaming is a user-specific private mapping from petname to participant,
;; never to be shared or trusted on the network.
(define-type Petnaming
  (HashTable Participant <- Petname))

(define-type ChenilleContext
  (Record
   petnames: [Petnaming]
   peers: [Participant]
   ;; peers: For each supported AddressContextName, the contact point URL,
   ;; contract address, RPC endpoint, etc., to interact with Chenille on that address context.
   ;; The entry may be an empty byte array or an all zeroes address if Chenille is directly supported
   ;; without any such data necessary; e.g. this might be the case for Bitcoin or Cardano
   ;; state channels, that work without a Chenille contract.
   ;; Absence of such an entry means do not connect to that protocol or network
   ;; within that ChenilleContext.
   ))

;; Login to an account, allowing us to use it in an active role.
;; The first argument is a ChenillesContext.
;; The second argument is a Petname (String).
;; If needed, a UI request for passphrase, 2FA access, etc., will be requested from the user,
;; but not if the user or a super-user of it was already logged in.
(declare-type login (Fun Bool <- ChenillesContext Petname))

;; Return the current participant petname
(declare-type whoami (Fun (Maybe Petname) <- ChenillesContext))

;; Asset name; e.g. "ETH", "LNT", "USDC"...
(define-type AssetName String)

;; Asset Id, relative to a specific Chenilles address context.
;; e.g. on the Ethereum Chenilles contract, an empty byte string might be ETH,
;; a 33-byte string starting with 0 might be a ERC-20 token,
;; a 65-byte string starting with 0 might be an ERC-721 token,
;; a 65-byte string starting with 1 might be an ERC-1155 token, etc.
;; Or we might have a table of pre-registered ERC-20, ERC-721 and ERC-1155 contracts
;; to compress the AssetId representation, since it is especially expensive to use gas-wise.
(define-type AssetId Bytes)

;; Assets are a bag of assets relative a given Chenilles peer, mapping each asset id to a quantity.
;; For an NFT, the quantity will be 0 or 1.
(define-type Assets
  (HashTable BigInt <- AssetId))

;; NamedAssets for use in user-level frontends rather than contract-level backends.
(define-type NamedAssets
  (HashTable BigInt <- AssetName))

;; Balances are a table mapping addresses to assets
;; Note that in the context of Chenilles, the AddressBytes could be the digest of some ChenilleTerms
(define-type Balances
  (HashTable Assets <- AddressBytes))

;; digest of the network name and chenille peer address on that network,
;; e.g. "Ethereum" followed by the address of the Ethereum Chenilles contract.
(define-type ParentId Digest)

;; ChenilleIdentificationBytes are what a ChenilleId is a digest of. Its design is necessary
;; to ensure security, so you cannot be rugpulled into signing a message that ends up being
;; interpreted in a state channel that was not initialized to what you believed it would be.
(define-type ChenilleIdentificationBytes
  (Record
   nonce: [Digest] ;; for security, participants hash the parent-id with a mutually random number
   initial-balances: [Balances] ;; the assets to be deposited upon channel creation.
   initial-state: [Digest])) ;; digest of the initial state

;; A ChenilleId is computed as a digest of the ChenilleIdentificationBytes,
;; as checked by the Chenilles contract on Ethereum, which will also check that
;; the initial-balances corresponds to the assets deposited into the state channel during creation.
;; On Bitcoin it might be just the hash of the UTXO of state channel.
;; State Channels involve a lot of pre-signing transactions. You must only pre-sign transactions
;; as part of state channels that you agreed upon, for which you properly contributed
;; a random number to the nonce.
(define-type ChenilleId Digest)


;; LOW-LEVEL CHENILLE API FOR EUTXO, etc.

;; Terms driving the release of some assets, in an encoding relative
;; to the context of a specific Chenilles peer.
;; Analogous to a Bitcoin lock script or an Ethereum smart contract.
;; E.g. on our Ethereum Chenilles contract, it will encode parameters for
;; a StaticCall to some contract validating the terms of the agreement.
;; Term validators will be called with length-prefix serializations of:
;; (1) the term parameters, (2) a ChenilleTransaction to validate including data and witnesses,
;; (3) a list for each of the inputs and outputs that the validator is directly associated with
;; of pointers to (a) the transaction, (b) the element of data (or revealed nested witness data)
;; corresponding to that witness, and (c) the sub-element of the witness that corresponds to
;; this validator. A term validator will either fail, or succeed while outputing a set of
;; "spending witnesses" that must be disjoint from those of other validators and sub-validators.
;; Typical spending witnesses are digests of an input (plus byte 0 as input tag) or
;; an output (same with byte 1) marking said input or output as not to be accounted multiple times.
(define-type ChenilleTerms
  (Sum
   Uninitialized: Unit
   ;; Default terms of a Chenille, only valid if the ChenillesId is uninitialized.
   ;; Never valid as input.
   ;; Valid as output if the witness is a list of the inputs claimed,
   ;; (that will be claimed as spending witnesses) together with a validation of the
   ;; ChenilleId as being correctly digested from some revealed ChenilleIdentificationBytes
   ;; that match the claimed inputs and initial state.
   ;; Spending witnesses for the claimed inputs and outputs are issued.

   Ecdsa: Address
   ;; Valid on input if transaction authorized by standard EOA ECDSA signature.
   ;; Witness: the signature by that address of the entire ChenilleTransaction.
   ;; Spending Witnesses: digest of the UTXOs being claimed by the transaction.
   ;; Always valid on output.

   Outputs: (ContentAddressed (HashTable ChenilleState <- ChenilleId))
   ;; Valid on inputs if the outputs exactly matches the digested chenilles and states.
   ;; Witness: empty.
   ;; Spending Witnesses: digest of this UTXOs being accounted for.
   ;; Always valid on output.
   ;; Notably used to implement proposed settlements and Nested state channels, etc.

   Disjunction: (List (ContentAddressed ChenilleTerms)) ;; A or B or C...
   ;; Valid if one of the terms matches
   ;; witness: the UInt16 index of the sub-term,
   ;; the length-prefixed bytes of the chosen term, and
   ;; the length-prefixed bytes of its witness.
   ;; Notably used to implement multisig as taking priority over challenge
   ;; Always valid on output.

   Conjunction: (List (ContentAddressed ChenilleTerms)) ;; A and B and C...
   ;; valid if all of the terms match
   ;; witness: a sequence of length-prefixed bytes of each of the terms' witnesses.
   ;; Notably used to implement simple n-of-n multisig.

   Challenge: (ContentAddressed
               (Record
                control: [ChenilleTerms] ;; who can post a challenge update
                proposed: [ProposedState]))
   ;; Valid on input if one of either
   ;; (a) the witness starts with a challenge update prefix (and the deadline hasn't passed?),
   ;; followed by a witness for the data being digest of timestamp followed by previous data
   ;; followed by a witness for transaction from previous data to agreed state.
   ;; for the control of a proposed state from this ChenilleId
   ;; with the previous state and some more recent proposed state, or
   ;; (b) the deadline has passed and the witness starts with a deadline passed prefix
   ;; followed by a witness for the proposed state's terms.
   ;; Always valid on output, except that the but consumes all the data and produces
   ;; a deadline followers by the previous data as output.
   ;; Usually used in disjunction with a multisig for settlemetnsl

   #| TO BE IMPLEMENTED LATER:
   EthSchnorr: Address
   ;; valid if authorized by Ethereum-modified Schnorr signature as per chainlink code.
   ;; witness: the signature by that address of the entire ChenilleTransaction
   ;; input data consumed: none. output data produced: none. -- but checked by the signature

   LnHtlc: Bytes ;; parameters for a HTLC compatible with the Bitcoin Lightning Network
   StaticCall: (Tuple Address Bytes) ;; parameters to a STATICCALL to some validating contract.
   ;; Groth16: ... ;; validating contract interactions via a Groth16 zk SNARK ?
   ;; Plonk: ... ;; validating contract interactions via a Plonk zk SNARK ?
   |#))

(define-type ProposedState
  (Record
   timestamp: [UInt256] ;; sequence number to compare which state is more recent
   balances: [(ContentAddressed Balances)])) ;; terms for distribution if this state is confirmed after deadline

;; A Chenille is essentially a EUTXO (UTXO with multiple assets and a lock script)
;; with a persistent id (its ChenilleId) and additional data that can be queried.
(define-type ChenilleState
  (Record
   balances: [Balances]
   control: [(ContentAddressed ChenilleTerms)]
   data: Bytes32)) ;; optional data synchronously updated by the control program,
   ;; typically a mutually agreed random nonce to prevent replay attacks on multi-session chenilles,
   ;; or the hash of such a nonce and some data to be used as input for the term
   ;; and verified as output for more elaborate chenilles.
   ;; For instance, it could include a digest of the cumulated-deposits and
   ;; cumulated-withdrawals balances for state channels with asynchronous deposits and withdrawals, etc.

;; Additional data, interaction-specific, associated to a Chenille
;; that can be queried asynchronously by other contracts.
;; That's where e.g. the state of the Laconic Bridge will be stored,
;; or messages sent by oracles and bridges, etc.
;; In particular, some state channels that allow for asynchronous deposits and withdrawals
;; will have in their heap entries as follow
;; cumulatedDeposits: [Balances] ;; cumulated deposits of each participant since creation
;; cumulatedWithdrawals: [Balances] ;; cumulated withdrawals of each participant since creation
(define-type Heap
  (HashTable Bytes32 <- Bytes32))

;; Type of the data in the overall Ethereum Chenilles contract
(define-type ChenillesContractState
  (Record
   states: (HashTable ChenilleState <- ChenilleId)
   heaps: (HashTable Heap <- ChenilleId)))

;; a ChenilleTransaction is very similar to a Bitcoin transaction with UTXOs,
;; except using persistent ChenilleId instead of ever-changing content-addressed UTXO hash.
;; A ChenilleId may appear in multiple inputs and outputs, as long as there initial state is
;; the first input, and each successive input state is the state from the previous output,
;; and at most one output state doesn't match an input, at which point it is the final state
;; of the Chenille. If no such state is specified, that Chenille is deleted
;; at the end of the transaction.
;; Note: for external deposits from outside the Chenilles contract, we do all ERC-20 transfer calls
;; with given input addresses, but for raw ETH we only validate the sum and trust the poster
;; not to credit any address he did not otherwise receive funds for.
;; Thus, it is always safer to first deposit into a Chenille of yours, then
;; do a regular transfer without external deposits and withdrawals. In some cases this may be required.
(declare-type ChenilleTransaction
  (Record
    inputs: (List (Tuple ChenilleId ChenilleState)) ;; the participating Chenilles in their current state
    outputs: (List (Tuple ChenilleId ChenilleState)) ;; the new Chenilles in their new states.
    deposits: Balances
    withdrawals: Balances))

;; Have the validation witnesses be a separate object.
(declare-type ChenilleTransactionWitness
  Bytes)

(declare-type post-chenille-transaction
  (Fun Bool <- ChenilleTransaction))

;; HIGH-LEVEL STATE CHANNEL API TO IMPLEMENT IN TERMS OF THE ABOVE

;; TODO: design the State Channel properly on top of Chenilles, adding the speculation needed,
;; any feature desired.
(declare-type ChenilleStateChannel
  (Record
   participants: [(List Participants)]
   chenille-id: ChenilleId
   multisig-terms: ChenilleTerms
   latest-unanimously-signed-terms:
   (Record
    proposed: [ProposedState] ;; typically a Nested
    witness: [Bytes]) ;; the witnesses for each terms under the *current* terms for
   latest-proposed-terms:
   (Record
    proposed: [ProposedState] ;; typically a Nested
    witness: [Bytes]))) ;; the witnesses for each terms under the *current* terms for

;; Open a simple state channel between two known participants,
;; wherein the initial balances will match for each petnamed participant the named assets.
(declare-type state-channel-open
  (Fun Chenille <- ChenillesContext (List (Tuple Petname NamedAssets))))
;; Deposit, Withdraw, etc.
(declare-type state-channel-deposit (Fun Bool <- Chenille Assets))
(declare-type state-channel-withdraw (Fun Bool <- Chenille Assets))
(declare-type state-channel-close (Fun Bool <- Chenille Balances))

;; Send a micropayment on a Chenille
(declare-type state-channel-send (Fun Bool <- Chenille Petname Assets))

;; TODO: adversarial challenge API for the State Channel (NB: invisible to end-user).

#|
;; A state channel with Alice and Bob will state like this:

1. Alice and Bob already know each other's participant addresses on Ethereum and libp2p,
after having exchanged business cards or some such via some website, decentralized service,
or peer to peer handshake in person.

(define Alice (hash ("SelfProposedName" "fare")
                    ("ChenilleSecp256k1Communication" "0x...")
                    ("EthereumMainnet "0x...")))))
(define Bob ...)

2. They agree on putting respectively 10 ETH and 20 ETH in a common channel
on the Ethereum Chenilles Contract with its known parent-id.

3. They each pick a random number that they commit to then reveal to each other via libp2p,
based on which they can generate the ChenilleId with a state reflecting the initial balances,
and a multisig term for unanimous transaction, for which we will use a simple multisig
(Conjunction (Ecdsa AliceEthAddress) (Ecdsa BobEthAddress)) for now, but in the future it could be
for a more elaborate threshold multisig (Ecdsa AliceAndBobThresholdEthAddress) or
(Schnorr AliceAndBobThresholdSchnorrAddress).

4. Alice code: ...

5. But *before* they actually sign the transaction that deposit the assets in the channel,
they will sign a challenge transaction with the multisig that
transfers money from this Chenille to be initial proposed state.
|#
