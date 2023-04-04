;;;;; State channels API in Scheme

(import
  (only-in :std/sugar defrule)
  (only-in :clan/poo/mop define-type)
  (only-in :clan/poo/type String)
  (only-in :mukn/ethereum/ethereum Address))

(defrule (declare-type id type) (void)) ;; Type declarations for functions to be implemented :-/
(defrule (HashTable Value <- Key) Any) ;; Type to be properly defined in :clan/poo/type

;; Petname, locally-assigned unique name for account, *never* shared externally, see
;; https:;;spritelyproject.org/news/petname-systems.html
;; https:;;spritely.institute/news/two-petnames-papers-are-released.html
(define-type Petname String) ;; e.g. "bob", "auntMay", "peter", "spiderman"...

;; Network identifier. To Be Specified.
(define-type Network String) ;; e.g. "ethereum", "cosmos", "laconic"...

;; Mutable Context for Chenilles. To Be Specified.
(define-type ChenillesContext
  (Record
   petnames: (HashTable Account <- Petname)
   ;; TODO: specify more, e.g. network context, etc.
   ))

(declare-type make-ChenillesContext (Fun ChenillesContext <-))

;; Accounts
(define-type Account
  (Record
   ;;class: (Exactly 'Account)
   petname: [Petname] ;; petname of the account
   network: [(Maybe Network)] ;; network to which the account is restricted, if any
   address: [Address])) ;; address of the account, which might span multiple networks (BTC+BCH, ETH+ETC)

;; Login to an account, allowing us to use it in an active role.
;; The first argument is a ChenillesContext.
;; The second argument is a Petname (String).
;; If needed, a UI request for passphrase, 2FA access, etc., will be requested from the user,
;; but not if the user or a super-user of it was already logged in.
(declare-type login (Fun Bool <- ChenillesContext Petname))

;; Return the current account
(declare-type whoami (Fun (Maybe Account) <- ChenillesContext))

;; Asset name; e.g. "ETH", "LNT", "USDC"...
(define-type AssetName String)

;; Vector of assets
(define-type Assets (MonomorphicObject AssetName BigInt))

;; Assignment of assets to Petname-identified accounts
;; used to specify deposits to or withdrawal from state channels, etc.
;; for the initial deposit, specify an account with an empty vector of assets
;; for participants who will be part of the state channel yet won't be depositing at first
(define-type PetLedger
  (HashTable Assets <- Petname))

;; Condition driving the release of table stakes in case of challenge.
;; Table stakes are all assets in the channel that are assigned to a participant
(define-type ChenilleCondition Bytes)

;; For now, have a PetLedger as a simplified state for a Chenilles state channel.
;; As we add features, the state will become more complicated
(define-type ChenilleTerms
  (Record
   sequence-number: [UInt256]
   balances: [PetLedger] ;; assets fully assigned to each participant
   condition: [(Digested ChenilleCondition)])) ;; associated condition for table stakes

(define-type ChenilleChallengedState
  (Record
   ledger: [PetLedger]))

(define-type ChenilleState
  (Record
   sequence: [BigInt] ;; sequence number of latest consensually signed state
   assets: [Assets] ;; Total assets under management within the shared contract
   terms: [(Digested ChenilleTerms)] ;; terms to use in case things break down.
   ;; Cumulated totals allow for asynchronous interaction between on-chain and
   ;; off-chain state with minimal speculative evaluation.
   ;; They are optional, so you don't pay the price if you use speculation.
   cumulatedDeposits: [PetLedger] ;; cumulated deposits of each participant since creation
   cumulatedWithdrawals: [PetLedger])) ;; cumulated withdrawals of each participant since creation

;; Our State Channel interface
(define-type Chenille
  (Record
   network: [Network] ;; blockchain network on which the state channel is managed
   parent: [(Maybe Address) default: (void)] ;; address for the contract that manages the channel
   nonce: [String])) ;; nonce identifying the state channel within the network or contract

;; Create a new Chenille from context and inital ledger
(declare-type chenille-open (Fun Chenille <- ChenillesContext ChenilleState))

;; Return current state of a Chenille

(declare-type chenille-onchain-state (Fun ChenilleState <- Chenille))

;; Send a micropayment on a Chenille
(declare-type chenille-send (Fun Bool <- Chenille Petname Assets)

;; Deposit, Withdraw, etc.
(declare-type chenille-deposit (Fun Bool <- Chenille Assets))
(declare-type chenille-withdraw (Fun Bool <- Chenille Assets))
(declare-type chenille-close (Fun Bool <- Chenille PetLedger))

;; TODO: in a UTXO context, a multiple deposit/withdraw to/from multiple UTXOs with sigs or scripts
