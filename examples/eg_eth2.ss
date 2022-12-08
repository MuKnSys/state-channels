(import ../src/runtime)

;; Creating the host
(define HOST1 (proch 'USER 'system
                     'UID "HOST1"))
(current-proch! HOST1)

;; Creating the procs
(define PR0 (proceth 'USER "blockchain"
                     'UID (eth-addr "SimpleStorage@40")))
(pralias! PR0 "PR")
(define PR1 (procl 'USER "dupont"
                   'UID "PR1"))

;; Creating the accounts
(account 'NAME 'smith
         'ACCNO 0 ;; FIXME: remove this and make that better
         'UID "0xa16979a982b94200d61aede91f6cf2a0c0ac3613")
(account 'NAME 'dupont
         'ACCNO 1
         'UID "0xfe9d3038aa1e064e4bca147fa978dece561b91f1")
(account 'NAME 'durand
         'ACCNO 2
         'UID "0x51569535b832588d346d80af67c3341088cfd8fc")

;; lstp
(define (lstp)
  (outraw "---\n")
  (_lsp2 PR0)(cr)
  (_lsp2 PR1)(cr))

(net-enter PR1)
(netlist 1)(cr)
(lstp)

;; Syncing PR0
(^ 'sync PR0)
(lstp)

;; Sending a message to PR0
(current-proc! PR1)
(outraw "---\n")
(outraw (^ 'fetch PR0 'get))
(cr)
(outraw (^ 'fetch PR0 'state))
(cr)
(outraw (^ 'fetch PR0 'accounts))
(cr)
(outraw (^ 'fetch PR0 'balances))
(cr)
;(^ 'send PR0 'set 5678)
;(^ 'send PR0 'set2 0123456789)
;(^ 'send PR0 'init '("0xa16979a982b94200d61aede91f6cf2a0c0ac3613"
;                    "0xfe9d3038aa1e064e4bca147fa978dece561b91f1"
;                    "0x51569535b832588d346d80af67c3341088cfd8fc"))
(current-proc! PR1)
;(^ 'send PR0 'deposit 10)
;(^ 'send PR0 'withdraw 2)
