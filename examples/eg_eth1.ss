(import ../src/runtime)

;; Creating the host
(define HOST1 (proch 'USER 'system
                     'UID "HOST1"))
(current-proch! HOST1)

;; Creating the procs
(define PR0 (proceth 'USER "dupont"
                     'UID (eth-addr "SimpleStorage@30")))
(pralias! PR0 "PR")
(define PR1 (procl 'USER "durand"
                   'UID "PR1"))

;; Creating the accounts
(account 'NAME 'durand
         'UID "0xa16979a982b94200d61aede91f6cf2a0c0ac3613")

;; lstp
(define (lstp)
  (outraw "---\n")
  (_lsp2 PR0)(cr)
  (_lsp2 PR1)(cr))

;(net-enter PR0)
(net-enter PR1)
(netlist 1)(cr)
(lstp)

;; Syncing PR0 (1)
(^ 'sync PR0)
(^ 'sync PR0) ;; 2nd sync, to test that it doesn't fetch 2 times the same logs
(lstp)

;; Sending a message to PR0
(current-proc! PR1)
;(^ 'send PR0 'set 5678)
;(^ 'send PR0 'set2 0123456789)
(lstp)
(exit 0)

;; Syncing PR0 (2)
;(^ 'sync PR0)
(lstp)
