(import ../src/runtime)
(import ./mp1)

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
;(^ 'send PR0 'set2 5678910)
(exit 0)
(lstp)

;; Syncing PR0 (2)
;(^ 'sync PR0)
(lstp)
