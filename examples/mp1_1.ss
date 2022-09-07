(import ../src/runtime)
(import ./mp1)

;; Creating the proc snapshots
(define MP1 (micropay 'smith 10 'dupont 10 'durand 10))
(define MP2 (micropay 'smith 10 'dupont 10 'durand 10))
(define MP3 (micropay 'smith 10 'dupont 10 'durand 10))

;; Displaying the snapshots
(^ 'lst MP1)(cr)
(^ 'lst MP2)(cr)
(^ 'lst MP3)(cr)

;; Creating the procs
(define PR1 (proc 'USER "smith"
                  'UID "PR1"
                  'SELF MP1))
(define PR2 (proc 'USER "dupont"
                  'UID "PR2"
                  'SELF MP2))
(define PR3 (proc 'USER "durand"
                  'UID "PR3"
                  'SELF MP3))
(net-enter PR1)
(net-enter PR2)
(net-enter PR3)
(proc-group Void PR1 PR2 PR3)
(netlist)(cr)

;; Doing a micropayment
(current-proc! PR1)
(^ 'send PR1 'transfer 'dupont 5)
(netlist)(cr)
(^ 'step PR1)
(netlist)(cr)
(^ 'lst MP1)(cr)
(^ 'step PR1)
(^ 'step PR2)
(^ 'step PR3)
(^ 'lst MP1)(cr)
(^ 'lst MP2)(cr)
(^ 'lst MP3)(cr)
