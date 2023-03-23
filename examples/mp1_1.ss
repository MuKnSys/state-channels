(export #t)
(import ../src/runtime)
(import ../src/cli)
(import ../src/apimon)
(import ./mp1)

(define (main . args)
  ;; Creating the proc snapshots
  (define MP1 (micropay 'smith 10 'dupont 10 'durand 10))
  (define MP2 (micropay 'smith 10 'dupont 10 'durand 10))
  (define MP3 (micropay 'smith 10 'dupont 10 'durand 10))

  ;; Displaying the snapshots
  (^ 'lst MP1)(cr)
  (^ 'lst MP2)(cr)
  (^ 'lst MP3)(cr)

  ;; Creating the host
  (define HOST1 (proch 'USER 'system
                       'UID "HOST1"))
  (current-proch! HOST1)

  ;; Creating the procs
  (define PR1 (procl 'USER "smith"
                     'UID "PR1"
                     'SELF MP1))
  (define PR2 (procl 'USER "dupont"
                     'UID "PR2"
                     'SELF MP2))
  (define PR3 (procl 'USER "durand"
                     'UID "PR3"
                     'SELF MP3))
  (net-enter PR1)
  (net-enter PR2)
  (net-enter PR3)
  (define GR1 (proc-group+attach Void PR1 PR2 PR3))
  (:= GR1 'UID "GR1")
  (:= GR1 'USER "nobody")
  (outraw "---\n")
  (netlist 1)(cr)

  (define (lstp . STATES)
    (outraw "---\n")
    (_lsp2 PR1)(cr)
    (_lsp2 PR2)(cr)
    (_lsp2 PR3)(cr)
    (if (not (empty? STATES))
        (begin
          (outraw "=>\n")
          (^ 'lst MP1)(cr)
          (^ 'lst MP2)(cr)
          (^ 'lst MP3)(cr))))

  ;; Doing a micropayment
  (current-proc! PR1)
  (^ 'send (: PR1 'GROUP) 'transfer 'dupont 5)
  (lstp)

  (^ 'step PR1)
  (lstp)

  (^ 'step PR2)
  (lstp 1)

  (^ 'step PR1)
  (lstp 1)

  (^ 'step PR1)
  (lstp)

  (^ 'step PR3)
  (lstp 1)

  (^ 'step PR1)
  (lstp))
