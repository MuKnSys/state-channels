(import ../src/runtime)
(import ./console)

;; Main
(outraw "I am process #")
(outraw (: (host-proc) 'HOSTID))
(cr)

(init "durand" "PR" (console))

(define PING (net-map "PING"))
(define S Void)

(^ 'send PING 'attach)
(start 'Once)

(define PREVS Void)
(while True
  (if (specified? PREVS)
  (begin
    (outraw "> ")
    (outraw PREVS)
    (cr)))
  (outraw "> ")
  (set! S (read-line (current-input-port)))
  (cursor-move 'Up)
  (clreol)
  (^ 'send PING 'echo S)
  (start 'Once)
  (set! PREVS S)
)
