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
(start 'Once 1)

(while True
  (set! S (read-line (current-input-port)))
  (^ 'send PING 'echo S)
  (start 'Once 1)
)
