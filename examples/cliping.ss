(import ../src/runtime)
(import ./console)

;; Main
(outraw "I am process #")
(outraw (: (host-proc) 'HOSTID))
(cr)

(init "durand" "PR" (console))

(define PING (net-map "PING"))
(^ 'send PING 'attach)
(start 'Once)
;(^ 'send PING 'ping)
(^ 'send PING 'echo "Coucou")
(start) ;; TODO: make it able to wait some time, and then exit
