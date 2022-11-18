(import ../src/runtime)
(import ./ping)

;; Creating the host
(define HOST (proch 'USER 'system
                    'UID "HOST_PING"))
(current-proch! HOST)

;; Creating the process
(define PING (procl 'USER "nobody"
                    'UID "PING"
                    'SELF (ping)))
(net-enter PING)
;(netlist 1)(cr)

;; Main
(outraw "I am process #")
(outraw (: (host-proc) 'HOSTID))
(cr)

;(net-log 1)
(start)
