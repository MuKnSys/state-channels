(load "./runtime.ss")

(define PROC (procph0 'PROCID "0" 'BIND 'Async))
(current-procph0! PROC)

(chlog (car (: PROC 'INCHAN)))
(cr)

(:= PROC 'RECVH (=> (MSG)
                  (chlog MSG)
                  (cr)))
(procph0-start PROC)
