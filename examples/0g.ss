(load "./runtime.ss")

(define (srv0)
  (define PROC (procph0 'PROCID "00" 'BIND 'Async))
  (current-procph0! PROC)

  (chlog (car (: PROC 'INCHAN)))
  (cr)

  (:= PROC 'RECVH (=> (MSG)
                    (chlog MSG)
                    (cr)))
  (procph0-start PROC))

(set! _HOSTID "00")
(init0)

(chlog2 (: (the-procph0) 'GADDR) "<< ")
(start)
