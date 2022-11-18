(export #t)
(import ../src/runtime)

;; Ping
(define tping (type "ping" '(CON N) `(
                _enter
                (tping
                ,(=> (THIS)
                  ;(outraw "Enter!\n")
                   (outopen 'String)
                 ))
                _leave
                (tping
                ,(=> (THIS)
                   (define CON (: THIS 'CON))
                   (define OUT (outgets))
                   (outclose)
                  ;(outraw "Leave!\n")
                   (if (specified? CON)
                     (^ 'send CON 'print OUT))
                 ))
                attach
                (tping
                ,(=> (THIS)
                   (:= THIS 'CON (sender-proc))
                 ))
                ping
                (tping
                ,(=> (THIS)
                   (define PROC (sender-proc))
                   (define MSGH (string+ "Ping[" (string (: THIS 'N)) "] from "))
                   (display MSGH)
                   (display (: PROC 'UID)) ;; TODO: add a way to switch between stdouts, and be able to use (out) all the time
                   (newline)
                   (outraw (string+ MSGH (: (current-proc) 'UID)))
                   (cr)
                   (:= THIS 'N (+ (: THIS 'N) 1))
                 ))
                echo
                (tping str
                ,(=> (THIS MSG)
                   (define PROC (sender-proc))
                   (display (string+ "Echo from " (: (current-proc) 'UID)))
                   (display " to ")
                   (display (: PROC 'UID))
                   (newline)
                   (outraw MSG)
                   (cr)
                 ))
              )))

(define (ping)
  (rexpr tping '(N 0)))
