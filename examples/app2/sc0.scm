(export #t)
(import ../src/runtime)

;; State channel #0
(define
  tsc0
  (type "sc0" '(STATE PINGNB) `(

    set
    (tsc0 str
    ,(=> (SC VAL)
       (:= SC 'STATE VAL)
       True
    ))

    print
    (volatile
     tsc0 str
    ,(=> (SC MSG)
       (outraw (string+ "Ping " MSG))
       True
    ))

    ping
    (volatile
     tsc0
    ,(=> (SC)
       (define PR (sender-proc))
       (^ 'send PR 'print (: SC 'PINGNB))
       (:= SC 'PINGNB (+ (: SC 'PINGNB) 1))))

    lst
    (volatile
     tsc0
    ,(=> (SC)
       (outraw (: SC 'STATE))
       (cr)
    ))
  )))

(define (sc0 VAL)
  (rexpr tsc0 `(STATE ,VAL
                PINGNB 0)))
