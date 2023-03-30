(export #t)
(import ../src/runtime)

;; State containing a string
(define
  tstr
  (type "str" '(STATE) `(

    set
    (tstr str
    ,(=> (SC VAL)
       (:= SC 'STATE VAL)
       True
    ))

  )))

(define (str VAL)
  (rexpr tstr `(STATE ,VAL)))
