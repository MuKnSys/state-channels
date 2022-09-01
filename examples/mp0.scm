;; Micropayments
(define
  twallet
  (type "wallet" '(USER BALANCE) `(

    receive
    (twallet num
    ,(=> (W AMOUNT)
       (define BALANCE (<: W 'BALANCE))
       (:= W 'BALANCE (+ BALANCE AMOUNT))
    ))

    transfer
    (twallet num str
    ,(=> (W AMOUNT UID)
       (define BALANCE (<: W 'BALANCE))
       (define PR (sender-proc))
       (define TO (net-resolve UID))
       (define GIVER Void)
       (if (nil? PR)
         (error "wallet.transfer : no sender"))
       (if (not TO)
         (error "wallet.transfer : no receiver"))
       (set! GIVER (sy (<: PR 'USER)))
       (if (!= GIVER (<: W 'USER))
         (error "wallet.transfer : sender should be owner"))
       (:= W 'BALANCE (- BALANCE AMOUNT))
       (^ 'send TO 'receive AMOUNT)
    ))
  )))

(define (wallet USER BALANCE)
  (rexpr twallet `(USER ,(sy USER)
                   BALANCE ,(number BALANCE))))
