(load "mp1.scm")

;; Micropayments (contract)
(define tcmicropay (type "cmicropay" '(LST)
                                     `(transfer ;; We suppose it works all the time ; model wallets, later
                                       (cmicropay sy num
                                       ,(=> (MP USER AMOUNT)
                                          (define LST (<: MP 'LST))
                                          (:= LST USER (+ (<: LST USER) AMOUNT))))
                                       lst
                                       (volatile
                                        cmicropay
                                       ,(=> (MP)
                                          (define LST (<: MP 'LST))
                                          (define FIRST True)
                                          (for-each (=> (A)
                                                      (if (!= (string-get (string (car A)) 0) (char ":"))
                                                      (begin
                                                        (if (not FIRST) (cr))
                                                        (set! FIRST False)
                                                        (outraw (car A))
                                                        (outraw " : ")
                                                        (outraw (cadr A)))))
                                                    LST))))))
(define (cmicropay . L)
  (let* ((LST (rexpr '@rexpr '()))
         (MP (rexpr tcmicropay `(LST ,LST))))
    (for-each (=> (U)
                (:= LST (sy U) 0)
              )
              L)
    MP))

;(out (cmicropay '(A B C)))
;(cr)
