(export #t)
(import ./mp1)

;; Micropayments (contract)
(define
  tcmicropay
  (type "cmicropay" '(STATE ACCOUNT) `(

    transfer ;; We suppose it works all the time ; model wallets, later
    (cmicropay sy num
    ,(=> (MP USER AMOUNT)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (:= ACCOUNT USER (+ (<: ACCOUNT USER) AMOUNT))))

    lst
    (volatile
     cmicropay
    ,(=> (MP)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (define FIRST True)
       (for-each (=> (A)
                   (if (!= (string-get (string (car A)) 0) (char ":"))
                   (begin
                     (if (not FIRST) (cr))
                     (set! FIRST False)
                     (outraw (car A))
                     (outraw " : ")
                     (outraw (cadr A)))))
                 ACCOUNT))))))

(define (cmicropay . L)
  (let* ((ACCOUNT (rexpr '@rexpr '()))
         (MP (rexpr tcmicropay `(STATE Init ACCOUNT ,ACCOUNT))))
    (for-each (=> (U)
                (:= ACCOUNT (sy U) 0)
              )
              L)
    MP))

;(out (cmicropay '(A B C)))
;(cr)
