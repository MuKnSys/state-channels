;; Micropayments
(define
  tmicropay
  (type "micropay" '(STATE ACCOUNT) `(

    transfer
    (tmicropay sy num
    ,(=> (MP USER AMOUNT)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (define PR (sender-proc))
       (define GIVER _)
       (if (nil? PR)
         (error "micropay.transfer : no sender"))
       (set! GIVER (sy (<: PR 'USER)))
       (:= ACCOUNT USER (+ (<: ACCOUNT USER) AMOUNT))
       (:= ACCOUNT GIVER (- (<: ACCOUNT GIVER) AMOUNT))))

    transfer/return ;; Return from the blockchain
    (tmicropay lst
    ,(=> (MP CALL)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (let* ((PARM (<: CALL 'PARM))
              (USER (sy (car PARM)))
              (AMOUNT (number (cadr PARM)))) ;; FIXME: DON'T do it like that ; methods should have typed parameters (2)
         (:= ACCOUNT USER (+ (<: ACCOUNT USER) AMOUNT)))))

    lst
    (volatile
     tmicropay
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

(define (micropay . L)
  (let* ((ACCOUNT (rexpr '@rexpr '()))
         (MP (rexpr tmicropay `(STATE Started ACCOUNT ,ACCOUNT))))
    (set! L (list-group L))
    (for-each (=> (A)
                (:= ACCOUNT (sy (car A)) (number (cadr A)))
              )
              L)
    MP))

;(out (micropay '(A B C)))
;(cr)

;(outraw "=======>")
;(>> (method tmicropay 'lst))(cr)
;(out (slotty tmicropay 'lst))(cr)
