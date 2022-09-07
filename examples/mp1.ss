(export #t)

;; Micropayments
(define
  tmicropay
  (type "micropay" '(STATE ACCOUNT) `(

    transfer
    (tmicropay sy num
    ,(=> (MP USER AMOUNT)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (define PR (sender-proc))
       (define GIVER Void)
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
              (USER (car PARM))
              (AMOUNT (cadr PARM)))
         (:= ACCOUNT USER (+ (<: ACCOUNT USER) AMOUNT)))))

    start ;; Start
    (tmicropay
    ,(=> (MP)
       (define ACCOUNT (<: MP 'ACCOUNT))
       (define RES True)
       (if (== (<: MP 'STATE) 'Init)
         (begin
           (for-each (=> (A)
                       (if (!= (string-get (string (car A)) 0) (char ":"))
                       (begin
                         (if (not (> (cadr A) 0))
                           (set! RES False)))))
                     ACCOUNT))
         (set! RES False))
       (if RES
         (:= MP 'STATE 'Started))
       RES))

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
         (MP (rexpr tmicropay `(STATE Init ACCOUNT ,ACCOUNT))))
    (set! L (list-group L))
    (for-each (=> (A)
                (:= ACCOUNT (sy (car A)) (number (cadr A)))
              )
              L)
    (^ 'start MP)
    MP))

;(out (micropay '(A B C)))
;(cr)

;(outraw "=======>")
;(>> (method tmicropay 'lst))(cr)
;(out (slotty tmicropay 'lst))(cr)
