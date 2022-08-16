;; Micropayments
(define tmicropay (type "micropay" '(STATE LST)
                                   `(mdf
                                     ,(=> (MP USER VAL)
                                        (define LST (<: MP 'LST))
                                        (set! USER (sy USER))
                                        (:= LST USER VAL)) ;; Should not be used directly
                                     transfer
                                     (tmicropay sy num
                                     ,(=> (MP USER AMOUNT)
                                        (define LST (<: MP 'LST))
                                        (define PR (current-proc))
                                        (define GIVER 0)
                                       ;(if (nil? PR)
                                       ;  (error "micropay.transfer : no current process"))
                                       ;(set! GIVER (<: PR 'USER))
                                        (:= LST USER (+ (<: LST USER) AMOUNT))))
                                     transfer/return ;; Return from the blockchain
                                     (tmicropay lst
                                     ,(=> (MP CALL)
                                        (define LST (<: MP 'LST))
                                        (let* ((PARM (<: CALL 'PARM))
                                               (USER (sy (car PARM)))
                                               (AMOUNT (number (cadr PARM)))) ;; FIXME: DON'T do it like that ; methods should have typed parameters (2)
                                          (:= LST USER (+ (<: LST USER) AMOUNT)))))
                                     lst
                                     (volatile
                                      tmicropay
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
(define (micropay . L)
  (let* ((LST (rexpr '@rexpr '()))
         (MP (rexpr tmicropay `(STATE Started LST ,LST))))
    (set! L (list-group L))
    (for-each (=> (A)
                (:= LST (sy (car A)) (number (cadr A)))
              )
              L)
    MP))

;(out (micropay '(A B C)))
;(cr)

;(outraw "=======>")
;(>> (method tmicropay 'lst))(cr)
;(out (slotty tmicropay 'lst))(cr)
