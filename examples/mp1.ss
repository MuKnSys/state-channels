(export #t)
(import ../src/runtime)

;; Micropayments
(define
  tmicropay
  (type "micropay" '(STATE ACCOUNT) `(

    transfer
    (tmicropay sy num
    ,(=> (MP USER AMOUNT)
       (define ACCOUNT (: MP 'ACCOUNT))
       (define PR (sender-proc))
       (define GIVER Void)
       (if (nil? PR)
         (error "micropay.transfer : no sender"))
       (set! GIVER (sy (: PR 'USER)))
       (:= ACCOUNT USER (+ (: ACCOUNT USER) AMOUNT))
       (:= ACCOUNT GIVER (- (: ACCOUNT GIVER) AMOUNT))))

    deposit/return ;; Return from a (blockchain) deposit
    (tmicropay lst
    ,(=> (MP CALL)
       (define ACCOUNT (: MP 'ACCOUNT))
       (define PR (sender-proc))
       (if (nil? PR)
         (error "micropay.deposit/return : no sender"))
       (let* ((PARM (: CALL 'PARM))
              (USER (sy (cadr (: CALL 'SIGN_E)))) ;; TODO: probably do the same kind of thing in withdraw/return
              (AMOUNT (car PARM)))
         (:= ACCOUNT USER (+ (: ACCOUNT USER) AMOUNT)))))

    withdraw ;; Withdrawing money from the state channel
    (committed
     tmicropay num
    ,(=> (MP AMOUNT)
       (define ACCOUNT (: MP 'ACCOUNT))
       (define WITHDRAW (: MP 'WITHDRAW))
       (define PR (sender-proc))
       (define USER Void)
       (if (nil? PR)
         (error "micropay.withdraw : no sender"))
       (set! USER (sy (: PR 'USER)))
       (:= ACCOUNT USER (- (: ACCOUNT USER) AMOUNT)) ;; TODO: test if ACCOUNT.USER>AMOUNT
       (:= WITHDRAW USER (+ (: WITHDRAW USER) AMOUNT))))

    withdraw/return ;; Return from a withdraw
    (tmicropay lst
    ,(=> (MP CALL)
       (define WITHDRAW (: MP 'WITHDRAW))
       (define PR (sender-proc))
       (if (nil? PR)
         (error "micropay.withdraw/return : no sender"))
       (let* ((PARM (: CALL 'PARM))
              (USER (sy (: PR 'USER)))
              (AMOUNT (car PARM)))
         (:= WITHDRAW USER (- (: WITHDRAW USER) AMOUNT)))))

    start ;; Start
    (tmicropay
    ,(=> (MP)
       (define ACCOUNT (: MP 'ACCOUNT))
       (define RES True)
       (if (== (: MP 'STATE) 'Init)
         (begin
           (for-each (=> (A)
                       (if (and (!= (car A) ':TYPE)
                                (!= (car A) ':ID))
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
       (define ACCOUNT (: MP 'ACCOUNT))
       (define FIRST True)
       (for-each (=> (A)
                   (if (and (!= (car A) ':TYPE)
                            (!= (car A) ':ID))
                   (begin
                     (if (not FIRST) (cr))
                     (set! FIRST False)
                     (outraw (unattr (car A)))
                     (outraw " : ")
                     (outraw (cadr A)))))
                 ACCOUNT))))))

(define (micropay . L)
  (let* ((ACCOUNT (rexpr '@rexpr '()))
         (WITHDRAW (rexpr '@rexpr '()))
         (MP (rexpr tmicropay `(STATE Init ACCOUNT ,ACCOUNT WITHDRAW ,WITHDRAW))))
    (set! L (list-group L))
    (for-each (=> (A)
                (:= ACCOUNT (sy (car A)) (number (cadr A)))
                (:= WITHDRAW (sy (car A)) 0)
              )
              L)
    (^ 'start MP)
    MP))

;(out (micropay '(A B C)))
;(cr)

;(outraw "=======>")
;(>> (method tmicropay 'lst))(cr)
;(out (slotty tmicropay 'lst))(cr)
