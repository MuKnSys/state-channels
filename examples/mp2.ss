(export #t)
(import ../src/runtime)
(import ./mp1)
(export
  (import: ./mp1))

;; Micropayments (contract)
(define
  tcmicropay
  (type "cmicropay" '(STATE ACCOUNT) `(

    transfer ;; We suppose it works all the time ; model wallets, later
    (cmicropay sy num
    ,(=> (MP USER AMOUNT)
       (define ACCOUNT (: MP 'ACCOUNT))
       (:= ACCOUNT USER (+ (: ACCOUNT USER) AMOUNT))))

    withdraw ;; We suppose it works all the time ; model wallets, later
    (cmicropay num
    ,(=> (MP AMOUNT)
       (define MSG (current-call))
       (define PR Void)
       (define ACCOUNT (: MP 'ACCOUNT))
       (define USER Void)
       (if (nil? MSG)
         (error "cmicropay.withdraw : no current call"))
       (set! PR (net-resolve (: MSG 'FROM)))
       (if (not (proc? PR))
         (error "cmicropay.withdraw : no sender"))
       (set! USER (sy (: PR 'USER)))
       (:= ACCOUNT USER (- (: ACCOUNT USER) AMOUNT))))

    lst
    (volatile
     cmicropay
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
