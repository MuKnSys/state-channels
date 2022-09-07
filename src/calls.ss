; calls.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (July 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)

;; Calls
(define tcall (type "call" 
                   '(USER   ;; UID user
                     FROM   ;; UID sender (process)
                     OUTNB  ;; Nth call sent by FROM (stored in OUT) ; only _one_ call per OUTNB
                     TO     ;; UID receiver (process)
                     INNB   ;; Nth (side-effecting) call received by TO
                     FUNC   ;; The function's name
                     PARM   ;; Parameters
                     RESULT ;; Result
                     PATCH  ;; Side effects
                     ACK    ;; Ack sending a signature
                     ACK*   ;; All procs ack-ed the call
                     REDIR  ;; Redirected message ;; no use for this, at the moment (0)
                     SIGN_B ;; Signature at begin (creation)
                     SIGN_E ;; Signature at end (end of local processing ; reemitting)
                    )
                    (empty)
              ))

(define (call . PARM)
  (define RES (rexpr tcall (list-group PARM))) ;; NOTE : perhaps call are values
  (:= RES 'ACK False)
  (:= RES 'ACK* False)
  (:= RES 'REDIR False)
  RES)

;; Encryption & identities
(define (sign O UID VAR)
  (:+ O VAR UID '(sign)))

(define (sign:+ . L) ;; TODO: clean all that
  (define RES (car L))
  (define SIGN (list-flatten (map (=> (O)
                                      (cdr (<: O 'SIGN_E)))
                                  L)))
  (:= RES 'SIGN_E `(sign . ,(list-rmdup SIGN)))
  RES)

(define (signed-all? CALL)
  (define SIGN (<: CALL 'SIGN_E))
  (define TO (<: CALL 'TO))
  (define RES True)
  (if (not (pair? SIGN))
    False
    (begin
      (set! SIGN (cdr SIGN))
      (if (not (pair? TO))
        (set! TO `(,TO)))
      (set! TO (map (=> (PR)
                      (set! PR (net-resolve PR))
                      (if (not PR)
                        (error "signed-all? : unresolveable target => " P))
                      (<: PR 'USER))
                    TO))
      (for-each (=> (USER)
                  (if (not (list-in? USER SIGN))
                    (set! RES False)))
                TO)
      RES)))

;; Net
(define _NET (make-hash-table))
(define (current-network)
  _NET)

(define (net-enter PROC)
  (define UID (<: PROC 'UID))
  (if (string? UID)
  (begin
   ;(outraw "nenter=> ") ;; TODO: turn that to debug logs
   ;(out (<: PROC 'ID))
   ;(outraw " as ")
   ;(outraw UID)
    (hash-set! _NET UID PROC))))

(define (net-leave PROC)
  (define UID (<: PROC 'UID))
  (if (string? UID)
    (hash-remove! _NET UID)))

(define (net-resolve NAME)
  (if (proc? NAME)
    (set! NAME (<: NAME 'UID)))
  (hash-ref _NET NAME))

(define (net-next)
  Nil)
