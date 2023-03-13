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
(import ./procs)
(import ./ipc)

;; Calls
(set! tcall (type "call" 
                 '(USER   ;; UID user
                   FROM   ;; UID sender (process)
                   OUTNB  ;; Nth call sent by FROM (stored in OUT) ; only _one_ call per OUTNB
                   TO     ;; UID receiver (process or group)
                   TO_    ;; UID receiver (process)
                   PHYSTO ;; Physical address receiver
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

(define (_incmsgno MSG) ;; TODO: remove this ; unused, it seems
  (define NO (if (unspecified? (: MSG 'MSGNO)) 0 (+ (: MSG 'MSGNO) 1)))
  (:= MSG 'MSGNO NO))

;; Encryption & identities
(define (sign O UID VAR)
  (if (not (and (list? (: O VAR))
                (list-in? UID (: O VAR)))) ;; TODO: check this
    (:+ O VAR UID '(sign))))

(define (sign:+ . L) ;; TODO: clean all that
  (define RES (car L))
  (define SIGN (list-flatten (map (=> (O)
                                      (cdr (: O 'SIGN_E)))
                                  L)))
  (:= RES 'SIGN_E `(sign . ,(list-rmdup SIGN)))
  RES)

(define (signed-by? CALL USER)
  (define SIGN (: CALL 'SIGN_E))
  (if (not (pair? SIGN))
    False
    (list-in? USER (cdr SIGN))))

(define (signed-all? CALL)
  (define SIGN (: CALL 'SIGN_E))
  (define TO (: CALL 'TO))
  (define RES True)
  (define PROCG (net-resolve TO))
  (set! TO (if (procg? PROCG)
             (: PROCG 'PEER)
             (: PROCG 'UID)))
  (if (not (pair? SIGN))
    False
    (begin
      (set! SIGN (cdr SIGN))
      (if (not (pair? TO))
        (set! TO `(,TO)))
      (set! TO (map (=> (UID)
                      (define PR (net-resolve UID))
                      (if (not PR)
                        (error "signed-all? : unresolveable target => " UID))
                      (: PR 'USER))
                    TO))
      (for-each (=> (USER)
                  (if (not (list-in? USER SIGN))
                    (set! RES False)))
                TO)
      RES)))
