; calls.scm
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (July 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

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
                    )
                    (empty)
              ))

(define (call . PARM)
  (rexpr tcall (list-group PARM))) ;; NOTE : perhaps call are values

;; Encryption & identities
(define (sign O UID VAR)
  (:+ O VAR UID '(sign)))

(define (sign:+ . L)
  (define RES (car L))
  (define SIGN (list-flatten (map (=> (O)
                                      (cdr (<: O 'SIGN_E)))
                                  L)))
  (:= RES 'SIGN_E SIGN)
  RES)

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
