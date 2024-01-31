; cli.ss
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
(import ./socks)
(import ./procs)
(import ./procph)
(import ./procc)

;; CLI API & methods
(define tcli (type "cli" '(API AUTORUN) (empty)))
(define tclif (type "clif" '(FUNC OP VARGS TYPES QUIT)))

(define (make-cli . API)
  (if (not (empty? API))
    (set! API (car API)))
  (if (empty? API)
    (set! API (rexpr '@rexpr '()))) ;; Should work as well with simply (empty) instead of (rexpr ...)
  (rexpr tcli `(API ,API AUTORUN ,False)))

(define (clif FUNC TYPES . OPT)
  (define RES (rexpr tclif `(FUNC ,FUNC OP ,False VARGS ,False TYPES ,TYPES QUIT ,False)))
  (set! OPT (list-group OPT))
  (for-each (=> (A)
              (:= RES (car A) (cadr A)))
            OPT)
  RES)

(define (clif? CLIF)
  (== (typeof CLIF) tclif))

(method! tcli 'method (=> (CLI NAME)
  (if (string? NAME)
    (: (: CLI 'API) NAME)
    Void)))

(method! tcli 'method! (=> (CLI NAME CLIF)
  (:= (: CLI 'API) NAME CLIF)))

(method! tcli 'autorun (=> (CLI . OPT)
  (if (empty? OPT)
    (: CLI 'AUTORUN)
    (:= CLI 'AUTORUN (if (car OPT) True False)))))

;; CLI
(method! tclif 'exec (=> (F0 PARM)
  (define (cvt VAL TY)
    (cond
      ((== TY 'bool)
       (boolean VAL))
      ((== TY 'num)
       (number VAL))
      ((== TY 'sy)
       (sy VAL))
      ((== TY 'str)
       (string2 VAL))
      ((== TY 'var)
       (eval (sy VAL) (interaction-environment)))
      (else
       VAL)))
  (define TYPES False)
  (define VARGS False)
  (define F F0)
  (if (clif? F0)
  (begin
    (set! TYPES (: F 'TYPES))
    (set! VARGS (: F 'VARGS))
    (set! F (: F 'FUNC))))
  (if (boxed-empty? PARM)
    (set! PARM '()))
  (set! PARM (if TYPES (if (or (and VARGS (> (list-length PARM) (list-length TYPES)))
                               (and (not VARGS) (!= (list-length PARM) (list-length TYPES))))
                         (begin
                           (outraw "")
                           (error F " : " (list-length TYPES) " parameters expected"))
                         (map cvt PARM (list-head TYPES (list-length PARM))))
                       PARM)) ;; TODO: implement repeat in TYPES for VARGS (clif)s
  (apply F PARM)))

(method! tcli 'cmd (=> (CLI CMD PARM . RES)
  (define FUNC (^ 'method CLI CMD))
  (if (and (specified? FUNC) (: FUNC 'OP))
    (set! FUNC Void))
  (if (unspecified? PARM)
    (set! PARM '()))
  (if (and (unspecified? FUNC) (> (list-length PARM) 0))
  (begin
    (set! FUNC (^ 'method CLI (car PARM)))
    (if (and (specified? FUNC) (: FUNC 'OP))
      (let* ((CMD0 CMD))
        (set! CMD (car PARM))
        (set-car! PARM CMD0))
      (set! FUNC Void))))
  (if (specified? FUNC)
    (let* ((PREV Nil)
           (PTR PARM)
           (FRES Void))
      (while (and (!= PTR Nil) (!= (car PTR) "#"))
        (set! PREV PTR)
        (set! PTR (cdr PTR)))
      (if (and (pair? PREV) (pair? PTR) (== (car PTR) "#"))
        (set-cdr! PREV Nil))
      (set! FRES (^ 'exec FUNC PARM))
      (if (and (not (empty? RES)) (: FUNC 'QUIT))
        (set-car! (car RES) True))
      FRES)
    (if (!= CMD "doesNotUnderstand")
      (^ 'cmd CLI "doesNotUnderstand" `(,CMD ,PARM))
      (outraw "Unknown command")))))

;; Init env
(method! tcli 'init-stdinout (=> (CLI PROMPT . SCRIPT)
  (:= (stdinout) 'CLI CLI)
  (:= (stdinout) 'PROMPT PROMPT)
  (if (and (not (empty? SCRIPT)) (car SCRIPT))
    (^ 'inbuf+* (stdinout) (car SCRIPT)))
  (outraw PROMPT)))
