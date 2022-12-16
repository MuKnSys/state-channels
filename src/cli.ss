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
(import ./procs)

;; CLI API & methods
(define tcli (type "cli" '(API) (empty)))
(define tclif (type "clif" '(FUNC OP VARGS TYPES QUIT)))

(define (make-cli . API)
  (if (not (empty? API))
    (set! API (car API)))
  (if (empty? API)
    (set! API (rexpr '@rexpr '()))) ;; Should work as well with simply (empty) instead of (rexpr ...)
  (rexpr tcli `(API ,API)))

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
  (: (: CLI 'API) NAME)))

(method! tcli 'method! (=> (CLI NAME CLIF)
  (:= (: CLI 'API) NAME CLIF)))

;; CLI
(method! tcli 'run (=> (CLI PROMPT . SCRIPT)
  (define (read-cmd)
    (filter (=> (X) (!= X ""))
            (string-split
              (string-trim
                (if SCRIPT
                  (let* ((CMD (car SCRIPT)))
                    (set! SCRIPT (cdr SCRIPT))
                    (outraw CMD)
                    (cr)
                    CMD)
                  (read-line (current-input-port))))
              (char " "))))
  (define (cmd CMD PARM)
    (define (cvt VAL TY)
      (cond
        ((== TY 'bool)
         (boolean VAL))
        ((== TY 'num)
         (number VAL))
        ((== TY 'sy)
         (sy VAL))
        ((== TY 'str)
         (string VAL))
        ((== TY 'var)
         (eval (sy VAL) (interaction-environment)))
        (else
          VAL)))
    (define (exec F0 PARM)
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
                               (error CMD " : " (list-length TYPES) " parameters expected"))
                             (map cvt PARM (list-head TYPES (list-length PARM))))
                           PARM)) ;; TODO: implement repeat in TYPES for VARGS (clif)s
      (apply F PARM))
    (define FUNC (^ 'method CLI CMD))
    (if (and (specified? FUNC) (: FUNC 'OP))
      (set! FUNC Void))
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
             (PTR PARM))
        (while (and (!= PTR Nil) (!= (car PTR) "#"))
          (set! PREV PTR)
          (set! PTR (cdr PTR)))
        (if (and (pair? PREV) (pair? PTR) (== (car PTR) "#"))
          (set-cdr! PREV Nil))
        (exec FUNC PARM)
        (if (: FUNC 'QUIT)
          (set! FINISHED True)))
      (outraw "Unknown command")))
  (define CMD Void)
  (define FINISHED False)
  (if (not (empty? SCRIPT))
    (set! SCRIPT (car SCRIPT))
    (set! SCRIPT False))
  (while (not FINISHED)
    (if (and SCRIPT (empty? SCRIPT))
      (set! SCRIPT False) ;;(set! FINISHED True)
      (begin
        (outraw PROMPT)
        (set! CMD (read-cmd))
        (if (or (empty? CMD)
                (== (string-length (car CMD)) 0)
                (== (string-get (car CMD) 0) (char "#")))
          (noop)
          (begin
            (catch-all (=> ()
              (indent+ 2)
              (atcol0 1)
              (cmd (car CMD) (cdr CMD))))
            (indent+ -2)))
        (if (not _COL0)
          (cr))))
  )
))
