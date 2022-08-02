; basics.scm
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (June 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

;; Error
(define _error error)
(define ERRORCATCH #f)
(define (error . MSG)
  (for-each (=> (X)
              (display X))
            MSG)
  (cr)
  (if (not ERRORCATCH)
    (_error))
  (exit))

(define (catch-all F)
  (if (not ERRORCATCH)
    (F)
    (catch True
      F
      (=> (E . OPT)
      ;;(display E)(display " ")(display OPT)
        False))))

;; No operation
(define (noop)
  _)

;; Nil & Unspecified
(define Nil '())
(define Unspecified ((lambda () (if #f 1234))))
(define _ Unspecified)

(define (specified? X)
  (not (unspecified? X)))

(define (nil? X)
  (null? X))

;; Booleans
(define True #t)
(define False #f)

;; Symbols
(define (sy S)
  (if (string? S)
    (string->symbol S)
    S))

;; Characters
(define (char S)
  (string-ref S 0))

;; Strings
(define _string string)
(define (string O)
  (if (number? O)
    (number->string O)
  (if (char? O)
    (_string O)
  (if (symbol? O)
    (symbol->string O)
    S))))

(define string-get string-ref)
(define string-set! string-set!) ;; Does nothing

(define string-add string-append)
(define string+ string-add)

(define string-trim string-trim-both) ;; TODO: refine this

;; Lists & other containers
(define (empty) ;; Empty list (a _real_ one) ;; TODO: use a special value "Void", rather than Unspec
  `(,_))

(define (empty? L)
  (null? L))

(define (boxed-empty? L)
  (and (not (empty? L)) (== (car L) Unspecified) (empty? (cdr L))))

(define (list-find F L)
  (while (and (not (empty? L)) (not (F (car L))))
    (set! L (cdr L)))
  (if (and (not (empty? L)) (F (car L)))
    (car L)
    Unspecified))

(define (list-find-prev F L)
  (define PREV Unspecified)
  (while (and (not (empty? L)) (not (F (car L))))
    (set! PREV L)
    (set! L (cdr L)))
  (if (and (not (empty? L)) (F (car L)))
    PREV
    Unspecified))

(define (list-length L)
  (if (boxed-empty? L)
    0
    (length L)))
(define list-last last-pair)

(define list-get list-ref)
(define list-set list-set!)

(define [ list-ref)
(define [:= list-set)

(define (push L VAL)
  (if (not (pair? L))
    (error "push")
    (set-cdr! (last-pair L) `(,VAL))))

(define list-add append)
(define (rcons L V)
  (if (boxed-empty? L) ;; FIXME?: (Relatively) shitty hack for empty lists
    (set-car! L V)
    (append! L `(,V))))

(define (list-flatten L)
  (cond ((null? L) Nil)
    ((list? (car L))
      (append (flatten (car L)) (flatten (cdr L))))
    (else
      (cons (car L) (flatten (cdr L))))))

;; RLists
(define (list-group L . BYN) ;; TODO: enable BYN ; as a default, BYN==2
  (define RES '())
  (define VAR Nil)
  (define N 0)
  (if (or (empty? L) (boxed-empty? L))
    L
    (begin
      (for-each (=> (X)
                    (if (== N 0)
                        (set! VAR X)
                        (set! RES (cons `(,VAR ,X) RES))) ;; TODO: check that var is a symbol
                    (set! N (if (== N 0) 1 0)))
                L)
      (reverse RES))))

;; Basic tests & symbols
(define => lambda)

(define == equal?) ;; FIXME: equal? works with only one parm ; == should NOT do that !!!
(define != (=> (X Y) (not (== X Y))))

;; Files
(define (file-exists? FNAME)
  (access? FNAME F_OK))

(define (file-read FNAME . TXT)
  (define (fetch P L0 READ) ;; Fetch data from file
    (let ((L (READ P))
         )
         (if (eof-object? L)
             L0
             (fetch P (cons L L0) READ))))
  (define READ (if (empty? TXT)
                 read read-line))
  (call-with-input-file FNAME
    (lambda (P)
      (reverse (fetch P Nil READ)))))

;; Basic I/O
(define _COL0 False)
(define (atcol0 . B)
  (if (empty? B)
    (set! B True)
    (set! B (car B)))
  (if (== B 0) (set! B False))
  (if (== B 1) (set! B True))
  (set! _COL0 B))

(define (out X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (write X))

(define (outraw X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (display X))

(define _INDENT 0)
(define (indent . N)
  (if (empty? N)
    _INDENT
    (set! _INDENT (car N))))

(define (indent+ INC)
  (indent (+ (indent) INC)))

(define (spc N)
  (if (> N 0)
    (atcol0 0))
  (while (> N 0)
    (display " ")
    (set! N (- N 1))))

(define (cr)
  (outraw "\n")
  (atcol0 1)
  (spc (indent)))
