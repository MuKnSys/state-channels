; basics.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (June 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./llruntime)

;; Error
(define _error error)
(define ERRORCATCH #t)
(define (error . MSG)
  (for-each (=> (X)
              (display X))
            MSG)
  (if (not ERRORCATCH)
  (begin
    (cr)
    (_error)))
  (exit))

(define (catch-all F)
  (if (not ERRORCATCH)
    (F)
    (catch True
      F
      (=> (E . OPT)
      ;;(display E)(display " ")(display OPT)
        False))))

(define (errlog O)
  (outraw "==============>\n")
  (out O)(cr))

;; No operation
(define (noop)
  Void)

;; Nil & Unspecified
(define Nil '())
(define Unspecified ((lambda () (if #f 1234))))
(define Void Unspecified)

(define (specified? X)
  (not (unspecified? X)))

(define (nil? X)
  (null? X))

;; Booleans
(define True #t)
(define False #f)

;; Numbers
(define (number O)
  (if (number? O)
    O
    (string->number (string O))))

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
    O))))

(define string-get string-ref)
(define string-set! string-set!) ;; Does nothing

(define string-add string-append)
(define string+ string-add)

(define string-trim string-trim-both) ;; TODO: refine this

;; Atoms
(define (atom? O)
  (or (unspecified? O)
      (null? O)
      (boolean? O)
      (number? O)
      (symbol? O)
      (char? O)
      (string? O)))

(define (strsy? O)
  (or (symbol? O)
      (string? O)))

;; Lists & other containers
(define (empty) ;; Empty list (a _real_ one) ;; TODO: use a special value "Void", rather than Unspec
  `(,Void))

(define (empty? L)
  (null? L))

(define (boxed-empty? L)
  (and (not (empty? L)) (pair? L) (== (car L) Unspecified) (empty? (cdr L))))

(define (list-find F L)
  (while (and (not (empty? L)) (not (F (car L))))
    (set! L (cdr L)))
  (if (and (not (empty? L)) (F (car L)))
    (car L)
    Unspecified))

(define (list-in? X L)
  (specified? (list-find (=> (E) (== E X)) L)))

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

;(define [ list-ref)
;(define [:= list-set)

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
      (append (list-flatten (car L)) (list-flatten (cdr L))))
    (else
      (cons (car L) (list-flatten (cdr L))))))

(define (list-rmdup L)
  (define H (make-hash-table))
  (define RES '())
  (for-each (lambda (E)
              (if (not (hash-ref H E))
                (set! RES (cons E RES)))
              (hash-set! H E 1))
            L)
  (reverse RES))

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
;(define => lambda)

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
(define _OUTP False)
(define (outopen MODE)
  (if _OUTP
    (error "outopen"))
  (set! _OUTP (open-output-string)))

(define (outclose)
  (if (not _OUTP)
    (error "outclose"))
  (set! _OUTP False))

(define (outgets)
  (if (not _OUTP)
    (error "outgets"))
  (get-output-string _OUTP))

(define (_write X)
  (apply write `(,X . ,(if _OUTP `(,_OUTP) '()))))

(define (_display X)
  (apply display `(,X . ,(if _OUTP `(,_OUTP) '()))))

(define _TABSEP "")
(define (tabsep SEP)
  (set! _TABSEP SEP))

(define (tabsf S)
  (define T (map (=> (S)
                   (string-split S (char "\t")))
                 (string-split S (char "\n"))))
  (define N 0)
  (define W Void)
  (for-each (=> (L)
              (if (> (list-length L) N)
                (set! N (list-length L))))
            T)
  (set! W (make-list N 0))
  (for-each (=> (L)
              (define M 0)
              (for-each (=> (C)
                          (if (> (string-length C) (list-get W M))
                            (list-set! W M (string-length C)))
                          (set! M (+ M 1)))
                        L))
            T)
  (set! T
    (map (=> (L)
           (define M 0)
           (map (=> (C)
                  (if (< (string-length C) (list-get W M))
                    (set! C (string+
                              C (make-string (- (list-get W M) (string-length C)) (char " ")))))
                  (set! M (+ M 1))
                  C)
                L))
         T))
  (set! T
    (map (=> (L)
           (string-join L _TABSEP))
         T))
  (string-join T "\n"))

(define (tabs CMD)
  (define RES Void)
  (cond
   ((== CMD 'Start)
    (outopen 'String))
   ((== CMD 'End)
    (set! RES (tabsf (outgets)))
    (outclose)
    RES)
   (else
     (error "tabs"))))

(define (tab)
  (outraw "\t"))

(define _COL0 False)
(define (atcol0?)
  _COL0)

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
  (_write X))

(define (outraw X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (_display X))

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
    (_display " ")
    (set! N (- N 1))))

(define (cr)
  (outraw "\n")
  (atcol0 1)
  (spc (indent)))

(define _HASCOLORS True) ;; Seems Guile always has ANSI emulation ; in case some Scheme has not, disable printing escape codes in the functions below

(define (color-red)
  (outraw (string-append (string #\esc) "[31;40m"))) ;; TODO: temporary s$%t ; fix this having a parm for the color, with names for these ...

(define (color-white)
  (outraw (string-append (string #\esc) "[39;40m")))
