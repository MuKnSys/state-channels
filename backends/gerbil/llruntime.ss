; llruntime[Gerbil].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./llioruntime
        :gerbil/gambit/ports
        :std/srfi/1
        :std/srfi/13
        :std/srfi/125
        :std/srfi/132)
(export
  (import: ./llioruntime)
  (import: :gerbil/gambit/ports)
  (import: :std/srfi/1)
  (import: :std/srfi/13)
  (import: :std/srfi/125)
  (import: :std/srfi/132))

;; Lang
(defsyntax (=> LS)
  (let ((L (syntax->list LS)))
    `(lambda ,(cadr L) . ,(cddr L))))

(defsyntax (while LS)
  (let ((L (syntax->list LS)))
    `(do ()
         ((not ,(cadr L)))
         .
         ,(cddr L))))

(define (defined? SYMB) ;; SYMB exists in the current namespace
  #f)

;; Exceptions & error
(define (exit2)
  (display "\n" (current-error-port))
  (exit))

(define (catch FLAG FUNC ERR)
  (with-exception-handler
    ERR
    FUNC))

(define (_error)
  (exit 1))

;; Atoms
(define (unspecified? X)
  (eq? X ((lambda () (if #f 1234)))))

;(defsyntax (_ X) Unspecified) Unused at the moment

;; Numbers
(define (logior X Y)
  #f)

;; Strings
;(define (_string X) (string X))

;(define (string-trim-both X Y) ;; SRFI-13
;  #f)

;; Lists
(define (list-head L I)
  (take L I))

;(define (list-copy L) ;; SRFI-1
;  #f)

(define (copy-tree L)
  (if (pair? L)
      (cons (copy-tree (car L)) (copy-tree (cdr L)))
      L))

;(define (append! X Y) ;; SRFI-1
;  #f)

(define (sort LST CMP)
  (list-sort CMP LST))

;; Hash tables
(define (make-hashq-table)
  (make-hash-table eq?))

(define (make-hashv-table)
  (make-hash-table equal?))

(define (hash-length HT)
  (hash-table-size HT))

(define (hash-ref HT KEY)
  (hash-table-ref/default HT KEY #f))

(define (hash-set! HT KEY VAL)
  (hash-table-set! HT KEY VAL))

(define (hashq-ref HT KEY NOVAL)
  (hash-table-ref/default HT KEY NOVAL))

(define (hashq-set! HT KEY VAL)
  (hash-set! HT KEY VAL))

(define (hash-remove! HT KEY)
  (hash-table-delete! HT KEY))

(define (hash-map->list FUNC HT)
  (hash-table-map->list FUNC HT))

;; Procedures
(define (procedure-name F)
  (error "procedure-name: !Yet"))

;; Files
(define (file-type FPATH)
  (define FTY (file-info-type (file-info FPATH)))
  (string (string-ref (symbol->string FTY) 0)))

(define (files FPATH)
  (directory-files (list path: FPATH ignore-hidden: #f)))

;; Modules
(define __mod-normalize-path #f) ;; Unused, but necessary inside mods0.ss
(define __mod-resolve-path #f)   ;; Idem
