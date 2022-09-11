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
(import :std/srfi/13)
(export
  (import: :std/srfi/13))

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

;; Exceptions & error
(define (catch X Y)
  #f)

(define (_error)
  #f)

;; Atoms
(define (unspecified? X)
  (eq? X ((lambda () (if #f 1234)))))

;; Strings
(define (_string X) (string X))

;(define (string-trim-both X Y) ;; TODO: perhaps move that in llruntime[guile].ss
;  #f)

;; Lists
(define (append! X Y)
  #f)

;; Hash tables
(define (make-hash-table)
  #f)

(define (hash-ref X Y)
  #f)

(define (hash-set! X Y Z)
  #f)

;; Files
(define F_OK (gensym))
(define (access? FNAME MODE)
  #f)
