; llruntime[Guile].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports))

;; Lang
(define-macro (=> LST . CODE)
  `(lambda ,LST . ,CODE))

;; Exceptions & error
(define (exit2)
  (display "\n" (current-error-port))
  (exit))

(define _error error)

;; Strings
;(define _string string) ;; FIXME: find why, in Gerbil, that says that string is unspecified

;; Hash tables
(define make-hashq-table make-hash-table)
(define make-hashv-table make-hash-table)

(define (hash-length HT)
  (hash-count (const True) HT))

;; Files
(define (file-exists? FNAME)
  (access? FNAME F_OK))

;; Procedures
(define (procedure-name F) ;; FIXME: doesn't work for anonymous procedures
  (if (not (procedure? F))
    (error "procedure-name"))
  (cadr (string-split (with-output-to-string (lambda () (write F))) #\space)))

;; Files
(define (file-type FPATH)
  (define ST (stat FPATH))
  (string (string-ref (symbol->string (stat:type ST)) 0)))

(define (files FPATH)
  (define RES '())
  (define DIR (opendir FPATH))
  (define FNAME (readdir DIR))
  (while (not (eof-object? FNAME))
    (set! RES (cons FNAME RES))
    (set! FNAME (readdir DIR)))
  (reverse RES))

;; Modules
(define _MODS (make-hash-table))

(define (_mod-resolve NAME)
  (hash-ref _MODS NAME))

(define (_mod-store NAME)
  (if (string? NAME)
  (begin
   ;(display "storing=> ")
   ;(display NAME)
   ;(display "\n")
    (hash-set! _MODS NAME #t))))

(define _CURFILE '())
(define (_pushcf FNAME)
  (set! _CURFILE (cons FNAME _CURFILE)))
(define (_popcf)
  (set! _CURFILE (cdr _CURFILE)))
(define (_getcf)
  (if (pair? _CURFILE)
    (car _CURFILE)
    #f))

(define __mod-normalize-path #f)
(define (_mod-normalize-path PATH)
  (if __mod-normalize-path
    (set! PATH (__mod-normalize-path PATH)))
  PATH)

(define __mod-resolve-path #f)
(define (_mod-resolve-path PATH)
  (if __mod-resolve-path
    (set! PATH (__mod-resolve-path PATH)))
  PATH)

(define (_mod-load DIR MODS)
  (set! DIR (_mod-normalize-path DIR))
 ;(display "From ")(display DIR)(newline)
 ;(display "Try ")(display MODS)(newline)
  (if (pair? MODS)
    (for-each (=> (MOD)
                (set! MOD (symbol->string MOD))
                (let* ((NAME (car (reverse (string-split MOD #\/)))))
                  (if (not (_mod-resolve NAME)) ;; FIXME: doesn't resolves the problem of modules that load themselves
                  (begin
                    (_mod-store NAME)
                    (if (eq? (string-ref MOD 0) #\:)
                      (set! MOD (_mod-resolve-path MOD))
                    (if (not (eq? (string-ref MOD 0) #\/))
                      (set! MOD (string-append DIR "/" MOD))))
                    (if (not (equal? (car (reverse (string-split MOD #\.))) "ss"))
                      (set! MOD (string-append MOD ".ss")))
                    (_pushcf MOD)
                    (set! MOD (_mod-normalize-path MOD))
                   ;(display "Loading ")(display MOD)(newline)
                    (load MOD)
                    (_popcf)
                   ;(display (string-append " " NAME " loaded ...\n"))
                  ))))
              MODS)))

(define-macro (export . X)
  #t)

(define-macro (import . MODS)
  `(_mod-load (string-append (dirname (_getcf)) "/") (quote ,MODS)))

(_mod-store "llruntime") ;; Because it can be loaded directly, by means of (load)

(if (pair? (command-line))
  (let* ((CLI (car (command-line))))
    (_pushcf (if (eq? (string-ref CLI 0) #\/) CLI
                                              (string-append (getcwd) "/" CLI)))))

;(display (command-line))(newline)
;(display (current-filename))(newline)
(load (string-append (dirname (current-filename)) "/mods0.ss"))

;; Shell
(define (shell CMD)
  (let* ((PORT (open-input-pipe CMD)) ;; FIXME: seems (open-input-pipe) doesn't exists in Gerbil ; find a way
         (S (read-line PORT))
         (RES '()))
    (while (not (eof-object? S))
      (set! RES (cons S RES))
      (set! S (read-line PORT)))
    (close-pipe PORT)
    (reverse RES)))
