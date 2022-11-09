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
             (ice-9 rdelim))

;; Lang
(define-macro (=> LST . CODE)
  `(lambda ,LST . ,CODE))

;; Exceptions & error
(define (exit2)
  (display "\n")
  (exit))

(define _error error)

;; Strings
(define _string string) ;; FIXME: find why, in Gerbil, that says that string is unspecified

;; Hash tables
(define make-hashq-table make-hash-table)
(define make-hashv-table make-hash-table)

;; Files
(define (file-exists? FNAME)
  (access? FNAME F_OK))

;; Paths
;(define (path-normalize PATH) ;; TODO: throw this away
;  (define HOME (getenv "HOME"))
;  (if HOME
;    (set! PATH (string-replace PATH "~" HOME)))
;  (canonicalize-path PATH)) ;; FIXME: implement one that _DOESNT_ errors when the path doesnt exists

;; Procedures
(define (procedure-name F) ;; FIXME: doesn't work for anonymous procedures
  (if (not (procedure? F))
    (error "procedure-name"))
  (cadr (string-split (with-output-to-string (lambda () (write F))) #\space)))

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

(define (_mod-load DIR MODS)
  (if (pair? MODS)
    (for-each (=> (MOD)
                (set! MOD (symbol->string MOD))
                (let* ((NAME (car (reverse (string-split MOD #\/)))))
                  (if (not (_mod-resolve NAME)) ;; FIXME: doesn't resolves the problem of modules that load themselves
                  (begin
                    (_mod-store NAME)
                    (if (not (eq? (string-ref MOD 0) #\/))
                      (set! MOD (string-append DIR MOD)))
                    (_pushcf (string-append MOD ".ss"))
                    (load (string-append MOD ".ss"))
                    (_popcf)
                   ;(display (string-append " " NAME " loaded ...\n"))
                  ))))
              MODS)))

(define-macro (export . X)
  #t)

(define-macro (import . MODS)
  `(_mod-load (string-append (dirname (_getcf)) "/") (quote ,MODS)))

(define (loadi FNAME . UNUSED)
  (set! FNAME (string->symbol (string-join (reverse (cdr (reverse (string-split FNAME #\.)))) "."))) ;; TODO: manage the case when the extension is given (in _mod-load, probably)
  (eval `(import ,FNAME) (interaction-environment)))

(_mod-store "llruntime") ;; Because it can be loaded directly, by means of (load)

(if (pair? (command-line))
  (let* ((CLI (car (command-line))))
    (_pushcf (if (eq? (string-ref CLI 0) #\/) CLI
                                              (string-append (getcwd) "/" CLI)))))

;; Shell
(define _SH_CMD_LOG #f)
(define (sh-cmd CMD)
  (if _SH_CMD_LOG
  (begin
    (outraw ":> ")
    (outraw CMD)
    (cr)))
  (let* ((PORT (open-input-pipe CMD)) ;; FIXME: seems (open-input-pipe) doesn't exists in Gerbil ; find a way
         (S (read-line PORT))
         (RES '()))
    (while (not (eof-object? S))
      (set! RES (cons S RES))
      (set! S (read-line PORT)))
    (close-pipe PORT)
    (if _SH_CMD_LOG
    (begin
      (outraw "=> ")
      (outraw (reverse RES))
      (cr)))
    (reverse RES)))
