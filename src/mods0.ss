; mods0.ss  ;; kind-of an initial bootloader, indeed
;
;  Copyright (C) 2023, Henri Lesourd
;
;  Authors: Henri Lesourd (August 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./llruntime)

(export
  (import: ./llruntime))

;; Zero-dependencies (or quasi) runtime needed to implement this
;; Env
(define (env-get VAR)
  (define RES #f)
  (catch #t
    (=> ()
      (set! RES (getenv VAR)))
    (=> (E)
      #f))
  RES)

;; Paths
(define (path-abs? FPATH)
  (and (> (string-length FPATH) 1) (equal? (string-ref FPATH 0) #\/)))

(define (path-nsmod? FPATH)
  (and (> (string-length FPATH) 1) (equal? (string-ref FPATH 0) #\:)))

(define (path-naked? FPATH)
  (if (> (string-length FPATH) 1)
    (let* ((FC (string-ref FPATH 0)))
      (not (or (equal? FC #\/)
               (equal? FC #\.)
               (equal? FC #\:))))
    #t))

(define (path-normalize FPATH)
  (define (p2l PATH)
    (set! PATH (string-split PATH #\/))
    (filter (=> (S)
              (not (equal? S ""))) PATH))
  (define (evp L)
    (define RES '())
    (define (push X)
      (set! RES (cons X RES)))
    (define (pop)
      (set! RES (cdr RES)))
    (while (not (null? L))
      (cond ((equal? (car L) ".")
             #f)
            ((equal? (car L) "..")
             (pop))
            (else
             (push (car L))))
      (set! L (cdr L)))
    (string-append "/" (string-join (reverse RES) "/")))
  (define FPATH0 #f)
  (define HOME (p2l (env-get "HOME")))
  (define CWD (p2l (getcwd)))
  (define ABS #f)
  (set! FPATH (string-trim FPATH #\space))
  (set! FPATH0 FPATH)
  (set! ABS (path-abs? FPATH))
  (if (equal? FPATH "")
    (set! FPATH "."))
  (set! FPATH (p2l FPATH))
  (cond ((null? FPATH)
         "/")
        ((or (equal? (car FPATH) ".") (equal? (car FPATH) ".."))
         (evp (append CWD FPATH)))
        ((equal? (car FPATH) "~")
         (evp (append HOME (cdr FPATH))))
        (else
         (set! FPATH (evp FPATH))
         (if (and (not ABS) (path-abs? FPATH) (not (equal? FPATH0 "/")))
           (set! FPATH (substring FPATH 1 (string-length FPATH))))
         FPATH)))
(if (defined? '__mod-normalize-path)
  (set! __mod-normalize-path path-normalize))

;; Strings
(define (string-prefix PREF S)
  (define NP (string-length PREF))
  (define N (string-length S))
  (if (< N NP)
    #f
    (equal? PREF (substring S 0 NP))))

(define (string-subr PREF S)
  (define NP (string-length PREF))
  (define N (string-length S))
  (define RES #f)
  (if (string-prefix? PREF S)
    (set! RES (substring S NP (string-length S))))
  RES)

;; Physical & logical module paths
(define _MODSL #f)
(define _MODSF #f)
(define (mod-by-lpath LPATH)
  (hash-ref _MODSL (string->symbol LPATH)))

(define (mod-by-fpath FPATH)
  (hash-ref _MODSF FPATH))

(define (mod-resolve-lpath LPATH)
  (define FOUND #f)
  (if (equal? (string-ref LPATH 0) #\:)
    (set! LPATH (substring LPATH 1 (string-length LPATH))))
 ;(display "Resolving ")(display LPATH)(newline)
  (hash-for-each (=> (LROOT FROOT)
                   (define LPL #f)
                  ;(display "=> ")
                  ;(write LROOT)(display " ")(write FROOT)(newline)
                   (set! LROOT (symbol->string LROOT))
                   (set! LPL (string-subr LROOT LPATH))
                  ;(write LPL)(newline)
                   (if (and LPL (not FOUND))
                     (set! FOUND (string-append FROOT LPL))))
                 _MODSL)
 ;(display "===> ")
 ;(write FOUND)(newline)
  FOUND)
(if (defined? '__mod-resolve-path)
  (set! __mod-resolve-path mod-resolve-lpath))

(define (mod-resolve-fpath FPATH)
  (define FOUND #f)
 ;(display "Resolving ")(display FPATH)(newline)
  (hash-for-each (=> (FROOT LROOT)
                   (define FPL #f)
                  ;(display "=> ")
                  ;(write FROOT)(display " ")(write LROOT)(newline)
                   (set! LROOT (symbol->string LROOT))
                   (set! FPL (string-subr FROOT FPATH))
                  ;(write FPL)(newline)
                   (if (and FPL (not FOUND))
                     (set! FOUND (string-append ":" LROOT FPL))))
                 _MODSF)
 ;(display "===> ")
 ;(write FOUND)(newline)
  FOUND)

(define (mods-list)
  (define FIRST #t)
  (hash-for-each (=> (LPATH FPATH)
                   (if FIRST
                     (set! FIRST #f)
                     (newline))
                   (display LPATH)
                   (display " => ")
                   (display FPATH))
                 _MODSL))

;; Store
(define (mod-store LPATH FPATH)
  (if (not (symbol? LPATH))
    (set! LPATH (string->symbol LPATH)))
 ;(display "Storing ")(write LPATH)
 ;(display " => ")(write FPATH)(newline)
  (hash-set! _MODSL LPATH FPATH)
  (hash-set! _MODSF FPATH LPATH))

;; Fetch modules
(define (mods-fetch)
  (define LP (append '("~/.gerbil/lib") (string-split (env-get "GERBIL_LOADPATH") #\:)))
  (define (fetchp PATH REC)
    (define FPATH #f)
    (define L #f)
    (define L2 #f)
    (define MNAME #f)
    (set! PATH (string-trim-right (path-normalize PATH) #\/))
   ;(display "Looking into ")
   ;(display PATH)
   ;(newline)
    (set! L (files PATH))
    (if (pair? (filter (=> (X) (equal? X "gerbil.pkg")) L))
      (begin
       ;(display "Found!\n")
        (set! FPATH (string-append PATH "/gerbil.pkg"))
        (set! L2 (call-with-input-file FPATH read))
        (set! MNAME (basename PATH))
        (if (pair? L2)
          (begin
            (while (and (pair? L2) (not (equal? (car L2) 'package:)))
              (set! L2 (cdr L2)))
            (if (pair? L2)
              (set! L2 (cdr L2)))
            (if (pair? L2)
              (set! MNAME (car L2)))))
        (mod-store MNAME PATH))
      (if REC
        (for-each (=> (FNAME)
                    (set! FPATH (string-append PATH "/" FNAME))
                    (if (equal? (file-type FPATH) "d")
                      (fetchp FPATH #f)))
                  L))))
  (for-each (=> (PATH)
              (fetchp PATH #t))
            LP))

;; Loading modules
(define (mod-load PATH . LDC)
  (define PATH_ #f)
  (if (path-naked? PATH)
    (set! PATH (string-append "./" PATH)))
  (set! PATH_ PATH)
  (if (not (path-nsmod? PATH))
    (begin
      (set! PATH (path-normalize PATH))
      (set! PATH (mod-resolve-fpath PATH))))
 ;(display "Loading ")(write PATH)(newline)
  (set-command-line (cons PATH_ (filter (lambda (X) (not (equal? X ""))) LDC)))
  (if PATH
    (eval `(import ,(string->symbol PATH)) (interaction-environment))))

;; Init
(define (init-mods0)
  (set! _MODSL (make-hashv-table))
  (set! _MODSF (make-hashv-table))
  (mods-fetch))

(init-mods0)
