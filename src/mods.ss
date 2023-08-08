; mods.ss
;
;  Copyright (C) 2023, Henri Lesourd
;
;  Authors: Henri Lesourd (August 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)

;; Physical & logical module paths
(define _MODSL Void)
(define _MODSF Void)
(define (mod-by-lpath LPATH)
  (hash-ref _MODSL (sy LPATH)))

(define (mod-by-fpath FPATH)
  (hash-ref _MODSF (string2 FPATH)))

(define (mods-list)
  (define FIRST True)
  (hash-for-each-in-order (=> (LPATH MOD)
                            (if FIRST
                              (set! FIRST False)
                              (cr))
                            (outraw (: MOD 'LPATH))
                            (outraw " => ")
                            (outraw (: MOD 'FPATH)))
                          _MODSL))

;; Modules
(define tmod (type "mod"    ;; Scheme module
                   '(LPATH  ;; Logical path
                     FPATH  ;; Physical path
                    )))

;; Constructor
(define (mod . PARM)
  (define RES (rexpr tmod PARM))
  (hash-set! _MODSL (: RES 'LPATH) RES)
  (hash-set! _MODSF (: RES 'FPATH) RES)
  RES)

;; Fetch modules
(define (mods-fetch)
  (define LP (append '("~/.gerbil/lib") (string-split (getenv "GERBIL_LOADPATH") #\:)))
  (define (fetchp PATH REC)
    (define FPATH Void)
    (define L Void)
    (define L2 Void)
    (define MNAME Void)
    (set! PATH (path-normalize PATH))
   ;(outraw "Looking into ")
   ;(outraw PATH)
   ;(cr)
    (set! L (files PATH))
    (if (list-in? "gerbil.pkg" L)
      (begin
        (set! FPATH (string-append PATH "/gerbil.pkg"))
        (set! L2 (file-read FPATH))
        (set! MNAME (if (empty? L2)
                      (path-fname PATH)
                      (list-get (car L2) (+ (list-pos (car L2) 'package:) 1))))
        (mod 'LPATH MNAME 'FPATH PATH))
      (if REC
        (for-each (=> (FNAME)
                    (set! FPATH (string+ PATH "/" FNAME))
                    (if (== (file-type FPATH) "d")
                      (fetchp FPATH False)))
                  L))))
  (for-each (lambda (PATH)
              (fetchp PATH True))
            LP))
;; Init
(define (init-mods)
  (set! _MODSL (make-hashv-table))
  (set! _MODSF (make-hashv-table))
  (mods-fetch))

(init-mods)
