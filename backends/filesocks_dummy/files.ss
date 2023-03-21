; files[filesocks_dummy].ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (March 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./llruntime)
(import ./basics)

;; Env
(define (env-get VAR)
  (cond ((== VAR "HOME")
         "/A/B")
        (else
         False)))

;; Current source file
(define (current-source-file)
  "A/B/src/xyz.ss")

;; Current working directory
(define (current-working-directory) ;; TODO: find a better name
  "/A/B")

;; Files
(define (fexists? FNAME)
  False)

(define (file-read FNAME . TXT)
  (error "filesocks_dummy::file-read"))

(define (file-write FNAME OBJ . WRITE)
  (error "filesocks_dummy::file-write"))

(define (file-delete FNAME)
  (error "filesocks_dummy::file-delete"))

;; Fcntl
(define (filep-fcntl FP CMD . VAL)
  Void) ;(error "filesocks_dummy::filep-fcntl"))

;; Paths [those ones we keep them] ; TODO: move that in the non-backendified code
(define (path-dir FPATH) ;; TODO: find a better name
  (dirname FPATH))

(define (path-abs? FPATH)
  (and (> (string-length FPATH) 1) (== (string-ref FPATH 0) #\/)))

(define (path-noseps? FPATH)
  (define L (string->list FPATH))
  (and (not (list-in? #\. L))
       (not (list-in? #\/ L))))

(define (path-normalize FPATH)
  (define RES Void)
  (define (p2l PATH)
    (set! PATH (string-split PATH #\/))
    (filter (=> (S)
              (!= S "")) PATH))
  (define (evp L)
    (define RES '())
    (define (push X)
      (set! RES (cons X RES)))
    (define (pop)
      (set! RES (cdr RES)))
    (while (not (empty? L))
      (cond ((== (car L) ".")
             (noop))
            ((== (car L) "..")
             (pop))
            (else
             (push (car L))))
      (set! L (cdr L)))
    (string+ "/" (string-join (reverse RES) "/")))
  (define FPATH0 Void)
  (define HOME (p2l (env-get "HOME")))
  (define CWD (p2l (current-working-directory)))
  (define ABS Void)
  (set! FPATH (string-trim FPATH #\space))
  (set! FPATH0 FPATH)
  (set! ABS (path-abs? FPATH))
  (if (== FPATH "")
    (set! FPATH "."))
  (set! FPATH (p2l FPATH))
  (cond ((empty? FPATH)
         "/")
        ((or (== (car FPATH) ".") (== (car FPATH) ".."))
         (evp (list-add CWD FPATH)))
        ((== (car FPATH) "~")
         (evp (list-add HOME (cdr FPATH))))
        (else
         (set! FPATH (evp FPATH))
         (if (and (not ABS) (path-abs? FPATH) (!= FPATH0 "/"))
           (set! FPATH (substring FPATH 1 (string-length FPATH))))
         FPATH)))

;; Shell
(define (sh-cmd CMD)
  '("")) ;(error "filesocks_dummy::sh-cmd"))

;; Command line
(define (command-parm I . ALTV)
  Void)

;; Own IP ;; TODO: cache values
(define (ownipmac)
  '("127.0.0.255 A.B.C.D.E.F"))

(define (ownip)
  (car (ownipmac)))

(define (ownmac)
  (cadr (ownipmac)))

(define (ownproxy)
  "1.2.3.4")

(define (ownnpath) ;; Own npath [this one we keep it] ; TODO: move that in the non-backendified code
  (define PROXY (ownproxy))
  (define ADDR (ownip))
  (if (== PROXY ADDR)
     ADDR
     (npath PROXY ADDR)))
