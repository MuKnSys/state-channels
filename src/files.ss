; files.ss
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
  (getenv VAR))

;; Current source file
(define (current-source-file)
  (_getcf))

;; Current working directory
(define (current-working-directory) ;; TODO: find a better name
  (getcwd))

;; Files
(define (fexists? FNAME) ;; TODO: crappy naming due to name interference with imported namespaces => restore using (file-exists?)
  (file-exists? FNAME))

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
    (=> (P)
      (reverse (fetch P Nil READ)))))

(define (file-write FNAME OBJ . WRITE)
  (with-output-to-file FNAME
    (=> ()
      (if (empty? WRITE)
        (_write OBJ)
        ((car WRITE) OBJ))
      (cr))))

(define (file-delete FNAME) ;; TODO: should also work under Gerbil ; verify that it is so
  (delete-file FNAME))

;; Paths
(define (path-dir FPATH) ;; TODO: find a better name
  (dirname FPATH))

(define (path-abs? FPATH)
  (and (> (string-length FPATH) 1) (== (string-ref FPATH 0) #\/)))

(define (path-noseps? FPATH)
  (define L (string->list FPATH))
  (and (not (list-in? #\. L))
       (not (list-in? #\/ L))))

(define (path-normalize FPATH)
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
(define _SH_CMD_LOG #f)
(define (sh-cmd-log B)
  (set! _SH_CMD_LOG B))

(define (sh-cmd CMD)
  (if _SH_CMD_LOG
  (begin
    (outraw ":> ")
    (outraw CMD)
    (cr)))
  (let* ((RES (shell CMD)))
    (if _SH_CMD_LOG
    (begin
      (outraw "=> ")
      (outraw RES)
      (cr)))
    RES))

;; Command line
(define (command-parm I . ALTV)
  (define L (command-line))
  (set! ALTV (if (empty? ALTV)
                Void
                (car ALTV)))
  (if (> (list-length L) I)
    (list-ref L I)
    ALTV))
