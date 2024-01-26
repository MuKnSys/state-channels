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
(import ./mods0)
(import ./basics)

;; Current source file
(define (current-source-file)
  (define RES (_getcf))
  (if (== RES "")
    (string+ (current-working-directory) "/A/B.x") ;; FIXME: hack
    RES))

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

;; Fcntl
(define (filep-fcntl FP CMD . VAL) ;; TODO: get rid of this, directly implement a command to make ports blocking or nonblocking
  (define PARM `(,FP ,CMD . ,(if (empty? VAL)
                               '()
                               `(,(car VAL)))))
  (apply fcntl PARM))

;; Paths
(define (path-dir FPATH) ;; TODO: find a better name
  (dirname FPATH))

(define (path-fname FPATH . FULL) ;; TODO: find a better name
  (define RES Void)
  (set! FULL (not (empty? FULL)))
  (set! RES (basename FPATH))
  (if (not FULL)
    (set! RES (car (string-split RES #\.))))
  RES)

(define (path-ext FPATH)
  (define L (string-split FPATH #\.))
  (if (> (list-length L) 1)
    (cadr L)
    Void))

(define (path-noseps? FPATH)
  (define L (string->list FPATH))
  (and (not (list-in? #\. L))
       (not (list-in? #\/ L))))

;; Shell
(define _SH_CMD_LOG False)
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

(define rexpr Void)
(define := Void) ;; to workaround the cyclic dependencies problem ;; TODO: get rid of it
(define (command-line-parse . LDC)
  (define RES (rexpr Void '()))
  (define VAR Void)
  (set! LDC (if (empty? LDC)
              (cdr (command-line))
              (car LDC)))
  (for-each (=> (S)
                (if (== (string-ref S 0) #\-)
                  (begin
                    (if (specified? VAR)
                      (:= RES VAR True))
                    (set! VAR (substring S 1 (string-length S))))
                  (begin
                    (if (specified? VAR)
                      (:= RES VAR S)
                      (list-push RES S))
                    (set! VAR Void))))
            LDC)
  RES)

;; Own IP ;; TODO: cache values
(define (ownipmac)
  (define IPMAC (sh-cmd (string+ SC_PATH "/bin/ownip")))
  (string-split
    (string-replace
      (if (pair? IPMAC) (car IPMAC) "127.0.0.255 A:B:C:D:E:F")
      ":" ".")
    #\space))

(define (ownip)
  (car (ownipmac)))

(define (ownmac)
  (cadr (ownipmac)))

(define _OWNPROXY Void)
(define (ownproxy)
  (define IP Void)
  (if (unspecified? _OWNPROXY)
    (begin
      (set! IP (sh-cmd (string+ SC_PATH "/bin/extip")))
      (set! _OWNPROXY (if (pair? IP) (car IP) Void))))
  _OWNPROXY)

(define (ownnpath)
  (define PROXY (ownproxy))
  (define ADDR (ownip))
  (if (== PROXY ADDR)
     ADDR
     (npath PROXY ADDR)))
