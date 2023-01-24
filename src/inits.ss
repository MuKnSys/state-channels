; inits.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (December 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)

;; Inits
(define tinits (type "inits" '(FNAME DATA)))

(define (inits . FNAME)
  (define RES Unspecified)
  (set! FNAME (if (empty? FNAME)
                Unspecified
                (path-normalize (car FNAME))))
  (set! RES (rexpr tinits `(FNAME ,FNAME
                            DATA ,(rexpr Void '()))))
  (if (and (string? FNAME) (file-exists? FNAME))
  (let* ((VAL (file-read FNAME)))
    (if (not (empty? VAL))
      (set! RES (rexpr-link (car VAL))))))
  RES)

(method! tinits 'get (=> (I NAME)
                         (: (: I 'DATA) NAME)))

(method! tinits 'set! (=> (I NAME VAL)
                            (:= (: A 'DATA) NAME ADDR)))

(method! tinits 'save (=> (I)
  (if (specified? (: I 'FNAME))
    (file-write (: I 'FNAME) I
      (=> (O)
        (>> O 'Indent))))))

;; Conf
(define _INITF Unspecified)
(define _INITS Unspecified)
(define (init-conf FNAME)
  (set! _INITF (path-normalize FNAME))
  (set! _INITS (inits _INITF)))

(define (conf-get VAR)
  (^ 'get _INITS VAR))

;; BOOT.ini
(define _BOOTF (let* ;; TODO: should be (most of the time) the machine's _public_ IP
                 ((FNAME (getenv "SC_BOOT")))
                 (if (not (string? FNAME))
                   (set! FNAME (string+ SC_PATH "/BOOT.ini")))
                 FNAME))
(init-conf _BOOTF)
