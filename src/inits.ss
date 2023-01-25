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

(define (file-readini FNAME . TO)
  (define RES (if (empty? TO) (rexpr Void '()) (car TO)))
  (define (val V)
    (define N (string-length V))
    (cond
      ((== V "")
       Nil)
      ((string-digits? V)
       (string->number V))
      ((and (> N 1) (== (string-ref V 0) #\")
                    (== (string-ref V (- N 1)) #\"))
       (substring V 1 (- N 1)))
      (else
       V)))
  (define (parse S)
    (set! S (filter (=> (X) (!= X "")) S))
    (set! S (map string-trim S))
    (set! S (map (=> (S2) (map string-trim (string-split S2 #\=))) S))
    (for-each (=> (E)
                (define VAR Void)
                (define VAL Void)
                (if (< (list-length E) 1)
                  (error "file-readini"))
                (set! VAR (car E))
                (if (not (and (> (string-length VAR) 0) (== (string-ref VAR 0) #\;)))
                (begin
                  (if (!= (list-length E) 2)
                    (error "file-readini(2)"))
                  (set! VAL (cadr E))
                  (set! VAL (string-trim (car (string-split VAL #\;))))
                  (:= RES VAR (val VAL)))))
              S))
  (if (and (string? FNAME) (file-exists? FNAME))
    (parse (file-read FNAME 1)))
  RES)

(define (inits . FNAME)
  (define RES Unspecified)
  (set! FNAME (if (or (empty? FNAME) (not (string? (car FNAME))))
                Unspecified
                (path-normalize (car FNAME))))
  (set! RES (rexpr tinits `(FNAME ,FNAME
                            DATA ,(rexpr Void '()))))
 ;(if (and (string? FNAME) (file-exists? FNAME))
 ;(let* ((VAL (file-read FNAME)))
 ;  (if (not (empty? VAL))
 ;    (set! RES (rexpr-link (car VAL))))))
  (:= RES 'DATA (file-readini FNAME (: RES 'DATA)))
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
(define _INITS Unspecified)
(define (init-conf FNAME)
  (set! _INITS (inits FNAME)))

(define (merge-conf FNAME)
  (define SRC Void)
  (define DEST (: _INITS 'DATA))
  (set! SRC (: (inits FNAME) 'DATA))
  (for-each (=> (X)
              (define VAR (car X))
              (if (and (!= VAR ':TYPE) (!= VAR ':ID))
                (:= DEST (unattr VAR) (cadr X))))
            SRC))

(define (conf-get VAR . ALTV)
  (define RES (^ 'get _INITS VAR))
  (if (unspecified? RES)
    (set! RES (if (empty? ALTV) Void (car ALTV))))
  RES)

(define (conf-get2 VAR . ALTV)
  (getenv2 VAR (apply conf-get `(,VAR . ,ALTV))))

;; BOOT.ini
(init-conf (string+ SC_PATH "/BOOT.ini"))
(merge-conf (getenv "SC_BOOT"))

;; Inits
(set! ERRORCATCH (boolean (conf-get "ERROR_CATCH" ERRORCATCH)))
