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
    (define PREVVAR Void)
    (set! S (filter (=> (X) (!= X "")) S))
    (set! S (map (=> (S2) (map (=> (S)
                                 (define ST (string-trim S))
                                 (if (<= (string-length ST) 0)
                                   (error "file-readini::split/trim"))
                                 (if (and (== (string-ref S 0) #\space)
                                          (not (== (string-ref ST 0) #\;)))
                                   (string+ " " ST)
                                   ST))
                               (string-split S2 #\=))) S))
    (for-each (=> (E)
                (define VAR Void)
                (define VAL Void)
                (define VAL0 Void)
                (define (setval0 VAR)
                  (set! VAL0 (: RES VAR))
                  (if (not (rexpr? VAL0))
                    (begin
                      (set! VAL0 (rexpr Void '()))
                      (if (specified? (: RES VAR))
                        (rcons VAL0 (: RES VAR)))
                      (:= RES VAR VAL0))))
                (if (< (list-length E) 1)
                  (error "file-readini"))
                (set! VAR (car E))
                (if (and (!= VAR "")
                         (not (and (> (string-length VAR) 0) (== (string-ref VAR 0) #\;))))
                (begin
                  (if (== (string-ref VAR 0) #\space)
                    (begin
                      (if (unspecified? PREVVAR)
                        (error "file-readini::PREVVAR"))
                      (set! VAR (if (== (list-length E) 1)
                                  PREVVAR
                                  (string-trim VAR)))
                      (setval0 PREVVAR)
                      (set-car! E (string-trim (car E))))
                    (let* ((L (string-split (string-trim VAR) #\space)))
                      (if (> (list-length L) 2)
                        (error "file-readini::ATTR"))
                      (if (== (list-length L) 2)
                      (begin
                        (setval0 (car L))
                        (set! VAR (cadr L))))
                      (set! PREVVAR (car L))))
                  (if (< (list-length E) 2)
                    (begin
                      (if (unspecified? VAL0)
                        (error "file-readini(2)" E))
                      (set! VAR Void)
                      (set! VAL (car E)))
                    (set! VAL (cadr E)))
                  (set! VAL (string-trim (car (string-split VAL #\;))))
                  (if (specified? VAL0)
                    (begin
                      (if (unspecified? VAR)
                        (rcons VAL0 (val VAL))
                        (:= VAL0 VAR (val VAL))))
                    (:= RES VAR (val VAL))))))
              S))
  (if (and (string? FNAME) (fexists? FNAME))
    (parse (file-read FNAME 1)))
  RES)

(define (inits . FNAME)
  (define RES Unspecified)
  (set! FNAME (if (or (empty? FNAME) (not (string? (car FNAME))))
                Unspecified
                (path-normalize (car FNAME))))
  (set! RES (rexpr tinits `(FNAME ,FNAME
                            DATA ,(rexpr Void '()))))
 ;(if (and (string? FNAME) (fexists? FNAME))
 ;(let* ((VAL (file-read FNAME)))
 ;  (if (not (empty? VAL))
 ;    (set! RES (rexpr-link (car VAL))))))
  (:= RES 'DATA (file-readini FNAME (: RES 'DATA)))
  RES)

(method! tinits 'get (=> (I NAME)
                         (: (: I 'DATA) NAME)))

(method! tinits 'set! (=> (I NAME VAL)
                            (:= (: I 'DATA) NAME VAL)))

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

;; Getenv2
(define (getenv2 VAR . ALTV)
  (define RES (env-get VAR))
  (if (not (string? RES))
    (set! RES (if (empty? ALTV) Void (car ALTV))))
  RES)

;; Self path
(set! SC_PATH (path-dir (path-dir (path-normalize (current-source-file)))))

;; BOOT.ini
(init-conf (string+ SC_PATH "/BOOT.ini"))
(merge-conf (env-get "SC_BOOT"))

;; Inits
(set! ERRORCATCH (boolean (conf-get "ERROR_CATCH" ERRORCATCH)))
