; aliases.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (September 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)

;; Aliases
(define taliases (type "aliases" '(ROOT ALIAS ADDR)))

(define (aliases . FNAME)
  (define RES Unspecified)
  (set! FNAME (if (empty? FNAME)
                Unspecified
                (path-normalize (car FNAME))))
  (set! RES (rexpr taliases `(FNAME ,FNAME
                              ROOT ,(rexpr Void '())
                              ALIAS ,(rexpr Void '())
                              ADDR ,(rexpr Void '()))))
  (if (and (string? FNAME) (file-exists? FNAME))
  (let* ((VAL (file-read FNAME)))
    (if (not (empty? VAL))
      (set! RES (rexpr-link (car VAL))))))
  RES)

(method! taliases 'root! (=> (A S)
                           (if (unspecified? (: (: A 'ROOT) S))
                             (:= (: A 'ROOT) S 0))))

(method! taliases 'alias (=> (A ADDR)
                           (: (: A 'ADDR) ADDR)))

(method! taliases 'addr (=> (A NAME)
                          (: (: A 'ALIAS) NAME)))

(method! taliases 'alias! (=> (A NAME ADDR)
                            (:= (: A 'ALIAS) NAME ADDR)
                            (if (specified? (: (: A 'ADDR) ADDR))
                              (<- (: A 'ALIAS) (: (: A 'ADDR) ADDR)))
                            (:= (: A 'ADDR) ADDR NAME)))

(method! taliases 'new! (=> (A R ADDR . OPT)
                          (define N (: (: A 'ROOT) R))
                          (define SEP (if (empty? OPT) "@" (car OPT)))
                          (if (unspecified? N)
                            (^ 'root! A R))
                          (set! N (: (: A 'ROOT) R))
                          (^ 'alias! A (strsy+ R SEP N) ADDR)
                          (:= (: A 'ROOT) R (+ N 1))))

(method! taliases 'save (=> (A)
  (if (specified? (: A 'FNAME))
    (file-write (: A 'FNAME) A
      (=> (O)
        (>> O 'Indent))))))
