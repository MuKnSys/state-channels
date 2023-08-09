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

;; Modules
(define tmod (type "mod"    ;; Scheme module
                   '(LPATH  ;; Logical path
                     FPATH  ;; Physical path
                    )))

;; Constructor
(define (mod . PARM)
  (define RES (rexpr tmod PARM))
 ;(hash-set! _MODSL (: RES 'LPATH) RES)
 ;(hash-set! _MODSF (: RES 'FPATH) RES)
  RES)

;; Init
(define (init-mods)
  Void)

(init-mods)
