; sc_lib.ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (April 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./runtime)
(import ./micropay)

;; State channel library
(set! tsc_lib (type "sc_lib"  ;; Class for the whole library ; it's a singleton
                    '()))

;; Constructor
(define (sc_lib)
  ;; create [avec host identity]
  Void)

;; Fetch various kinds of things
;=> accounts ;
;=> classes (?) ;
;=> hosts ;
;=> endpoints ;
;=> groups ;

;; Identities
;whoami
;login
;iam
