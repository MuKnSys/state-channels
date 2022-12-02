; accounts.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (November 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)
(import ./procs)

;; Account
(set! taccount (type `("account" ,tproc)  ;; Account (SSH, Ethereum-based, etc.)
                     '(CATEG              ;; Category (SSH, Ethereum, Bitcoin, UNIX, Windows, etc.)
                       NAME               ;; Account name (e.g. "nobody", "root", "smith", etc.)
                       PRIVKEY            ;; Private key
                       PUBKEY             ;; Public key
                       PASSWORD           ;; Password
                      )))

;; Constructor
(define (account . PARM)
  (define RES (apply proc `(,taccount . ,PARM)))
  (:? RES 'CATEG 'Ethereum)
  RES)

;; Step
(method! taccount 'step (=> (PROC)
  (error "account::step")))
