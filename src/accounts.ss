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
                      ;UID                ;; UID is the public key
                       PASSWORD           ;; Password
                      )))

;; Constructor
(define _ACCOUNT (make-hashv-table))
(define _ACCOUNTByName (make-hashv-table))
(define (allaccounts . BYNAME) ;; NOTE: we keep that at the moment ; see if (or how) we should separate accounts from procs
  (if (empty? BYNAME) _ACCOUNT _ACCOUNTByName))

(define (account . PARM)
  (define RES (apply proc `(,taccount . ,PARM)))
  (define PUBKEY Void)
  (define NAME Void)
  (:? RES 'CATEG 'Ethereum)
  (set! PUBKEY (: RES 'UID))
  (if (specified? PUBKEY)
    (hash-set! (allaccounts) PUBKEY RES))
  (set! NAME (: RES 'NAME))
  (if (specified? NAME)
    (hash-set! (allaccounts 1) (sy NAME) RES))
  RES)

;; Step
(method! taccount 'step (=> (PROC)
  (error "account::step")))
