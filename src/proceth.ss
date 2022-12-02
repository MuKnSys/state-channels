; proceth.ss
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
(import ./scheds)
(import ./procs)
(import ./ipc)
(import ./calls)
(import ./accounts)

;; Ethereum proc
(set! tproceth (type `("proceth" ,tproc)  ;; Ethereum process
                     '(LASTBLOCK          ;; Last block number it has been updated from
                      )))

;; Constructor
(define (proceth . PARM)
  (define RES (apply proc `(,tproceth . ,PARM)))
  (:= RES 'ROLE 'Mapping)
  (:= RES 'LASTBLOCK 0)
  RES)

(define (ethuid? UID)
  (== (substring UID 0 2) "0x"))

;; Send
(method! tproceth 'send (=> (PROC FNAME . PARM)
 ;(define FROM (current-proc))
  (define CNAME (eth-cname (: PROC 'UID)))
  (eth-callMethod CNAME (: PROC 'UID) (string FNAME) PARM)
 ;(^ 'out+ FROM CALL)
  (noop)))

;; Update
(define (proc-log L)
  (map (=> (X)
         `(,(strnum-trim (cadr (: X 'topics))) ;; _from => TODO: is that solid ? (1)
           ,(strnum-trim (: X 'address))       ;; _to   => TODO: is that solid ? (2)
           .
           ,(eth-decode-parms (: X 'data))))
       L))

(method! tproceth 'sync (=> (PROC) ;; TODO: set OUTNB, INNB & signatures
  (define UID (: PROC 'UID))
  (define LOGS Void)
  (define CALLS '())
  (define BLOCKNO (eth-blockNumber))
 ;(define OUTNB ???) TODO: there should always be an OUTNB, because messages should never be sent directly from an account, but from an impersonating (local) proc, mentioned in the Solidity parms as "uidSender", or something like that ; accounts are only used for signing, and it should be verified somehow that the UID of the sender proc is actually owned by the user having the account from where he signs.
  (define INNB (list-length (: PROC 'IN!)))
  (if (not (ethuid? UID))
    (error "tproceth::UID"))
  (if (<= (+ (: PROC 'LASTBLOCK) 1) (eth-blockNumber))
  (begin
    (set! LOGS (eth-getFilterLogs (+ (: PROC 'LASTBLOCK) 1) UID))
    (set! LOGS (proc-log LOGS))
    (for-each (=> (X)
                (set! CALLS (cons (call 'USER 'unknown
                                        'FROM (car X)
                                        'TO (cadr X)
                                        'FUNC (sy (caddr X))
                                        'PARM (cdddr X)
                                        'INNB INNB)
                                  CALLS))
                (set! INNB (+ INNB 1)))
              LOGS)
    (for-each (=> (CALL)
                (:+ PROC 'IN CALL)
                (:+ PROC 'IN! CALL))
              (reverse CALLS))
    (:= PROC 'LASTBLOCK BLOCKNO)))))

;; Step
(method! tproceth 'step (=> (PROC)
  (error "proceth::step")))

;; Init
(eth-init "~/StateChannels/chain")
(eth-create-init "~/StateChannels/a.out")
