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
(import ./json)
(import ./scheds)
(import ./procs)
(import ./ipc)
(import ./calls)
(import ./accounts)
(import ./procl)
(import ./eth)

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

;; Fetch
(method! tproceth 'fetch (=> (PROC FNAME . PARM) ;; NOTE: for calling views (synchronous, direct result)
  (define FROM (current-proc))
  (define CNAME (eth-cname (: PROC 'UID)))
  (json-parse (eth-callMethod CNAME (: PROC 'UID)
                                    (string2 FNAME)
                                    PARM))))

;; Send
(method! tproceth 'send (=> (PROC FNAME . PARM) ;; TODO: use the ABI to decide between (fetch) and (send)
  (define FROM (current-proc))                  ;; TODO: have a SELF & its methods in a proceth, and use (mvparms) to convert parms
  (define CNAME (eth-cname (: PROC 'UID)))      ;;       => use also this do decide between (fetch) and (send) (?)
  (define ACC Void)
  (set! ACC (hash-ref (allaccounts 1) (sy (: FROM 'USER))))
  (if (not ACC)
    (error "eth.send::ACC"))
  (eth-callMethod CNAME (: PROC 'UID)
                        (string2 FNAME)
                        (list-add PARM `(,(: FROM 'UID) ,(string+ "x"
                                                                  (string2 (list-length (: FROM 'OUT))))))
                                                                  ;; FIXME: the shitty "x" stems from the fact that "0" is
                                                                  ;;        turned to an empty string by web3.js when in fact,
                                                                  ;;        rather than this, it should be encoded as "30000..."
                        (: ACC 'ACCNO_LOCETH) "1234") ;; FIXME: use the account's password, not an hardwired "1234"
  (apply proc-send0 `(,PROC ,FNAME . ,PARM))
  (:= FROM 'OUTPTR Nil) ;; FIXME: remove this and make scheduling of messages sent to proceths work exactly the same as with procls
  (noop)))

;; Update
(define (proc-log L)
  (map (=> (X)
         `(,(strnum-trim (cadr (: X 'topics))) ;; _from => TODO: is that solid ? (1)
           ,(strnum-trim (: X 'address))       ;; _to   => TODO: is that solid ? (2)
           .
           ,(eth-decode-parms (: X 'data))))
       L))

(method! tproceth 'sync (=> (PROC)
  (define UID (: PROC 'UID))
  (define LOGS Void)
  (define CALLS '())
  (define BLOCKNO (eth-blockNumber))
  (define INNB (list-length (: PROC 'IN!)))
  (if (not (ethuid? UID))
    (error "tproceth::UID"))
  (if (<= (+ (: PROC 'LASTBLOCK) 1) (eth-blockNumber))
  (begin
    (set! LOGS (eth-getFilterLogs (+ (: PROC 'LASTBLOCK) 1) UID))
    (set! LOGS (proc-log LOGS))
    (for-each (=> (X)
                (define SIGNA (car X))
                (define FROMI0 (list-tail (cdddr X) (- (list-length (cdddr X)) 3))) ;; FIXME: crappy encoding of FROM info at the end of the listparms
                (define FROMI Void)
                (define OUTNB Void)
                (define CALL Void)
                (set! FROMI (cdr FROMI0))
                (set-cdr! FROMI0 Nil)
                (set! OUTNB (cadr FROMI)) ;; NOTE: there is always an OUTNB, because messages are never be sent directly from
                                          ;;   an account, but from an impersonating (local) proc, mentioned in the Solidity
                                          ;;   parms as "uidSender", or something like that ; accounts are only used for signing,
                                          ;;   and it should be verified somehow that the UID of the sender proc is actually owned
                                          ;;   by the user having the account from where he signs.
                (set! OUTNB (if (and (string? OUTNB) (> (string-length OUTNB) 1) (== (substring OUTNB 0 1) "x"))
                              (number (substring OUTNB 1 (string-length OUTNB)))
                              Void)) ;; FIXME: shitty "x" at the beginning of the nonce (see comment in method (send) above)
                (set! CALL (call 'USER 'unknown
                                 'FROM (car FROMI)
                                 'OUTNB OUTNB
                                 'TO (cadr X)
                                 'INNB INNB
                                 'FUNC (sy (caddr X))
                                 'PARM (cdddr X)))
                (sign CALL SIGNA 'SIGN_E)
                (set! CALLS (cons CALL CALLS))
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
(eth-init (string+ SC_PATH "/chain"))
(eth-create-init (string+ SC_PATH "/a.out"))
