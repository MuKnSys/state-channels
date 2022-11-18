; libp2p[Mockup].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (November 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ../src/runtime)

;; LibP2P API
(define (libp2p-connect PEERID) ;; TODO: implement that
  (noop))

(define DHT (make-hashv-table)) ;; TODO: implement that as the actual LibP2P's DHT

(define (libp2p-DHT-get KEY)
  (hash-ref DHT KEY))

(define (libp2p-DHT-put KEY VAL)
  (hash-set! DHT KEY VAL))

(define (libp2p-DHT-forget KEY)
  (hash-remove! DHT KEY))

;; Client
(define (libp2pd-addr)
  (define ADDR (getenv "LIBP2PD_ADDR"))
  (if (not (string? ADDR))
    (set! ADDR "./LIBP2PD"))
  ADDR)
(define _LIBP2PD-ADDR (libp2pd-addr))

(define (libp2pd-connect ADDR) ;; TODO: implement a scheme for addresses that are in the same time external & internal
  (sock-cli ADDR))

(define (libp2pd MSG)
  (define RES '())
  (define L False)
  (define SOCK (libp2pd-connect _LIBP2PD-ADDR))
  (sock-write SOCK (sexpr-serialize MSG))
  (while (not (eof-object? L))
    (set! L (sock-read SOCK))
    (if (not (eof-object? L))
      (set! RES (cons (sexpr-parse L) RES))))
  (reverse RES))

;; Client's PeerID
(define _LIBP2P-PEERID (current-machine)) ;; TODO: replace that by a generated key
(libp2p-connect _LIBP2P-PEERID)

;; Client API
(define (libp2p-net-enter UID)
  (car (libp2pd `(net-enter ,UID ,_LIBP2P-PEERID))))

(define (libp2p-net-leave UID)
  (car (libp2pd `(net-leave ,UID))))

(define (libp2p-net-resolve UID)
  (car (libp2pd `(net-resolve ,UID))))
