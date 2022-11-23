; netp2p[Mockup].ss
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

;; NetP2P
(define (netp2pd-addr . WHERE)
  (define ADDR Void)
  (set! WHERE (if (empty? WHERE) 'Local
                                 (car WHERE)))
  (set! ADDR (getenv (== WHERE 'Local "NET2PD_LOCAL"
                                      "NETP2PD_GLOBAL")))
  (if (and (not (string? ADDR)) (== WHERE 'Global))
    (set! ADDR (getenv "NETP2PD_LOCAL")))
  (if (not (string? ADDR))
   ;(set! ADDR "./NETP2PD") TODO: should be (current-machine) on a default port
    (error "netp2pd-addr"))
  ADDR)

(define _NETP2PD-GLOBAL (netp2pd-addr 'Global))
(define _NETP2PD-ADDR (netp2pd-addr))
(define (netp2p-root?)
  (== _NETP2PD-ADDR _NETP2PD-GLOBAL))

(define (netp2p-connect PEERID) ;; TODO: implement that
  (if (netp2p-root?)
    (noop)
    (netp2p-net-enter PEERID)))

(define _NETP2PD-PEERID (current-machine)) ;; TODO: replace that by a generated key
(netp2p-connect _NETP2PD-PEERID)

;; Client
(define (netp2pd MSG)
  (define RES '())
  (define L False)
  (define SOCK (sock-cli _NETP2PD-ADDR))
  (sock-write SOCK (sexpr-serialize MSG))
  (while (not (eof-object? L))
    (set! L (sock-read SOCK))
    (if (not (eof-object? L))
      (set! RES (cons (sexpr-parse L) RES))))
  (reverse RES))

;; Client API
(define (netp2p-net-enter UID)
  (car (netp2pd `(net-enter ,UID ,_NETP2PD-ADDR))))

(define (netp2p-net-leave UID)
  (car (netp2pd `(net-leave ,UID))))

(define (netp2p-net-resolve UID)
  (car (netp2pd `(net-resolve ,UID))))
