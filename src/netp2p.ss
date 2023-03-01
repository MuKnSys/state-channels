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
(import ./rexpr)
(import ./scheds)
(import ./procs)
(import ./ipc)
(import ./calls)

;; Address
(define _NETP2PD-PORT 10002) ;; TODO: read that from the environment
(define _NETP2PD-ADDR Void)
(define (netp2pd-addr . WHERE)
  (define ADDR Void)
  (set! WHERE (if (empty? WHERE) 'Local
                                 (car WHERE)))
  (set! ADDR (getenv (if (== WHERE 'Local) "NETP2PD_LOCAL"
                                           "NETP2PD_GLOBAL")))
  (if (and (not (string? ADDR)) (== WHERE 'Local))
    (set! ADDR (addr-netm (current-machine))))
  (if (and (not (string? ADDR)) (== WHERE 'Global))
    (set! ADDR (npath-machine _NETP2PD-ADDR)))
  (if (not (string? ADDR))
   ;(set! ADDR "./NETP2PD") TODO: should be (current-machine) on a default port
    (error "netp2pd-addr"))
  (set! ADDR (string+ ADDR ":" (string _NETP2PD-PORT)))
  ADDR)

(set! _NETP2PD-ADDR (netp2pd-addr))
(define _NETP2PD-GLOBAL (netp2pd-addr 'Global))
(define (netp2p-rootm?)
  (== _NETP2PD-ADDR _NETP2PD-GLOBAL))

;; Client
(define _NETP2P-PEERID Void)
(define (netp2pd?)
  (or (== _NETP2P-PEERID _NETP2PD-ADDR)
      (== _NETP2P-PEERID _NETP2PD-GLOBAL)))

(define (netp2pd-local?)
  (and (== _NETP2P-PEERID _NETP2PD-ADDR)
       (!= _NETP2P-PEERID _NETP2PD-GLOBAL)))

(define (netp2pd-global?)
  (== _NETP2P-PEERID _NETP2PD-GLOBAL))

;(if (net-log)
;(begin
;  (outraw "NETP2PD_ROOT=")
;  (outraw _NETP2PD-GLOBAL)
;  (cr)
;  (outraw "NETP2PD=")
;  (outraw _NETP2PD-ADDR)
;  (cr)))

(define (netp2pd MSG . SOCKA0)
  (define RES '())
  (define L False)
  (define SOCKA (if (empty? SOCKA0)
                  (if (netp2pd?)
                    (if (netp2pd-local?) _NETP2PD-GLOBAL #f)
                    _NETP2PD-ADDR)
                  (car SOCKA0)))
  (define SOCK Void)
  (set! RES
        (if SOCKA
          (begin
            (catch True (=> ()
                          (set! SOCK (sock-cli SOCKA)))
                        (=> (E . OPT)
                         ;(if (net-log)
                         ;(begin
                         ;  (outraw "Can't connect to ")
                         ;  (outraw (netp2pd-addr))
                         ;  (cr)))
                          (noop)))
            (if (unspecified? SOCK)
              (empty)
              (begin
                (sock-write SOCK (sexpr-serialize MSG))
                (while (not (eof-object? L))
                  (set! L (sock-read SOCK))
                  (if (not (eof-object? L))
                    (set! RES (cons (sexpr-parse L) RES))))
                (reverse RES))))
          (empty)))
  (if (nil? RES)
  (begin
   ;(errlog MSG)
   ;(errlog SOCKA0)
    (set! RES (empty)))) ;; FIXME: not super nice, should never be empty lists as result (?)
  RES)

;; DHT (used by root server)
(define DHT (make-hashv-table)) ;; TODO: implement that as the actual LibP2P's DHT

(define (netp2p-DHT-get KEY)
  (hash-ref DHT KEY))

(define (netp2p-DHT-put KEY VAL)
  (hash-set! DHT KEY VAL))

(define (netp2p-DHT-forget KEY)
  (hash-remove! DHT KEY))

;; API (used by root & local servers)
(define (_netp2p-net-enter UID ADDR)
 ;(if (net-log)
 ;(begin
 ;  (outraw "enter ")(outraw UID)
 ;  (outraw "=>")(outraw ADDR)
 ;  (cr)))
  (if (netp2pd?)
    (hash-set! (net-phys) UID ADDR))
  (car (netp2pd `(net-enter ,UID ,ADDR))))

(define (_netp2p-net-leave UID) ;; NOTE: dunno if we can do that on the DHT directly
  (if (netp2pd?)
    (hash-remove! (net-phys) UID))
  (car (netp2pd `(net-leave ,UID))))

(define (_netp2p-net-resolve UID)
  (define RES False)
  (if (netp2pd?)
    (set! RES (hash-ref (net-phys) UID)))
  (if (and (not RES) (not (netp2pd-global?)))
    (set! RES (car (netp2pd `(net-resolve ,UID)))))
  (if (unspecified? RES)
    (set! RES False))
  RES)

(define (_netp2p-net-dispatch MSG ADDR)
  (if (netp2pd?)
    (if (== (addr-netm ADDR) (addr-netm _NETP2PD-ADDR)) ;; Local machine
      (begin
        (outraw "Proc ")
        (outraw (: MSG 'TO))
        (outraw " is being redispatched to ")
        (outraw (addr-netm ADDR))
        (outraw "/")
        (outraw (addr-subm ADDR))
        (cr)
        (host-phys-send (network-addr ADDR "0") MSG)) ;; FIXME: (host-phys-send) doesn't exists anymore
      (netp2pd `(net-dispatch ,MSG ,ADDR) (string+ (addr-netm ADDR) ":" (string _NETP2PD-PORT)))) ;; TODO: test this one
    (error "_netp2p-net-dispatch")))

(define (_netp2p-net-send MSG)
  (if (netp2pd?)
    (let* ((ADDR False))
      (set! ADDR (hash-ref (net-phys) (: MSG 'TO)))
      (if ADDR
        (_netp2p-net-dispatch MSG ADDR)
        (if (netp2pd-global?)
          (begin
            (outraw "Proc ")
            (outraw (: MSG 'TO))
            (outraw " is unknown")
            (cr))
          (car (netp2pd `(net-send ,MSG))))))
    (car (netp2pd `(net-send ,MSG)))))

;; Client API
(define (netp2p-net-enter UID)
  (_netp2p-net-enter UID (if (netp2pd?) _NETP2PD-ADDR
                                        (current-machine))))

(define (netp2p-connect PEERID)
  (if (specified? _NETP2P-PEERID)
    (error "netp2p-connect"))
  (set! _NETP2P-PEERID PEERID)
  (outraw "NETP2P_PEERID=")
  (outraw _NETP2P-PEERID)
  (cr)
  (netp2p-net-enter PEERID))

(define (netp2p-net-leave UID)
  (_netp2p-net-leave UID))

(define (netp2p-net-resolve UID)
  (_netp2p-net-resolve UID))

(define (netp2p-net-send MSG)
  (_netp2p-net-send MSG))
