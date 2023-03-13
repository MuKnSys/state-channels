; socks[Guile].ss
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

(define (sock-srv PORT) ;; NOTE: PORT is an naddr, so IP <=> (naddr-machine PORT), PATH/PORT <=> (naddr-port/path PORT)
  (define FAM (naddr-port PORT))
  (define SOCK Void)
  (define IP Void)
  (if (string? FAM)
    (begin
      (set! IP (naddr-machine PORT))
      (if (unspecified? IP)
        (set! IP (npath-last (gaddr-phys _VMACHINE_GADDR))))
      (set! PORT (number FAM)))
    (begin
      (set! FAM (naddr-path PORT))
      (if (not (string? FAM))
        (error "sock-srv"))
      (set! PORT FAM)))
  (set! FAM (if (number? PORT) PF_INET PF_UNIX))
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (setsockopt SOCK SOL_SOCKET SO_REUSEADDR 1)
  (if (== FAM PF_INET)
   ;(bind SOCK AF_INET INADDR_ANY PORT) ;; All interfaces
    (bind SOCK AF_INET (inet-pton AF_INET IP) PORT) ;; Specific address
    (begin
      (if (not (fexists? (path-dir PORT)))
       ;(error "sock-srv: directory " (path-dir PORT) " not found"))
        (mkdir (path-normalize (path-dir PORT))))
      (if (fexists? PORT)
        (file-delete PORT))
      (bind SOCK AF_UNIX PORT))) ;; TODO: detect interferences ; if the file exists, try to connect to it:
                                 ;; => if it doesn't work, delete the file socket, create a new one & use it ;
                                 ;; => if it works, it means that the filesocket is active, raise err["sock already in use"] ;
  (listen SOCK 5)
  (if (== FAM PF_INET)
    `(socksrvi ,SOCK False False)
    `(socksrvf ,SOCK ,PORT False)))

(define (sock-select LRD LWR LXC . TIMINGS)
  (define SECS Void)
  (define USECS Void)
  (if (>= (list-length TIMINGS) 1)
    (begin
      (set! SECS (car TIMINGS))
      (if (>= (list-length TIMINGS) 2)
      (set! USECS (cadr TIMINGS)))))
  (if (or (specified? SECS) (specified? USECS))
    (begin
      (if (unspecified? SECS)
        (set! SECS 0))
      (if (unspecified? USECS)
        (set! USECS 0))
      (select LRD LWR LXC SECS USECS))
    (select LRD LWR LXC)))

(define (sock-accept SRV)
  (define TAG (if (== (car SRV) 'socksrvi) 'socksrvclii 'socksrvclif))
  (define SOCK (accept (cadr SRV)))
  (if SOCK
   `(,TAG ,(car SOCK) ,(if (== TAG 'socksrvclii) (cdr SOCK) (caddr SRV)) False)
    False))

(define (sock-cli ADDR . PORT) ;; TODO: implement _nonblocking_ (sock-cli), with async writes (which bufferize the msgs &
  (define ADDR0 ADDR)          ;;       act as async (send)s once it's connected) ; (read) is then nonblocking as a default
  (define FAM Void)
  (define SOCK Void)
  (define FPATH Void)
  (if (empty? PORT)
    (begin
      (set! FPATH (naddr-path ADDR))
      (set! PORT (naddr-port ADDR))
      (set! ADDR (naddr-machine ADDR)))
    (set! PORT (car PORT)))
  (if (string? PORT)
    (set! PORT (number PORT)))
  (if (and (unspecified? FPATH) (or (unspecified? ADDR) (unspecified? PORT)))
    (error "sock-cli"))
  (set! FAM (if (specified? FPATH) PF_UNIX PF_INET))
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (if (== FAM PF_INET)
    (let* ((IP (naddr-machine (gaddr-naddr (string+ _VMACHINE_GADDR ":00")))))
      (bind SOCK AF_INET (inet-pton AF_INET IP) 0) ;; TODO: bind to the address corresponding to the current SUBM
      (connect SOCK AF_INET (inet-pton AF_INET ADDR) PORT))
    (begin
      (if (not (fexists? (path-dir FPATH)))
       ;(error "sock-cli: directory " (path-dir FPATH) " not found"))
        (mkdir (path-normalize (path-dir FPATH))))
      (connect SOCK AF_UNIX FPATH)))
 `(sock ,SOCK False ,ADDR0)) ;; FIXME: there should be no "False" in (sock ...)

(define (sock-read SOCK)
  (read-line (cadr SOCK)))

(define (sock-read-n SOCK N)
  (get-string-n (cadr SOCK) N))

(define (sock-write SOCK MSG . NL) ;; TODO: encapsulate (cadr SOCK), or (caddr SOCK) everywhere
 ;(outraw MSG)
 ;(outraw " [=>")
 ;(outraw (cadddr SOCK)) ;; FIXME: (cadddr SOCK) is likely wrong
 ;(outraw "]")
 ;(cr)
  (set! NL (if (empty? NL)
             True
             (boolean (car NL))))
  (if NL
    (set! MSG (string+ MSG "\n")))
  (_display MSG (cadr SOCK))) ;; FIXME: (_display) has to become (_write), here ;; ah no wouldn't work, we need the newline

(define (sock-close SOCK)
  (close (cadr SOCK)))

(define (sock-touch ADDR . FETCH)
  (define RES Void)
  (set! FETCH (if (empty? FETCH)
                Void
                (car FETCH)))
  (catch True (=> ()
                (define SOCK (sock-cli ADDR))
                (if (specified? FETCH)
                  (begin
                    (sock-write SOCK "Unspecified")
                    (set! RES (sock-read SOCK)))
                  (set! RES True))
                (sock-close SOCK))
              (=> (E . OPT)
                Void))
  RES)

(define (sock-details SOCK)
  (caddr SOCK))

(define (sock-address SOCK)
  (caddr SOCK))

(define (sock-ip-address SOCK)
  (define A (sock-address SOCK))
  (if (not (vector? A))
    Void
    (vector-ref A 1)))
