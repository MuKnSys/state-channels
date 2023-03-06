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
(import ./basics)

(define (sock-srv PORT . IP) ;; FIXME: no need for IP ; PORT is an npath, so IP <=> (npath-machine PORT)
  (define FAM (npath-port PORT))
  (define SOCK Void)
  (define IP Void)
  (if (string? FAM)
    (begin
      (set! IP (npath-machine PORT))
      (if (unspecified? IP)
        (set! IP (addr-netm _VMACHINE_GADDR)))
      (set! PORT (number FAM)))
    (begin
      (set! FAM (npath-path PORT))
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
      (if (not (file-exists? (dirname PORT)))
       ;(error "sock-srv: directory " (dirname PORT) " not found"))
        (mkdir (path-normalize (dirname PORT))))
      (bind SOCK AF_UNIX PORT))) ;; TODO: detect interferences ; if the file exists, try to connect to it:
                                 ;; => if it doesn't work, delete the file socket, create a new one & use it ;
                                 ;; => if it works, it means that the filesocket is active, raise err["sock already in use"] ;
  (listen SOCK 5)
  (if (== FAM PF_INET)
    `(socksrvi ,SOCK False False)
    `(socksrvf ,SOCK ,PORT False)))

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
      (set! FPATH (npath-path ADDR))
      (set! PORT (npath-port ADDR))
      (set! ADDR (npath-machine ADDR)))
    (set! PORT (car PORT)))
  (if (string? PORT)
    (set! PORT (number PORT)))
  (if (and (unspecified? FPATH) (or (unspecified? ADDR) (unspecified? PORT)))
    (error "sock-cli"))
  (set! FAM (if (specified? FPATH) PF_UNIX PF_INET))
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (if (== FAM PF_INET)
    (let* ((IP (npath-machine (gaddr-npath (string+ _VMACHINE_GADDR ":00")))))
      (bind SOCK AF_INET (inet-pton AF_INET IP) 0) ;; TODO: bind to the address corresponding to the current SUBM
      (connect SOCK AF_INET (inet-pton AF_INET ADDR) PORT))
    (begin
      (if (not (file-exists? (dirname FPATH)))
       ;(error "sock-cli: directory " (dirname FPATH) " not found"))
        (mkdir (path-normalize (dirname FPATH))))
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
  (display MSG (cadr SOCK))) ;; FIXME: (display) has to become (write), here ;; ah no wouldn't work, we need the newline

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
