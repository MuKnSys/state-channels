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

(define (sock-srv PORT)
  (define FAM (npath-port PORT))
  (define SOCK Void)
  (if (string? FAM)
    (set! PORT (number FAM))
    (begin
      (set! FAM (npath-path PORT))
      (if (not (string? FAM))
        (error "sock-srv"))
      (set! PORT FAM)))
  (set! FAM (if (number? PORT) PF_INET PF_UNIX))
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (setsockopt SOCK SOL_SOCKET SO_REUSEADDR 1)
 ;(bind SOCK AF_INET (inet-pton AF_INET "127.0.0.1") PORT) ;; Specific address?
  (if (== FAM PF_INET)
    (bind SOCK AF_INET INADDR_ANY PORT)
    (begin
      (if (not (file-exists? (dirname PORT)))
       ;(error "sock-srv: directory " (dirname PORT) " not found"))
        (mkdir (path-normalize (dirname PORT))))
      (bind SOCK AF_UNIX PORT)))
  (listen SOCK 5)
  (if (== FAM PF_INET)
    `(socksrvi ,SOCK False)
    `(socksrvf ,SOCK ,PORT)))

(define (sock-accept SRV)
  (define TAG (if (== (car SRV) 'socksrvi) 'socksrvclii 'socksrvclif))
  (define SOCK (accept (cadr SRV)))
  (if SOCK
   `(,TAG ,(car SOCK) ,(if (== TAG 'socksrvclii) (cdr SOCK) (caddr SRV)))
    False))

(define (sock-cli ADDR . PORT)
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
    (connect SOCK AF_INET (inet-pton AF_INET ADDR) PORT)
    (begin
      (if (not (file-exists? (dirname FPATH)))
       ;(error "sock-cli: directory " (dirname FPATH) " not found"))
        (mkdir (path-normalize (dirname FPATH))))
      (connect SOCK AF_UNIX FPATH)))
 `(sock ,SOCK False))

(define (sock-read SOCK)
  (read-line (cadr SOCK)))

(define (sock-write SOCK MSG)
  (display (string-append MSG "\n") (cadr SOCK)))

(define (sock-close SOCK)
  (close (cadr SOCK)))

(define (sock-details SOCK)
  (caddr SOCK))

(define (sock-address SOCK)
  (define TAG (car SOCK))
  (if (== TAG 'socksrvclii) 
    (gethostbyaddr (sockaddr:addr (sock-details SOCK)))
    (caddr SOCK)))
