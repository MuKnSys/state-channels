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
  (define FAM (if (number? PORT) PF_INET PF_UNIX))
  (define SOCK Void)
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (setsockopt SOCK SOL_SOCKET SO_REUSEADDR 1)
 ;(bind SOCK AF_INET (inet-pton AF_INET "127.0.0.1") PORT) ;; Specific address?
  (if (== FAM PF_INET)
    (bind SOCK AF_INET INADDR_ANY PORT)
    (bind SOCK AF_UNIX PORT))
  (listen SOCK 5)
  (if (== FAM PF_INET)
    `(socksrvi ,SOCK #f)
    `(socksrvf ,SOCK ,PORT)))

(define (sock-accept SRV)
  (define TAG (if (== (car SRV) 'socksrvi) 'socksrvclii 'socksrvclif))
  (define SOCK (accept (cadr SRV)))
 `(,TAG ,(car SOCK) ,(if (== TAG 'socksrvclii) (cdr SOCK) (caddr SRV))))

(define (sock-cli ADDR . PORT)
  (define FAM (if (empty? PORT) PF_UNIX PF_INET))
  (define SOCK Void)
  (set! PORT (if (empty? PORT) #f (car PORT)))
  (set! SOCK (socket FAM SOCK_STREAM 0))
  (if (== FAM PF_INET)
    (connect SOCK AF_INET (inet-pton AF_INET ADDR) PORT)
    (connect SOCK AF_UNIX ADDR))
 `(sock ,SOCK #f))

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
