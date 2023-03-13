; socks[filesocks_dummy].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (March 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)

(define (sock-srv PORT)
  Void) ;(error "filesocks_dummy::sock-srv"))

(define (sock-select LRD LWR LXC . TIMINGS)
  (error "filesocks_dummy::sock-select"))

(define (sock-accept SRV)
  (error "filesocks_dummy::sock-accept"))

(define (sock-cli ADDR . PORT)
  Void); (error "filesocks_dummy::sock-cli"))

(define (sock-read SOCK)
  (error "filesocks_dummy::sock-read"))

(define (sock-read-n SOCK N)
  (error "filesocks_dummy::sock-read-n"))

(define (sock-write SOCK MSG . NL)
  (error "filesocks_dummy::sock-write"))

(define (sock-close SOCK)
  (error "filesocks_dummy::sock-close"))

(define (sock-touch ADDR . FETCH)
  (error "filesocks_dummy::sock-touch"))

(define (sock-details SOCK)
  (error "filesocks_dummy::sock-details"))

(define (sock-address SOCK)
  (error "filesocks_dummy::sock-address"))

(define (sock-ip-address SOCK)
  (error "filesocks_dummy::sock-ip-address"))
