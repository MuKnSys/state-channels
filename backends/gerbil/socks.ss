; socks[Gerbil].ss
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
  (noop))

(define (sock-select LRD LWR LXC . TIMINGS)
  (error "gerbil::sock-select"))

(define (sock-accept SRV)
  (noop))

(define (sock-cli ADDR . PORT)
  (noop))

(define (sock-read SOCK)
  (noop))

(define (sock-read-n SOCK N)
  (error "gerbil::sock-read-n"))

(define (sock-write SOCK MSG)
  (noop))

(define (sock-close SOCK)
  (noop))

(define (sock-touch ADDR . FETCH)
  (error "gerbil::sock-touch"))

(define (sock-details SOCK)
  (noop))

(define (sock-address SOCK)
  (noop))

(define (sock-ip-address SOCK)
  (error "gerbil::sock-ip-address"))
