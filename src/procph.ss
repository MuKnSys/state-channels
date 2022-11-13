; procph.ss
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
(import ./netp2p)
(import ./calls)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./netp2p)
  (import: ./calls))

;; Physical host proc
(set! tprocph (type `("procph" ,tproch) ;; Physical host process (_one_ per physical OS-level process) ; by means
                    '(HOSTID            ;; of migrating host processes to various physical host processes, one can
                     )))                ;; deploy a distributed app in various ways.

;; Constructor
(define (procph . PARM)
  (define RES (apply proc `(,tprocph . ,PARM)))
  (:? RES 'SCHED (sched))
  (:? RES 'HOSTID "")
  RES)

;; Send
(method! tprocph 'send (=> (PROC FNAME . PARM) ;; NOTE: PROC is the _target_ (i.e., the TO)
  (define CALL (if (call? FNAME)
                 FNAME
                 (call 'FUNC FNAME
                       'PARM PARM)))
  (net-send CALL PROC)))

;; Step
(method! tprocph 'step (=> (PROC)
  (define RES ((method tproch 'step) PROC))
  Void ;; TODO: check the sock
  RES))

;; Current physical process
(define _SOCK0A (host-fsock "0"))
(define _HOSTID Void)
(catch True (=> ()
              (define SOCK (sock-cli _SOCK0A))
              (sock-write SOCK "Unspecified")
              (set! _HOSTID (sock-read SOCK))
              (sock-close SOCK))
            (=> (E . OPT)
             ;(outraw E)
             ;(outraw OPT)
             ;(cr)
              (if (file-exists? _SOCK0A)
                (file-delete _SOCK0A))))
;(outraw _HOSTID)
;(cr)
(if (unspecified? _HOSTID)
  (set! _HOSTID "0"))

(define _HOSTIDNB 0)
(define (_handler0 MSG)
  (define (log)
    (outraw "<<")
    (write MSG)
    (outraw ">>")
    (cr))
  (cond ((== MSG Void)
         (log)
         (set! _HOSTIDNB (+ _HOSTIDNB 1))
         (string _HOSTIDNB))
        ((and (pair? MSG) (== (car MSG) 'enter))
         (noop))
        ((and (pair? MSG) (== (car MSG) 'leave))
         (log))
        (else
         (log)
         Void)))

(define (_handler1 MSG)
  (outraw "<<")
  (write MSG)
  (outraw ">>")
  (cr))

(host-proc! (if (== _HOSTID "0")
              (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                      'UID 'phys
                      'HOSTID "0"
                      'HANDLER _handler0)
              (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                      'UID 'phys
                      'HOSTID _HOSTID
                      'HANDLER _handler1)))

;; Start
(define (start . HOST)
  (define (the-srv)
    (cadr SRV))
  (define OFLAGS Void)
  (define (blockio)
    (outraw "Blocking !!!\n")
    (fcntl (the-srv) F_SETFL OFLAGS)) ;; TODO: improve this, by means of really changing the bit on the current state
  (define (nonblockio)
    (outraw "Nonblocking !!!\n")
    (fcntl (the-srv) F_SETFL (logior O_NONBLOCK OFLAGS)))
  (define SOCK Void)
  (define PREVRES False)
  (define RES True)
  (define RES2 Void)
  (define SRV Void)
  (set! HOST (if (empty? HOST)
               (host-proc)
               (car HOST)))
  (if (not (procph? HOST))
    (error "start"))
  (set! SRV (sock-srv (host-fsock (: HOST 'HOSTID))))
  (set! OFLAGS (fcntl (the-srv) F_GETFL))
  (while True
  (begin
    (set! RES (^ 'step (host-proc)))
    (if (and RES PREVRES) 
      (noop))
    (if (and (not RES) (not PREVRES))
      (noop))
    (if (and RES (not PREVRES))
      (nonblockio))
    (if (and (not RES) PREVRES)
      (blockio))
    (set! PREVRES RES)
    (set! SOCK (sock-accept SRV))
    (if (!= SOCK False)
    (begin
      (outraw "New client: ")
      (out (sock-details SOCK))
      (cr)
      (outraw "Address: ")
      (out (sock-address SOCK))
      (cr)
      (set! RES2 (sock-read SOCK))
      (set! RES2 (sexpr-parse RES2))
      (set! RES2 ((: (host-proc) 'HANDLER) RES2))
      (if (specified? RES2)
        (sock-write SOCK RES2))
      (sock-close SOCK))))))

;(if (== _HOSTID "0")
;  (start))
