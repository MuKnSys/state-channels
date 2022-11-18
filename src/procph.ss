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
(define (_handler-log MSG)
  (outraw "<<")
  (write MSG)
  (outraw ">>")
  (cr))
(define (_handler0 MSG)
  (cond ((== MSG Void)
        ;(_handler-log MSG)
         (set! _HOSTIDNB (+ _HOSTIDNB 1))
         (string _HOSTIDNB))
        ((and (pair? MSG) (== (car MSG) 'enter))
         (if (net-log)
           (_handler-log MSG))
         (let* ((UID (cadr MSG))
                (ADDR (caddr MSG)))
          ;(outraw "nenter(0)=> ") ;; TODO: turn that to debug logs
          ;(outraw UID)
          ;(cr)
           (hash-set! (net-phys) UID ADDR)
          ;(hash-set! (net-procs) UID PROC) ; TODO: create a mapping (?)
           Void))
        ((and (pair? MSG) (== (car MSG) 'leave))
         (_handler-log MSG)
         Void)
        (else
         (let* ((ADDR (hash-ref (net-phys) (: MSG 'TO))))
           (if (not ADDR)
             (begin
               (outraw "Proc ")
               (outraw (: MSG 'TO))
               (outraw " is unknown")
               (cr))
             (_handler1 MSG))
           Void))))

(define (_handler1 MSG)
 ;(_handler-log MSG)
  (net-send MSG)
 ;(lstproc (net-resolve (: MSG 'TO)))
 ;(cr)
)

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
   ;(outraw "Blocking !!!\n")
    (fcntl (the-srv) F_SETFL OFLAGS)) ;; TODO: improve this, by means of really changing the bit on the current state
  (define (nonblockio)
   ;(outraw "Nonblocking !!!\n")
    (fcntl (the-srv) F_SETFL (logior O_NONBLOCK OFLAGS)))
  (define SOCK Void)
  (define PREVRES False)
  (define RES True)
  (define RES2 Void)
  (define SRV Void)
  (define ONCE (list-in? 'Once HOST)) ;; FIXME: improve this (1)
  (define ONCENB (if (> (list-length HOST) 1) (cadr HOST) 0)) ;; FIXME: doesn't work if there is a proc parm
  (define FINI False)
  (define SRVA Void)
  (set! HOST (filter (=> (X) ;; FIXME: improve this (2)
                       (proc? X))
                     HOST))
  (set! HOST (if (empty? HOST)
               (host-proc)
               (car HOST)))
  (if (not (procph? HOST))
    (error "start"))
  (set! SRVA (host-fsock (: HOST 'HOSTID)))
  (if (file-exists? SRVA)
    (file-delete SRVA))
  (set! SRV (sock-srv SRVA))
  (set! OFLAGS (fcntl (the-srv) F_GETFL))
  (while (not FINI)
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
    (if (and ONCE (<= ONCENB 0) (not RES))
      (begin
        (set! SOCK False)
        (set! FINI True))
      (set! SOCK (sock-accept SRV)))
    (if (!= SOCK False)
    (begin
      (if (and ONCE (> ONCENB 0))
        (set! ONCENB (- ONCENB 1)))
     ;(outraw "New client: ")
     ;(out (sock-details SOCK))
     ;(cr)
     ;(outraw "Address: ")
     ;(out (sock-address SOCK))
     ;(cr)
      (set! RES2 (sock-read SOCK))
      (set! RES2 (sexpr-parse RES2))
      (set! RES2 ((: (host-proc) 'HANDLER) RES2))
      (if (specified? RES2)
        (sock-write SOCK RES2))
      (sock-close SOCK))))))

;(if (== _HOSTID "0")
;  (start))

;; Init main
(define (init USER UID SELF)
  (define RES Void)
  (define HOST (proch 'USER 'system
                      'UID "HOST1"))
  (current-proch! HOST)
  (set! RES (procl 'USER USER
                   'UID (string+ UID (: (host-proc) 'HOSTID))
                   'SELF SELF))
  (net-enter RES)
  (current-proc! RES)
  RES)
