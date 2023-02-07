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
(import ./ipc)
(import ./calls)
(import ./netp2p)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./ipc)
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
(define _HOSTID ((=> ()
                  ;(define VAL (sock-touch _SOCK0A 1))
                   (define VAL (channel-touch ":0" 1))
                   (if (unspecified? VAL)
                     (if (file-exists? _SOCK0A)
                       (file-delete _SOCK0A))
                     (: VAL 'MSG)))))
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
           (netp2p-net-enter UID)
           Void))
        ((and (pair? MSG) (== (car MSG) 'leave))
         (_handler-log MSG)
         Void)
        ((and (pair? MSG) (== (car MSG) 'dispatch))
         (set! MSG (cadr MSG))
         (let* ((ADDR (hash-ref (net-phys) (: MSG (if (: MSG 'ACK) 'FROM 'TO)))))
           (if (not ADDR)
             (begin
               (outraw "Proc ")
               (outraw (: MSG 'TO))
               (outraw " is unknown [can't redispatch]")
               (cr))
             (_handler1 MSG))))
        (else
         (let* ((ADDR (hash-ref (net-phys) (: MSG (if (: MSG 'ACK) 'FROM 'TO)))))
           (if (not ADDR)
             (begin
               (outraw "Proc ")
               (outraw (: MSG 'TO))
               (outraw " is being resent")
               (cr)
               (netp2p-net-send MSG))
             (_handler1 MSG))
           Void))))

(define (_handler1 MSG)
 ;(outraw "[H1]")(_handler-log MSG)
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
(define _START-SRV Void) ;; FIXME: doesn't work with multiple hosts
(define _START-OFLAGS Void)
(define _START-ISBLOCK True)
(define _START-NEVERBLOCK False)
(define (the-srv-sock)
  (: _START-SRV 'SOCK))
(define (the-srv)
  (cadr (: _START-SRV 'SOCK)))
(define (blockio)
 ;(outraw "Blocking !!!\n")
  (if (not _START-NEVERBLOCK)
  (begin
    (set! _START-ISBLOCK True)
    (fcntl (the-srv) F_SETFL _START-OFLAGS)))) ;; TODO: improve this, by means of really changing the bit on the current state
(define (nonblockio)
 ;(outraw "Nonblocking !!!\n")
  (set! _START-ISBLOCK False)
  (fcntl (the-srv) F_SETFL (logior O_NONBLOCK _START-OFLAGS)))
(define (start . HOST)
  (define SOCK Void)
  (define PREVRES False)
  (define RES True)
  (define RES2 Void)
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
  (set! SRVA (host-fsock (: HOST 'HOSTID))) ;; FIXME: doesn't work for if we call (start) several times with different hosts
  (if (unspecified? _START-SRV)
    (begin
      (if (file-exists? SRVA)
        (file-delete SRVA))
     ;(set! _START-SRV (sock-srv SRVA))
      (set! _START-SRV (channel-srv (string+ ":" (: HOST 'HOSTID))))
      (channel-mode! _START-SRV 'Sync)
      (set! _START-OFLAGS (fcntl (the-srv) F_GETFL))))
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
    (if (and ONCE (<= ONCENB 0)) ; (not RES))
      (begin
        (set! SOCK False)
        (set! FINI True))
      (begin
     ;(errlog _START-ISBLOCK)
      (set! SOCK (channel-accept _START-SRV))))
    (if (and ONCE (> ONCENB 0) (not SOCK))
      (set! ONCENB (- ONCENB 1)))
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
      (set! RES2 (: (channel-read SOCK) 'MSG))
     ;(outraw "Message0: ")
     ;(out RES2)
     ;(cr)
      (set! RES2 (sexpr-parse RES2))
     ;(outraw "Message: ")
     ;(out RES2)
     ;(cr)
      (set! RES2 ((: (host-proc) 'HANDLER) RES2))
      (if (specified? RES2)
        (channel-write SOCK RES2))
      (channel-eof! SOCK))))))

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
