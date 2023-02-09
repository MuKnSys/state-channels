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
(set! tprocph (type `("procph" ,tproch) ;; Physical host process (_one_ per physical OS-level process) ; by means of migrating
                    '(HOSTID            ;;  [ host procs to various physical host procs, one can deploy a dapp in various ways.
                      PROCPH0           ;; The low-level avatar of a procph (aka. procph0) [TODO: reunite later with procph].
                     )))

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
(define DHT_LOG (com-log "dht"))
(define (_handler0 MSG)
  (cond ((== MSG Void)
        ;(_handler-log MSG)
         (if DHT_LOG
           (chlog2 MSG ">> "))
         (set! _HOSTIDNB (+ _HOSTIDNB 1))
         (string _HOSTIDNB))
        ((and (pair? MSG) (== (car MSG) 'enter))
         (if (net-log)
           (_handler-log MSG))
         (if DHT_LOG
           (chlog2 MSG ">> "))
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
        ;(_handler-log MSG)
         (if DHT_LOG
           (chlog2 MSG ">> "))
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
  (if DHT_LOG
    (chlog2 MSG ">> "))
  (net-send MSG)
 ;(lstproc (net-resolve (: MSG 'TO)))
 ;(cr)
)

;; Init (host-proc) (1)
(host-proc! (if (== _HOSTID "0")
              (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                      'UID 'phys
                      'HOSTID "0"
                      'HANDLER _handler0)
              (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                      'UID 'phys
                      'HOSTID _HOSTID
                      'HANDLER _handler1)))

;; Context
(define (the-procph0)
  (: (host-proc) 'PROCPH0))

(define (the-srv-chan)
  (car (: (the-procph0) 'INCHAN)))

(define (the-srv-sock)
  (: (the-srv-chan) 'SOCK))

(define (the-srv)
  (cadr (the-srv-sock)))

;; Init (host-proc) (2)
(:= (host-proc)
    'PROCPH0
    (procph0 'PROCID (gaddr-host (string+ ":" (: (host-proc) 'HOSTID))) 'BIND 'Async))
(:= (the-procph0)
    'PROCPH
    (host-proc))
(:= (the-procph0)
    'ACTIONH
    ((=> ()
       (define PREVRES False)
       (=> (PROC)
         (define RES (^ 'step (: PROC 'PROCPH)))
         (if (and RES PREVRES) 
           (noop))
         (if (and (not RES) (not PREVRES))
           (noop))
         (if (and RES (not PREVRES))
           (nonblockio))
         (if (and (not RES) PREVRES)
           (blockio))
         (set! PREVRES RES)))))
(:= (the-procph0)
    'RECVH
    (=> (PROC MSG)
      (define RES (: MSG 'MSG))
      (set! RES (sexpr-parse RES))
      ((: (: PROC 'PROCPH) 'HANDLER) RES)))
(current-procph0! (the-procph0))

(channel-mode! (the-srv-chan) 'Sync)

;; Blocking/nonblocking modes (init)
(set! _START-OFLAGS (fcntl (the-srv) F_GETFL)) ;; FIXME: integrate this inside procph0

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
