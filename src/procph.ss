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
(import ./channels)
(import ./scheds)
(import ./procs)
(import ./ipc)
(import ./calls)
(import ./procl)
(import ./proch)

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
                     (if (fexists? _SOCK0A)
                       (file-delete _SOCK0A))
                     (if (atom? VAL)
                       VAL (: VAL 'MSG))))))
;(outraw _HOSTID)
;(cr)
(if (unspecified? _HOSTID)
  (set! _HOSTID "0"))

(define _HOSTIDNB 0)
(define DHT_LOG (com-log "dht"))
(define (_handler0 MSG)
  (cond ((== MSG Void)
         (if DHT_LOG
           (chlog2 MSG ">> "))
         (set! _HOSTIDNB (+ _HOSTIDNB 1))
         (string _HOSTIDNB))
        ((and (pair? MSG) (== (car MSG) 'enter))
         (if DHT_LOG
           (chlog2 MSG ">> "))
         (let* ((UID (cadr MSG))
                (ADDR (caddr MSG)))
           (hash-set! (net-phys) UID ADDR)
          ;(hash-set! (net-procs) UID PROC) ; TODO: create a mapping (?)
           Void))
        ((and (pair? MSG) (== (car MSG) 'leave))
         (if DHT_LOG
           (chlog2 MSG ">> "))
         Void)
        ((and (pair? MSG) (== (car MSG) 'dispatch))
         (error "_handler0::dispatch"))
        (else
         (let* ((ADDR (hash-ref (net-phys) (: MSG (if (specified? (: MSG '_TO)) '_TO ;; TODO: clean this wart
                                                                                (if (: MSG 'ACK) 'FROM 'TO))))))
           (if (not ADDR)
             (begin
               (outraw* "Proc " (: MSG 'TO) " is not here")
               (cr)
               Void)
             (_handler1 MSG))
           Void))))

(define (_handler1 MSG)
  (if DHT_LOG
    (chlog2 MSG ">> "))
  (net-send MSG))

;; Context
(define (the-srv-chan)
  (car (: (the-procph0) 'INCHAN)))

(define (the-srv-sock)
  (: (the-srv-chan) 'SOCK))

(define (the-srv)
  (define SOCK (the-srv-sock))
  (if (specified? SOCK)
    (set! SOCK (cadr SOCK)))
  SOCK)

;; Blocking/nonblocking modes
(define _START-OFLAGS Void)
(define _START-ISBLOCK True)
(define _START-NEVERBLOCK False)

(define (blockio)
 ;(outraw "Blocking !!!\n")
  (if (not _START-NEVERBLOCK)
  (begin
    (set! _START-ISBLOCK True)
    (channel-blocking! (the-srv-chan) True)
    (filep-fcntl (the-srv) F_SETFL _START-OFLAGS))))

(define (nonblockio)
 ;(outraw "Nonblocking !!!\n")
  (set! _START-ISBLOCK False)
  (channel-blocking! (the-srv-chan) False)
  (filep-fcntl (the-srv) F_SETFL (logior O_NONBLOCK _START-OFLAGS)))

;; Init (host-proc)
(if (and DHT_LOG (not (defined? '__STANDALONE__))) ;; FIXME: fix that shit (& add (defined?) to Gerbil's llruntime)
  (chlog2 (gaddr (current-machine) _HOSTID) "<< "))
(define (init0)
  (host-proc! (if (or (== _HOSTID "00") (== _HOSTID "0"))
                (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                        'UID 'phys
                        'HOSTID _HOSTID
                        'HANDLER _handler0)
                (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                        'UID 'phys
                        'HOSTID _HOSTID
                        'HANDLER _handler1)))
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
  (catch True (=> ()
                (set! _START-OFLAGS (filep-fcntl (the-srv) F_GETFL))) ;; TODO: always be async, and only block by means of (select)
              (=> (E . OPT)
                Void))) ;; FIXME: integrate this inside procph0

;; Start
(define (start . TIMES)
  (define SOCK Void)
  (define MSG Void)
  (define RES Void)
  (define ONCE (list-in? 'Once TIMES)) ;; FIXME: improve this (1)
  (define ONCENB (if (> (list-length TIMES) 1) (cadr TIMES) 0))
  (define FINI False)
  (while (not FINI)
  (begin
    ((: (the-procph0) 'ACTIONH)
     (the-procph0))
    (if (and ONCE (<= ONCENB 0)) ; (not RES)) ;; RES from inside (the-procph0).ACTIONH ; in case we need it, return it
      (begin
        (set! SOCK False)
        (set! FINI True))
      (begin
       ;(errlog _START-ISBLOCK)
        (set! SOCK (channel-accept (the-srv-chan)))))
    (if (and ONCE (> ONCENB 0) (not SOCK))
      (set! ONCENB (- ONCENB 1)))
    (if (!= SOCK False)
    (begin
      (set! MSG (channel-read SOCK))
      (if CHAN_LOG
        (chlog2 MSG ">  "))
      (if (and (chmsg? MSG) ;; FIXME: temporary fix ; remove this asap
               (not (procph0-reroute (the-procph0) MSG)))
        (begin
          (set! RES
                ((: (the-procph0) 'RECVH)
                 (the-procph0)
                 MSG))
          (if (specified? RES)
            (channel-write SOCK RES))
          (if (and ONCE (> ONCENB 0))
            (set! ONCENB (- ONCENB 1)))))
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
