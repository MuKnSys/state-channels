; procs.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (July 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)
(import ./scheds)

;; Procs
(define tproc (type "proc"     ;; Abstract class for procs
                    '(         ;; Shell : Logical/Remote representation
                      UID      ;; Proc's name (unique identity in the network)
                      USER     ;; User's identity
                      GROUP    ;; Proc's group (if any)
                      STATE    ;; Proc's state : Idle[msgs in the queues], Active[pending msgs in the queues], Waiting
                      COND     ;; Proc's waiting condition : set of messages to be received
                      IN IN!   ;; Incoming messages
                      INPTR    ;; Cons of the next message to be processed
                      OUT      ;; Outcoming (asynchronous) messages
                      OUTPTR   ;; Cons of the next message to be processed
                      STOPPED  ;; Stopped
                      VERBOSE  ;; Verbosity level
                               ;; Physical
                      ROLE     ;; Role: Mapping, Core
                      HEAP     ;; Persisteable heap
                      HANDLER  ;; Proc handler => usually calls SELF's methods (when there is a SELF)
                      HOST     ;; Host process
                     )))

(define (proc? PROC)
  (inherits? (typeof PROC) tproc))

;; Procs (kinds of proc)
(method! tproc 'core? (=> (PROC) ;; Core is similar to "server" ; there is only one core, several mappings
  (== (: PROC 'ROLE) 'Core)))

(method! tproc 'mapping? (=> (PROC) ;; Mapping is similar to "client"
  (== (: PROC 'ROLE) 'Mapping)))

;; Constructor
(define _PROC (make-hashv-table))
(define (allprocs)
  _PROC)

(define (_procId PROC)
  (define (postfix)
    (cond ((== (typeof PROC) tprocl)
           "")
          ((== (typeof PROC) tproch)
           "h")
          ((== (typeof PROC) tprocph)
           "ph")
          ((== (typeof PROC) tprocg)
           "g")
          ((== (typeof PROC) tproceth)
           "eth")
          ((== (typeof PROC) taccount)
           "acc")
          ((and (^ 'mapping? PROC) (== (typeof PROC) tproc))
           "m")
          (else
           (error "_procId.postfix"))))
  (define ID (string (: PROC 'ID)))
  (string+ (car (list-last (string-split ID #\@))) (postfix)))

(define (_getProc PID)
  (define PR Void)
  (if (proc? PID)
    (set! PID (_procId PID)))
  (hash-ref (allprocs) PID))

(define (proc TYPE . PARM)
  (let* ((L (list-group PARM))
         (TAG (<- L 'TAG))
         (RES Void))
    (if (not (type? TYPE))
      (error "proc " TYPE))
    (set! RES (rexpr TYPE L))
    (hash-set! (allprocs) (_procId RES) RES)
    (:? RES 'GROUP Nil)
    (:? RES 'STATE 'Idle)
    (:= RES 'COND Nil)
    (:? RES 'IN (empty))
    (:= RES 'INPTR Nil)
    (:? RES 'IN! (empty))
    (:? RES 'OUT (empty))
    (:= RES 'OUTPTR Nil)
    (:= RES 'STOPPED False)
    (:? RES 'VERBOSE 'Short)
    (:? RES 'ROLE 'Core)
    (:? RES 'HEAP Nil)
    (:? RES 'HANDLER Nil)
    (:? RES 'HOST Nil)
    RES))

(define (allprocsh . PROC)
  (define PROCH (current-proch)) ;; FIXME: hmm, not nice, should dispatch more on what kind of proc it is
  (if (and (not (empty? PROC)) (proc? (car PROC)))
    (set! PROCH (car PROC)))
  (: (: PROCH 'SCHED) 'ALLPROCS))

;; Petname
(define (proc-petname PR)
  (define NAME Void)
  (if (proc? PR)
    (set! NAME (: PR (if (account? PR) 'NAME 'USER))))
  NAME)

;; Map
(method! tproc 'prog! (=> (PROC O) ;; Set the proc's servlet
  (:= PROC 'SELF O)))

(method! tproc 'map (=> (PROC ADDR) ;; Map the proc's heap ;; ADDR is a distributed resource UID
  Void))

;; Proc queues
(method! tproc 'in-idle? (=> (PROC)
  (nil? (: PROC 'INPTR))))

(method! tproc 'out-idle? (=> (PROC)
  (nil? (: PROC 'OUTPTR))))

(method! tproc 'idle? (=> (PROC)
  (and (^ 'in-idle? PROC)
       (^ 'out-idle? PROC))))

(method! tproc 'outnb (=> (PROC)
  (list-length (: PROC 'OUT))))

(method! tproc 'out+ (=> (PROC CALL)
  (:+ PROC 'OUT CALL)
  (if (^ 'out-idle? PROC)
    (:= PROC 'OUTPTR (list-last (: PROC 'OUT))))))

(method! tproc 'out++ (=> (PROC)
  (define RES Nil)
  (if (not (^ 'out-idle? PROC))
  (begin
    (set! RES (car (: PROC 'OUTPTR)))
    (:= PROC 'OUTPTR (cdr (: PROC 'OUTPTR)))))
  RES))

(method! tproc 'in+ (=> (PROC CALL)
  (:+ PROC 'IN CALL)
  (if (^ 'in-idle? PROC)
    (:= PROC 'INPTR (list-last (: PROC 'IN))))))

(method! tproc 'in++ (=> (PROC)
  (define RES Nil)
  (if (not (^ 'in-idle? PROC))
  (begin
    (set! RES (car (: PROC 'INPTR)))
    (:= PROC 'INPTR (cdr (: PROC 'INPTR)))))
  RES))

;; Init
(method! tproc 'schedule (=> (PROC)
  (sched-proc (: (: PROC 'HOST) 'SCHED) PROC)))

;; Init/start
(method! tproc 'host-init (=> (PROC)
  (if (nil? (: PROC 'HOST))
    (:= PROC 'HOST (cond
      ((or (procl? PROC)
           (procg? PROC))
       (current-proch))
      ((== (typeof PROC) tproch)
       (host-proc))
      (else
       (error "host-init")))))
  (if (nil? (: PROC 'HOST))
    (error "host-init(2)"))
  (hash-set! (allprocsh (: PROC 'HOST)) (: PROC 'ID) PROC)
  (^ 'schedule PROC)))

;; Start/stop
(method! tproc 'start (=> (PROC)
  Void))

(method! tproc 'stop (=> (PROC . B)
  Void))

(define (step . PROC)
  (if (and (not (empty? PROC))
           (proc? (car PROC)))
    (^ 'step (car PROC))
    (while (^ 'step (host-proc)))))

;; (define)s for proclets (and others below), due to the impossibility of creating cyclic modules
(define tprocl Void)
(define (procl? PROC)
  (inherits? (typeof PROC) tprocl))

;; (define)s for host procs
(define tproch Void)
(define (proch? PROC)
  (inherits? (typeof PROC) tproch))

(define tprocph Void)
(define (procph? PROC)
  (inherits? (typeof PROC) tprocph))

;; (define)s for proc groups
(define tprocg Void)
(define (procg? PROC)
  (inherits? (typeof PROC) tprocg))

(define tstatech Void)
(define (statech? PROC)
  (== (typeof PROC) tstatech))

;; (define)s for Ethereum-related procs & accounts
(define taccount Void)
(define (account? PROC)
  (== (typeof PROC) taccount))

(define tproceth Void)
(define (proceth? PROC)
  (== (typeof PROC) tproceth))

;; (define)s for calls
(define tcall Void)
(define (call? CALL)
  (== (typeof CALL) tcall))

;; Global context (current hproc)
(define _CURPROCH Nil)
(define (current-proch)
  _CURPROCH)

(define (current-proch! PROC)
  (if (not (or (nil? PROC) (== (typeof PROC) tproch)))
    (error "current-proch! : not a proch" (typeof PROC)))
  (set! _CURPROCH PROC))

;; Global context (current proc)
(define _CURPROC Nil)
(define (current-proc)
  _CURPROC)

(define (current-proc! PROC)
  (if (not (or (nil? PROC)
               (account? PROC) ;; TODO: hsss ... check this (how exactly accounts can become the current proc)
               (and (== (typeof PROC) tprocl)
                    (== (: PROC 'ROLE) 'Core))))
    (error "current-proc! : not a proc" (typeof PROC)))
  (set! _CURPROC PROC)
  (if (and (not (nil? PROC))
           (not (account? PROC))) ;; TODO: hsss ... keep the current proch in case of accounts is a trick
    (current-proch! (: PROC 'HOST))))

;; Global context (sender proc)
(define _SENDPROC Nil)
(define (sender-proc)
  _SENDPROC)

(define (sender-proc! PROC)
  (if (not (or (nil? PROC) (proc? PROC)))
    (error "sender-proc! : not a proc"))
  (set! _SENDPROC PROC))

;; Global context (current call)
(define _CURCALL Nil)
(define (current-call)
  _CURCALL)

(define (current-call! MSG)
  (if (not (or (nil? MSG) (call? MSG)))
    (error "current-call! : not a call"))
  (set! _CURCALL MSG))

;; Global context (physical host proc)
(define _HOSTPROC Nil)
(define (host-proc)
  _HOSTPROC)

(define (host-proc! PROC)
  (if (not (or (nil? PROC) (procph? PROC)))
    (error "host-proc! : not a physical host proc"))
  (set! _HOSTPROC PROC))

(define (the-procph0)
  (: (host-proc) 'PROCPH0))
