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
(import ./calls)

(export
  (import: ./calls))

;; Procs
(set! tproc (type "proc"     ;; Abstract class for procs
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

;; Various physical kinds of proc (standard proclet ; UNIX proc ; and Ethereum contract, are the basic ones)
(set! tprocl (type `("procl" ,tproc)  ;; Proclet
                   '(SELF             ;; Server's own servlet (1st object in the heap)
                    )))

(set! tproch (type `("proch" ,tproc)  ;; Host process
                   '(IP PORT          ;; Address : IP+PORT/Filesocket ; or pure in-mem
                     SCHED            ;; Local host's scheduler
                    )))

(set! tprocph (type `("procph" ,tproch) ;; Physical host process (_one_ per physical OS-level process) ; by means
                    '()))               ;; of migrating host processes to various physical host processes, one can
                                        ;; deploy a distributed app in various ways.

;; Proc groups
;; =>
;;   proc groups have proc UIDs ; but we dispatch on them only locally, at the moment ;
;;   proc groups can receive and send messages, but they are virtual ; so, that requires consensus ;
;;   procs can belong to only one group (for state-channel-replicated procs, seems that it can't be otherwise) ;
;;
(set! tprocg (type `("procg" ,tproc)  ;; Proc group
                   '(PARENT           ;; Parent process
                     PEER             ;; Peers
                    )))

(set! tstatech (type `("statech" ,tprocg) ;; State channel
                     '()))

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

(define (procl . PARM)
  (define RES (apply proc `(,tprocl . ,PARM)))
  (^ 'host-init RES)
  RES)

(define (proch . PARM)
  (define RES (apply proc `(,tproch . ,PARM)))
  (:? RES 'SCHED (sched))
  (if (nil? (: RES 'HOST))
    (:= RES 'HOST (host-proc)))
  (^ 'host-init RES)
  RES)

(define (procph . PARM)
  (define RES (apply proc `(,tprocph . ,PARM)))
  (:? RES 'SCHED (sched))
  RES)

;(define (proc? PROC)) ;; FIXME: done in calls.ss
;(define (procl? PROC))
;(define (proch? PROC))
;(define (procph? PROC))

;; Proc groups
(define (procg . PARM)
  (define RES (apply proc `(,tprocg . ,PARM)))
  (:? RES 'PEER (empty))
  (^ 'host-init RES)
  RES)

(define (proc-group . L) ;; TODO: improve this
  (if (not (empty? L))
  (let* ((FROM (car L))
         (L2 Void)
         (RES (procg)))
    (set! L (cdr L))
    (set! L2 (map (=> (PR)
                    (if (proc? PR)
                      (: PR 'UID)
                      PR))
                  L))
    (:= RES 'PARENT FROM)
    (:= RES 'PEER (list-copy L2))
    (for-each (=> (PR)
                (:= PR 'GROUP RES)
              )
              L)
    RES)))

(define (statech . PARM)
  (apply proc `(,tstatech . ,PARM)))

;(define (procg? PROC))
;(define (statech? PROC))

;; Current hproc
;(define _CURPROCH Nil) ;; FIXME: done in calls.ss
;(define (current-proch)
;  _CURPROCH)

(define (current-proch! PROC)
  (if (not (or (nil? PROC) (== (typeof PROC) tproch)))
    (error "current-proc! : not a proch" (typeof PROC)))
  (set! _CURPROCH PROC))

;; Current proc
(define _CURPROC Nil)
(define (current-proc)
  _CURPROC)

(define (current-proc! PROC)
  (if (not (or (nil? PROC)
               (and (== (typeof PROC) tprocl)
                    (== (: PROC 'ROLE) 'Core))))
    (error "current-proc! : not a proc" (typeof PROC)))
  (set! _CURPROC PROC)
  (if (not (nil? PROC))
    (current-proch! (: PROC 'HOST))))

;; Sender proc
(define _SENDPROC Nil)
(define (sender-proc)
  _SENDPROC)

(define (sender-proc! PROC)
  (if (not (or (nil? PROC) (proc? PROC)))
    (error "sender-proc! : not a proc"))
  (set! _SENDPROC PROC))

;; Call
(define (fname-isret? F)
  (set! F (string F))
  (let* ((LF (string-length F))
         (LP (string-length "/return")))
    (and (> LF LP)
         (== (substring F (- LF LP) LF) "/return"))))

(method! tproc 'call (=> (PROC FNAME . PARM)
  (let* ((SELF (: PROC 'SELF)) ;; TODO: add what to do if HANDLER is set (is HANDLER necessary ?)
        )
    Void ;; TODO: context-switch to SELF.HEAP
    (if (unspecified? SELF)
      (error "proc<" (: PROC 'UID) ">::call : no SELF"))
    (if (fname-isret? FNAME)
      (let* ((FROM (: (: PROC 'GROUP) 'PARENT))
             (CALL (car PARM))) ;; FIXME: should ensure the same return is not evaluated 2 times
        (if (unspecified? FROM)
          (error "proc.call : return without FROM " (: PROC 'UID)))
        (if (string? CALL)
          (set! CALL (string->number CALL)))
        (let* ((CALL0 CALL))
          (set! CALL (list-get (: FROM 'IN!) CALL))
          (if (unspecified? CALL)
            (error "tproc::call::isret " (: FROM 'UID) ".rl[" CALL0 "] : no such call"))
          (set! CALL (copy-tree CALL))
          (:= CALL 'PARM (cdr (mvparms (: CALL 'FUNC)
                                       (cons (: (net-resolve (: CALL 'TO)) 'SELF)
                                             (: CALL 'PARM)))))
          (apply ^? `(,FNAME ,SELF . (,CALL)))))
      (apply ^? `(,FNAME ,SELF . ,PARM))))))

(method! tproc 'sync (=> (PROC)
  (define RETS (map (=> (CALL)
                      (number (car (: CALL 'PARM))))
                    (filter (=> (CALL)
                              (and (fname-isret? (: CALL 'FUNC))
                                   (not (: CALL 'ACK))))
                            (: PROC 'OUT))))
  (define RL0 Void)
  (define MASTER (: (: PROC 'GROUP) 'PARENT))
  (if (and (specified? MASTER) (not (boxed-empty? (: MASTER 'IN!))))
  (begin
    (set! RL0 (filter (=> (CALL)
                        (not (list-in? (cadr CALL) RETS)))
                      (map (=> (CALL)
                             `(,(: CALL 'FUNC) ,(: CALL 'INNB)))
                           (: MASTER 'IN!))))
    (for-each (=> (CALL)
                (^ 'send (: PROC 'GROUP) (sy (string+ (string (car CALL)) "/return")) (cadr CALL)))
              RL0)))))

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

;; Proc send
(method! tproc 'send (=> (PROC FNAME . PARM) ;; NOTE: PROC is the _target_ (i.e., the TO)
  (let* ((FROM (current-proc))
         (STATE (: FROM 'STATE))
         (CALL Void))
    (if (not (or (== STATE 'Idle) (== STATE 'Active)))
      (error "proc::send"))
    (:= FROM 'STATE 'Active)
    (set! CALL (call 'USER (: FROM 'USER)
                     'FROM (: FROM 'UID)
                     'OUTNB (^ 'outnb FROM)
                     'TO (: PROC 'UID)
                     'FUNC FNAME
                     'PARM PARM))
    (sign CALL (: FROM 'USER) 'SIGN_B) ;; TODO: verify that it's necessary (cf. (sign) step in (out-step))
    (^ 'out+ FROM CALL)
    (^ 'schedule FROM))))

;; Stepping
(define _TRACESTEPS False)
(define (trace-steps . B)
  (if (empty? B)
    _TRACESTEPS
    (set! _TRACESTEPS (car B))))

(method! tproc 'out-step (=> (PROC)
  (ifTrue (and (not (^ 'out-idle? PROC))
               (== (: PROC 'STATE) 'Active))
          (=> ()
            (define MSG (^ 'out++ PROC))
            (sign MSG (: PROC 'USER) 'SIGN_E) ;; TODO: verify that it's necessary
            (net-send MSG)))))

(method! tproc 'core-call (=> (PROC MSG)
  (define SPROC Void)
  (define RES Void)
  (if (not (^ 'core? PROC))
    (error "in-step"))
  (set! SPROC (net-resolve (: MSG 'FROM)))
  (if (not SPROC)
    (error "proc<" (: MSG 'FROM) ">::core-call : no sender proc"))
  (sender-proc! SPROC)
  (set! RES (apply ^ `(call ,PROC ,(: MSG 'FUNC) . ,(: MSG 'PARM))))
  (:= MSG 'RESULT RES)
  (sender-proc! Nil)
  RES))

(method! tproc 'post-to (=> (PROC MSG) ;; TODO: discard duplicated MSGs
  (^ 'in+ PROC MSG)
  (^ 'update-state PROC)
  (^ 'schedule PROC)))

(method! tproc 'core-call-RSM (=> (PROC MSG)
  (error "tproc::core-call-RSM : abstract")))

(method! tproc 'in-step (=> (PROC)
  (ifTrue (not (^ 'in-idle? PROC))
          (=> ()
            (define MSG Void)
            (cond ((== (: PROC 'STATE) 'Active)
                   (set! MSG (^ 'in++ PROC))
                   (if (: MSG 'ACK)
                     (noop)
                     (^ 'core-call-RSM PROC MSG)))
                  ((== (: PROC 'STATE) 'Waiting)
                   (if ((: PROC 'COND) PROC 1)
                   (begin
                     (:= PROC 'COND Nil)
                     (:= PROC 'STATE 'Active))))
                  (else
                   (error "in-step")))))))
    
(method! tproc 'update-state (=> (PROC)
  (cond ((== (: PROC 'STATE) 'Active)
         (if (^ 'idle? PROC)
           (:= PROC 'STATE 'Idle)))
        ((== (: PROC 'STATE) 'Idle)
         (if (not (^ 'idle? PROC))
           (:= PROC 'STATE 'Active))))))

(method! tproc 'step (=> (PROC)
  (ifTrue (or (^ 'out-step PROC)
              (^ 'in-step PROC))
          (=> ()
            (^ 'update-state PROC)
            (^ 'schedule PROC)))))

;; RSM
(method! tprocg 'post-to (=> (PROC MSG)
  (for-each (=> (PR)
              (net-send MSG (net-resolve PR)))
            (: PROC 'PEER))))
 ;(outraw "post-to[group] ")
 ;(outraw (: PROC 'UID))
 ;(cr)))

(define (method-descr TYPE FNAME)
  (define SLOTTY Void)
  (define DESCR Void)
  (if (unspecified? (method TYPE (sy FNAME)))
    (error "method-descr::no method " FNAME " in " (: TYPE 'NAME)))
  (set! SLOTTY (: TYPE 'SLOTTY))
  (: SLOTTY (sy FNAME)))

(define (proc-replay-list++ PROC MSG)
 ;(outraw "RL++ ")
 ;(outraw (: PROC 'UID))
 ;(outraw " ")
 ;(outraw (: MSG 'TO))
 ;(outraw " ")
 ;(outraw (: MSG 'FUNC))
 ;(cr)
  (let* ((INNB (: MSG 'INNB))
         (INNB2 (list-length (: PROC 'IN!)))
        )
    (if (and (specified? INNB) (!= INNB INNB2))
      (error "proc<" (: PROC 'UID) ">::RL++ : wrong INNB"))
    (:= MSG 'INNB INNB2)
    (set! MSG (copy-tree MSG)) ;; TODO: verify it's necessary
    (sign MSG (: PROC 'USER) 'SIGN_E)
    (:+ PROC 'IN! MSG)
    MSG))

(define (proc-send-acks PROC PROCG MSG)
  (define PEER (filter (=> (UID)
                         (!= UID (: PROC 'UID)))
                       (: PROCG 'PEER)))
 ;(outraw "Send ACKs ")
 ;(outraw (: PROC 'UID))
 ;(outraw " to ")
 ;(outraw PEER)
 ;(cr)
  (if (or (!= (: PROC 'UID) (: MSG 'FROM)) ;; CHECK: no need for an additional ACK from FROM when message already sent from FROM
          (not (: MSG 'RESULT))) ;; FIXME: hack to nullify failed replicated messages ; only works in simple cases
  (begin
    (set! MSG (copy-tree MSG))
    (:= MSG 'ACK True)
    (for-each (=> (PROC)
                (set! PROC (net-resolve PROC))
                (net-send MSG PROC))
              PEER))))

;; ATTENTION: __UNICITÉ DE LA REPLAY LIST__
;; Il faut au moins que:
;; (1) le proc group contienne, pour chaque proc, le user qui est habilité à parler en
;;     utilisant ce proc ;
;; (2) le message avec le INNB, au moment où il est inséré dans la replay list de celui
;;     qui l'a proposé, doit être signé _à nouveau_ par celui qui l'a proposé (pour signer
;;     le INNB) ; il faut probablement aussi la signature du master courant, pour s'assurer
;;     qu'on ne peut pas fabriquer de fausses replay list en se passant du master (quid de
;;     la collusion entre le master et un des players ? à priori, pour ca les deux n'ont pas
;;     besoin de forger le message avec INNB, il suffit que le master choisisse le message
;;     du player avec qui il collude) ;
;; => VÉRIFIER tout ca.
(define (msg-find Q FROM OUTNB INNB ACK RESULT USER)
  (list-find (=> (MSG2)
    (and (or (unspecified? FROM) (== FROM (: MSG2 'FROM)))
         (or (unspecified? OUTNB) (== OUTNB (: MSG2 'OUTNB)))
         (or (unspecified? INNB) (== INNB (: MSG2 'INNB)))
         (or (unspecified? ACK) (== ACK (: MSG2 'ACK)))
         (or (unspecified? RESULT) (== RESULT (: MSG2 'RESULT)))
         (or (unspecified? USER) (signed-by? MSG2 USER))))
    Q))

(define (proc-await-cond PROC MSG PEER)
  (=> (PROC . DOIT)
    (define IN (: PROC 'INPTR)) ;; TODO: verify that searching inside only the non-processed inputs actually always works
    (define RES True)
    (define ACKL (empty))
   ;(outraw "Checking condition on ")
   ;(outraw (: PROC 'UID))
    (for-each (=> (UID)
                (define PR (net-map UID)) ;; FIXME: how are we sure that PR.USER is not hackeable ???
                (define ACKM Void)
                (if (specified? (: MSG 'INNB))
                  (set! ACKM (msg-find IN
                                       (: MSG 'FROM)
                                       (: MSG 'OUTNB)
                                       (: MSG 'INNB)
                                       True
                                       Void
                                       (: PR 'USER))))
                (if (unspecified? ACKM)
                  (set! RES False)
                  (rcons ACKL ACKM)))
              PEER)
   ;(outraw " ")
   ;(outraw (if RES "OK" "FAIL"))
   ;(cr)
    (if (and RES (not (empty? DOIT)) (car DOIT))
      (let* ((MSG0 (list-get (: PROC 'IN!) (: MSG 'INNB))))
        (for-each (=> (MSG)
                    (sign:+ MSG0 MSG))
                  ACKL)
        (:= MSG0 'ACK* (signed-all? MSG0))))
    RES))

(define (proc-await-acks PROC PROCG MSG)
  (define PEER (filter (=> (UID)
                         (!= UID (: PROC 'UID)))
                       (: PROCG 'PEER)))
 ;(outraw "Await ACKs ")
 ;(outraw (: PROC 'UID))
 ;(outraw " from ")
 ;(outraw (: PROCG 'PEER))
 ;(cr)
  (:= PROC 'STATE 'Waiting)
  (:= PROC 'COND (proc-await-cond PROC MSG PEER)))

(define (proc-RSM-acks PROC MSG)
  (define PROCG (net-resolve (: MSG 'TO)))
  (if (procg? PROCG)
  (begin
    (proc-send-acks PROC PROCG MSG)
    (if (: MSG 'RESULT) ;; FIXME: hack to make things simpler ; but in fact, in case of FAIL too, there should be full ACK handshake
      (proc-await-acks PROC PROCG MSG)))))

(method! tprocl 'core-call-RSM (=> (PROC MSG) ;; TODO: when MSG.TO is a group, verify that PROC belongs to it
  (define DESCR (method-descr (typeof (: PROC 'SELF)) (: MSG 'FUNC)))
  (define MSGF (msg-find (: PROC 'IN) (: MSG 'FROM) (: MSG 'OUTNB) Void True False Void))
  (if (specified? MSGF) ;; FIXME: hack to make failed RSM calls simpler
    (:= MSG 'RESULT False)
    (begin
      (^ 'core-call PROC MSG)
      (if (not (list-in? 'volatile DESCR))
      (begin
        (if (: MSG 'RESULT)
          (set! MSG (proc-replay-list++ PROC MSG)))
        (proc-RSM-acks PROC MSG)))))))

;; Stepping (hosts)
(method! tproch 'step (=> (PROC)
  (define (logit PROC)
    (if (trace-steps)
    (begin
      (outraw "!Stepping ")
      (outraw (: PROC 'UID))
      (cr))))
  (define RES False)
  (logit PROC)
  (hash-for-each (=> (UID PR) ;; TODO: replace that by hash-for-each-in-order
    (logit PR)
    (if (^ 'step PR)
      (set! RES True)))
    (allprocsh PROC))
  RES))

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

(method! tproc 'start (=> (PROC)
  Void))

(method! tproc 'stop (=> (PROC . B)
  Void))

(define (step . PROC)
  (if (and (not (empty? PROC))
           (proc? (car PROC)))
    (^ 'step (car PROC))
    (while (^ 'step (host-proc)))))

;; Schedulers/simulated support procs
(set! tsched (type "sched"     ;; Scheduler
                   '(IDLE      ;; Idle procs
                     ACTIVE    ;; Active procs
                     WAITING   ;; Waiting procs (on some condition)
                     ALLPROCS  ;; All procs (hashtable)
                    )))

(define (sched)
  (define RES (rexpr tsched '()))
  (:= RES 'IDLE (queue))
  (:= RES 'ACTIVE (queue))
  (:= RES 'WAITING (queue))
  (:= RES 'ALLPROCS (make-hashv-table))
  RES)

(define (sched-queue SCH STATE)
  (cond
   ((== STATE 'Idle)
    (: SCH 'IDLE))
   ((== STATE 'Active)
    (: SCH 'ACTIVE))
   ((== STATE 'Waiting)
    (: SCH 'WAITING))
   (else
    (error "sched-queue"))))

(define (sched-proc SCH PROC)
  (define IN Void)
  (if (not (sched? SCH))
    (error "sched-proc"))
  (set! IN (sched-queue SCH (: PROC 'STATE))) ;; FIXME: recurring wart, one cannot put test code before (define ...)
  (queue-remove (: SCH 'IDLE) PROC)
  (queue-remove (: SCH 'ACTIVE) PROC)
  (queue-remove (: SCH 'WAITING) PROC)
  (queue-push IN PROC))

(method! tproc 'schedule (=> (PROC)
  (sched-proc (: (: PROC 'HOST) 'SCHED) PROC)))

(define (sched-idle? SCH)
  (queue-empty? (: SCH 'ACTIVE)))

(define (sched-step SCH) ;; NOTE: unused
  (define PR Void)
  (if (not (sched-idle? SCH))
  (begin
    (set! PR (queue-shift (: SCH 'ACTIVE)))
    (if (!= (: PR 'STATE) 'Active)
      (error "sched-step"))
    (outraw "Stepping ")
    (outraw (: PR 'UID))
    (cr)
    (^ 'step PR)
    (sched-proc SCH PR))))

;; Physical host proc
(define _HOSTPROC Nil)
(define (host-proc)
  _HOSTPROC)

(define (host-proc! PROC)
  (if (not (or (nil? PROC) (procph? PROC)))
    (error "host-proc! : not a physical host proc"))
  (set! _HOSTPROC PROC))

(host-proc! (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                    'UID 'phys))
