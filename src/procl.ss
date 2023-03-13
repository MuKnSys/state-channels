; procl.ss
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
(import ./procs)
(import ./ipc)
(import ./calls)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./ipc)
  (import: ./calls))

;; Proclet
(set! tprocl (type `("procl" ,tproc)  ;; Proclet
                   '(SELF             ;; Server's own servlet (1st object in the heap)
                    )))

;; Constructor
(define (procl . PARM)
  (define RES (apply proc `(,tprocl . ,PARM)))
  (^ 'host-init RES)
  RES)

;; Call
(define (fname-isret? F)
  (set! F (string F))
  (let* ((LF (string-length F))
         (LP (string-length "/return")))
    (and (> LF LP)
         (== (substring F (- LF LP) LF) "/return"))))

(define (call-switchin SELF NAME)
  (define F (method (typeof SELF) NAME))
  (if (specified? F)
    (^ NAME SELF)))

(method! tproc 'call (=> (PROC FNAME . PARM)
  (let* ((SELF (: PROC 'SELF)) ;; TODO: add what to do if HANDLER is set (is HANDLER necessary ?)
         (RES Void)
        )
    Void ;; TODO: context-switch to SELF.HEAP
    (if (unspecified? SELF)
      (error "proc<" (: PROC 'UID) ">::call : no SELF"))
    (call-switchin SELF '_enter)
    (call-switchin SELF '_switchin)
    (set! RES
      (if (fname-isret? FNAME)
        (let* ((FROM (: (: PROC 'GROUP) 'PARENT))
               (CALL (car PARM))) ;; FIXME: should ensure the same return is not evaluated 2 times
          (if (unspecified? FROM)
            (error "proc.call : return without FROM " (: PROC 'UID)))
          (if (string? CALL)
            (set! CALL (string->number CALL)))
          (let* ((CALL0 CALL)
                 (PR Void))
            (set! CALL (list-get (: FROM 'IN!) CALL))
            (if (unspecified? CALL)
              (error "tproc::call::isret " (: FROM 'UID) ".rl[" CALL0 "] : no such call"))
            (set! CALL (rexpr-copy CALL))
            (set! PR (net-resolve (: CALL 'TO)))
            (if (procg? PR)
              (set! PR (car (: PR 'PEER))))
            (if (not (proc? PR))
              (set! PR (net-resolve PR)))
            (if (not (proc? PR))
              (error "proc.call : no receiver process"))
            (:= CALL 'PARM (cdr (mvparms (: CALL 'FUNC)
                                         (cons (: PR 'SELF)
                                               (: CALL 'PARM)))))
            (apply ^? `(,FNAME ,SELF . (,CALL)))))
        (apply ^? `(,FNAME ,SELF . ,PARM))))
    (call-switchin SELF '_switchout)
    (call-switchin SELF '_leave)
    RES)))

(method! tproc 'sync (=> (PROC) ;; FIXME: in case of replicated call, all the group's processes should be able to sync
  (define RETS (map (=> (CALL)
                      (number (car (: CALL 'PARM))))
                    (filter (=> (CALL)
                              (and (fname-isret? (: CALL 'FUNC))
                                   (not (: CALL 'ACK))))
                            (: PROC 'IN))))
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

;; Proc send
(define (proc-send0 PROC FNAME . PARM) ;; NOTE: PROC is the _target_ (i.e., the TO)
  (let* ((FROM (current-proc))
         (CALL Void))
    (if (nil? FROM)
      (error "proc-send0 : no current proc"))
    (set! CALL (call 'USER (: FROM 'USER)
                     'FROM (: FROM 'UID)
                     'OUTNB (^ 'outnb FROM)
                     'TO (: PROC 'UID)
                     'FUNC FNAME
                     'PARM PARM))
    (sign CALL (: FROM 'USER) 'SIGN_B) ;; TODO: verify that it's necessary (cf. (sign) step in (out-step))
    (^ 'out+ FROM CALL)))

(method! tproc 'send (=> (PROC FNAME . PARM) ;; NOTE: PROC is the _target_ (i.e., the TO)
  (let* ((FROM (current-proc))
         (STATE Void))
    (apply proc-send0 `(,PROC ,FNAME . ,PARM))
    (set! STATE (: FROM 'STATE))
    (if (not (or (== STATE 'Idle) (== STATE 'Active)))
      (error "proc::send=> " STATE))
    (:= FROM 'STATE 'Active)
    (^ 'schedule FROM))))

;; Stepping
(define _TRACESTEPS False)
(define (trace-steps . B)
  (if (empty? B)
    _TRACESTEPS
    (set! _TRACESTEPS (car B))))

(method! tproc 'out-step (=> (PROC)
  (ifTrue (and (not (^ 'out-idle? PROC))
               True) ;(== (: PROC 'STATE) 'Active))
          (=> ()
            (define MSG (^ 'out++ PROC))
            (sign MSG (: PROC 'USER) 'SIGN_E) ;; TODO: verify that it's necessary
            (net-send MSG)))))

(method! tproc 'core-call (=> (PROC MSG)
  (define OPROC (current-proc))
  (define SPROC Void)
  (define RES Void)
  (if (not (^ 'core? PROC))
    (error "in-step"))
  (set! SPROC (net-resolve (: MSG 'FROM)))
  (if (not SPROC)
    (error "proc<" (: MSG 'FROM) ">::core-call : no sender proc"))
  (current-proc! PROC)
  (sender-proc! SPROC)
  (current-call! MSG)
  (set! RES (apply ^ `(call ,PROC ,(: MSG 'FUNC) . ,(: MSG 'PARM))))
  (:= MSG 'RESULT RES)
  (current-proc! OPROC)
  (sender-proc! Nil)
  (current-call! Nil)
  RES))

(method! tproc 'post-to (=> (PROC MSG) ;; TODO: discard duplicated MSGs
  (define MSGR (msg-find (: PROC 'IN) (: MSG 'FROM) (: MSG 'OUTNB) Void Void Void Void))
 ;(outraw "post-to\n")
 ;(errlog MSG)
  (if (or (unspecified? MSGR) (: MSG 'ACK))
  (begin
   ;(outraw "posted\n")
    (set! MSG (rexpr-copy MSG))
    (^ 'in+ PROC MSG)
    (^ 'update-state PROC)
    (^ 'schedule PROC)))))

(method! tproc 'core-call-RSM (=> (PROC MSG)
  (error "tproc::core-call-RSM : abstract")))

(method! tproc 'in-step (=> (PROC)
  (define RES (not (^ 'in-idle? PROC)))
  (define MSG Void)
  (if RES
    (cond ((== (: PROC 'STATE) 'Active)
           (set! MSG (^ 'in++ PROC))
           (if (: MSG 'ACK)
             (noop)
             (^ 'core-call-RSM PROC MSG)))
          ((== (: PROC 'STATE) 'Waiting)
           (if ((: PROC 'COND) PROC 1)
             (begin
               (:= PROC 'COND Nil)
               (:= PROC 'STATE 'Active))
             (set! RES False)))
          (else
           (error "in-step"))))
  RES))
    
(method! tproc 'update-state (=> (PROC)
  (cond ((== (: PROC 'STATE) 'Active)
         (if (^ 'idle? PROC)
           (:= PROC 'STATE 'Idle)))
        ((== (: PROC 'STATE) 'Idle)
         (if (not (^ 'idle? PROC))
           (:= PROC 'STATE 'Active))))))

(method! tproc 'step (=> (PROC)
  (define RES
          (ifTrue (or (^ 'out-step PROC)
                      (^ 'in-step PROC))
                  (=> ()
                    (^ 'update-state PROC)
                    (^ 'schedule PROC))))
  (if RES
  (begin
   ;(outraw (string+ "Proc " (: PROC 'UID) " fired[" (string RES) "]\n"))
   ;(errlog PROC)
    (noop)
  ))
  RES))

;; RSM
(define (method-descr TYPE FNAME) ;; TODO: move that to rexpr.ss
  (define SLOTTY Void)
  (define DESCR Void)
  (if (unspecified? (method TYPE (sy FNAME)))
    (error "method-descr::no method " FNAME " in " (: TYPE 'NAME)))
  (set! SLOTTY (: TYPE 'SLOTTY))
  (: SLOTTY (sy FNAME)))

(define (proc-master? PROC)
  (define GROUP (: PROC 'GROUP))
  (define PARENT Void)
  (define RES False)
  (if (proc? GROUP)
    (set! PARENT (: GROUP 'PARENT)))
  (if (proc? PARENT)
    (set! RES (== (: PARENT 'UID) (: PROC 'UID))))
  RES)

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
      (if (proc-master? PROC)
        (noop)
        (error "proc<" (: PROC 'UID) ">::RL++ : wrong INNB")))
    (:= MSG 'INNB INNB2)
    (set! MSG (rexpr-copy MSG)) ;; TODO: verify it's necessary
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
  (if True
     ;(or (!= (: PROC 'UID) (: MSG 'FROM)) ;; CHECK: no need for an additional ACK from FROM when message already sent from FROM
     ;    (not (: MSG 'RESULT))) ;; FIXME: hack to nullify failed replicated messages ; only works in simple cases
     ; NOTE: the above (or) does nothing useful ; it removes some ACK messages in in-process mode, and makes the consensus fail in inter-process (due to the lack of these ACKs)
     ; TODO: once things will have been a bit more surveyed, remove it.
  (begin
   ;(outraw "Send ACKs (the real thing)")
   ;(cr)
    (set! MSG (rexpr-copy MSG))
    (:= MSG 'ACK True)
    (for-each (=> (PROC)
                (set! PROC (net-resolve PROC))
               ;(outraw "Send ACKs (the real thing)(2)")
                (_incmsgno MSG)
                (if (not (^ 'core? PROC))
                  (begin
                    (:= MSG '_TO (: PROC 'UID))
                    (net-send MSG PROC)
                    (<- MSG '_TO))
                  (net-send MSG PROC)))
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
  (define (log MSG2 A B)
    (out A)(outraw " ")(out (: MSG2 B))(cr))

  (if (boxed-empty? Q)
    Void
    (list-find (=> (MSG2)
      (define RES Void)
      (if (not (call? MSG2))
        (error "msg-find"))
     ;(log MSG2 FROM 'FROM)
     ;(log MSG2 OUTNB 'OUTNB)
     ;(log MSG2 INNB 'INNB)
     ;(log MSG2 ACK 'ACK)
     ;(log MSG2 RESULT 'RESULT)
     ;(log MSG2 USER 'USER)
      (set! RES
            (and (or (unspecified? FROM) (== FROM (: MSG2 'FROM)))
                 (or (unspecified? OUTNB) (== OUTNB (: MSG2 'OUTNB)))
                 (or (unspecified? INNB) (== INNB (: MSG2 'INNB)))
                 (or (unspecified? ACK) (== ACK (: MSG2 'ACK)))
                 (or (unspecified? RESULT) (== RESULT (: MSG2 'RESULT)))
                 (or (unspecified? USER) (signed-by? MSG2 USER))))
     ;(outraw "=>")(out RES)(cr)
      RES)
      Q)))

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
      (let* ((MSG0 (list-get (: PROC 'IN!) (: MSG 'INNB)))
             (DESCR (method-descr (typeof (: PROC 'SELF)) (: MSG0 'FUNC))))
        (for-each (=> (MSG)
                    (sign:+ MSG0 MSG))
                  ACKL)
        (:= MSG0 'ACK* (signed-all? MSG0))
        (if (and (: MSG0 'ACK*) (list-in? 'committed DESCR))
          (^ 'post-to-master (: PROC 'GROUP) MSG0))))
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
        (if (not (proc-master? PROC))
          (proc-RSM-acks PROC MSG))))))))
