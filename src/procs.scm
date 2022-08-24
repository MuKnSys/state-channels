; procs.scm
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (July 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

;; Procs
(define tproc (type "proc" 
                   '(IP PORT  ;; Address : IP/Filesocket [Remote];; Serversocket/Mem [Local]
                     ID       ;; PID
                     UID      ;; Proc's name (unique identity in the network)
                     USER     ;; User's identity
                     FROM     ;; Upstream server
                     PEER     ;; Peers
                     HEAP     ;; Persisteable heap
                     IN IN!   ;; Incoming messages
                     INPTR    ;; Cons of the current message
                     OUT      ;; Outcoming (asynchronous) messages
                     OUTPTR   ;; Cons of the last message
                     HANDLER  ;; Proc handler => usually calls SELF's methods
                     SELF     ;; Server's own servlet (1st object in the heap)
                     STOPPED  ;; Stopped
                     VERBOSE  ;; Verbosity level
                    )
                    (empty)
              ))

;; Procs (kinds of proc)
(method! tproc 'local? (=> (PROC)
  (unspecified? (<: PROC 'IP))))

(method! tproc 'remote? (=> (PROC)
  (not (^ 'local? PROC))))

(method! tproc 'inmem? (=> (PROC)
  (and (^ 'local? PROC)
       (unspecified? (<: PROC 'PORT)))))

(method! tproc 'ssock? (=> (PROC)
  (and (^ 'local? PROC)
       (specified? (<: PROC 'PORT)))))

;; Constructor
(define _PROC (make-hash-table))
(define _PROCNO 0)
(define (allprocs)
  _PROC)

(define (proc . PARM)
  (let* ((L (list-group PARM))
         (TAG (<- L 'TAG)))
    (define RES (rexpr tproc L))
    (:= RES 'ID _PROCNO)
    (hash-set! (allprocs) _PROCNO RES)
    (set! _PROCNO (+ _PROCNO 1))
    (:? RES 'PEER (empty))
    (:? RES 'IN (empty))
    (:? RES 'INPTR (<: RES 'IN))
    (:? RES 'IN! (empty))
    (:? RES 'OUT (empty))
    (:? RES 'OUTPTR Nil)
    (:? RES 'STOPPED False)
    (:? RES 'VERBOSE 'Short)
    RES))

(define (proc? PROC)
  (== (typeof PROC) tproc))

;; Group
(define (proc-group . L)
  (if (not (empty? L))
  (let* ((FROM (car L))
         (L2 _))
    (set! L (cdr L))
    (set! L2 (map (=> (PR)
                    (if (proc? PR)
                      (<: PR 'UID)
                      PR))
                  L))
    (for-each (=> (PR)
                (:= PR 'FROM FROM)
                (:= PR 'PEER (list-copy L2))
              )
              L))))

;; Current proc
(define _CURPROC Nil)
(define (current-proc)
  _CURPROC)

(define (current-proc! PROC)
  (if (not (or (nil? PROC) (proc? PROC)))
    (error "current-proc! : not a proc"))
  (set! _CURPROC PROC))

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
  (cond
    ((^ 'remote? PROC)
     _ ;; TODO: send (cons FNAME PARM) thru the socket
    )
    (else
      (let* ((SELF (<: PROC 'SELF)) ;; TODO: add what to do if HANDLER is set (is HANDLER necessary ?)
            )
        _ ;; TODO: context-switch to SELF.HEAP
        _ ;; TODO: log the call in SELF.LOG
        (if (unspecified? SELF)
          (error "proc<" (<: PROC 'UID) ">::call : no SELF"))
        (if (fname-isret? FNAME)
          (let* ((FROM (<: PROC 'FROM))
                 (CALL (car PARM))) ;; FIXME: should ensure the same return is not evaluated 2 times
            (if (unspecified? FROM)
              (error "proc.call : return without FROM"))
            (if (string? CALL)
              (set! CALL (string->number CALL)))
            (set! CALL (copy-tree ([ (<: FROM 'IN!) CALL)))
            (:= CALL 'PARM (cdr (mvparms (<: CALL 'FUNC)
                                         (cons (<: (net-resolve (<: CALL 'TO)) 'SELF)
                                               (<: CALL 'PARM)))))
            (apply ^? `(,FNAME ,SELF . (,CALL))))
          (apply ^? `(,FNAME ,SELF . ,PARM))))))))

(method! tproc 'sync (=> (PROC)
  (define RETS (map (=> (CALL)
                      (number (car (<: CALL 'PARM))))
                    (filter (=> (CALL)
                              (and (fname-isret? (<: CALL 'FUNC))
                                   (not (<: CALL 'ACK))))
                            (<: PROC 'IN))))
  (define RL0 _)
  (define MASTER (<: PROC 'FROM))
  (if (and (specified? MASTER) (not (boxed-empty? (<: MASTER 'IN!))))
  (begin
    (set! RL0 (filter (=> (CALL)
                        (not (list-in? (cadr CALL) RETS)))
                      (map (=> (CALL)
                             `(,(<: CALL 'FUNC) ,(<: CALL 'INNB)))
                           (<: MASTER 'IN!))))
    (for-each (=> (CALL)
                (^ 'send* PROC (sy (string+ (string (car CALL)) "/return")) (cadr CALL)))
              RL0)))))

;; Map
(method! tproc 'prog! (=> (PROC O) ;; Set the proc's servlet
  (:= PROC 'SELF O)))

(method! tproc 'map (=> (PROC ADDR) ;; Map the proc's heap ;; ADDR is a distributed resource UID
  _))

;; Send ;; TODO: only for (inmem?)s at the moment
(method! tproc 'outnb (=> (PROC)
  (define PTR (<: PROC 'OUTPTR))
  (if (nil? PTR)
    0
    (+ (<: (car PTR) 'OUTNB) 1))))

(method! tproc 'out+ (=> (PROC CALL)
  (:+ PROC 'OUT CALL)
  (:= PROC 'OUTPTR (list-last (<: PROC 'OUT)))))

(method! tproc 'in+ (=> (PROC CALL)
  (:+ PROC 'IN CALL)
  (if (nil? (<: PROC 'INPTR))
    (:= PROC 'INPTR (list-last (<: PROC 'IN))))))

(method! tproc 'in++ (=> (PROC)
  (define RES Nil)
  (if (not (nil? (<: PROC 'INPTR)))
  (begin
    (set! RES (car (<: PROC 'INPTR)))
    (:= PROC 'INPTR (cdr (<: PROC 'INPTR)))))
  RES))

(define (proc-resolve L)
  (if (strsy? L)
    (set! L `(,L)))
  (map (=> (P)
           (define PR (net-resolve P))
           (if (not PR)
             (error "proc-resolve : unresolveable target => " P))
           PR)
       L))

(define (proc-queue CALL)
  (define FROM (<: (current-proc) 'UID))
  (define TO (<: CALL 'TO))
  (define L _)
  (define MASTER (<: (current-proc) 'FROM))
  (if (or (unspecified? FROM) (empty? FROM) (boxed-empty? FROM))
    (error "proc-queue : no FROM => " FROM))
  (if (not (or (unspecified? TO) (empty? TO) (boxed-empty? TO)))
  (begin
    (if (and (unspecified? (<: CALL 'INNB)) (or (unspecified? MASTER) (!= TO (<: MASTER 'UID))))
      (set! L `(,FROM))
      (set! L TO))
    (set! L (proc-resolve L))
    (if (specified? (<: CALL 'INNB))
      (set! L (filter (=> (PR)
                          (!= (<: PR 'UID) FROM))
                      L)))
    (for-each (=> (P)
      (^ 'in+ P (copy-tree CALL))) ;; FIXME: hmm (tree-copy) risks breaking, at some point (the copy belongs to another heap, in fact)
      L))))

(define (proc-send PROC MULTI FNAME . PARM) ;; NOTE: PROC is the _target_ (i.e., the TO)
  (define PR (current-proc))
  (define L '())
  (define TO Nil)
  (define CALL Nil)
  (if (nil? PR)
    (error "Send : no current proc"))
  (if MULTI
    (set! L (map (=> (P)
                     (define PR (net-resolve P))
                     (if (not PR)
                       (error "proc-send::MULTI : unresolveable target => " P))
                     PR)
                 (<: PR 'PEER)))
    (if (not (net-resolve PROC))
      (error "proc-send : unresolveable target => " (<: PROC 'UID))))
  (set! TO
        (if MULTI (map (=> (P) (<: P 'UID))
                       L)
                  (<: PROC 'UID)))
  (set! CALL (call 'USER (<: PR 'USER)
                   'FROM (<: PR 'UID)
                   'OUTNB (^ 'outnb PR)
                   'TO TO
                   'FUNC FNAME
                   'PARM PARM))
  (sign CALL (<: PR 'USER) 'SIGN_B)
  (^ 'out+ PR CALL)
  (proc-queue CALL))

(method! tproc 'send (=> (PROC FNAME . PARM)
  (apply proc-send `(,PROC ,False ,FNAME . ,PARM))))

(method! tproc 'send* (=> (PROC FNAME . PARM)
  (apply proc-send `(,PROC ,True ,FNAME . ,PARM))))

(method! tproc 'step (=> (PROC)
  (define OCURPROC (current-proc)) ;; Not absolutely necessary ; (current-proc) should be Nil, in fact
  (define MSG (^ 'in++ PROC))
  (define RES _)
  (define PEER (<: PROC 'PEER))
  (define ISPEER _)
  (define ISVOLATILE False)
  (set! ISPEER (not (or (empty? PEER) (boxed-empty? PEER))))
  (if (not (nil? MSG))
  (begin
    (current-proc! PROC)
    (let* ((FUNC (<: MSG 'FUNC))
           (SELF (<: PROC 'SELF))
           (SLOTTY _)
           (DESCR _)
           (SPROC _)
          )
      (if (unspecified? SELF)
        (error "proc<" (<: PROC 'UID) ">::step : no SELF"))
      (if (unspecified? (method (typeof SELF) (sy FUNC)))
        (error "proc<" (<: PROC 'UID) ">::step : no method " FUNC))
      (set! SLOTTY (<: (typeof SELF) ':SLOTTY))
      (set! DESCR (<: SLOTTY FUNC))
      (if (and (specified? DESCR) (not (empty? DESCR)))
        (set! ISVOLATILE (eq? (car DESCR) 'volatile))) ;; FIXME: make something more general than this !!!
      (cond
        ((<: MSG 'ACK)
         (if (not (specified? (<: MSG 'INNB)))
           (error "proc<" (<: PROC 'UID) ">::step : ACKed MSG without INNB"))
         (let* ((MSG0 ([ (<: PROC 'IN!) (<: MSG 'INNB)))
               )
           (sign:+ MSG0 MSG)
           (:= MSG0 'ACK* (signed-all? MSG0)))
        )
        (else
         (set! SPROC (net-resolve (<: MSG 'FROM)))
         (if (not SPROC)
           (error "proc<" (<: MSG 'FROM) ">::step : no sender proc"))
         (sender-proc! SPROC)
         (set! RES (apply ^ `(call ,PROC ,FUNC . ,(<: MSG 'PARM))))
         (:= MSG 'RESULT RES)
         (sender-proc! Nil)
         (if (and RES (not ISVOLATILE)) ;; TODO: when (not RES), rollback changes
           (let* ((INNB (<: MSG 'INNB))
                  (INNB2 (list-length (<: PROC 'IN!)))
                 )
             (if (and (specified? INNB) (!= INNB INNB2))
               (error "proc<" (<: PROC 'UID) ">::step : wrong INNB"))
             (:= MSG 'INNB INNB2)
             (set! MSG (copy-tree MSG)) ;; NOTE : keep this ?
             (sign MSG (<: PROC 'USER) 'SIGN_E)
             (if (specified? INNB)
               (:= MSG 'ACK True))
             (:+ PROC 'IN! MSG)
             (proc-queue MSG)
             (:= MSG 'ACK False)))
      )))
    (current-proc! OCURPROC)))
  RES))

;; Start
(method! tproc 'start (=> (PROC)
  (cond
    ((^ 'remote? PROC)
     (noop)) ;; No start ;; connect is to be used instead (?)
    ((^ 'inmem? PROC)
     (noop))
    ((^ 'ssock? PROC)
     _ ;; TODO: start the server socket
    ))))
