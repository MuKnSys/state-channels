; calls.ss
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

;; Calls
(define tcall (type "call" 
                   '(USER   ;; UID user
                     FROM   ;; UID sender (process)
                     OUTNB  ;; Nth call sent by FROM (stored in OUT) ; only _one_ call per OUTNB
                     TO     ;; UID receiver (process)
                     INNB   ;; Nth (side-effecting) call received by TO
                     FUNC   ;; The function's name
                     PARM   ;; Parameters
                     RESULT ;; Result
                     PATCH  ;; Side effects
                     ACK    ;; Ack sending a signature
                     ACK*   ;; All procs ack-ed the call
                     REDIR  ;; Redirected message ;; no use for this, at the moment (0)
                     SIGN_B ;; Signature at begin (creation)
                     SIGN_E ;; Signature at end (end of local processing ; reemitting)
                    )
                    (empty)
              ))

(define (call . PARM)
  (define RES (rexpr tcall (list-group PARM))) ;; NOTE : perhaps call are values
  (:= RES 'ACK False)
  (:= RES 'ACK* False)
  (:= RES 'REDIR False)
  RES)

;; Encryption & identities
(define (sign O UID VAR)
  (if (not (and (list? (: O VAR))
                (list-in? UID (: O VAR)))) ;; TODO: check this
    (:+ O VAR UID '(sign))))

(define (sign:+ . L) ;; TODO: clean all that
  (define RES (car L))
  (define SIGN (list-flatten (map (=> (O)
                                      (cdr (: O 'SIGN_E)))
                                  L)))
  (:= RES 'SIGN_E `(sign . ,(list-rmdup SIGN)))
  RES)

(define (signed-by? CALL USER)
  (define SIGN (: CALL 'SIGN_E))
  (if (not (pair? SIGN))
    False
    (list-in? USER (cdr SIGN))))

(define (signed-all? CALL)
  (define SIGN (: CALL 'SIGN_E))
  (define TO (: CALL 'TO))
  (define RES True)
  (define PROCG (net-resolve TO))
  (set! TO (if (procg? PROCG)
             (: PROCG 'PEER)
             (: PROCG 'UID)))
  (if (not (pair? SIGN))
    False
    (begin
      (set! SIGN (cdr SIGN))
      (if (not (pair? TO))
        (set! TO `(,TO)))
      (set! TO (map (=> (UID)
                      (define PR (net-resolve UID))
                      (if (not PR)
                        (error "signed-all? : unresolveable target => " UID))
                      (: PR 'USER))
                    TO))
      (for-each (=> (USER)
                  (if (not (list-in? USER SIGN))
                    (set! RES False)))
                TO)
      RES)))

;; (define)s for procs, due to the impossibility of creating cyclic modules
(define tproc Void)
(define (proc? PROC)
  (inherits? (typeof PROC) tproc))

(define tprocl Void)
(define (procl? PROC)
  (inherits? (typeof PROC) tprocl))

(define tproch Void)
(define (proch? PROC)
  (inherits? (typeof PROC) tproch))

(define tprocph Void)
(define (procph? PROC)
  (inherits? (typeof PROC) tprocph))

(define tsched Void) ;; FIXME(?): should scheds be here ?
(define (sched? SCHED)
  (== (typeof SCHED) tsched))

;; (define)s for proc groups
(define tprocg Void)
(define (procg? PROC)
  (inherits? (typeof PROC) tprocg))

(define tstatech Void)
(define (statech? PROC)
  (== (typeof PROC) tstatech))

;; (current-proch) is here, due to the impossibility of defining cyclic modules
(define _CURPROCH Nil)
(define (current-proch)
  _CURPROCH)

;; Net
;; => (net-enter PROC) works only when PROC is a local process ; and then, it broadcasts
;;    its UID, plus the IP:PORT of the current host serversocket to the other net peers,
;;    so that in the peers, PROC.UID => IP:PORT/UID [physical address of PROC] ;
;;    =>
;;      Basically, the net keeps the relation <proc UID> <=> <physical address>, where
;;      <physical address> can be:
;;                                => in case of a proclet, the proc UID of its host ;
;;                                => in case of an host, its <IP,PORT> serversocket address,
;;                                                  [ or the <IP,PORT> of its physical host ;
;;
;;      Inside some of the middleware's peers, the tables containing these relations can become
;;      outdated, especially when some processes have been migrated. How to detect these kinds
;;      of errors must be managed, especially to re-route the messages that have not been delivered.
;;
;; => (net-leave PROC) : idem, in reverse ;
;;
;; => (net-resolve UID) : depends if we want to allow it or not ; or either, sends a (proc-connect)
;;    message to the middleware, the answer being a mapping, i.e. a serialized version of the remote
;;    representation of the remote process we want to connect to ; in that case, probably the processes
;;    which depend on pending connects are put in a queue, where they wait ;
;;
;; => (net-send MSG) : MSG.FROM is a local process ; MSG is broadcasted to the net support
;;    process which hosts MSG.TO ; the serversocket handler of this net support process adds
;;    MSG to MSG.TO's incoming queue ; (net-send) marshalls the message from MSG.TO's remote
;;    image to MSG.TO's local instance (i.e., the physical one) ;
;;
;; => (proc-send) is only posting the messages to the outcoming list of the local proclet mappings, and
;;    then to the remote TO core proclets ; and then one can sync these (remote) core TO proclets to update
;;    the incoming lists of the local mappings of the remote processes we sent a message to (or perhaps
;;    there is a callback). When one posts on a state channel, there is a turntaking allocation phase,
;;    which works by means of having the message being sent first to the master, which posts the allocated
;;    message to all peers. Then the peer who sent the allocated message turns it to non-pending anymore,
;;    while the others keep it pending ; it's only when the message is sent by the master, that it is
;;    inserted in the incoming queue of a state channel process. Additionnally, when a state channel
;;    proc(let) has a pending message in its outcoming list, it cannot propose another one ; masters
;;    which have a pending message chose this one as the allocated message ; each time a message is
;;    allocated, the state channel procs know that the master is the next one proc in the state channel ;
;;
;; => the scheduler works by means of saturating the messages in the local images, and sending them 
;;    to the appropriate remote processes' incoming queues, then it fetches the incoming messages
;;    from outside and queues them in the appropriate local processes' incoming queues, then the
;;    local processes are finally saturated, and the physical method calls finally performed ;
;;
(define _NET (make-hashv-table))
(define (current-network)
  _NET)

(define (net-enter PROC)
  (define UID (: PROC 'UID))
  (if (not (^ 'core? PROC))
    (error "net-enter"))
  (if (string? UID)
  (begin
   ;(outraw "nenter=> ") ;; TODO: turn that to debug logs
   ;(out (: PROC 'ID))
   ;(outraw " as ")
   ;(outraw UID)
    (hash-set! (current-network) UID PROC))))

(define (net-leave PROC)
  (define UID (: PROC 'UID))
  (if (string? UID)
    (hash-remove! (current-network) UID)))

(define (net-resolve-group NAME) ;; Groups are always local
  (define RES Void)              ;; TODO: clean this & find a way to move that to procs.ss
  (if (proch? (current-proch))
    (hash-for-each (=> (ID PR)
                     (if (== (: PR 'UID) NAME)
                       (set! RES PR)))
       (: (: (current-proch) 'SCHED) 'ALLPROCS)))
  (if (not (procg? RES))
    (set! RES False))
  RES)

(define (net-resolve NAME)
  (define RES Void)
  (if (proc? NAME)
    (set! NAME (: NAME 'UID)))
  (if (or (not NAME) (unspecified? NAME))
    (error "net-resolve"))
  (set! RES (hash-ref (current-network) NAME))
  (if (not RES)
    (set! RES (net-resolve-group NAME)))
  RES)

(define (net-map NAME) ;; TODO: implement (connect) & downloading the mapping of a proc
  (net-resolve NAME))

(define (net-send MSG . PROC) ;; NOTE: PROC is here to be able to send an MSG to a proc which is not MSG.TO
  (if (empty? PROC)
    (set! PROC (net-resolve (: MSG 'TO)))
    (set! PROC (car PROC)))
  (if (not (proc? PROC))
    (error "net-send " PROC))
 ;(outraw "net-send=> ")
 ;(>> MSG)
 ;(outraw " ")
 ;(outraw (: PROC 'UID))
 ;(cr)
  (^ 'post-to PROC (copy-tree MSG))) ;; TODO: send that to the PROC's physical host

(define (net-next)
  Nil)
