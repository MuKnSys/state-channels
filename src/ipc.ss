; ipc.ss
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
(import ./procs)

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

;; Addresses
(define (network-addr IP HOST . UID)
  (define RES (string+ IP ":" HOST))
  (if (not (empty? UID))
    (string+ RES ":" UID)
    RES))

(define (addr-proc ADDR)
  (define L (string-split ADDR #\:))
  (if (> (list-length L) 2)
    (caddr L)
    Void))

;; Net logs
(define _NET_LOG False)
(define (net-log . B) ;; FIXME: clean & organize the net logging system, with levels
  (if (empty? B)
    _NET_LOG
    (set! _NET_LOG (list-in? (car B) '(#t 1 "1")))))

;; Hosts (physical hosts)
(define (proc-hostph PROC)
  (define HOST PROC)
  (define FINI False)
  (if (not (proc? PROC))
    (error "proc-hostph"))
  (while (not FINI)
    (if (or (procph? HOST) (not (proc? HOST)))
      (set! FINI True)
      (set! HOST (: HOST 'HOST))))
  HOST)

(define (host-fsock HOSTID)
  (path-normalize (string+ (host-phys-socks) "/" (addr-subm (current-machine)) "/" HOSTID)))

(define (host-fsock2 ADDR) ;; TODO: to be reunited with the previous one
  (define SUBM (addr-subm ADDR))
  (define HOST (addr-host ADDR))
  (if (unspecified? HOST)
    (error "host-fsock2::no host"))
  (if (!= (addr-netm ADDR) (addr-netm (current-machine)))
    (error "host-fsock2 " ADDR " " (current-machine)))
  (path-normalize (string+ (host-phys-socks) "/" SUBM "/" HOST)))

(define (host-phys-send HOSTA MSG) ;; TODO: also be able to send to hosts on another machine (local at the moment)
  (define SOCKA Void)
  (define SOCK Void)
  (if (not (string? HOSTA))
    (error "host-phys-send"))
  (set! SOCKA (host-fsock2 HOSTA))
 ;(outraw MSG)
 ;(outraw " [> ")
 ;(outraw HOSTA)
 ;(outraw "=>")
 ;(outraw SOCKA)
 ;(outraw "]")
 ;(cr)
  (catch True (=> ()
                (set! SOCK (sock-cli SOCKA)))
              (=> (E . OPT)
                (error "host-phys-send(2) " SOCKA)))
  (sock-write SOCK (sexpr-serialize MSG))
  (sock-close SOCK))

(define (host-send PROC MSG)
  (define SENT False)
  (if (proc? PROC)
    (let* ((ADDR (hash-ref (net-phys) (: PROC 'UID))))
      (if (and ADDR (!= (addr-host ADDR) (: (host-proc) 'HOSTID)))
      (begin
        (host-phys-send ADDR MSG)
        (set! SENT True)))))
  (if (not SENT) ;; FIXME: messy code
  (begin
    (if (and (proc? PROC) (not (procph? PROC)))
      (set! PROC (proc-hostph PROC)))
    (if (and (string? PROC) (== PROC (: (host-proc) 'HOSTID)))
      (set! PROC (host-proc)))
    (if (proc? PROC)
    (begin
      (if (not (procph? PROC))
        (error "host-send"))
      (if (== (: PROC 'ROLE) 'Core)
        ((: PROC 'HANDLER) MSG)
        (host-phys-send (network-addr (current-machine) (: PROC 'HOSTID)) MSG)))
    (begin
      (if (nil? PROC)
        (set! PROC "0"))
      (if (not (string? PROC))
        (error "host-send(2)"))
      (host-phys-send (network-addr (current-machine) PROC) MSG))))))

;; Networks
(define tnetwork (type "network"
                       '(PHYS   ;; UID proc => Physical address
                         PROC   ;; UID proc => Proc (core) object
                         MAPPED ;; UID proc => Proc (mapped) object
                        )))

(define (network)
  (define RES (rexpr tnetwork '()))
  (:= RES 'PHYS (make-hashv-table))
  (:= RES 'PROC (make-hashv-table))
  (:= RES 'MAPPED (make-hashv-table))
  RES)

(define _MACHINE (conf-get2 "MACHINE" "127.0.0.1"))
               ;; TODO: should be (most of the time) the machine's _public_ IP
(define (current-machine)
  _MACHINE)
(if (net-log)
(begin
  (outraw "MACHINE=")
  (outraw (current-machine))
  (cr)))

(define _NET (network))
(define (current-network)
  _NET)

(define (net-phys)
  (: (current-network) 'PHYS))

(define (net-procs)
  (: (current-network) 'PROC))

(define (net-mapped)
  (: (current-network) 'MAPPED))

(define (net-enter PROC)
  (define UID (: PROC 'UID))
  (if (not (proc? PROC))
    (error "net-enter")
  (if (^ 'mapping? PROC)
    (hash-set! (net-mapped) UID PROC)
  (if (not (^ 'core? PROC))
    (error "net-enter(2)")
  (if (string? UID)
    (let* ((ADDR (network-addr (current-machine)
                               (: (host-proc) 'HOSTID))))
      (if (net-log)
      (begin
        (outraw "nenter=> ") ;; TODO: turn that to (clean) debug logs
        (out (: PROC 'ID))
        (outraw " as ")
        (outraw UID)
        (cr)))
      (hash-set! (net-phys) UID ADDR)
      (hash-set! (net-procs) UID PROC)
      (host-send "0" `(enter ,UID ,ADDR))))))))

(define (net-leave PROC)
  (define UID (: PROC 'UID))
  (if (string? UID)
  (begin
    (hash-remove! (net-phys) UID))
    (hash-remove! (net-procs) UID))
    (host-send "0" `(leave ,UID)))

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

(define (net-map NAME) ;; TODO: implement (connect) & downloading the mapping of a proc
  (define RES Void)
  (if (not (strsy? NAME))
    (error "net-map" NAME))
  (set! RES (hash-ref (net-procs) NAME))
  (if (not RES)
    (set! RES (hash-ref (net-mapped) NAME)))
  (if (not RES)
  (begin
   ;(host-send "0" `(resolve ,NAME)))
    (set! RES (proc tproc 'ROLE 'Mapping
                          'UID NAME
                          'USER "unknown")) ; NOTE: poor man's mapping
    (hash-set! (net-mapped) NAME RES)))
  RES)

(define (net-resolve NAME . OPT) ;; TODO: resolve (and then physically send) globally ???
  (define RES Void)
  (define ISCORE (and (proc? NAME) (^ 'core? NAME)))
  (define PROC Void)
  (if (proc? NAME)
  (begin
    (set! PROC NAME)
    (set! NAME (: NAME 'UID))))
  (if (or (not NAME) (unspecified? NAME))
    (error "net-resolve " NAME))
  (set! RES (hash-ref (net-procs) NAME))
  (if (not RES)
    (set! RES (net-resolve-group NAME)))
  (if (or (proceth? PROC)
          (account? PROC)) ;; TODO: improve this
    (set! RES PROC)
    (if (and (not RES) (not ISCORE) (empty? OPT)) ;; TODO: populate & test OPT
      (set! RES (net-map NAME))))
  RES)

(define (net-send MSG . PROC) ;; NOTE: PROC is here to be able to send an MSG to a proc which is not MSG.TO
  (if (empty? PROC)
    (set! PROC (net-resolve (: MSG (if (specified? (: MSG '_TO)) '_TO 'TO))))
    (set! PROC (car PROC)))
  (if (not (proc? PROC))
    (error "net-send " PROC))
  (if (net-log)
  (begin
    (outraw "net-send=> ")
    (>> MSG)
    (outraw " ")
    (outraw (: PROC 'UID))
    (cr)))
  (if (== (: PROC 'ROLE) 'Core)
    (^ 'post-to PROC MSG)
    (begin
      (if (== (proc-hostph PROC) (host-proc))
        (error "net-send: same hostph"))
      (host-send PROC MSG))))

(define (net-next)
  Nil)
