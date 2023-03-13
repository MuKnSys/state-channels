; procg.ss
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
(import ./procl)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./ipc)
  (import: ./calls))

;; Proc groups
;; =>
;;   proc groups have proc UIDs ; but we dispatch on them only locally, at the moment ;
;;   proc groups can receive and send messages, but they are virtual ; so, that requires consensus ;
;;   procs can belong to only one group (for state-channel-replicated procs, seems that it can't be otherwise) ;
;;
;; NOTE: proc groups should become actual processes in the future ; the group is then the mapping of the group's
;;       core in the local mem of one member. The group, since it is a process, can act as the master of the
;;       interaction (e.g. in RSM) ; in case it is more trusted than the members, it's a win ; otherwise, we
;;       can still use the turning master algorithm, and the members manage the turntaking in between themselves.
;;       The group-as-process is still necessary to get initial information about the group. Finally, only empty
;;       servlets can join a group ; they post a join to the current master, which makes it be multisigned, and
;;       then downloads the current version of the shared state to the newly entering member.
;;       
;;       2 cases:
;;       => cooperative mode: in that case, nothing prevents the group master to fulfill all the functions
;;                            pertaining to a master, i.e., initial connection, join/leave, and turntaking
;;                            management (aka. consensus) ;
;;       => adversarial/byzantine mode: in that situation, the group's member can in fact fulfill all the
;;                            functions, including join/leave ; the initial connection is somehow problematic,
;;                            in case the group starts from one member, who happens to be rogue ; for this, its
;;                            behaviour has to be constrained enough by the protocol to prevent useful exploitation
;;                            of the initial connection mechanism ; probably starting from an initial snapshot of
;;                            the group's master _and_ of the group's participant process provides a strong enough
;;                            starting point for a design (this is also valid for a restart after migration) ; there
;;                            are hybrid cases, e.g. the case of a contract in the chain driving a state channel is
;;                            a case where the master is trusted, and where the participants are not ;
;;
(set! tprocg (type `("procg" ,tproc)  ;; Proc group
                   '(PARENT           ;; Parent process (master of the group's interaction)
                     PEER             ;; Peers
                     CPEER            ;; Peers' SELF constructor
                    )))

(set! tstatech (type `("statech" ,tprocg) ;; State channel
                     '()))

;; Constructor
(define (procg . PARM)
  (define RES (apply proc `(,tprocg . ,PARM)))
  (:? RES 'PEER '())
  (^ 'host-init RES)
  RES)

(define (proc-group GR . L) ;; NOTE: (and (proc? (car L)) (xor (all (proc? (cdr L))) (all (string? (cdr L)))))
  (if (not (empty? L))
  (let* ((FROM (car L))
         (PEER (cdr L))
         (RES (if (unspecified? GR)
                  (procg) ;; TODO: check what happens in (host-init) ; do groups have to be scheduled ?
                  GR))
         (UIDG Void)
         (NPEER Void))
    (if (specified? FROM)
      (begin
        (if (not (proc? FROM))
          (error "proc-group::MASTER"))
        (^ 'attach RES FROM)))
    (:= RES 'PARENT FROM)
    (if (number? (car PEER))
      (begin
        (set! NPEER (car PEER))
        (set! PEER (cdr PEER))))
    (:= RES 'PEER (map (=> (PR)
                         (if (proc? PR)
                           (: PR 'UID)
                           PR))
                       PEER))
    (if (specified? NPEER)
      (let* ((I 1)
             (L '()))
        (set! UIDG (: GR 'UID)) ;; TODO(?): allow getting the UIGD from the FROM (if it exists)
        (if (or (not (empty? PEER)) (not (string? UIDG)))
          (error "proc-group::NPEER"))
        (if (and (specified? FROM) (unspecified? (: FROM 'UID)))
          (:= FROM 'UID (string+ UIDG "#0")))
        (while (<= I NPEER)
          (set! L (cons (string+ UIDG "#" (string I)) L))
          (set! I (+ I 1)))
        (:= RES 'PEER (reverse L))))
    RES)))

(define (proc-group+attach . L)
  (define GR (apply proc-group `(,Void . ,L)))
  (for-each (=> (PR)
              (^ 'attach GR PR))
            (cdr L))
  GR)

(method! tprocg 'attach (=> (GR PR)
  (if (or (not (proc? PR)) (not (nil? (: PR 'GROUP))))
    (error "procg::attach " (: PR 'UID) " " (: (: PR 'GROUP) 'UID)))
  (:= PR 'GROUP GR)))

(method! tprocg 'endpoint (=> (GR . NAME)
  (define UID Void)
  (define PROC Void)
  (define (id)
    (define FID (method (typeof (: (: GR 'PARENT) 'SELF)) 'peerId))
    (if (specified? FID)
      (FID GR NAME)
      Void))
  (define (initp)
    (define INIT (method (typeof (: (: GR 'PARENT) 'SELF)) 'peerInit))
    (if (specified? INIT)
      (INIT GR)
     '()))
  (define (create)
    (define PR (procl 'USER NAME 'UID UID))
    (^ 'prog! PR (apply (: GR 'CPEER) (initp)))
    (^ 'attach GR PR)
    PR)
  (set! NAME (if (empty? NAME)
               (proc-petname (current-proc))
               (car NAME)))
  (set! UID (id))
  (if (specified? UID)
    (begin
      (set! PROC (net-resolve? UID)) ;; Check if UID exists somewhere
      (if (not PROC)
        (set! PROC (if (!= NAME (proc-petname (current-proc)))
                     (net-map UID)
                     (create))))))
  PROC))

(define (procg-ready? GR)
  (if (not (proc? GR))
    (error "procg-ready?"))
  (specified? (: GR 'STATE)))

(define (statech . PARM)
  (apply proc `(,tstatech . ,PARM)))

;; RSM
(method! tprocg 'post-to (=> (PROC MSG)
  (for-each (=> (PR)
             ;(outraw "Sendto ")(outraw PR)(cr)
              (set! PR (net-resolve PR))
              (_incmsgno MSG)
              (if (not (^ 'core? PR))
                (begin
                  (:= MSG '_TO (: PR 'UID))
                  (net-send MSG PR)
                  (<- MSG '_TO))
                (net-send MSG PR)))
            (: PROC 'PEER))
 ;(outraw "post-to[group] ")
 ;(outraw (: PROC 'UID))
 ;(cr)
  (noop)))

(method! tprocg 'post-to-master (=> (PROC MSG)
  (net-send MSG (net-resolve (: PROC 'PARENT)))))
 ;(outraw "post-to-master[group] ")
 ;(outraw (: PROC 'UID))
 ;(cr)))
