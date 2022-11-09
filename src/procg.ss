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
(import ./netp2p)
(import ./calls)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./netp2p)
  (import: ./calls))

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

;; Constructor
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
    (if (proc? FROM)
      (:= FROM 'GROUP RES)) ;; TODO: hmm, should work, it's unambiguous ... check this
    RES)))

(define (statech . PARM)
  (apply proc `(,tstatech . ,PARM)))

;; RSM
(method! tprocg 'post-to (=> (PROC MSG)
  (for-each (=> (PR)
              (net-send MSG (net-resolve PR)))
            (: PROC 'PEER))))
 ;(outraw "post-to[group] ")
 ;(outraw (: PROC 'UID))
 ;(cr)))

(method! tprocg 'post-to-master (=> (PROC MSG)
  (net-send MSG (net-resolve (: PROC 'PARENT)))))
 ;(outraw "post-to-master[group] ")
 ;(outraw (: PROC 'UID))
 ;(cr)))
