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
(import ./netp2p)
(import ./calls)

(export
  (import: ./scheds)
  (import: ./procs)
  (import: ./netp2p)
  (import: ./calls))

;; Physical host proc
(set! tprocph (type `("procph" ,tproch) ;; Physical host process (_one_ per physical OS-level process) ; by means
                    '(HOSTID            ;; of migrating host processes to various physical host processes, one can
                     )))                ;; deploy a distributed app in various ways.

;; Constructor
(define (procph . PARM)
  (define RES (apply proc `(,tprocph . ,PARM)))
  (:? RES 'SCHED (sched))
  (:? RES 'HOSTID "")
  RES)

;; Current physical process
(host-proc! (procph 'USER 'system ;; TODO: see if "system" is ok, as an identity
                    'UID 'phys))
