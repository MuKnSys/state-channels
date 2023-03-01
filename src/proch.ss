; proch.ss
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

;; Host proc
(set! tproch (type `("proch" ,tproc)  ;; Host process
                   '(IP PORT          ;; Address : IP+PORT/Filesocket ; or pure in-mem
                     SCHED            ;; Local host's scheduler
                    )))

;; Constructor
(define (proch . PARM)
  (define RES (apply proc `(,tproch . ,PARM)))
  (:? RES 'SCHED (sched))
  (if (nil? (: RES 'HOST))
    (:= RES 'HOST (host-proc)))
  (^ 'host-init RES)
  RES)

;; Step
(define _STEPNO 0)
(method! tproch 'step (=> (PROC)
  (define (logit PROC)
    (if (trace-steps)
    (begin
      (outraw "!Stepping ")
      (outraw _STEPNO)
      (outraw " ")
      (set! _STEPNO (+ _STEPNO 1))
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
