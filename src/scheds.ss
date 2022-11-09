; scheds.ss
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

;; Schedulers
(define tsched (type "sched"     ;; Scheduler
                     '(IDLE      ;; Idle procs
                       ACTIVE    ;; Active procs
                       WAITING   ;; Waiting procs (on some condition)
                       ALLPROCS  ;; All procs (hashtable)
                      )))

(define (sched? SCHED)
  (== (typeof SCHED) tsched))

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
