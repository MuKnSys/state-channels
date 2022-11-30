; proceth.ss
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
(import ./accounts)

;; Ethereum proc
(set! tproceth (type `("proceth" ,tproc)  ;; Ethereum process
                     '()))

;; Constructor
(define (proceth . PARM)
  (define RES (apply proc `(,tproceth . ,PARM)))
  RES)

;; Step
(method! tproceth 'step (=> (PROC)
  (error "proceth::step")))
