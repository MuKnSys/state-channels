; llioruntime[Gerbil].ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (March 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import :std/srfi/13)

;; Files
(define (getcwd)
  "A/B")

(define (dirname PATH)
  "A/B")

(define (_getcf)
  "A/B")

(define F_GETFL 0)
(define F_SETFL 1)
(define O_NONBLOCK 2)
(define (fcntl FP CMD . VAL) ;; TODO: implement it ; no: only implement (select), rather
  #f) ;(error "fcntl: !Yet"))

;; Threads
(define (sleep SECS)
  #f)

(define (usleep USECS)
  (error "usleep: !Yet"))

;; Shell
(define (shell CMD)
  #f)
