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
(import :std/srfi/13
        :gerbil/gambit/ports
        :gerbil/gambit/os
        :gerbil/gambit/threads)
(export
  (import: :gerbil/gambit/ports)
  (import: :gerbil/gambit/os)
  (import: :gerbil/gambit/threads))

;; Files
(define (getcwd)
  (string-trim-right (current-directory) #\/))

(define (dirname PATH)
  (string-trim-right (path-directory PATH) #\/))

(define (_getcf) ;; FIXME: not exactly the current source file ; (import) should keep this info updated
  (list-ref (command-line) 0))

(define F_GETFL 0)
(define F_SETFL 1)
(define O_NONBLOCK 2)
(define (fcntl FP CMD . VAL) ;; TODO: implement it ; no: only implement (select), rather
  #f) ;(error "fcntl: !Yet"))

;; Threads
(define (sleep SECS)
  (thread-sleep! (seconds->time (+ (time->seconds (current-time)) SECS))))

(define (usleep USECS)
  (error "usleep: !Yet"))

;; Shell
(define (shell CMD)
  (let* ((RES (shell-command CMD #t)))
    (string-split (cdr RES) (string-ref "\n" 0))))
