; procc.ss
;
;  Copyright (C) 2023, Henri Lesourd
;
;  Authors: Henri Lesourd (August 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)
(import ./json)
(import ./channels)
(import ./scheds)
(import ./procs)
(import ./ipc)
(import ./calls)
(import ./accounts)
(import ./procl)

;; Console
(set! tprocc (type `("procc" ,tproc)  ;; UNIX console
                   '(SOCKIN           ;; Input
                     SOCKOUT          ;; Output
                     INBUF            ;; Buffered (i.e. not-yet-read input)
                     CLI              ;; Command processor ;; TODO: replace that by a genuine process
                     PROMPT           ;; Prompt            ;; TODO: the CLI process should manage that
                    )))

;; Constructor
(define _CONNO 0)
(define (procc . PARM)
  (define RES (apply proc `(,tprocc . ,PARM)))
  (:? RES 'UID (string+ "CON" (string2 _CONNO)))
  (set! _CONNO (+ _CONNO 1))
  (:? RES 'INBUF (queue))
  RES)

;; Global context
(define _STDINOUT Void)
(define (stdinout)
  _STDINOUT)

(define _ALLCON Nil)
(define (all-con)
  _ALLCON)

(define _ALLCON_SOCKIN Nil)
(define (all-con-sockin)
  _ALLCON_SOCKIN)

(define (all-con! L)
  (for-each (=> (CON)
              (if (not (procc? CON))
                (error "all-con!")))
            L)
  (set! _ALLCON L)
  (set! _ALLCON_SOCKIN (map proc-sockin L)))

;; In & out socks
(method! tprocc 'inp (=> (CON)
  (: CON 'SOCKIN)))

(method! tprocc 'outp (=> (CON)
  (: CON 'SOCKOUT)))

;; Inputs
(method! tprocc 'read-line (=> (CON)
  (read-line (^ 'inp CON))))

(method! tprocc 'inbuf-empty? (=> (CON)
  (queue-empty? (: CON 'INBUF))))

(method! tprocc 'inbuf+ (=> (CON O)
  (queue-push (: CON 'INBUF) O)))

(method! tprocc 'inbuf+* (=> (CON L)
  (for-each (=> (O)
              (^ 'inbuf+ CON O))
            L)))

(method! tprocc 'inbuf++ (=> (CON)
  (define RES Nil)
  (if (not (^ 'inbuf-empty? CON))
    (set! RES (queue-shift (: CON 'INBUF))))
  RES))

;; Outputs
(method! tprocc 'outraw (=> (CON O)
  (with-output-to-port (^ 'outp CON)
    (=> () (outraw O)))))

(method! tprocc 'out (=> (CON O)
  (with-output-to-port (^ 'outp CON)
    (=> () (out O)))))

;; CLI
(method! tprocc 'step (=> (CON . FINISHED)
  (define CLI (: CON 'CLI))
  (define PROMPT (: CON 'PROMPT))
  (define CMD Void)
  (set! FINISHED (if (empty? FINISHED)
                   (list False)
                   (car FINISHED)))
  (set! CMD
    (filter (=> (X) (!= X ""))
            (string-split
              (string-trim
                (if (not (^ 'inbuf-empty? CON))
                  (let* ((CMD (^ 'inbuf++ CON)))
                    (outraw CMD)
                    (cr)
                    CMD)
                  (^ 'read-line CON)))
              (char " "))))
  (if (or (empty? CMD)
          (== (string-length (car CMD)) 0)
          (== (string-get (car CMD) 0) (char "#")))
    (if (empty? CMD) (atcol0 1))
    (begin
      (catch-all (=> ()
        (indent+ 2)
        (atcol0 1)
        (^ 'cmd CLI (car CMD) (cdr CMD) FINISHED))) ;; FIXME: do something dummy if (void? CLI)
      (indent+ -2)))
  (if (not _COL0)
    (cr))
  (if (not (car FINISHED))
    (outraw PROMPT))))

;; Init
(set! _STDINOUT
      (procc 'SOCKIN (current-input-port)
             'SOCKOUT (current-output-port)))
(define (init-procc)
  (all-con! `(,(stdinout)))) ;; TODO: do this only if the procph is a console ; if not, there are only outputs
