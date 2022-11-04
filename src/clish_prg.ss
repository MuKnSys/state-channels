; clish_prg.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(import ./runtime)

(define L (command-line))

(if (< (list-length L) 2)
  (error "clish <PROG> [SCRIPT] expected"))

(define PROG (string+ "../examples/" (list-ref L 1) ".ss"))
(define PROGFP (string+ (dirname (_getcf)) "/" PROG))

(define SCRIPT False)
(if (> (list-length L) 2)
  (set! SCRIPT (string+ (dirname (_getcf)) "/../examples/" (list-ref L 2) ".scsh")))

(if (not (file-exists? PROGFP))
  (error "File " PROGFP " not found"))

(if (and SCRIPT (not (file-exists? SCRIPT)))
  (error "File " SCRIPT " not found"))

(loadi PROG (car L))
(if SCRIPT
  (set! SCRIPT (file-read SCRIPT 1)))
(^ 'run APIMON "> " SCRIPT)
