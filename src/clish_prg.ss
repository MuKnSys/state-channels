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
(import ./cli ./apimon)

(define L (command-line))

(if (< (list-length L) 2)
  (error "clish <PROG> [SCRIPT] expected"))

(define PROG (string+ "../examples/" (list-ref L 1) ".ss")) ;; FIXME: make that s$&t work in case $1 is a full path ; need a way to substract 2 paths
(define PROGFP (string+ (path-dir (current-source-file)) "/" PROG))

(define SCRIPT False)
(if (> (list-length L) 2)
  (set! SCRIPT (if (path-noseps? (list-ref L 2))
                 (string+ (path-dir (current-source-file)) "/../examples/" (list-ref L 2) ".scsh")
                 (path-normalize (list-ref L 2)))))

(if (not (fexists? PROGFP))
  (error "File " PROGFP " not found"))

(if (and SCRIPT (not (fexists? SCRIPT)))
  (error "File " SCRIPT " not found"))

(loadi PROG (car L))
(if SCRIPT
  (set! SCRIPT (file-read SCRIPT 1)))
(^ 'init-stdinout APIMON "> " SCRIPT)
(main-loop)
