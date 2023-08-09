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

(define (prog FPATH EXT)
  (if (unspecified? (path-ext FPATH))
    (set! FPATH (string+ FPATH "." EXT))
    (if (!= (path-ext FPATH) EXT)
      (error (string+ "File " FPATH " must have ." EXT " extension"))))
  FPATH)

(define L (command-line))

(if (< (list-length L) 2)
  (error "clish <PROG> [SCRIPT] expected"))

(define PROG (list-ref L 1))
(set! PROG (prog PROG "ss"))

(define SCRIPT False)
(if (> (list-length L) 2)
  (set! SCRIPT (prog (list-ref L 2) "scsh")))

(if (not (fexists? PROG))
  (error "File " (path-normalize PROG) " not found"))

(if (and SCRIPT (not (fexists? SCRIPT)))
  (error "File " SCRIPT " not found"))

(mod-load PROG)
(if SCRIPT
  (set! SCRIPT (file-read SCRIPT 1)))
(^ 'init-stdinout APIMON "> " SCRIPT)
(main-loop)
