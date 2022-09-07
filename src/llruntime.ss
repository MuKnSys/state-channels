; llruntime[Guile].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(use-modules (ice-9 rdelim))

;; =>
(define-macro (=> LST . CODE)
  `(lambda ,LST . ,CODE))

;; Modules
(define _MODS (make-hash-table))

(define (_mod-resolve NAME)
  (hash-ref _MODS NAME))

(define (_mod-store NAME)
  (if (string? NAME)
  (begin
   ;(display "storing=> ")
   ;(display NAME)
   ;(display "\n")
    (hash-set! _MODS NAME #t))))

(define (_mod-load . MODS)
  (if (pair? MODS)
    (for-each (=> (MOD)
                (set! MOD (symbol->string MOD))
                (let* ((NAME (car (reverse (string-split MOD #\/)))))
                  (if (not (_mod-resolve NAME)) ;; FIXME: doesn't resolves the problem of modules that load themselves
                  (begin
                    (_mod-store NAME)
                    (load (string-append MOD ".ss"))
                   ;(display (string-append " " NAME " loaded ...\n"))
                  ))))
              (car MODS))))

(define-macro (export . X)
  #t)

(define-macro (import . MODS)
  (_mod-load MODS))

(_mod-store "llruntime") ;; Because it can be loaded directly, by means of (load)
