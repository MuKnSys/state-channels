; basics.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (June 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./llruntime)

;; Error
(define ERRORCATCH #t)
(define (error_ . MSG)
  (for-each (=> (X)
              (_display X (current-error-port)))
            MSG)
  (if (not ERRORCATCH)
  (begin
    (_newline (current-error-port))
    (_error)))
  (exit2))
(set! error error_) ;; FIXME: shitty hack due to the way Guile seems to prevent redefining (error)

(define (catch-all F)
  (if (not ERRORCATCH)
    (F)
    (catch True
      F
      (=> (E . OPT)
      ;;(_display E)(_display " ")(_display OPT)
        False))))

(define (errlog O)
  (_display "==============>\n")
  (_write O)(_newline))

;; No operation
(define (noop)
  Void)

;; Nil & Unspecified
(define Nil '())
(define Unspecified ((lambda () (if #f 1234))))
(define Void Unspecified)

(define (specified? X)
  (not (unspecified? X)))

(define (nil? X)
  (null? X))

;; Basic I/O (0)
(define _display display) ;; NOTE: prototype is: (display OBJ [PORT])
(define _write write)
(define _newline newline)
(define _display2 Void) ;; NOTE: prototype is: (display OBJ [PORT])
(define _write2 Void)
(define _newline2 Void)

;; Booleans
(define True #t)
(define False #f)

(define (boolean O)
  (cond ((or (unspecified? O) (nil? O)) False)
        ((boolean? O) O)
        ((number? O) (!= O 0))
        ((string? O)
          (!= O "0"))
        (else
          True)))

(define (ifTrue TEST FN)
  (if (and TEST (not (unspecified? TEST)))
  (begin
    (FN)
    True)
  False))

;; Numbers
(define (number O)
  (if (number? O)
    O
    (string->number (string2 O))))

;; Symbols
(define (sy S) ;; TODO: perhaps return (string->symbol (string2 S))
  (if (string? S)
    (string->symbol S)
    S))

;; Characters
(define (char S)
  (string-ref S 0))

(define (char-digit? C)
  (or (eq? C #\0) (eq? C #\1) (eq? C #\2) (eq? C #\3) (eq? C #\4)
      (eq? C #\5) (eq? C #\6) (eq? C #\7) (eq? C #\8) (eq? C #\9)))

;; Strings
(define (string2 O)
  (cond ((unspecified? O)
         "#u")
        ((null? O)
         "#n")
        ((boolean? O)
         (if O "#t" "#f"))
        ((number? O)
         (number->string O))
        ((char? O)
         (string O))
        ((symbol? O)
         (symbol->string O))
        (else
         O)))

(define (string-digits? S) ;; TODO: do more functions like this, e.g. that recognize the format of floating numbers
  (define RES (and (string? S) (> (string-length S) 0)))
  (define I 0)
  (while (and RES (< I (string-length S)))
    (set! RES (and RES (char-digit? (string-ref S I))))
    (set! I (+ I 1)))
  RES)

(define string-get string-ref)
;(define string-set! string-set!) ;; Does nothing ; FIXME: find why, in Gerbil, that says that string-set! is unspecified

(define string-add string-append)
(define string+ string-add)

(define string-trim string-trim-both) ;; TODO: refine this

(define (string-replace S SS NEW) ;; TODO: seems ok (with SRFI-13) ; verify this
  (let ((SSL (string-length SS)))
    (with-output-to-string
      (lambda ()
        (let LP ((START 0))
	  (let* ((END (string-contains S SS START)))
          (cond
           ((number? END)
            ((lambda (END)
               (_display (substring/shared S START END))
               (_display NEW)
               (LP (+ END SSL)))
	     END))
           (else
            (_display (substring/shared S START))))))))))

;; Strsys
(define (strsy+ . L)
  (define SY Void)
  (define RES Void)
  (if (empty? L)
    (error "strsy+"))
  (set! SY (symbol? (car L)))
  (set! L (map string2 L))
  (set! RES (apply string+ L))
  (if SY
    (sy RES)
    RES))

;; Atoms
(define (atom? O)
  (or (unspecified? O)
      (null? O)
      (boolean? O)
      (number? O)
      (symbol? O)
      (char? O)
      (string? O)
      (eof-object? O)))

(define (strsy? O)
  (or (symbol? O)
      (string? O)))

;; Attributes
(define (attr? S)
  (if (not (strsy? S))
    False ;;(error "attr? : " S " is not an strsy"))
    (begin
      (set! S (if (symbol? S) (symbol->string S) S))
      (and (> (string-length S) 0) (== (string-get S 0) #\:)))))

(define (attr S)
  (define ISSY (symbol? S))
  (if (attr? S)
    S
    (begin
      (set! S (string+ ":" (if ISSY (symbol->string S) S)))
      (if ISSY (sy S) S))))

(define (unattr S)
  (define ISSY (symbol? S))
  (if (not (attr? S))
    S
    (begin
      (set! S (if ISSY (symbol->string S) S))
      (set! S (substring/shared S 1 (string-length S)))
      (if ISSY (sy S) S))))

;; Lists & other containers
(define (empty) ;; Empty list (a _real_ one) ;; TODO: use a special value "Void", rather than Unspec
  `(,Void))

(define (empty? L)
  (null? L))

(define (boxed-empty? L)
  (and (not (empty? L)) (pair? L) (== (car L) Unspecified) (empty? (cdr L))))

(define (list-find F L)
  (while (and (not (empty? L)) (not (F (car L))))
    (set! L (cdr L)))
  (if (and (not (empty? L)) (F (car L)))
    (car L)
    Unspecified))

(define (list-in? X L)
  (specified? (list-find (=> (E) (== E X)) L)))

(define (list-find-prev F L)
  (define PREV Unspecified)
  (while (and (not (empty? L)) (not (F (car L))))
    (set! PREV L)
    (set! L (cdr L)))
  (if (and (not (empty? L)) (F (car L)))
    PREV
    Unspecified))

(define (list-find-prev-ref L PTR)
  (define PREV Unspecified)
  (while (and (not (empty? L)) (not (eq? L PTR)))
    (set! PREV L)
    (set! L (cdr L)))
  (if (eq? L PTR)
    PREV
    Unspecified))

(define (list-pos L VAL)
  (define I 0)
  (define RES Void)
  (for-each (=> (ELT)
              (if (and (unspecified? RES) (equal? ELT VAL))
                (set! RES I))
              (set! I (+ I 1)))
            L)
  RES)

(define (list-length L)
  (if (boxed-empty? L)
    0
    (length L)))
(define list-last last-pair)

(define list-get list-ref)
(define list-set list-set!)

;(define [ list-ref)
;(define [:= list-set)

(define (list-push L VAL)
  (if (not (pair? L))
    (error "list-push")
    (set-cdr! (last-pair L) `(,VAL))))

(define list-add append)

(define (rcons L V)
  (if (boxed-empty? L) ;; FIXME?: (Relatively) shitty hack for empty lists
    (set-car! L V)
    (append! L `(,V))))
(define rpush rcons) ;; TODO: rpop

(define (rshift L)
  (define RES (if (boxed-empty? L)
                Unspecified
                (car L)))
  (if (not (boxed-empty? L))
  (begin
    (set-car! L Unspecified)
    (if (not (empty? (cdr L)))
    (begin
      (set-car! L (cadr L))
      (set-cdr! L (cddr L))))))
  RES)
    
(define (runshift L V)
  (define V0 (car L))
  (if (not (boxed-empty? L))
    (set-cdr! L (cons V0 (cdr L))))
  (set-car! L V))

(define (list-flatten L)
  (cond ((null? L) Nil)
    ((list? (car L))
      (append (list-flatten (car L)) (list-flatten (cdr L))))
    (else
      (cons (car L) (list-flatten (cdr L))))))

(define (list-rmdup L)
  (define H (make-hashv-table))
  (define RES '())
  (for-each (lambda (E)
              (if (not (hash-ref H E))
                (set! RES (cons E RES)))
              (hash-set! H E 1))
            L)
  (reverse RES))

(define (list-copy-until L PTR)
  (define RES '())
  (if (or (unspecified? PTR) (nil? PTR))
    (set! RES (list-copy L))
    (while (!= L PTR)
      (set! RES (cons (car L) RES))
      (set! L (cdr L))))
  RES)

(define (dotted-for-each FUNC L . DOTFUNC)
  (set! DOTFUNC (if (empty? DOTFUNC)
                  Void
                  (car DOTFUNC)))
  (while (pair? L)
    (FUNC (car L))
    (set! L (cdr L)))
  (if (not (nil? L))
    (begin
      (if (specified? DOTFUNC)
        (DOTFUNC))
      (FUNC L))))

(define SymbDot (string->symbol (string2 #\.)))
(define (dotted-map FUNC L . DOT)
  (define RES '())
  (set! DOT (if (empty? DOT)
              SymbDot
              (car DOT)))
  (dotted-for-each (=> (X)
                     (set! RES (cons (FUNC X) RES)))
                   L
                   (=> () (set! RES (cons DOT RES))))
  (reverse RES))

;; Queues
(define (queue)
  (empty))

(define (queue? Q)
  (and (pair? Q) (== (car Q) Unspecified)))

(define (queue-length Q)
  (if (not (queue? Q))
    (error "queue-length"))
  (- (length Q) 1))

(define (queue-empty? Q)
  (boxed-empty? Q))

(define (queue-shift Q)
  (if (or (not (queue? Q)) (queue-empty? Q))
    (error "queue-shift"))
  (let* ((RES (cadr Q)))
    (set-cdr! Q (cddr Q))
    RES))
  
(define (queue-unshift Q VAL)
  (if (not (queue? Q))
    (error "queue-shift"))
  (set-cdr! Q (cons VAL (cdr Q))))
  
(define (queue-push Q VAL)
  (if (not (queue? Q))
    (error "queue-push"))
  (list-push Q VAL))

(define (queue-pop Q)
  (if (or (not (queue? Q)) (queue-empty? Q))
    (error "queue-pop"))
  (let* ((PTR (list-last Q))
         (PREV (list-find-prev-ref Q PTR)))
    (set-cdr! PREV Nil)
    (car PTR)))

(define (queue-remove Q VAL)
  (if (not (queue? Q))
    (error "queue-remove"))
  (if (not (queue-empty? Q))
    (let* ((PTR (list-find-prev (=> (X)
                                  (== X VAL))
                                Q)))
      (if (specified? PTR)
        (set-cdr! PTR (cddr PTR))))))

;; RLists
(define (list-group L . BYN) ;; TODO: enable BYN ; as a default, BYN==2
  (define RES '())
  (define VAR Nil)
  (define N 0)
  (if (or (empty? L) (boxed-empty? L))
    L
    (begin
      (for-each (=> (X)
                    (if (== N 0)
                        (set! VAR X)
                        (set! RES (cons `(,VAR ,X) RES))) ;; TODO: check that var is a symbol
                    (set! N (if (== N 0) 1 0)))
                L)
      (reverse RES))))

(define (list-groupa L)
  (map (=> (A)
         (set-car! A (attr (car A)))
         A)
       (list-group L)))

(define (list-splice L1 L2)
  (define L '())
  (if (!= (list-length L1) (list-length L2))
    (error "list-splice"))
  (for-each (=> (E1)
              (define E2 (car L2))
              (set! L2 (cdr L2))
              (set! L (cons E2 (cons E1 L))))
            L1)
  (reverse L))

;; Hash tables
(define (hash-for-each-in-order FUNC HT)
  (set! HT (sort (hash-map->list cons HT)
                 (=> (ELT1 ELT2) (string< (string2 (car ELT1)) (string2 (car ELT2)))))) ;; TODO: see if it's always OK with string comparisons
  (for-each (=> (ELT) (FUNC (car ELT) (cdr ELT)))
            HT))

;; Basic tests & symbols
(define == equal?) ;; FIXME: equal? works with only one parm ; == should NOT do that !!!
(define != (=> (X Y) (not (== X Y))))

;; IP addresses
(define (ipaddr? IP)
  (define L (string-split IP #\.))
  (and (== (list-length L) 4)
       (string-digits? (list-get L 0))
       (string-digits? (list-get L 1))
       (string-digits? (list-get L 2))
       (string-digits? (list-get L 3))))

(define (ipaddr->vector IP)
  (define RES (make-vector 4))
  (define L (string-split IP #\.))
  (if (not (ipaddr? IP))
    (error "ipaddr->vector"))
  (vector-set! RES 0 (number (car L)))
  (vector-set! RES 1 (number (cadr L)))
  (vector-set! RES 2 (number (caddr L)))
  (vector-set! RES 3 (number (cadddr L)))
  RES)

(define (ipaddr-loopback? IP)
  (if (not (ipaddr? IP))
    (error "ipaddr-loopback"))
  (and (>= (string-length IP) 8)
       (== (substring IP 0 8) "127.0.0.")))

(define (ipaddr-phys? IP)
  (if (not (ipaddr? IP))
    (error "ipaddr-phys?"))
  (not (ipaddr-loopback? IP)))

(define (ipaddr-private? IP)
  (define V (ipaddr->vector IP))
  (if (not (ipaddr? IP))
    (error "ipaddr-private?"))
  (or (ipaddr-loopback? IP)
      (or (== (vector-ref V 0) 10)       ;; Class A
          (and (== (vector-ref V 0) 172) ;; Class B
               (>= (vector-ref V 1) 16)
               (<= (vector-ref V 1) 31))
          (and (== (vector-ref V 0) 192) ;; Class C
               (== (vector-ref V 1) 168)))))

(define (ipaddr IP)
  (cond ((number? IP)
         (set! IP (number->string IP 16))
         (string+
           (number->string (string->number (substring IP 0 2) 16))
           "."
           (number->string (string->number (substring IP 2 4) 16))
           "."
           (number->string (string->number (substring IP 4 6) 16))
           "."
           (number->string (string->number (substring IP 6 8) 16))))
        ((string? IP)
         (if (ipaddr? IP)
           IP
           Void))
        (else
         Void)))

;; Network addrs
(define (_naddr-port? ADDRE)
  (string-digits? ADDRE))

(define (_naddr-path? ADDRE)
  (define A0 (if (and (string? ADDRE) (> (string-length ADDRE) 0))
                 (string-get ADDRE 0)
                 Void))
  (if (char? A0)
    (or (eq? A0 #\.) (eq? A0 #\/))
    False))

(define (_naddr-machine? ADDRE)
  (and (not (_naddr-port? ADDRE))
       (not (_naddr-path? ADDRE))))

(define (naddr MACH PATH) ;; NOTE: PATH can also be a port ; otherwise, it has to either start with "." or "/"
  (if (and (not (_naddr-path? PATH))
           (not (_naddr-port? PATH)))
    (error "naddr"))
  (string+ MACH ":" (string2 PATH)))

(define (naddr-machine ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:)))
      (if (_naddr-machine? (car L))
        (car L)
        Void))
    Void))

(define (naddr-port ADDR) ;; TODO: unify (naddr-port) and (naddr-path)
  (if (number? ADDR)
    (string2 ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:))
           (S (if (<= (list-length L) 1)
                (car L) (cadr L))))
      (if (_naddr-port? S)
        S
        Void))
    Void)))

;; Network paths
(define (npath . L)
  (string-join L "/"))

(define (npath->list NP)
  (if (or (nil? NP) (== NP ""))
    Nil
    (string-split NP #\/)))

(define (list->npath L)
  (if (pair? L)
    (string-join L "/")
    Nil))

(define (npath-up NP)
  (define L (reverse (npath->list NP)))
  (define RES Void)
  (list->npath (reverse (cdr L))))

(define (npath-first NP)
  (define L (npath->list NP))
  (if (empty? L)
    Void
    (car L)))

(define (npath-last NP)
  (define L (npath->list NP))
  (if (empty? L)
    Void
    (car (reverse L))))

(define (npath-cons IP NP)
  (define L (npath->list NP))
  (define FIRST (npath-first NP))
  (if (!= IP FIRST)
    (set! L (cons IP L)))
  (list->npath L))

(define (npath-prefix NP1 NP2) ;; Common prefix
  (define L1 (npath->list NP1))
  (define L2 (npath->list NP2))
  (define RES '())
  (define B True)
  (while (and B (and (pair? L1) (pair? L2)))
    (if (== (car L1) (car L2))
      (begin
        (set! RES (cons (car L1) RES))
        (set! L1 (cdr L1))
        (set! L2 (cdr L2)))
      (set! B False)))
  (list->npath (reverse RES)))

(define (npath-minus NP1 NP2) ;; Substract 2 paths => (cdr* NP1 (length common(NP1, NP2)))
  (define PREF (npath-prefix NP1 NP2))
  (define L1 Void)
  (define L2 Void)
  (define N Void)
  (if (nil? PREF)
    NP1
    (begin
      (set! L1 (npath->list NP1))
      (set! L2 (npath->list NP2))
      (set! N (list-length (npath->list PREF)))
      (while (> N 0)
        (set! L1 (cdr L1))
        (set! N (- N 1)))
      (list->npath L1))))

(define (npath-phys NP)
  (define L (npath->list NP))
  (define RES Void)
  (set! RES
    (if (or (empty? L) (not (ipaddr? (car L))))
      Nil
      (let* ((B True)
             (PTR (cdr L)))
        (set! RES '())
        (while (and B (not (empty? PTR)))
          (if (ipaddr? (car PTR))
            (begin
              (set! RES (cons (car PTR) RES))
              (set! PTR (cdr PTR)))
            (set! B False)))
        (cons (car L) (reverse RES)))))
  (list->npath RES))

(define (npath-local NP)
  (npath-minus NP (npath-phys NP)))

(define (npath-ip NP)
  (npath-last (npath-phys NP)))

;; Basic I/O (2) => plugging in (display), (write) and (newline)
(define (init-tty DISPLAY WRITE NEWLINE)
  (set! _display2 DISPLAY)
  (set! _write2 WRITE)
  (set! _newline2 NEWLINE))
(init-tty display write newline)

;; Basic I/O
(define _OUTP False)
(define (outopen MODE)
  (if _OUTP
    (error "outopen"))
  (set! _OUTP (open-output-string)))

(define (outclose)
  (if (not _OUTP)
    (error "outclose"))
  (set! _OUTP False))

(define (outgets)
  (if (not _OUTP)
    (error "outgets"))
  (get-output-string _OUTP))

(define (out_write X)
  (apply _write2 `(,X . ,(if _OUTP `(,_OUTP) '()))))

(define (out_display X)
  (apply _display2 `(,X . ,(if _OUTP `(,_OUTP) '()))))

(define _TABSEP "")
(define (tabsep SEP)
  (set! _TABSEP SEP))

(define (tabsf S)
  (define T (map (=> (S)
                   (string-split S (char "\t")))
                 (string-split S (char "\n"))))
  (define N 0)
  (define W Void)
  (for-each (=> (L)
              (if (> (list-length L) N)
                (set! N (list-length L))))
            T)
  (set! W (make-list N 0))
  (for-each (=> (L)
              (define M 0)
              (for-each (=> (C)
                          (if (> (string-length C) (list-get W M))
                            (list-set! W M (string-length C)))
                          (set! M (+ M 1)))
                        L))
            T)
  (set! T
    (map (=> (L)
           (define M 0)
           (map (=> (C)
                  (if (< (string-length C) (list-get W M))
                    (set! C (string+
                              C (make-string (- (list-get W M) (string-length C)) (char " ")))))
                  (set! M (+ M 1))
                  C)
                L))
         T))
  (set! T
    (map (=> (L)
           (string-join L _TABSEP))
         T))
  (string-join T "\n"))

(define (tabs CMD)
  (define RES Void)
  (cond
   ((== CMD 'Start)
    (outopen 'String))
   ((== CMD 'End)
    (set! RES (tabsf (outgets)))
    (outclose)
    RES)
   (else
     (error "tabs"))))

(define (tab)
  (outraw "\t"))

(define _COL0 False)
(define (atcol0?)
  _COL0)

(define (atcol0 . B)
  (if (empty? B)
    (set! B True)
    (set! B (car B)))
  (if (== B 0) (set! B False))
  (if (== B 1) (set! B True))
  (set! _COL0 B))

(define (out X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (out_write X))

(define (outraw X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (out_display X))

(define (out* . L)
  (for-each out L))

(define (outraw* . L)
  (for-each outraw L))

(define _INDENT 0)
(define (indent . N)
  (if (empty? N)
    _INDENT
    (set! _INDENT (car N))))

(define (indent+ INC)
  (indent (+ (indent) INC)))

(define (spc . N)
  (set! N (if (empty? N)
            1 (car N)))
  (if (> N 0)
    (atcol0 0))
  (while (> N 0)
    (out_display " ")
    (set! N (- N 1)))
  Void)

(define (cr)
  (outraw "\n")
  (atcol0 1)
  (spc (indent)))

(define _HASCOLORS True) ;; Seems Guile always has ANSI emulation ; in case some Scheme has not, disable printing escape codes in the functions below

(define (color-red)
  (outraw (string-append (string2 #\esc) "[31;49m"))) ;; TODO: temporary s$%t ; fix this having a parm for the color, with names for these ...

(define (color-white)
  (outraw (string-append (string2 #\esc) "[39;49m")))

(define (cursor-move DIRN . N)
  (set! N (if (empty? N) 1 (car N)))
  (set! DIRN (cond ((== DIRN 'Left) "D")
                   ((== DIRN 'Right) "C")
                   ((== DIRN 'Down) "B")
                   ((== DIRN 'Up) "A")
                   (else (error "cursor-move"))))
  (outraw (string+ (string2 #\esc) "[" (string2 N) DIRN)))

(define (clreol)
  (outraw (string+ (string2 #\esc) "[0K")))

(define _OOUT Void)
(define _OOUTRAW Void)
(define _OCR Void)
(define (rawouts B)
  (if (boolean B)
    (begin
      (if (specified? _OOUT)
        (error "rawouts"))
      (set! _OOUT out)
      (set! _OOUTRAW outraw)
      (set! _OCR cr)
      (set! out _write)
      (set! outraw _display)
      (set! cr (=> () (_display "\n"))))
    (begin
      (if (unspecified? _OOUT)
        (error "rawouts"))
      (set! out _OOUT)
      (set! outraw _OOUTRAW)
      (set! cr _OCR)
      (set! _OOUT Void)
      (set! _OOUTRAW  Void)
      (set! _OCR Void))))

;; Shell (0)
(define (sh-display L) ;; TODO: it's used in some examples ; scrap that at some point
  (for-each (lambda (S)
              (_display S)
              (_display "\n"))
            L))

;; SC_PATH
(define SC_PATH Void)
