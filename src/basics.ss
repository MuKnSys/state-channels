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
(define ERRORCATCH #f)
(define (error_ . MSG)
  (for-each (=> (X)
              (display X))
            MSG)
  (if (not ERRORCATCH)
  (begin
    (cr)
    (_error)))
  (exit2))
(set! error error_) ;; FIXME: shitty hack due to the way Guile seems to prevent redefining (error)

(define (catch-all F)
  (if (not ERRORCATCH)
    (F)
    (catch True
      F
      (=> (E . OPT)
      ;;(display E)(display " ")(display OPT)
        False))))

(define (errlog O)
  (outraw "==============>\n")
  (out O)(cr))

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

;; Booleans
(define True #t)
(define False #f)

(define (boolean O)
  (if (boolean? O)
    O
  (if (number? O)
    (!= O 0)
  (if (string? O)
    (!= O "0")
    True))))

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
    (string->number (string O))))

;; Symbols
(define (sy S) ;; TODO: perhaps return (string->symbol (string S))
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
(define (string O)
  (if (boolean? O)
    (if O "#t" "#f")
  (if (number? O)
    (number->string O)
  (if (char? O)
    (_string O)
  (if (symbol? O)
    (symbol->string O)
    O)))))

(define (string-digits? S) ;; TODO: do more functions like this, e.g. that recognize the format of floating numbers
  (define RES (string? S))
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
               (display (substring/shared S START END))
               (display NEW)
               (LP (+ END SSL)))
	     END))
           (else
            (display (substring/shared S START))))))))))

;; Strsys
(define (strsy+ . L)
  (define SY Void)
  (define RES Void)
  (if (empty? L)
    (error "strsy+"))
  (set! SY (symbol? (car L)))
  (set! L (map string L))
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
      (string? O)))

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

;; Hash tables
(define (hash-for-each-in-order FUNC HT)
  (set! HT (sort (hash-map->list cons HT)
                 (=> (ELT1 ELT2) (string< (string (car ELT1)) (string (car ELT2)))))) ;; TODO: see if it's always OK with string comparisons
  (for-each (=> (ELT) (FUNC (car ELT) (cdr ELT)))
            HT))

;; Basic tests & symbols
(define == equal?) ;; FIXME: equal? works with only one parm ; == should NOT do that !!!
(define != (=> (X Y) (not (== X Y))))

;; Files
(define (file-read FNAME . TXT)
  (define (fetch P L0 READ) ;; Fetch data from file
    (let ((L (READ P))
         )
         (if (eof-object? L)
             L0
             (fetch P (cons L L0) READ))))
  (define READ (if (empty? TXT)
                 read read-line))
  (call-with-input-file FNAME
    (=> (P)
      (reverse (fetch P Nil READ)))))

(define (file-write FNAME OBJ . WRITE)
  (with-output-to-file FNAME
    (=> ()
      (if (empty? WRITE)
        (write OBJ)
        ((car WRITE) OBJ))
      (cr))))

(define (file-delete FNAME) ;; TODO: should also work under Gerbil ; verify that it is so
  (delete-file FNAME))

;; Paths
(define (path-abs? FPATH)
  (and (> (string-length FPATH) 1) (== (string-ref FPATH 0) #\/)))

(define (path-noseps? FPATH)
  (define L (string->list FPATH))
  (and (not (list-in? #\. L))
       (not (list-in? #\/ L))))

(define (path-normalize FPATH)
  (define (p2l PATH)
    (set! PATH (string-split PATH #\/))
    (filter (=> (S)
              (!= S "")) PATH))
  (define (evp L)
    (define RES '())
    (define (push X)
      (set! RES (cons X RES)))
    (define (pop)
      (set! RES (cdr RES)))
    (while (not (empty? L))
      (cond ((== (car L) ".")
             (noop))
            ((== (car L) "..")
             (pop))
            (else
             (push (car L))))
      (set! L (cdr L)))
    (string+ "/" (string-join (reverse RES) "/")))
  (define FPATH0 Void)
  (define HOME (p2l (getenv "HOME")))
  (define CWD (p2l (getcwd)))
  (define ABS Void)
  (set! FPATH (string-trim FPATH #\space))
  (set! FPATH0 FPATH)
  (set! ABS (path-abs? FPATH))
  (if (== FPATH "")
    (set! FPATH "."))
  (set! FPATH (p2l FPATH))
  (cond ((empty? FPATH)
         "/")
        ((or (== (car FPATH) ".") (== (car FPATH) ".."))
         (evp (list-add CWD FPATH)))
        ((== (car FPATH) "~")
         (evp (list-add HOME (cdr FPATH))))
        (else
         (set! FPATH (evp FPATH))
         (if (and (not ABS) (path-abs? FPATH) (!= FPATH0 "/"))
           (set! FPATH (substring FPATH 1 (string-length FPATH))))
         FPATH)))

;; Self path
(define SC_PATH (dirname (dirname (path-normalize (_getcf)))))

;; Network paths
(define _HOST-SOCKS
        (string+ SC_PATH "/sock"))
(define (host-phys-socks)
  _HOST-SOCKS)

(define (_npath-port? ADDRE)
  (string-digits? ADDRE))

(define (_npath-path? ADDRE)
  (define A0 (if (and (string? ADDRE) (> (string-length ADDRE) 0))
                 (string-get ADDRE 0)
                 Void))
  (if (char? A0)
    (or (eq? A0 #\.) (eq? A0 #\/))
    False))

(define (_npath-machine? ADDRE)
  (and (not (_npath-port? ADDRE))
       (not (_npath-path? ADDRE))))

(define (npath-machine ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:)))
      (if (_npath-machine? (car L))
        (car L)
        Void))
    Void))

(define (npath-port ADDR)
  (if (number? ADDR)
    (string ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:))
           (S (if (<= (list-length L) 1)
                (car L) (cadr L))))
      (if (_npath-port? S)
        S
        Void))
    Void)))

(define (npath-path ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:))
           (S (if (<= (list-length L) 1)
                (car L) (cadr L))))
      (if (_npath-path? S)
        (if (eq? (string-get S 0) #\.)
          (string+ (host-phys-socks) "/" S)
          S)
        Void))
    Void))

;; Own IP
(define (ownip)
  (define IP (sh-cmd (string+ SC_PATH "/bin/ownip")))
  (if (pair? IP) (car IP) "127.0.0.255"))

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

(define (_write X)
  (apply write `(,X . ,(if _OUTP `(,_OUTP) '()))))

(define (_display X)
  (apply display `(,X . ,(if _OUTP `(,_OUTP) '()))))

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
  (_write X))

(define (outraw X)
  (if _COL0
    (spc (indent)))
  (atcol0 0)
  (_display X))

(define _INDENT 0)
(define (indent . N)
  (if (empty? N)
    _INDENT
    (set! _INDENT (car N))))

(define (indent+ INC)
  (indent (+ (indent) INC)))

(define (spc N)
  (if (> N 0)
    (atcol0 0))
  (while (> N 0)
    (_display " ")
    (set! N (- N 1))))

(define (cr)
  (outraw "\n")
  (atcol0 1)
  (spc (indent)))

(define _HASCOLORS True) ;; Seems Guile always has ANSI emulation ; in case some Scheme has not, disable printing escape codes in the functions below

(define (color-red)
  (outraw (string-append (string #\esc) "[31;49m"))) ;; TODO: temporary s$%t ; fix this having a parm for the color, with names for these ...

(define (color-white)
  (outraw (string-append (string #\esc) "[39;49m")))

(define (cursor-move DIRN . N)
  (set! N (if (empty? N) 1 (car N)))
  (set! DIRN (cond ((== DIRN 'Left) "D")
                   ((== DIRN 'Right) "C")
                   ((== DIRN 'Down) "B")
                   ((== DIRN 'Up) "A")
                   (else (error "cursor-move"))))
  (outraw (string+ (string #\esc) "[" (string N) DIRN)))

(define (clreol)
  (outraw (string+ (string #\esc) "[0K")))

;; Shell
(define (sh-cmd-log B)
  (set! _SH_CMD_LOG B)) ;; FIXME: _SH_CMD_LOG has to be defined inside llruntime

(define (sh-display L)
  (for-each (lambda (S)
              (display S)
              (display "\n"))
            L))
