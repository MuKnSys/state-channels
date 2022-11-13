; rexpr.ss
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
(import ./basics)

(export
  (import: ./llruntime)
  (import: ./basics))

;; Heaps
(define (heap)
  `((:TYPE @heap)
    (:OBJS ,(make-hashq-table))
    (:FILE "")
    (:SRV Nil)))

(define (heap-exists? H K)
  (eq? (heap-get H K) Unspecified))

(define (heap-get H K)
  (hashq-ref (rexpr-get H 'OBJS) K Unspecified))

(define (heap-set! H K O)
  (hashq-set! (rexpr-get H 'OBJS) K O)
  O) ;; TODO: set O's ID to K if there is no ID

;; Current heap
(define _HEAP (heap))

(define (heap-current)
  _HEAP)

(define (heap-current! H)
  (set! _HEAP H))

;; Ids
(define (make-id S . OPT)
  (if (symbol? S)
    (set! S (symbol->string S)))
  (if (!= (string-ref S 0) (char "@"))
    (set! S (string-add "@" S)))
  (if (not (empty? OPT))
    (set! S (string-add S ":" (string (car OPT)))))
  (sy S))

;; Types
(define _TYPES (heap))

(define (type-by-name NAME)
  (heap-get _TYPES (make-id NAME)))

(define (_valn A)
  (if (pair? A)
    (car (reverse A))
    A))
(define (_valv A)
  (if (pair? A)
    (reverse (cdr (reverse A)))
    Nil))
(define (_methodty A)
  (if (pair? A)
  (let* ((NAME (car A))
         (VAL (cadr A)))
   `(,NAME
     ,(_valv VAL)))))
(define (_methodval A)
  (if (pair? A)
  (let* ((NAME (car A))
         (VAL (cadr A)))
   `(,NAME
     ,(_valn VAL)))))

(define (type NAME SLOT . METHOD)
  (define SLOTTY '())
  (define METHODTY '())
  (define INHERITS Nil)
  (if (pair? NAME)
  (begin
    (set! INHERITS (cadr NAME))
    (set! NAME (car NAME))))
  (set! SLOT (list-add (if (nil? INHERITS) '((type :TYPE) (int :ID)) '())
                       (map (=> (A)
                              (if (pair? A)
                                (set-car! (list-last A) (attr (car (list-last A))))
                                (set! A (attr A)))
                              A)
                            SLOT)))
  (set! SLOTTY (map (=> (A)
                      `(,(_valn A)
                        ,(_valv A)))
                    SLOT))
  (set! SLOT (map (=> (A)
                    (_valn A))
                  SLOT))
  (set! METHOD (if (empty? METHOD)
                 (empty)
                 (car METHOD)))
  (set! METHOD (map (=> (A)
                 (if (pair? A) ;; FIXME: f$%&/(g problem of the (boxed-empty?) ; get rid of that
                   (set-car! A (attr (car A))))
                 A)
               (list-group METHOD)))
  (set! METHODTY (map _methodty (if (boxed-empty? METHOD) '() METHOD)))
  (set! METHOD (map _methodval METHOD))
  (append! SLOTTY METHODTY)
  (heap-set!
    _TYPES
    (make-id NAME)
    (rexpr '@type `(:INHERITS ,INHERITS
                    :ID ,(make-id NAME)
                    :NAME ,NAME
                    :INSTNO 0
                    :SLOT ,SLOT
                    :METHOD ,METHOD
                    :SLOTTY ,SLOTTY))))

(define (type? O)
  (eq? (typeof O) '@type))

;; Rexprs (Record expressions)
(define (rexpr TYPE L) ;; Creates an rexpr having the type TYPE
  (define (slots TY)
    (define LS '())
    (while (not (nil? TY))
      (set! LS (list-add (: TY 'SLOT) LS))
      (set! TY (: TY 'INHERITS)))
    LS)
  (define TYPE0 TYPE) ;; TODO: make :TYPE attribute virtual, i.e., ((:A 1)(:B 2)) is an rexpr
  (define RES Nil)
  (define INSTNO Nil)
  (if (unspecified? TYPE)
    (set! TYPE "rexpr"))
  (if (or (symbol? TYPE) (string? TYPE))
    (set! TYPE (type-by-name TYPE)))
  (if (and (list? L) (not (empty? L)) (symbol? (car L)))
    (set! L (map (=> (A)
                   (set-car! A (attr (car A)))
                   A)
                 (list-group L))))
  (if (symbol? TYPE)
    (set! RES (cons `(:TYPE ,TYPE) L))
    (begin
      (set! RES (map (=> (VAR) `(,VAR ,Unspecified))
                     (slots TYPE)))
      (:= RES 'TYPE TYPE)
      (for-each (=> (A)
        (:= RES (car A) (cadr A)))
        L)))
        
  (if (pair? TYPE) ;; FIXME: test (== (typeof TYPE) <<type type>>) ;; or (type? TYPE)
  (begin
    (set! INSTNO (rexpr-get TYPE 'INSTNO))
    (rexpr-set! TYPE 'INSTNO (+ INSTNO 1)))) ;; TODO: combine that with (make-id)
  (if (and (not (nil? INSTNO))
           (unspecified? (rexpr-get RES 'ID)))
    (rexpr-set! RES 'ID (sy (string+ (string (rexpr-get TYPE 'ID)) "@" (string INSTNO)))))
  RES)

(define (rexpr? O)
  (and (pair? O) (pair? (car O)) (== (caar O) ':TYPE)))

(define (rexpr-exists? O K)
  (heap-exists? (heap-current) K))

(define (rexpr-ref O K)
  (set! K (attr K))
  (list-find (=> (X) (and (pair? X) (== (car X) K))) O))

(define (rexpr-get O K)
  (define A (rexpr-ref O K))
  (if (unspecified? A)
    Unspecified
    (cadr A)))

(define (rexpr-prev-ref O K)
  (set! K (attr K))
  (list-find-prev (=> (X) (and (pair? X) (== (car X) K))) O))

(define (rexpr-remove! O K)
  (define A (rexpr-prev-ref O K))
  (define RES Unspecified)
  (set! K (attr K))
  (if (specified? A)
  (begin
    (set! RES (cadr (cadr A)))
    (set-cdr! A (cddr A)))
  (if (and (not (empty? O)) (== (caar O) K))
  (begin
    (set! RES (cadar O))
    (set-car! (car O) (caadr O))
    (set-car! (cdar O) (cadadr O))
    (set-cdr! O (cddr O)))))
  RES)

(define (rexpr-set! O K V . OPT) ;; default with (rcons), otherwise (cons)-ing a pair after the 1st
  (define A (rexpr-ref O K))
  (set! K (attr K))
  (if (specified? A)
    (set-car! (cdr A) V)
    (if (not (empty? O)) ;; FIXME: should not have an (null? O) at that stage
      (rcons O `(,K ,V)))))

(define (rexpr-set-init! O K V0)
  (define V (rexpr-get O K))
  (if (unspecified? V)
    (rexpr-set! O K V0)))

(define (rexpr-copy O . OPT) ;; TODO: __improve__ that S$%T !!! (do that recursively, for example ...)
  (define TYPE (rexpr-get O 'TYPE))
  (define RES Void)
  (if (type? TYPE)
    (rexpr-set! O 'TYPE (rexpr-get TYPE 'ID)))
  (set! RES (copy-tree O))
  (if (type? TYPE)
  (begin
    (rexpr-set! O 'TYPE TYPE)
    (if (empty? OPT)
      (rexpr-set! RES 'TYPE TYPE))))
  RES)

(define (rexpr-add! O K V . LV0)
  (define L (rexpr-get O K))
  (if (specified? L)
    (if (pair? L)
      (rcons L V)
      (error "rexpr-add! :: list expected"))
    (begin
      (if (not (empty? LV0))
        (set! L (rcons (rexpr-copy (car LV0)) V)) ;; TODO: put aka. (copy-tree) & (list-copy) in basics.ss
        (set! L `(,V)))
      (rexpr-set! O K L))))

(define : rexpr-get)
(define <- rexpr-remove!)
(define := rexpr-set!) ;; TODO: turn that to a macro, to also cover the case of "set!"
(define :? rexpr-set-init!)
(define :+ rexpr-add!)

;; Parse/serialize
(define (sexpr-serialize O . OPT)
  (define RES Void)
  (if (not (list-in? 'no-unlink OPT))
    (set! O (rexpr-unlink O)))
  (set! RES (with-output-to-string (=> ()
                                     (write O))))
  (if (not (list-in? 'no-unlink OPT))
    (set! O (rexpr-link O)))
  (string-replace RES "#<unspecified>" "Unspecified"))

(define (sexpr-parse S . OPT)
  (define (repl O) ;; FIXME: f$%&/(g s)=??y way, due to the fact that there is no 1st class representation for #<unspecified>
    (define RES O)
    (if (pair? O)
    (while (not (null? O))
      (if (eq? (car O) 'Unspecified)
        (set-car! O Unspecified))
      (repl (car O))
      (set! O (cdr O))))
    RES)
  (define RES Void)
  (if (not (string? S))
    (error "sexpr-parse"))
  (set! RES (with-input-from-string S (=> ()
                                        (read))))
  (set! RES (car (repl `(,RES))))
  (if (not (list-in? 'no-link OPT))
    (set! RES (rexpr-link RES)))
  RES)

(define (rexpr-serialize O . OPT) ;; either to the level of the 1st rexprs having IDs, or full
  (define RAW (and (pair? OPT) (== (car OPT) 'Raw)))
  (define INDENT (and (pair? OPT) (== (car OPT) 'Indent))) ;; FIXME: use (contains), instead
  (if (list? O)
    (if (empty? O)
      (outraw "Nil")
      (if (and (== (list-length O) 2) (attr? (car O)))
      (begin
        (outraw "(")
        (out (car O))
        (outraw " ")
        (if (type? (cadr O))
          (out (: (cadr O) 'ID))
          (apply rexpr-serialize (cons (cadr O) OPT)))
        (outraw ")"))
        
      (let* ((FIRST True))
        (outraw "(")
        (if INDENT (indent+ 2))
        (for-each (=> (ELT)
          (if (not FIRST)
            (if (and INDENT (not (and (pair? ELT) (attr? (car ELT)) (type? (cadr ELT)))))
                (cr) (outraw " "))
            (set! FIRST False))
          (apply rexpr-serialize (cons ELT OPT))) ;; Shitty apply ; no spread operator available
          O)
        (if INDENT (indent+ -2))
        (outraw ")"))))
    (if (unspecified? O)
      (outraw "_")
    (if (boolean? O)
      (outraw (if O "True" "False"))
    (if (procedure? O)
      (outraw "proc<>") ;(string+ "proc<" (procedure-name O) ">"))
      ((if RAW outraw out) O)))))) ;; TODO: finish this ; there are other compound datastructs, like e.g. vectors

(define (rexpr-unlink O)
  (if (pair? O)
    (if (== (car O) ':TYPE)
      (if (type? (cadr O))
        (let* ((TYVAL (rexpr-get (cadr O) 'ID)))
          (if (specified? TYVAL)
            (set-car! (cdr O) TYVAL))))
      (map rexpr-unlink O)))
  O)

(define (rexpr-link O)
  (if (pair? O)
    (if (== (car O) ':TYPE)
      (if (atom? (cadr O))
        (let* ((TYVAL (type-by-name (cadr O))))
          (if (specified? TYVAL)
            (set-car! (cdr O) TYVAL))))
      (map rexpr-link O)))
  O)

(define (rexpr-parse S . OPT) ;; links to when they exists, or creates folded entries for IDed rexprs
  Nil)

(define >> rexpr-serialize)

;; Pretty-printing
(define _PRETTY_OMIT '()) ;; TODO: implement genuine pretty-printing patterns
(define (rexpr-pretty O)
  (if (list? O)
    (if (empty? O)
      (outraw "Nil")
      (let* ((FIRST True)
             (A (rexpr-ref O 'TYPE)) 
            )
        (if (specified? A)
          (begin
            (if (list? (typeof O))
              (let* ((TYID (: (typeof O) 'ID)))
                (out (if (specified? TYID) TYID (typeof O))))
              (outraw (typeof O)))
            (indent+ 2)
            (for-each (=> (ELT)
                        (if (and (pair? ELT) (== (list-length ELT) 2)
                                 (strsy? (car ELT)))
                          (if (and (!= (car ELT) ':TYPE)
                                   (!= (car ELT) ':ID)
                                   (not (list-in? (unattr (car ELT)) _PRETTY_OMIT)))
                            (begin
                              (cr)
                              (outraw (unattr (car ELT)))
                              (outraw " = ")
                              (rexpr-pretty (cadr ELT))))
                          (begin
                            (cr)
                            (out ELT))))
                      O)
            (indent+ -2))
          (out O))))
    (if (unspecified? O)
      (outraw "_")
    (if (boolean? O)
      (outraw (if O "True" "False"))
      (out O)))))

;; First-class objects API
(define (typeof O) ;; TODO: test that it's an rexpr ; if not, return a type for predefined Scheme objs
  (if (pair? O) ;; FIXME: Crappy
    (: O 'TYPE)
    Void))

(define (inherits? TYPE TY0)
  (cond ((or (not (type? TYPE)) (not (type? TY0)))
         False)
        ((== TYPE TY0)
         True)
        (else
          (inherits? (: TYPE 'INHERITS) TY0))))

(define (method TYPE F)
  (define M Void)
  (while (and (not (nil? TYPE)) (unspecified? M))
    (set! M (: (: TYPE 'METHOD) F))
    (set! TYPE (: TYPE 'INHERITS)))
  M)

(define (slotty TYPE F)
  (: (: TYPE 'SLOTTY) F))

(define (method! TYPE NAME F)
  (if (not (symbol? NAME))
    (error "method! : (symbol? NAME) expected"))
  (:= (: TYPE 'METHOD) NAME (_valn F))
  (:= (: TYPE 'SLOTTY) NAME (_valv F)))

(define (mcall F . PARM) ;; TODO: turn that to a macro to be able to give F with no quote
  (apply (method (typeof (car PARM)) F) PARM))

(define (mvparmcvt VAL TY)
  (cond
    ((== TY 'num)
     (number VAL))
    ((== TY 'sy)
     (sy VAL))
    ((== TY 'str)
     (string VAL))
    ((== TY 'var)
     (eval (sy VAL) (interaction-environment)))
    (else
      VAL)))

(define (mvparms F PARM)
  (let* ((TYPE (typeof (car PARM)))
         (PROTO (filter (=> (X)
                          (not (list-in? X '(volatile logged committed)))
                        )
                        (slotty TYPE F))))
    (if (> (list-length PARM) (list-length PROTO))
      (error (: TYPE 'NAME) "." F " : " (list-length PROTO) " arguments expected"))
    (set! PARM (map mvparmcvt PARM (list-head PROTO (list-length PARM))))
    PARM))

(define (mcallv F . PARM)
  (set! PARM (mvparms F PARM))
  (apply mcall `(,F . ,PARM)))

(define ^ mcall) ;; TODO: improve this ugly thing
(define ^? mcallv)

;; Init
(heap-set! _TYPES '@type '@type) ;; Bootstrap ; TODO: add type "type"
(type "rexpr" '() (empty))
