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
  (hashq-ref (rexpr-get H ':OBJS) K Unspecified))

(define (heap-set! H K O)
  (hashq-set! (rexpr-get H ':OBJS) K O)
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
  (set! SLOT (list-add '((type :TYPE) (int :ID)) (map attr SLOT)))
  (set! SLOTTY (map (=> (A)
                      `(,(_valn A)
                        ,(_valv A)))
                    SLOT))
  (set! SLOT (map (=> (A)
                    (_valn A))
                  SLOT))
  (if (not (empty? METHOD))
    (set! METHOD (car METHOD)))
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
    (rexpr '@type `(:ID ,(make-id NAME)
                    :NAME ,NAME
                    :INSTNO 0
                    :SLOT ,SLOT
                    :METHOD ,METHOD
                    :SLOTTY ,SLOTTY))))

;; Rexprs (Record expressions)
(define (rexpr TYPE L) ;; Creates an rexpr having the type TYPE
  (define TYPE0 TYPE)
  (define RES Nil)
  (define SLOT Nil)
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
      (set! SLOT (: TYPE ':SLOT))
      (set! RES (map (=> (VAR) `(,VAR ,Unspecified))
                     (: TYPE ':SLOT)))
      (:= RES ':TYPE TYPE)
      (for-each (=> (A)
        (:= RES (car A) (cadr A)))
        L)))
        
  (if (pair? TYPE) ;; FIXME: test (== (typeof TYPE) <<type type>>) ;; or (type? TYPE)
  (begin
    (set! INSTNO (rexpr-get TYPE ':INSTNO))
    (rexpr-set! TYPE ':INSTNO (+ INSTNO 1)))) ;; TODO: combine that with (make-id)
  (if (and (not (nil? INSTNO))
           (unspecified? (rexpr-get RES 'ID)))
    (rexpr-set! RES ':ID (sy (string+ (string (rexpr-get TYPE ':ID)) "@" (string INSTNO)))))
  RES)

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

(define (rexpr-add! O K V . LV0)
  (define L (rexpr-get O K))
  (if (specified? L)
    (if (pair? L)
      (rcons L V)
      (error "rexpr-add! :: list expected"))
    (begin
      (if (not (empty? LV0))
        (set! L (rcons (copy-tree (car LV0)) V)) ;; TODO: put aka. (copy-tree) & (list-copy) in basics.ss
        (set! L `(,V)))
      (rexpr-set! O K L))))

(define : rexpr-get)
(define <- rexpr-remove!)
(define := rexpr-set!) ;; TODO: turn that to a macro, to also cover the case of "set!"
(define :? rexpr-set-init!)
(define :+ rexpr-add!)

;; Parse/serialize
(define (rexpr-serialize O . OPT) ;; either to the level of the 1st rexprs having IDs, or full
  (define RAW (and (pair? OPT) (== (car OPT) 'Raw)))
  (if (list? O)
    (if (empty? O)
      (outraw "Nil")
      (let* ((FIRST True)
             (A (rexpr-ref O ':TYPE)) 
            )
        (outraw "(")
        (if (and (specified? A) (list? (typeof O)))
        (let* ((TYID (: (typeof O) ':ID)))
          (outraw "(:TYPE ")
          (out (if (specified? TYID) TYID (typeof O))) ;; FIXME: hack for lists which syntactically, look like the beginning of an rexpr, e.g. in :SLOTTY => (:SLOTTY ((:TYPE (type)) (:ID (int)) ...))
          (outraw ")")))
        (for-each (=> (ELT)
          (if (not (and (specified? A) (list? (typeof O)) FIRST))
          (begin
            (if (not FIRST)
              (outraw " "))
            (apply rexpr-serialize (cons ELT OPT)) ;; Shitty apply ; no spread operator available
          ))
          (set! FIRST False))
          O)
        (outraw ")")))
    (if (unspecified? O)
      (outraw "_")
    (if (boolean? O)
      (outraw (if O "True" "False"))
      ((if RAW outraw out) O))))) ;; TODO: finish this ; there are other compound datastructs, like e.g. vectors

(define (rexpr-parse S . OPT) ;; links to when they exists, or creates folded entries for IDed rexprs
  Nil)

(define >> rexpr-serialize)

;; Pretty-printing
(define (rexpr-pretty O)
  (if (list? O)
    (if (empty? O)
      (outraw "Nil")
      (let* ((FIRST True)
             (A (rexpr-ref O ':TYPE)) 
            )
        (if (specified? A)
          (begin
            (if (list? (typeof O))
              (let* ((TYID (: (typeof O) ':ID)))
                (out (if (specified? TYID) TYID (typeof O))))
              (outraw (typeof O)))
            (indent+ 2)
            (for-each (=> (ELT)
                        (if (and (pair? ELT) (== (list-length ELT) 2)
                                 (strsy? (car ELT)))
                          (if (and (!= (car ELT) ':TYPE)
                                   (!= (car ELT) ':ID))
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
    (: O ':TYPE)
    Void))

(define (method TYPE F)
  (: (: TYPE ':METHOD) F))

(define (slotty TYPE F)
  (: (: TYPE ':SLOTTY) F))

(define (method! TYPE NAME F)
  (if (not (symbol? NAME))
    (error "method! : (symbol? NAME) expected"))
  (:= (: TYPE ':METHOD) NAME (_valn F))
  (:= (: TYPE ':SLOTTY) NAME (_valv F)))

(define (mcall F . PARM)
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
                          (not (list-in? X '(volatile logged)))
                        )
                        (slotty TYPE F))))
    (if (> (list-length PARM) (list-length PROTO))
      (error (: TYPE ':NAME) "." F " : " (list-length PROTO) " arguments expected"))
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
