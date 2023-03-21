; scm2js.ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (March 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;
;  TODO:
;  1)  Compiler
;  [X] Implement the basic datastructures one needs in a compiler:
;      [X] types (extends to classes, functions and modules ; atomic datatypes, too) ;
;      [X] addresses/abstract values (variable/slot declarations ; appears in types) ;
;      [X] code : expressions & instructions ;
;      [ ] bytecode (perhaps) ;
;  [\] Implement a code reader/parser ;
;      [ ] finish parsing code:
;          [ ] dot in a func's parms ;
;          [ ] if, while, cond, etc. ;
;          [ ] let, let*, lambda, etc. ;
;          [ ] quote, backquote, dot ;
;  [\] Implement linking ;
;      [ ] implement declaration visibility that works forward, in a module ;
;  [\] Implement a "pretty"-printer for the parsed code ;
;  [\] Generate Javascript code, source-to-source ;
;      [ ] rewrite scheme identifiers containing "-", "*", "!", "?", etc. ;
;      [ ] generate the return instructions at the position of the last leaves of a given function's code ;
;  [\] Read several modules, recursively ; flatten the module structure ;
;      [ ] seems it loops when reading src/account.ss ; see what's going on (circular dependencies ?) ; 
;          [ ] no, it's in fact super slow ; same (not so bad) with src/procs.ss ; fix this ;
;  [X] Have a function to list the undefined identifiers ;
;  [\] Have an option to automatically generate stubs for these identifiers ;
;  [X] Implement decoding the CLI parameters of the compiler command ;
;
;  2)  Runtime
;  [ ] Implement a simple class system in Javascript ;
;  [ ] Use that to implement cons-based lists, hash tables, and perhaps some
;      atomic datatypes (the way Javascript coerces numbers to strings is highly
;      undesirable) ;
;  [ ] Implement the missing runtime, along with an FFI system ;
;  [ ] Implement continuations/coroutines [later] ;

(export #t)
(import ./runtime)

;; Basic datatypes
(define tobject Void)
(define tclass Void)
(define tmodule Void)
(define tfunction Void)
(define tboolean Void)
(define tnumber Void)
(define tstring Void)
(define tlist Void)
(define tinstr Void)

;; Basic datastructure (addresses)
(define taddr (type "addr"
                    '(CATEG   ;; Parm, Loc, Ext
                      ACCESS  ;; Priv, Pub
                      CONST   ;; If it is a constant (functions are constants)
                      VARG    ;; Varg parameter
                      NAME    ;; Name of the declared object
                      TYA     ;; Type of the declared object
                      VAL     ;; Value of the declared object (if it is constant)
                      ENV     ;; The environment it lives in
                      XREF    ;; The actual addr it refers to (if it is Extern)
                     )))

(define (addr? A)
  (inherits? (typeof A) taddr))

(define (addr CATEG NAME . TYPE)
  (define RES Void)
  (set! TYPE (if (empty? TYPE)
               tobject (car TYPE)))
  (set! RES (rexpr taddr `(CATEG ,CATEG
                           NAME ,NAME
                           TYA  ,TYPE)))
  (:? RES 'ACCESS 'Pub)
  (:? RES 'CONST False)
  (:? RES 'VARG False)
  (:? RES 'ENV Nil)
  RES)
  
;; Basic datastructure (classes) ; NOTE: should be directly reusing the type type of rexpr.ss ; but no time
(define tclass (type "class"
                     '(CATEG   ;; Atom, Type, Function, Module
                       NAME    ;; Name of the datatype
                       ENV     ;; Its env (classes are created in a given environment)
                       SLOT    ;; Slots (for Types, Functions & Modules)
                       METHOD  ;; Functions (for Types & Modules)
                       FNAME   ;; The path of the source code (modules)
                       IMPORT  ;; Imports (all non-atomic types : closures also import external environments)
                       EXPORT  ;; Exports (modules)
                       TXT     ;; Its source code (as an s-expression)
                       SCODE   ;; Its parsed code (for modules and functions)
                       CCODE   ;; Its compiled code (later: several levels of CCODE, each corresponding to a language)
                      )))

(define (class? A)
  (inherits? (typeof A) tclass))

(define (module? A)
  (and (class? A) (== (: A 'CATEG) 'Module)))

(define (function? A)
  (and (class? A) (== (: A 'CATEG) 'Function)))

(define (class CATEG NAME . PARM)
  (define RES Void)
  (define L (list-group PARM))
  (set! RES (rexpr tclass `(CATEG ,CATEG
                            NAME ,NAME
                            .
                           ,L)))
  (:? RES 'ENV Nil)
  (:? RES 'SLOT (empty))
  (:? RES 'METHOD (empty))
  (:? RES 'IMPORT (empty))
  (:? RES 'EXPORT (empty))
  (:? RES 'SCODE (empty))
  (:? RES 'CCODE (empty))
  RES)

;; Basic datastructure (instructions)
(define tinstr (type "instr"
                     '(TAG   ;; Func, If, Cond, Do, Foreach, Call, Let, Equal, And, Or, Not, Add, Sub, etc.
                       PARM  ;; The parameters (which are either instrs, or addrs)
                      )))

(define (instr? A)
  (inherits? (typeof A) tinstr))

(define (instr TAG PARM)
  (define RES Void)
  (set! RES (rexpr tinstr `(TAG ,TAG PARM ,(empty))))
  (for-each (=> (I)
              (rpush (: RES 'PARM) I))
            PARM)
  RES)
  
;; Parsing
(define (env-enter ENV NAME TYPE VAL)
  (define RES (addr 'Loc NAME))
  (if (specified? VAL)
    (begin
      (:= RES 'CONST True)
      Void ;; TODO: calculate TYA  by means of VAL
      (:= RES 'VAL VAL)
      (if (class? VAL)
        (:= VAL 'ENV ENV))))
  (:= RES 'TYA TYPE)
  (:= RES 'ENV ENV)
  (rpush (: ENV 'SLOT) RES)
  RES)

(define (tag-parse TAG)
  (cond ((== TAG 'if) 'If)
        ((== TAG '<=) 'InfEq)
        ((== TAG '*) 'Mul)
        ((== TAG '-) 'Sub)
        (else
         Void)))

(define (code-parse SRC NAME) ;; NOTE: SRC is for syntactic parse trees ; TXT would be for code as a string
  (define RES (class 'Module NAME))
  (define (imp ENV LST)
    (set! LST (map (=> (NAME)
                     (define A (addr 'Ext NAME tmodule))
                     A)
                   LST))
    (for-each (=> (IMP)
                (rpush (: ENV 'IMPORT) IMP))
              LST))
  (define (dec ENV NAME VAL)
    (env-enter ENV NAME Void (code ENV VAL)))
  (define (def ENV NAME PARM BODY)
    (define RES (class 'Function NAME))
    (define VAR Void)
    (while (not (atom? PARM))
      (set! VAR (car PARM))
      (env-enter RES VAR tobject Void)
      (set! PARM (cdr PARM)))
    (if (not (null? PARM))
      (env-enter RES PARM tobject Void)) ;; TODO: it's then a VARG, set the flag
    (body RES BODY)
    (env-enter ENV NAME tfunction RES)
    RES)
  (define (code ENV O)
    (define TAG Void)
    (define TAG_ Void)
    (define RES Void)
    (if (pair? O)
      (begin
        (set! TAG (car O))
        (set! TAG_ (tag-parse TAG))
        (if (unspecified? TAG_)
          (set! TAG_ 'Call)
          (set! O (cdr O)))
        (instr TAG_ (dotted-map (=> (I)
                                  (code ENV I))
                                O)))
      O))
  (define (body ENV SRC)
    (for-each (=> (O)
                (define TAG Void)
                (define PARM Void)
                (if (not (pair? O))
                  (rpush (: ENV 'SCODE) (code ENV O)) ;(error "code-parse::atom: " O))
                  (begin
                    (set! TAG (car O))
                    (set! PARM (cdr O))
                    (cond ((== TAG 'export)
                           Void)
                          ((== TAG 'import)
                           (imp ENV PARM))
                          ((== TAG 'define)
                           (if (symbol? (car PARM))
                             (dec ENV (car PARM) (cadr PARM))
                             (def ENV (caar PARM) (cdar PARM) (cdr PARM))))
                          (else
                           (rpush (: ENV 'SCODE) (code ENV O)))))))
              SRC))
  (:= RES 'TXT SRC)
  (body RES SRC)
  RES)

;; Linking
(define (env-root ENV)
  (define RES ENV)
  (while (!= ENV Nil)
    (set! RES ENV)
    (set! ENV (: ENV 'ENV)))
  RES)

(define (env-find ENV NAME)
  (define L (: ENV 'SLOT))
  (define RES Nil)
  (define ENV0 Void)
  (if (not (boxed-empty? L))
    (begin
      (set! RES (filter (=> (A)
                          (== (: A 'NAME) NAME))
                        L))
      (set! RES (if (pair? RES)
                  (car RES)
                  Nil))))
  (if (nil? RES)
    (begin
      (set! ENV0 (env-root ENV))
     ;(outraw* (: ENV 'NAME) " => " (: ENV0 'NAME) "\n")
      (if (!= ENV0 ENV)
        (set! RES (env-find ENV0 NAME)))))
  (if (nil? RES)
    (begin
      (set! L (: ENV0 'IMPORT))
      (if (not (boxed-empty? L))
        (while (and (nil? RES) (not (empty? L)))
          (set! ENV (: (car L) 'VAL))
          (set! RES (env-find ENV NAME))
          (set! L (cdr L))))))
  RES)

(define (link-addr ADDR) ;; dunno if it is needed
  Void)

(define _UNDEFIDF (empty))
(define (link-idf ENV O)
  (define A Void)
  (if (not (atom? O))
    (error "link-idf"))
  (if (symbol? O)
    (begin
      (set! A (env-find ENV O))
      (if (and (nil? A) (not (list-in? O _UNDEFIDF)))
        (rpush _UNDEFIDF O))))
  Void)

(define (link-instr ENV I)
  (if (instr? I)
    (for-each (=> (J)
                (link-instr ENV J))
              (: I 'PARM))
    (link-idf ENV I)))

(define (link-linstr ENV L)
  (if (not (boxed-empty? L))
    (for-each (=> (I)
                (link-instr ENV I))
              L)))

(define (link-mod ADDR)
  Void)

(define (link-code SCODE . ENV)
  (define TY (typeof SCODE))
  (define L Void)
  (set! ENV (if (empty? ENV) Nil (car ENV)))
  (cond ((== TY tclass)
        ;(if (== (: SCODE 'CATEG) 'Module)
        ;  (outraw* "Linking " (: SCODE 'NAME) "...\n"))
         (set! L (: SCODE 'IMPORT))
         (if (not (boxed-empty? L))
           (for-each (=> (I)
                       (link-mod I))
                     L))
         (if (not (boxed-empty? (: SCODE 'SLOT)))
           (for-each (=> (X)
                       (link-code X))
                     (: SCODE 'SLOT)))
         (link-linstr SCODE (: SCODE 'SCODE)))
        ((== TY taddr)
         (link-addr SCODE)
         (if (specified? (: SCODE 'VAL))
           (link-code (: SCODE 'VAL) (: SCODE 'ENV))))
        (else
         (if (nil? ENV)
           (error "link-code::else " SCODE))
         (link-instr ENV SCODE))))

;; Pretty-printing
(define (addr-categ->str CATEG)
  (cond ((== CATEG 'Parm) "P")
        ((== CATEG 'Loc) "L")
        ((== CATEG 'Ext) "X")
        (else "?")))

(define (addr-access->str ACCESS)
  (cond ((== ACCESS 'Priv) "a")
        ((== ACCESS 'Pub) "A")
        (else "?")))

(define (pretty-addr ADDR)
  (define CATEG (: ADDR 'CATEG))
  (define ACCESS (: ADDR 'ACCESS))
  (define CONST (: ADDR 'CONST))
  (define VARG (: ADDR 'VARG))
  (outraw* (addr-categ->str CATEG)
           (addr-access->str ACCESS))
  (outraw* (if CONST "C" "V")
           (if VARG "*" ""))
  (outraw* " " (: ADDR 'NAME)))

(define (pretty-instr I)
  (if (instr? I)
    (begin
      (outraw* (: I 'TAG))
      (for-each (=> (J)
                  (if (pair? J)
                    (begin
                      (indent+ 2)
                      (cr)
                      (pretty-instr J)
                      (indent+ -2))
                    (begin
                      (outraw " ")
                      (pretty-instr J))))
                (: I 'PARM)))
    (outraw I)))

(define (pretty-linstr L)
  (indent+ 2)
  (for-each (=> (I)
              (cr)
              (pretty-instr I))
            L)
  (indent+ -2))

(define (pretty-code SCODE) ;; SCODE is for symbolic code ; there are more and more low level kinds of SCODE
  (define TY (typeof SCODE))
  (define L Void)
  (cond ((== TY tclass)
         (outraw* (: SCODE 'CATEG)
                  " "
                  (: SCODE 'NAME))
         (if (specified? (: SCODE 'FNAME))
            (outraw* " [" (: SCODE 'FNAME) "]"))
         (cr)
         (set! L (: SCODE 'IMPORT))
         (outraw* "IMPORT = ")
         (if (boxed-empty? L)
           (outraw "_")
           (for-each (=> (I)
                       (outraw* (: I 'NAME) " "))
                     L))
         (cr)
         (outraw* "ENV = ")
         (if (nil? (: SCODE 'ENV))
           (outraw "_")
           (outraw (: (: SCODE 'ENV) 'NAME)))
         (cr)
         (outraw "SLOT = ")
         (indent+ 2)
         (if (boxed-empty? (: SCODE 'SLOT))
           (outraw "_")
           (for-each (=> (X)
                       (cr)
                       (pretty-code X))
                     (: SCODE 'SLOT)))
         (indent+ -2)
         (cr)
         (outraw "BODY = ")
         (set! L (: SCODE 'SCODE))
         (if (boxed-empty? L)
           (begin
             (indent+ 2)
             (cr)
             (outraw "_")
             (indent+ -2))
           (pretty-linstr L)))
        ((== TY taddr)
         (pretty-addr SCODE)
         (if (specified? (: SCODE 'VAL))
           (begin
             (outraw " ")
             (pretty-code (: SCODE 'VAL)))))
        ((== TY tinstr)
         (pretty-instr SCODE))
        (else
         (outraw SCODE))))

;; Compiling
(define (compile-addr ADDR)
  (define VAL (: ADDR 'VAL))
  (if (not (and (class? VAL) (== (: VAL 'CATEG) 'Function)))
    (outraw* (: ADDR 'NAME))))

(define (instr-instr? I)
  (list-in? (: I 'TAG) '(If While)))

(define (instr-op? I)
  (list-in? (: I 'TAG) '(And Or Not
                         Eq Neq
                         Inf InfEq Sup SupEq
                         Add Sub Mul Div)))

(define _OPTAGS (rexpr Void '(If "if" While "while"
                              And "&&" Or "||" Not "!"
                              Eq "==" Neq "!="
                              Inf "<" InfEq "<=" Sup ">" SupEq ">="
                              Add "+" Sub "-" Mul "*" Div "/")))
(define (optag->string TAG)
  (define RES (: _OPTAGS TAG))
  (if (unspecified? RES) "??" RES))

(define (compile-instr I)
  (define (call I)
    (define PARM (: I 'PARM))
    (define FIRST True)
    (outraw* (car PARM) "(")
    (for-each (=> (J)
                (if FIRST
                  (set! FIRST False)
                  (outraw ","))
                (compile-instr J))
              (cdr PARM))
    (outraw ")"))
  (define (op I)
    (define PARM (: I 'PARM))
    (if (empty? (cdr PARM))
      (begin
        (outraw (optag->string (: I 'TAG)))
        (compile-instr (car PARM)))
      (begin
        (compile-instr (car PARM))
        (outraw (optag->string (: I 'TAG)))
        (compile-instr (cadr PARM)))))
  (define (instr I)
    (define PARM (: I 'PARM))
    (define FIRST True)
    (outraw* (optag->string (: I 'TAG)) " (")
    (compile-instr (car PARM))
    (outraw ") {")
    (indent+ 2)
    (for-each (=> (J)
                (cr)
                (compile-instr J))
              (cdr PARM))
    (indent+ -2)
    (cr)
    (outraw "}"))
  (define TAG Void)
  (if (instr? I)
    (begin
      (set! TAG (: I 'TAG))
      (cond ((== TAG 'Call)
             (call I))
            ((instr-op? I)
             (op I))
            ((instr-instr? I)
             (instr I))
            (else
             (outraw (optag->string TAG))
             (for-each (=> (J)
                         (indent+ 2)
                         (cr)
                         (compile-instr J)
                         (indent+ -2))
                       (: I 'PARM)))))
    (outraw I)))

(define (compile-linstr L)
  (if (not (boxed-empty? L))
    (for-each (=> (I)
                (cr)
                (compile-instr I)
                (outraw ";"))
              L)))

(define (compile-code SCODE) ;; => lower-level SCODE
  (define (imp SCODE)
    (define L Void)
    (cr)
    (set! L (: SCODE 'IMPORT))
    (outraw* "IMPORT = ")
    (if (boxed-empty? L)
      (outraw "_")
      (for-each (=> (I)
         (outraw* (: I 'NAME) " "))
       L)))
  (define (parms SCODE)
    (define FIRST True)
    (outraw "(")
    (if (not (boxed-empty? (: SCODE 'SLOT)))
      (for-each (=> (X)
                  (if FIRST
                    (set! FIRST False)
                    (outraw " "))
                  (compile-addr X))
                (: SCODE 'SLOT)))
    (outraw ")"))
  (define (dec SCODE)
    (set! VAL (: SCODE 'VAL))
    (if (specified? VAL)
      (if (function? VAL)
        (compile-code VAL)
        (outraw* "var " (: SCODE 'NAME)))))
  (define (lvars SCODE)
    (if (not (boxed-empty? (: SCODE 'SLOT)))
      (for-each (=> (X)
                  (cr)
                  (dec X))
                (: SCODE 'SLOT))))
  (define TY (if (atom? SCODE)
               False ;; TODO: improve this
               (typeof SCODE))) ;; TODO: (typeof) should _definitely_ return a non-null value, for _any_ input value, now.
  (define CATEG Void)
  (define NAME Void)
  (define VAL Void)
  (cond ((== TY tclass)
         (set! CATEG (: SCODE 'CATEG))
         (set! NAME (: SCODE 'NAME))
         (cond ((== CATEG 'Module)
                (outraw* "// Module " NAME)
               ;(imp SCODE)
                (lvars SCODE)
                )
               ((== CATEG 'Function)
                (outraw* "function " NAME)
                (parms SCODE) ;; TODO: add (lvars) for functions, too
                (outraw " {")
                (indent+ 2)))
         (compile-linstr (: SCODE 'SCODE))
         (if (== CATEG 'Function)
           (begin
             (indent+ -2)
             (cr)
             (outraw "}"))))
        (else
         (compile-instr SCODE))))

(define (generate-code SCODE) ;; => TXT, in a given syntax
  Void)

;; Reading
(define MODTABLE Void) ;; Table of all loaded modules
(define (mod-find FNAME)
  (define MOD Void)
  (if (boxed-empty? MODTABLE)
    Void
    (begin
      (set! MOD (list-find (=> (M)
                             (== (: M 'FNAME) FNAME))
                           MODTABLE))
      MOD)))

(define (code-read-bis FNAME . FETCHMODS)
  (define MOD Void)
  (define L Void)
  (if (not (string? FNAME))
    (error "code-read-bis(0) " FNAME))
  (set! FNAME (path-normalize FNAME))
  (if (not (fexists? FNAME))
    (error "code-read-bis::file " FNAME " not found"))
  (set! MOD (mod-find FNAME))
  (if (unspecified? MOD)
    (begin
      (set! MOD (code-parse (file-read FNAME) (path-fname FNAME)))
      (:= MOD 'FNAME FNAME)
      (rpush MODTABLE MOD)
      (set! L (: MOD 'IMPORT)) ;; Loading submodules
      (if (not (boxed-empty? L))
         (for-each (=> (I)
                     (:= I 'VAL (mod-load MOD (: I 'NAME))))
                   L))))
  MOD)

(define (mod-load ENV IMP)
  (define EFPATH (: ENV 'FNAME))
  (define FPATH Void)
  (define RES Void)
  (set! IMP (string2 IMP))
  (set! FPATH (if (path-abs? IMP)
                IMP
                (string+ (path-dir EFPATH) "/" IMP)))
  (set! FPATH (path-normalize FPATH))
  (if (unspecified? (path-ext FPATH))
    (set! FPATH (string+ FPATH ".ss")))
 ;(outraw* "Loading " FPATH "\n")
  (set! RES (code-read-bis FPATH))
  RES)

(define (mod-sort MOD)
  (define RES (empty))
  (define (sort MOD)
    (define L (: MOD 'IMPORT)) ;; Submodules come first
    (if (not (boxed-empty? L))
       (for-each (=> (I)
                   (sort (mod-find (: (: I 'VAL) 'FNAME))))
                 L))
    (rpush RES MOD))
  (sort MOD)
  RES)

(define (code-read FNAME . FETCHMODS)
  (define MOD (apply code-read-bis `(,FNAME . ,FETCHMODS)))
  (for-each link-code
            (mod-sort MOD))
  MOD)

;; Generating stubs
(define (compile-stubs)
  (define FIRST True)
  (if (not (boxed-empty? _UNDEFIDF))
    (begin
      (outraw "// Stubs for unimplemented functions\n")
      (for-each (=> (NAME)
                  (if FIRST
                    (set! FIRST False)
                    (cr))
                  (outraw* "function " NAME "(...X) {\n")
                  (outraw* "  error(\"" NAME ": not implemented\\n\");\n")
                  (outraw* "}"))
                _UNDEFIDF))))

;; Core app commands
(define (to-pretty FNAME)
  (pretty-code (code-read FNAME)))

;; Inits
(define (scm2js-init)
  (set! MODTABLE (empty)))

;; Main
(scm2js-init)

(define LDC (command-line-parse))
(define MOD Void)
(define FIRST True)

(define TRANS (rexpr Void `(AST ,pretty-code
                            Javascript ,compile-code)))
(define LANG (cond ((specified? (: LDC "ast")) 'AST)
                   ((specified? (: LDC "js")) 'Javascript)
                   (else
                    'AST)))
(define FTRANS (: TRANS LANG))
(define NM (specified? (: LDC "nm")))

(define IN (car (reverse LDC))) ;; TODO: implement access to the non-attribute slots of an rexpr
(if (pair? IN)
  (set! IN (if (atom? (cadr IN)) ;; FIXME: crappy hack ; we need a way to specify the arity of the flags in (command-line-parse)
             (cadr IN)
             False)))
(define OUT (if (specified? (: LDC "o"))
               (: LDC "o")
               False))
(define ALL (specified? (: LDC "a")))

(if (and IN
         LANG (specified? FTRANS))
  (begin
    (set! MOD (code-read IN))
    (if NM
      (if (== LANG 'Javascript)
        (compile-stubs)
        (for-each (=> (I)
                    (if FIRST
                      (set! FIRST False)
                      (cr))
                    (write I))
                  _UNDEFIDF))
      (if ALL
        (if (not (boxed-empty? MODTABLE))
          (for-each (=> (M)
                      (if FIRST
                        (set! FIRST False)
                        (outraw* "\n---\n"))
                      (FTRANS M))
                    (mod-sort MOD)))
        (FTRANS MOD))))
  (>> LDC))
(cr)
