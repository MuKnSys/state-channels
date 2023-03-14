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
;  [ ] Implement the basic datastructures one needs in a compiler:
;      [ ] -> types (extends to classes, functions and modules ; atomic datatypes, too) ;
;      [ ] -> addresses/abstract values (variable/slot declarations ; appears in types) ;
;      [ ] -> code : expressions & instructions ;
;      [ ] -> bytecode (perhaps) ;
;  [ ] Implement a code reader/parser ;
;  [ ] Implement a "pretty"-printer for the parsed code ;
;  [ ] Generate Javascript code, source-to-source ;
;  [ ] Read several modules, recursively ; flatten the module structure ;
;  [ ] Have an function to list the undefined identifiers ;
;  [ ] Implement decoding the CLI parameters of the compiler command ;
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
                    '(CATEG   ;; Parameter, Local, Extern
                      ACCESS  ;; Private, Public
                      CONST   ;; If it is a constant (functions are constants)
                      VARG    ;; Varg parameter
                      NAME    ;; Name of the declared object
                      TYPE    ;; Type of the declared object
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
                           TYPE ,TYPE)))
  (:? RES 'ACCESS 'Public)
  (:? RES 'VARG False)
  (:? RES 'ENV Nil)
  RES)
  
;; Basic datastructure (classes) ; NOTE: should be directly reusing the type type of rexpr.ss ; but no time
(define tclass (type "class"
                     '(CATEG   ;; Atom, Type, Function, Module
                       NAME    ;; Name of the datatype
                       SLOT    ;; Slots (for Types, Functions & Modules)
                       METHOD  ;; Functions (for Types & Modules)
                       FPATH   ;; The path of the source code (modules)
                       IMPORT  ;; Imports (all non-atomic types : closures also import external environments)
                       EXPORT  ;; Exports (modules)
                       TXT     ;; Its source code (as an s-expression)
                       SCODE   ;; Its parsed code (for modules and functions)
                       CCODE   ;; Its compiled code (later: several levels of CCODE, each corresponding to a language)
                      )))

;; Basic datastructure (instructions)
(define tinstr (type "instr"
                     '(TAG   ;; Func, If, Cond, Do, Foreach, Call, Let, Equal, And, Or, Not, Add, Sub, etc.
                       PARM  ;; The parameters (which are either instrs, or addrs)
                      )))

;; Parsing
(define (code-parse SRC) ;; NOTE: SRC is for syntactic parse trees ; TXT would be for code as a string
  Void)

(define (code-pretty SCODE) ;; SCODE is for symbolic code ; there are more and more low level kinds of SCODE
  Void)

;; Compiling
(define (code-compile SCODE) ;; => lower-level SCODE
  Void)

(define (code-generate SCODE) ;; => TXT, in a given syntax
  Void)

;; Reading
(define MODTABLE Void) ;; Table of all loaded modules
(define (code-read FNAME . FETCHMODS)
  Void)

;; Inits
(define (scm2js-init)
  Void)

;; Main
(scm2js-init)

(outraw* "Hello!\n")
