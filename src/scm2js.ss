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
(import ./llruntime)
