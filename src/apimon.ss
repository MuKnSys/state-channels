; apimon.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr)
(import ./procs)
(import ./cli)

;; Calls
(define (lstsign S)
  (if (unspecified? S)
    (outraw "_")
    (begin
      (outraw "(")
      (color-red)
      (outraw "S")
      (color-white)
      (for-each (=> (U)
                  (outraw " ")
                  (outraw U))
                (cdr S))
      (outraw ")"))))

(define (lstcall C . OPT)
  (set! OPT (and (not (nil? OPT)) (car OPT)))
  (let * ((USER (: C 'USER))
          (FROM (: C 'FROM))
          (OUTNB (: C 'OUTNB))
          (TO (: C 'TO))
          (INNB (: C 'INNB))
          (FUNC (: C 'FUNC))
          (PARM (: C 'PARM))
          (SIGN_B (: C 'SIGN_B))
          (SIGN_E (: C 'SIGN_E))
          (ACK (: C 'ACK))
          (ACK* (: C 'ACK*))
          (RESULT (: C 'RESULT))
         )
    (outraw (if OPT "*" " "))
    (outraw "<")
    (outraw OUTNB)
    (outraw " ")
    (>> INNB)
    (outraw ">")
    (outraw FUNC)
    (outraw " ")
    (outraw FROM)
    (outraw "=>")
    (outraw TO)
    (outraw " ")
    (>> PARM 'Raw)
  ;;(outraw " ")
  ;;(>> SIGN_B)
    (outraw " ")
    (lstsign SIGN_E) ;;(>> SIGN_E 'Raw)
    (color-red)
    (if (not RESULT)
      (outraw " FAIL")
    (if ACK*
      (outraw "!*")
    (if ACK
      (outraw "!!")
    (if (: C 'REDIR)
      (outraw "!>>")))))
    (color-white)))

(define (lstcalls L . PTR)
  (set! PTR (if (empty? PTR) Nil (car PTR)))
  (while (!= L Nil)
  (let* ((C (car L)))
    (if (specified? C)
    (begin
      (cr)
      (lstcall C (eq? PTR L))))
    (set! L (cdr L)))))

;; Procs
(define (lstproc PR . SHORT)
  (define UID (: PR 'UID))
  (define SELF (: PR 'SELF))
  (define INDENT (if (> (list-length SHORT) 1) (cadr SHORT) 0))
  (set! SHORT (and (not (empty? SHORT)) (== (car SHORT) 1)))
  (if (not SHORT)
  (begin
    (outraw (: PR 'ID))
    (outraw (if (net-resolve PR) "^" "_"))
    (outraw " ")))
  (outraw (if (specified? UID) UID "_"))
  (outraw " ")
  (outraw (: PR 'USER))
  (outraw " ")
  (outraw (if (: PR 'STOPPED) "_" "^"))
  (if (not SHORT)
  (begin
    (outraw " ")
    (>> (: PR 'PEER))
    (if (specified? (: PR 'FROM))
    (begin
      (outraw "@")
      (outraw (: (: PR 'FROM) 'UID))))
    (outraw " ")
    (>> (if (pair? SELF)
          (: SELF ':ID)
          SELF))))
  (cr)
  (outraw "in:")
  (indent+ INDENT)
  (lstcalls (: PR 'IN) (: PR 'INPTR))
  (indent+ (- 0 INDENT))
  (cr)
  (outraw "rl:")
  (indent+ INDENT)
  (lstcalls (: PR 'IN!))
  (indent+ (- 0 INDENT))
  (cr)
  (outraw "out:")
  (indent+ INDENT)
  (lstcalls (: PR 'OUT) (: PR 'OUTPTR))
  (indent+ (- 0 INDENT)))

(define (lstproc2 PR . INDENT)
  (define UID (: PR 'UID))
  (define SELF (: PR 'SELF))
  (outraw (: PR 'ID))
  (outraw (if (net-resolve PR) "^" "_"))
  (tab)
  (outraw (if (specified? UID) UID "_"))
  (tab)
  (outraw (: PR 'USER))
  (tab)
  (outraw (map (=> (X) (if (unspecified? X) "_" X)) 
               (: PR 'PEER)))
  (if (specified? (: PR 'FROM))
  (begin
    (outraw "@")
    (outraw (: (: PR 'FROM) 'UID))))
  (tab)
  (>> (if (pair? SELF)
        (: SELF ':ID)
        SELF))
)

;; Net
(define (netlist . SHORT) ;; FIXME: doesn't always lists the procs ordered by their PIDs
  (define N 0)
  (define FIRST True)
  (define INDENT (if (> (list-length SHORT) 1) (cadr SHORT) 0))
  (set! SHORT (and (not (empty? SHORT)) (== (car SHORT) 1)))
  (if SHORT
  (begin
    (tabs 'Start)
    (outraw "PID")
    (tab)
    (outraw "NAME")
    (tab)
    (outraw "USER")
    (tab)
    (outraw "PEER")
    (tab)
    (outraw "SELF")
    (cr)))
  (hash-for-each-in-order (=> (UID PR)
                            (if FIRST
                              (set! FIRST False)
                              (cr))
                            ((if SHORT lstproc2 lstproc) PR 0 INDENT)
                            (set! N (+ N 1)))
                          (allprocs))
  (if SHORT
  (begin
    (tabsep "  ")
    (outraw (tabs 'End))))
  (cr)(out N)
  (outraw " procs"))

;; APIMON
(define APIMON (make-cli '()))

(define (apimon LNAME FUNC TYPES . OPT)
  (define RES (apply clif `(,FUNC ,TYPES . ,OPT)))
  (if (not (pair? LNAME))
    (set! LNAME `(,LNAME)))
  (for-each (=> (NAME)
              (^ 'method! APIMON NAME RES))
            LNAME)
  RES)

;; CLI commands
(define OBJS (make-hashv-table))

(define (_help)
  (spc (indent))
  (outraw "pr USER UID    : create proc")(cr)
  (outraw "pr- PID        : del proc having PID")(cr)
  (outraw "pr! PID A V    : proc.A := V")(cr)
  (outraw "prog! PID $OBJ : prog(PID) := $OBJ")(cr)
  (outraw "npr PID        : net proc enter")(cr)
  (outraw "npr- PID       : net proc leave")(cr)
  (outraw "cpr [UID]      : current proc")(cr)
  (outraw "join [UID*]    : sc <= procs having the UIDs UID(i)")(cr) ;; FIXME: "_sc" is used
  (outraw "UID ^ MSG      : send MSG to proc named UID")(cr)
  (outraw "stop UID       : stop proc named UID")(cr)
  (outraw "unstop UID     : restart proc named UID")(cr)
  (outraw "prs UID        : proc step")(cr)
  (outraw "pr* [UID]      : all steps")(cr)
  (outraw "run            : run until saturation")(cr)
  (outraw "lsp            : list procs")(cr)
  (outraw "$VAR ! CONS    : CONS obj & store it in $VAR")(cr)
  (outraw "$VAR ^ MSG     : send MSG to obj stored in $VAR")(cr)
  (outraw "lso [$VAR]     : list obj(s)")(cr)
  (outraw "q              : quit")(cr)
  (outraw "h              : this help"))

(define (_quit)
  (noop))

(define (_err . L)
  (apply error L))

(define (_pr USER UID)
  (proc 'USER USER 'UID UID))

(define (_pr- PID)
  (hash-remove! _PROC PID))

(define (_pr! PID A V)
  (let* ((PR (hash-ref (allprocs) PID)))
    (if (proc? PR)
      (:= PR A V))))

(define (_prog! PID P1)
  (let* ((PR (hash-ref (allprocs) PID)))
    (if (proc? PR)
      (let* ((OBJ Void))
         (set! P1 (substring P1 1 (string-length P1)))
         (set! OBJ (hash-ref OBJS P1))
         (if OBJ
           (:= PR 'SELF OBJ)
           (begin
             (outraw "Object ")
             (outraw P1)
             (outraw " not found")))))))

(define (_npr PID)
  (net-enter (hash-ref (allprocs) PID)))

(define (_npr- PID)
  (net-leave (hash-ref (allprocs) PID)))

(define (_proc USER UID SELF)
  (define PR (proc 'USER USER 'UID UID))
  (define OBJ Void)
  (if (!= SELF "_")
  (begin
    (if (== (char "$") (string-get SELF 0))
      (set! SELF (substring SELF 1 (string-length SELF))))
    (set! OBJ (hash-ref OBJS SELF))))
  (if (or OBJ (== SELF "_"))
    (if OBJ
      (^ 'prog! PR OBJ))
    (begin
      (outraw "Object ")
      (outraw SELF)
      (outraw " not found")))
  (_npr (: PR 'ID)))

(define (_cpr . UID)
  (set! UID (if (empty? UID)
              Void
              (car UID)))
  (if (specified? UID)
    (let* ((PR (net-resolve UID)))
      (if PR
        (current-proc! PR)
        (outraw (string+ "Proc " UID " is not on the net"))))
    (begin
      (set! UID (current-proc))
      (if (nil? UID)
        (outraw "No current proc")
        (outraw (: UID 'ID))))))

(define (_sc . UID)
  (let* ((LP (map (=> (NAME)
                      (if (specified? NAME)
                        (if (== NAME "_")
                          Void
                          (let* ((P (net-resolve NAME)))
                            (if P
                              P
                              (outraw (string+ "Proc " NAME " is not on the net")))))))
                   UID)))
    (map (=> (PR)
           (:= PR 'FROM (car LP))
           (:= PR 'PEER (list-copy (cdr UID))))
         (cdr LP))))

(define (_prstop UID)
  (define PR (net-resolve UID))
  (if PR
    (begin
     ;(outraw (string+ "Stopping process " (: PR 'UID)))
      (^ 'stop PR))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prunstop UID)
  (define PR (net-resolve UID))
  (if PR
    (begin
     ;(outraw (string+ "Restarting process " (: PR 'UID)))
      (^ 'stop PR 0))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prs UID)
  (define PR (net-resolve UID))
  (if PR
    (^ 'step PR)
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prs* UID)
  (define PR (net-resolve UID))
  (if PR
    (while (not (nil? (: PR 'INPTR)))
      (^ 'step PR))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_lsp)
  (netlist))

(define (_lsp2 . UID)
  (set! UID (if (empty? UID) False (car UID)))
  (if UID
  (let* ((PR (net-resolve UID)))
    (if PR
      (lstproc PR 1 1)
      (outraw (string+ "Proc " UID " is not on the net"))))
  (netlist 1)))

(define (_lso . L)
  (define VAR (if (empty? L) False (car L)))
  (define PRETTY (if (and (pair? L) (> (list-length L) 1)) True False))
  (if VAR
    (let* ((O (hash-ref OBJS VAR)))
      (if (not O)
        (set! O (hash-ref _NET VAR)))
      (if O
        ((if PRETTY rexpr-pretty >>) O)
        (begin
          (outraw "Object ")
          (outraw VAR)
          (outraw " not found"))))
    (let* ((L (hash-map->list cons OBJS))
           (FIRST True))
      (for-each (=> (A)
                  (if (not FIRST)
                    (outraw " "))
                  (set! FIRST False)
                  (outraw (car A))
                 ;(outraw ": ")
                 ;(>> (cdr A))
                )
                L))))

(define (_lso2 . L)
  (define FIRST True)
  (for-each (=> (VAR)
              (if FIRST
                (set! FIRST False)
                (cr))
              (_lso VAR 1))
            L))

(define (_state . L)
  (define FIRST True)
  (for-each (=> (VAR)
              (if FIRST
                (set! FIRST False)
                (cr))
              (let* ((PR (hash-ref _NET VAR)))
                (if PR
                  (rexpr-pretty (: PR 'SELF))
                  (outraw (string+ "Proc " VAR " is not on the net")))))
            L))

(define (_cobj VAR FUNC . L) ;; Create obj
  (define O Void)
  (if (== "$" (string (string-get VAR 0)))
    (set! VAR (substring VAR 1 (string-length VAR))))
  (set! O (apply FUNC L))
  (hash-set! OBJS VAR O))

(define (_mesg VAR FUNC . PARM) ;; Message
  (if (== "$" (string (string-get VAR 0))) ;; Message to obj
  (let* ((OBJ Void))
    (set! VAR (substring VAR 1 (string-length VAR)))
    (set! OBJ (hash-ref OBJS VAR))
    (if OBJ
      (begin
        (set! PARM (cons OBJ PARM))
        (set! PARM (cons FUNC PARM))
       ;(out PARM)
        (apply ^? PARM))
      (begin
        (outraw "Object ")
        (outraw VAR)
        (outraw " not found"))))
  (begin ;; Message to proc
    (let* ((MULTI (== VAR "*"))
           (PR (if MULTI (current-proc) (net-resolve VAR))))
      (if PR
        (begin
          (set! PARM (cons FUNC PARM))
          (set! PARM (cons PR PARM))
          (set! PARM (cons (if MULTI 'send* 'send) PARM))
         ;(out PARM)
          (apply ^ PARM)
        )
        (outraw (string+ "Proc " VAR " is not on the net")))
    ))))

(define (_sync UID)
  (let* ((PR (net-resolve UID)))
    (if PR
      (^ 'sync PR)
      (outraw (string+ "Proc " UID " is not on the net")))))

;; CLI commands (declarations)
(apimon "h" _help '())
(apimon "q" _quit '() 'QUIT True)
(apimon "_err" _err '(str str str str) 'VARGS True)

(apimon "pr" _pr '(str str))
(apimon "pr-" _pr- '(num))
(apimon "pr!" _pr! '(num sy str))
(apimon "prog!" _prog! '(num str))

(apimon "npr" _npr '(num))
(apimon "npr-" _npr- '(num))
(apimon "proc" _proc '(str str str))
(apimon '("cpr" "iam" "whoami") _cpr '(str) 'VARGS True)

(apimon '("_sc!" "join") _sc '(str str str str) 'VARGS True)
(apimon "stop" _prstop '(str))
(apimon "unstop" _prunstop '(str))
(apimon '("prs" "step") _prs '(str))
(apimon '("prs*" "step*") _prs* '(str))
(apimon "run" step '())

(apimon "lsp" _lsp '())
(apimon '("lsp2" "netlist") _lsp2 '(str) 'VARGS True)
(apimon '("lso" "dump") _lso '(str) 'VARGS True)
(apimon '("lso2" "print") _lso2 '(str) 'VARGS True)
(apimon "state" _state '(str))

(apimon "!" _cobj '(str var any any any any any any any) 'OP True 'VARGS True)
(apimon "^" _mesg '(str sy any any any any any any any) 'OP True 'VARGS True)
(apimon "sync" _sync '(str))
