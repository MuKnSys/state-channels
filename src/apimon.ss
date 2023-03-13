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
(import ./accounts)
(import ./procl)
(import ./procg)
(import ./proch)
(import ./procph)
(import ./eth)
(import ./proceth)
(import ./aliases)
(import ./cli)

;; Aliases
(define _APIMONALIASES (aliases))
(define (pralias! UID APREF . RAW)
  (if (proc? UID)
    (set! UID (: UID 'UID)))
  (if (empty? RAW)
    (^ 'new! _APIMONALIASES APREF UID "")
    (^ 'alias! _APIMONALIASES APREF UID)))

(define (pralias UID . RAW)
  (define ALIAS (^ 'alias _APIMONALIASES UID))
  (if (unspecified? ALIAS) (if (empty? RAW) UID Void) ALIAS))

(define (praliasa A)
  (^ 'addr _APIMONALIASES A))

(define (netrsv UID)
  (define UID0 UID)
  (define PROC Void)
  (if (strsy? UID)
  (begin
    (set! UID (praliasa UID))
    (if (unspecified? UID)
      (set! UID UID0))))
  (if (proc? UID)
    (set! PROC UID))
  (if (and UID (specified? UID) (not (proc? UID)))
  (begin
    (set! PROC (net-resolve UID 1))
    (if (or (unspecified? PROC) (not PROC)) 
      (set! PROC (hash-ref (net-mapped) UID)))))
  PROC)

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
                  (define ACC Void)
                  (define NAME (pralias U))
                  (set! ACC (hash-ref (allaccounts) U))
                  (if ACC
                    (set! NAME (: ACC 'NAME)))
                  (outraw " ")
                  (outraw (if (unspecified? NAME) U NAME)))
                (cdr S))
      (outraw ")"))))

(define (lstcall C . OPT)
  (set! OPT (and (not (nil? OPT)) (car OPT)))
  (let* ((USER (: C 'USER))
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
    (outraw (if (specified? OUTNB) OUTNB "_"))
    (outraw " ")
    (>> INNB)
    (outraw ">")
    (outraw FUNC)
    (outraw " ")
    (outraw (pralias FROM))
    (outraw "=>")
    (outraw (pralias TO))
    (outraw " ")
    (>> PARM 'Raw)
  ;;(outraw " ")
  ;;(>> SIGN_B)
    (outraw " ")
    (lstsign SIGN_E) ;;(>> SIGN_E 'Raw)
    (color-red)
    (if (not RESULT)
      (outraw " FAIL"))
    (if ACK*
      (outraw "!*")
    (if ACK
      (outraw "!!")
    (if (: C 'REDIR)
      (outraw "!>>")))))
    (color-white))

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
(define (strpid PR) ;; FIXME: for physical hosts, display the HOSTID as a name, or as an ID
  (define L (string-split (string (: PR 'ID)) #\@))
  (if (== (list-length L) 1)
    L
    (let* ((TY (list-get L 1)))
      (set! TY (cond
                ((== TY "procl") "")
                ((and (== TY "proc") (^ 'mapping? PR)) "m")
                ((== TY "proch") "h")
                ((== TY "procph") "ph")
                ((== TY "proceth") "eth")
                ((== TY "account") "acc")
                (else
                  TY)))
      (string+ (car (list-last L)) TY))))

(define (lstprocg GR)
  (cond ((procg? GR)
         (>> (: GR 'PEER))
         (if (specified? (: GR 'PARENT))
           (begin
             (outraw "@")
             (outraw (: (: GR 'PARENT) 'UID))))
        ;(outraw "<:")
        ;(outraw (: GR 'UID))
         Void)
        ((nil? GR)
         (outraw "(_)"))
        (else
          (outraw "??"))))

(define (lstuser PR)
  (define NAME (: PR (if (account? PR) 'NAME 'USER)))
  (outraw (if (specified? NAME) NAME "_")))

(define (lstprstate PR)
  (outraw (if (and (not (procph? PR)) (net-resolve PR))
            (if (procg? PR)
              (if (procg-ready? PR) "^" "_")
              "^")
            "_")))

(define (lstproc PR . SHORT)
  (define UID (: PR 'UID))
  (define SELF (: PR 'SELF))
  (define INDENT (if (> (list-length SHORT) 1) (cadr SHORT) 0))
  (set! SHORT (and (not (empty? SHORT)) (== (car SHORT) 1)))
  (if (not SHORT)
  (begin
    (outraw (strpid PR))
    (lstprstate PR)
    (outraw " ")))
  (outraw (if (specified? UID) (pralias UID) "_"))
  (outraw " ")
  (lstuser PR)
  (outraw " ")
  (outraw (if (: PR 'STOPPED) "_" "^"))
  (outraw (if (== (: PR 'STATE) 'Waiting) "?" ""))
  (if (not SHORT)
  (begin
    (outraw " ")
    (lstprocg (: PR 'GROUP))
    (outraw " ")
    (>> (if (pair? SELF)
          (: SELF 'ID)
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
  (outraw (strpid PR))
  (lstprstate PR)
  (tab)
  (outraw (if (specified? UID) (pralias UID) "_"))
  (tab)
  (lstuser PR)
  (tab)
  (lstprocg (: PR 'GROUP))
  (tab)
  (>> (if (pair? SELF)
        (: SELF 'ID)
        SELF))
)

;; Net
(define _NETLIST-ACCOUNTS False)
(define (netlist-acc . B)
  (if (empty? B)
    _NETLIST-ACCOUNTS
    (set! _NETLIST-ACCOUNTS (car B))))
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
                            (if (or (netlist-acc) (and (not (netlist-acc)) (not (account? PR))))
                            (begin
                              (if FIRST
                                (set! FIRST False)
                                (cr))
                              ((if SHORT lstproc2 lstproc) PR 0 INDENT)
                              (set! N (+ N 1)))))
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
  (hash-remove! (allprocs) PID))

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
  (define PR (_getProc PID))
  (if (not PR)
    (error "_npr : proc " PID " not found"))
  (net-enter PR))

(define (_npr- PID)
  (define PR (_getProc PID))
  (if (not PR)
    (error "_npr- : proc " PID " not found"))
  (net-leave PR))

(define (_proch USER UID)
  (define HOST (proch 'USER USER
                      'UID UID))
  (_npr HOST) ;; FIXME: _npr should not be necessary, but is used in the shell
  HOST)

(define (_proc USER UID SELF)
  (define PR (procl 'USER USER 'UID UID))
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
  (_npr PR))

(define (_proce USER UID CNAME)
  (define PROC Void)
  (if (netrsv UID)
    (outraw (string+ "Proc " UID " already exists"))
    (begin
      (if (not (string-contains CNAME "@" 0))
        (let* ((NEXTID (if (unspecified? (: (: _ETHALIASES 'ROOT) CNAME))
                           0
                           (eth-nextId CNAME))))
          (outraw (eth-create CNAME))
          (set! CNAME (string+ CNAME "@" (string NEXTID)))
          (indent+ -2) ;; FIXME: crappy reindenting due to not being able to save & restore output contexts
          (^ 'save _ETHALIASES)
          (indent+ 2)
          (atcol0 0)))
      (if (specified? (pralias (eth-addr CNAME) 1))
        (outraw (string+ "Contract " CNAME " already mapped"))
        (begin
          (set! PROC (proceth 'USER USER
                              'UID (eth-addr CNAME)))
          (pralias! PROC UID 1)
          (net-enter PROC)))))
  PROC)

(define (_hash-lst HLIST)
  (hash-for-each (=> (UID PR)
                   (outraw UID)(outraw " ")
                   (outraw (: PR 'ID))(cr))
                 HLIST))

(define (_proce! UID . USER)
  (define UID0 UID)
  (set! USER (map account-byName USER))
  (set! USER (map (=> (ACC)
                    (if (account? ACC)
                       (: ACC 'UID)
                       ACC))
                  USER))
  (set! USER (filter (=> (UID)
                       UID)
                     USER))
  (if (not (proc? UID))
    (set! UID (netrsv UID)))
  (if (not UID)
    (outraw (string+ "Proc " UID0 " is not on the net"))
    (if (proceth? UID)
      (^ 'send UID 'init USER))))

(define (_ethblock)
  (outraw (eth-blockNumber)))

(define (_ethprocs)
  (define FIRST True)
  (for-each (=> (A)
              (if (string? (car A))
              (begin
                (if (not FIRST) (cr))
                (set! FIRST False)
                (outraw (unattr (car A)))
                (outraw " ")
                (outraw (cadr A)))))
            (: _ETHALIASES 'ROOT)))

(define (_ethwait N)
  (define BN (eth-blockNumber))
 ;(outraw (string+ "Waiting " (string N) " blocks, starting at "))(outraw BN)(cr)
  (eth-waitNBlocks N)
  (outraw (string+ "Waited " (string N) " blocks: " (string BN) " => " (string (eth-blockNumber)))))

(define (_procm UID USER)
  (define PR (net-map UID))
  (:= PR 'USER USER))

(define (_autorun AUTO)
  (^ 'autorun APIMON AUTO))

(define (_cpr . UID)
  (set! UID (if (empty? UID)
              Void
              (car UID)))
  (if (specified? UID)
    (let* ((PR (netrsv UID)))
      (if (not PR)
        (set! PR (account-byName (sy UID))))
      (if PR
        (current-proc! PR)
        (outraw (string+ "Proc " UID " is not on the net"))))
    (begin
      (set! UID (current-proc))
      (if (nil? UID)
        (outraw "No current proc")
        (outraw (: UID 'ID))))))

(define (_cprh . UID)
  (set! UID (if (empty? UID)
              Void
              (car UID)))
  (if (specified? UID)
    (let* ((PR (netrsv UID)))
      (if PR
        (current-proch! PR)
        (outraw (string+ "Host proc " UID " is not on the net"))))
    (begin
      (set! UID (current-proch))
      (if (nil? UID)
        (outraw "No current host proc")
        (outraw (: UID 'ID))))))

(define (_sc NAME . UID)
  (let* ((LP (map (=> (NAME)
                      (if (specified? NAME)
                        (if (== NAME "_")
                          Void
                          (let* ((P (netrsv NAME)))
                            (if P
                              P
                              (outraw (string+ "Proc " NAME " is not on the net")))))))
                   UID))
         (RES (apply proc-group+attach LP)))
    (:= RES 'UID NAME)
    (:= RES 'USER "nobody")
    RES))

(define (_gr UID CMASTER CPEER . PETNAMES)
  (define RES Void)
  (define MSELF (apply CMASTER PETNAMES))
  (define MASTER Void)
  (set! MASTER (procl 'USER "blockchain"))
  (^ 'prog! MASTER MSELF)
  (set! RES (proc-group (procg 'UID UID)
                        MASTER (list-length PETNAMES)))
  (:= RES 'CPEER CPEER)
  (_npr MASTER)
  RES)

(define (_gre UID)
  (define PR (netrsv UID))
  (define EP Void)
  (if PR
    (if (procg? PR)
      (begin
       ;(outraw (string+ "Creating endpoint for group " (: PR 'UID)))
        (set! EP (^ 'endpoint PR))
        (if (specified? EP)
          (_npr EP)))
      (outraw (string+ "Proc " UID " is not a group")))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prstop UID)
  (define PR (netrsv UID))
  (if PR
    (begin
     ;(outraw (string+ "Stopping process " (: PR 'UID)))
      (^ 'stop PR))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prunstop UID)
  (define PR (netrsv UID))
  (if PR
    (begin
     ;(outraw (string+ "Restarting process " (: PR 'UID)))
      (^ 'stop PR 0))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prs UID)
  (define PR (netrsv UID))
  (if PR
    (^ 'step PR)
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_prs* UID)
  (define PR (netrsv UID))
  (if PR
    (while (and (step PR) (== (: PR 'STATE) 'Active))
      (noop))
    (outraw (string+ "Proc " UID " is not on the net"))))

(define (_lsp)
  (netlist))

(define (_lsp2 . UID)
  (set! UID (if (empty? UID) False (car UID)))
  (if UID
  (let* ((PR (netrsv UID)))
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
        (set! O (hash-ref (net-procs) VAR)))
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

(define (_statef-eth PR)
  (define STATE (^ 'fetch PR 'state))
  (define ACCS (map account-byUID (^ 'fetch PR 'accounts)))
  (define BALS (^ 'fetch PR 'balances))
  (define I 0)
  (define N Void)
  (define NAME Void)
  (outraw "@proceth")
  (cr)
  (outraw "  STATE = ")
  (out STATE)(cr)
  (outraw "  ACCOUNT = @rexpr")
  (set! N (list-length ACCS))
  (while (< I N)
    (cr)
    (outraw "    ")
    (set! NAME (: (list-get ACCS I) 'NAME))
    (if (unspecified? NAME)
      (set! NAME (: (list-get ACCS I) 'UID)))
    (outraw NAME)
    (outraw " = ")
    (outraw (list-get BALS I))
    (set! I (+ I 1))))

(define (_statef . L)
  (define FIRST True)
  (for-each (=> (VAR)
              (if FIRST
                (set! FIRST False)
                (cr))
              (let* ((PR (netrsv VAR)))
                (if PR
                  (if (proceth? PR)
                    (_statef-eth PR)
                    (rexpr-pretty (: PR 'SELF)))
                  (outraw (string+ "Proc " VAR " is not on the net")))))
            L))

(define (_state . L)
  (set! _PRETTY_OMIT '(WITHDRAW _ACCOUNT))
  (apply _statef L)
  (set! _PRETTY_OMIT '()))

(define (_cobj VAR FUNC . L) ;; Create obj
  (define O Void)
  (if (== "$" (string (string-get VAR 0)))
    (set! VAR (substring VAR 1 (string-length VAR))))
  (set! O (apply FUNC L))
  (hash-set! OBJS VAR O))

(define (_fetchin . N)
  (define BLK _START-ISBLOCK) ;; TODO: clean that mess of (relative) hacks
  (set! N (if (empty? N) 1 (car N))) 
  (usleep 100000)
  (nonblockio)
  (set! _START-NEVERBLOCK True)
  (start 'Once N)
  (set! _START-NEVERBLOCK False)
  (if BLK (blockio) (nonblockio)))

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
           (PR (if MULTI (current-proc) (netrsv VAR))))
      (if PR
        (begin
          (set! PARM (cons FUNC PARM))
          (set! PARM (cons PR PARM))
          (set! PARM (cons (if MULTI 'send* 'send) PARM))
         ;(out PARM)
          (apply ^ PARM)
          (if (^ 'autorun APIMON)
          (begin
            (step)
            (_fetchin 5)))
        )
        (outraw (string+ "Proc " VAR " is not on the net")))
    ))))

(define (_sync UID)
  (let* ((PR (netrsv UID)))
    (if PR
      (^ 'sync PR)
      (outraw (string+ "Proc " UID " is not on the net")))))

;; Accounts
(define (_acc . OPT)
  (define I 0)
  (define N (accounts-length))
  (define ACC Void)
  (define LO (list-length OPT))
  (if (empty? OPT)
    (begin
      (tabsep " ")
      (tabs 'Start)
      (while (< I N) ;; TODO: do it with tabs
        (set! ACC (account-byNo I))
        (outraw (: ACC 'ACCNO))
        (tab)
        (outraw (if (unspecified? (: ACC 'ACCNO_LOCETH)) "_" (: ACC 'ACCNO_LOCETH)))
        (tab)
        (outraw (if (unspecified? (: ACC 'NAME)) "_" (: ACC 'NAME)))
        (tab)
        (outraw (: ACC 'UID))
        (if (< (+ I 1) N) (cr))
        (set! I (+ I 1)))
      (outraw (tabs 'End)))
    (cond ((== LO 1)
           (set! _PRETTY_DOIT '(UID CATEG NAME PASSWORD))
           (rexpr-pretty (account-byNo (car OPT)))
           (set! _PRETTY_DOIT Void))
          ((== LO 2)
           (set! ACC (account-byNo (car OPT)))
           (account-name! ACC (cadr OPT))))))

;; Aliases
(define (_alias)
  (>> _APIMONALIASES 'Indent))

;; CLI commands (declarations)
(apimon "h" _help '())
(apimon "q" _quit '() 'QUIT True)
(apimon "_err" _err '(str str str str) 'VARGS True)

(apimon "pr" _pr '(str str))
(apimon "pr-" _pr- '(num))
(apimon "pr!" _pr! '(num sy str))
(apimon "prog!" _prog! '(num str))

(apimon "npr" _npr '(str))
(apimon "npr-" _npr- '(str))
(apimon "proc" _proc '(str str str))
(apimon "proch" _proch '(str str))
(apimon "proce" _proce '(str str str))
(apimon "proce!" _proce! '(str sy sy sy sy sy sy sy) 'VARGS True)
(apimon '("cpr" "iam" "whoami") _cpr '(str) 'VARGS True)
(apimon "chost" _cprh '(str) 'VARGS True)
(apimon "acc" _acc '(num sy) 'VARGS True)
(apimon "alias" _alias '())
(apimon "ethblock" _ethblock '())
(apimon "ethprocs" _ethprocs '())
(apimon "ethwait" _ethwait '(num))
(apimon "procm" _procm '(str str))

(apimon '("_sc!" "join") _sc '(str str str str str) 'VARGS True)
(apimon "procg" _gr '(str var var str str str str str) 'VARGS True)
(apimon "procge" _gre '(str))
(apimon "stop" _prstop '(str))
(apimon "unstop" _prunstop '(str))
(apimon '("prs" "step") _prs '(str))
(apimon '("prs*" "step*") _prs* '(str))
(apimon "run" step '())
(apimon "autorun" _autorun '(bool))

(apimon "lsp" _lsp '())
(apimon '("lsp2" "netlist") _lsp2 '(str) 'VARGS True)
(apimon "netlist-acc" netlist-acc '(bool) 'VARGS True)
(apimon '("lso" "dump") _lso '(str) 'VARGS True)
(apimon '("lso2" "print") _lso2 '(str) 'VARGS True)
(apimon "state" _state '(str))
(apimon "statef" _statef '(str))

(apimon "!" _cobj '(str var any any any any any any any) 'OP True 'VARGS True)
(apimon "^" _mesg '(str sy any any any any any any any) 'OP True 'VARGS True)
(apimon "fetchin" _fetchin '(num) 'VARGS True)
(apimon "sync" _sync '(str))

;; Init
(csv-read (string+ SC_PATH "/a.out/PASSWD") account '(NAME (str PASSWORD) UID))
(let* ((N (accounts-length))
       (I 0))
  (while (< I N)
    (pralias! (: (account-byNo I) 'UID) "ACC")
    (set! I (+ I 1))))

(define _APIMONEthAccounts Void)
(catch True (=> ()
              (set! _APIMONEthAccounts (eth-accounts))) ;; Accounts
            (=> (E . OPT)
              False))
(if (pair? _APIMONEthAccounts)
  (let* ((I 0))
    (for-each (=> (ACC)
                (define PROC (account-byUID ACC))
                (if (not PROC)
                  (begin
                    (pralias! ACC "ACC")
                    (account 'ACCNO_LOCETH I
                             'UID ACC))
                  (begin
                    (if (specified? (: PROC 'ACCNO_LOCETH))
                      (error "APIMONEthAccounts"))
                    (:= PROC 'ACCNO_LOCETH I)))
                (set! I (+ I 1)))
              _APIMONEthAccounts)))

;; Start
(if (not (defined? '__STANDALONE__)) ;; FIXME: fix that shit (& add (defined?) to Gerbil's llruntime)
(begin
  (init0)
  (start 'Once)))
