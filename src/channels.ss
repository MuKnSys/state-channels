; channels.ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (Januar 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./socks)
(import ./rexpr)
(import ./inits)

;; Network addrs (NOTE: should be in basics.ss, but can't due to backend dependencies)
(define _HOST-SOCKS
        (string+ SC_PATH "/sock"))
(define (host-phys-socks)
  _HOST-SOCKS)

(define (naddr-path ADDR)
  (if (string? ADDR)
    (let* ((L (string-split ADDR #\:))
           (S (if (<= (list-length L) 1)
                (car L) (cadr L))))
      (if (_naddr-path? S)
        (if (eq? (string-get S 0) #\.)
          (path-normalize (string+ (host-phys-socks) "/" S))
          S)
        Void))
    Void))

;; GAddrs
;; NPATH:HOST ;; NPATH == [NETM(i)/]*NETM(n)
;;
(define (gaddr-npath ADDR)
  (define RES (car (string-split ADDR #\:)))
  (if (!= RES "")
    RES
    Void))

(define (gaddr-host ADDR) ;; Hosts == OS-allocated processes (i.e. procph0s)
  (define L (string-split ADDR #\:))
  (if (and (> (list-length L) 1) (!= (cadr L) ""))
    (cadr L)
    Void))

(define (gaddr-phys? ADDR) ;; Physical machines (=> IP representing them)
  ADDR)

(define (gaddr-phys ADDR) ;; NPath to the last physical machine
  (npath-phys (gaddr-npath ADDR)))

(define (gaddr-local ADDR) ;; Path inside the last physical machine's filesystem
  (npath-local (gaddr-npath ADDR)))

(define (gaddr-ip ADDR) ;; Last IP of the path
  (npath-ip (gaddr-npath ADDR)))

(define (gaddr NPATH PROCID)
  (if (number? PROCID)
    (set! PROCID (string PROCID)))
  (if (or (specified? (gaddr-host NPATH)) (not (string? PROCID)))
    (error "gaddr " NPATH " " PROCID))
  (string+ NPATH ":" PROCID))

(define (gaddr-normalize ADDR) ;; 127.0.0.SUBM:PROCID => NPATH/SUBM:PROCID or NPATH:00 if SUBM==255
  (define NPATH (gaddr-npath ADDR))
  (define PROCID (gaddr-host ADDR))
  (define L Void)
  (define RELPATH Void)
  (define (to-subm ADDR)
    (if (and (ipaddr? ADDR) (ipaddr-loopback? ADDR))
      (let* ((SUBM (substring ADDR 8 (string-length ADDR))))
        (cond ((== SUBM "254")
               "0")
              ((== SUBM "255")
               "127.0.0.255")
              (else
               SUBM)))
      (if (== ADDR "255")
        "127.0.0.255"
        ADDR)))
  (if (unspecified? NPATH)
    (set! NPATH _VMACHINE_GADDR))
  (set! L (map to-subm (string-split NPATH #\/)))
  (set! NPATH (car L))
  (set! RELPATH (or (== NPATH ".")
                    (not (ipaddr? NPATH)) ;; FIXME: if _all_ elements of NPATH are (ipaddr?)s
                    (== NPATH "127.0.0.255")))
  (set! L (filter (=> (E)
                    (and (!= E ".")
                         (!= E "127.0.0.255"))) ;; TODO: process ".."s
                  L))
  (set! NPATH (string-join L "/"))
  (if RELPATH
    (set! NPATH (string+ _PHMACHINE_GADDR (if (!= NPATH "") (string+ "/" NPATH) ""))))
  (gaddr NPATH (if (unspecified? PROCID)
                 "00" PROCID)))

(define (gaddr-naddr ADDR) ;; NPATH:PROCID => 127.0.0.SUBM:_PORT0 ou _PHMACHINE_GADDR:_PORT0 si 00
                           ;;              => PATH dans le filesystem sinon
  (define NPATH Void)
  (define HOST Void)
  (define PHYS Void)
  (define IP Void)
  (define LOCAL Void)
  (define PATH Void)
  (define (hostpath0)
    (if (== HOST "00")
      (string _PORT0)
      (string+ "./" HOST)))
  (define (hostpath1 L)
    (set! L (list->npath (map (=> (S)
                                (string+ "_" S)) L)))
    (string+ "./" L "/" HOST))
  (set! ADDR (gaddr-normalize ADDR))
  (set! NPATH (gaddr-npath ADDR))
  (set! HOST (gaddr-host ADDR))
  (set! PHYS (gaddr-phys NPATH))
  (set! IP (npath-last PHYS))
  (set! LOCAL (gaddr-local NPATH))
  (set! PATH
        (if (nil? LOCAL)
          (hostpath0)
          (let* ((L (npath->list LOCAL)))
            (if (and (== (list-length L) 1) (== HOST "00"))
              (let* ((SUBM (car L)))
                (set! IP (string+ "127.0.0." (cond ((== SUBM "0") "254")
                                                   ((== SUBM "254") (error "gaddr-naddr::254"))
                                                   ((== SUBM "255") (error "gaddr-naddr::255"))
                                                   (else SUBM))))
                (hostpath0))
              (hostpath1 L)))))
  (naddr IP PATH))

;; GAddrs (expand/shorten)
(define (gaddr-expand . ADDR)
  (define NPATH Void)
  (define HOST Void)
  (define L Void)
  (set! ADDR (if (or (empty? ADDR)
                     (unspecified? (car ADDR))) ":" (car ADDR)))
  (set! NPATH (gaddr-npath ADDR))
  (set! HOST (gaddr-host ADDR))
  (if (unspecified? NPATH)
    (set! NPATH "."))
  (if (unspecified? HOST)
    (set! HOST "0")) ;; TODO: when HOST is not given, try to have it allocated (?)
  (set! L (filter (=> (X) (!= X "")) (npath->list NPATH)))
  (if (== (string-ref NPATH 0) #\/)
    (set! L (cons _PHMACHINE_GADDR L)))
  (set! L (map (=> (X)
                 (if (== X ".")
                   _VMACHINE_GADDR
                   X))
                L))
  (if (and (not (empty? L)) (not (ipaddr? (npath-first (car L))))) ;; TODO: improve that
    (set! L (cons _PHMACHINE_GADDR L)))
  (gaddr (list->npath L) HOST))

(define (gaddr-shorten A)
  (define AP Void)
  (set! AP (gaddr-phys A))
  (if (== AP _PHMACHINE_GADDR)
    (set! A (substring A (string-length AP) (string-length A))))
  A)

;; Machines
(define _PHMACHINE_LADDR (ownip))      ;; Local IP address of the physical machine
(define _PHMACHINE_GADDR (ownnpath))   ;; NPATH of the physical machine
(define _VMACHINE_GADDR                ;; NPATH of the current machine (can also be 127.0.0.SUBM)
        (gaddr-npath (gaddr-normalize  ;; TODO: check that it's actually located inside PHMACHINE
          (conf-get2 "MACHINE" (npath _PHMACHINE_GADDR "0")))))

;; Proxied channels
(define _PROXIED (make-hashv-table)) ;; Physical (root) address => CliChans [outcoming kept sockets to (mainly proxied) procs]

;; Relays
(define (gaddr-proxied? ADDR) ;; Either physical machines with private IPs, or .ini file says so
  (define NPATH (gaddr-npath ADDR))
  (define HOST (gaddr-host ADDR))
  (define PHYS Void)
  (define IP Void)
  (set! PHYS (npath-phys NPATH))
  (set! IP (npath-ip PHYS))
  (and (== HOST "00")
    (or (and (== NPATH PHYS)
             (or (ipaddr-private? IP) ;; FIXME: hmmm ...
                 (hash-ref _PROXIED PHYS)))
        (hash-ref _PROXIED NPATH))))

(define (gaddr-proxied! ADDR)
  (define NPATH (gaddr-npath ADDR))
  (hash-set! _PROXIED NPATH True))

(define _KEEP (make-hashv-table))
(define (gaddr-keep? ADDR) ;; .ini file says so : proxied machines that ask for a keep (default: they don't)
  (define NPATH (gaddr-npath ADDR))
  (hash-ref _KEEP NPATH))

(define (gaddr-keep! ADDR)
  (define NPATH (gaddr-npath ADDR))
  (hash-set! _KEEP NPATH True))

(define _MASTER_GADDR (gaddr _PHMACHINE_LADDR "00")) ;; TODO: enable configuring it
(define (gaddr-next A1 A2 . OPT)
  (define PREF Void)
  (define NPATHM (gaddr-npath _MASTER_GADDR))
  (define NPATH1 Void)
  (define HOST1 Void)
  (define NPATH2 Void)
  (define HOST2 Void)
  (define NPATH "_")
  (define HOST "_")
 ;(define ALL? (list-in? 'AllHops OPT))
 ;(define UP? (list-in? 'Up OPT))
 ;(define DOWN? (list-in? 'Down OPT))
  (define (post)
    (if (!= NPATH1 NPATH2)
      (error "gaddr-next::post"))
    (set! NPATH NPATH2)
    (set! HOST (cond ((or (== HOST1 "0")
                          (== HOST1 "00")) HOST2)
                      (else
                       "0"))))
  (define (up)
    (if (nil? (npath-minus NPATH1 PREF))
      (error "gaddr-next::up"))
    (cond ((or (== HOST1 "0")
               (== HOST1 "00"))
           (set! NPATH (npath-up NPATH1))
           (if (empty? NPATH)
             (set! NPATH (if (== NPATH1 NPATHM)
                           (npath-first NPATH2)
                           NPATHM)))
           (set! HOST "00"))
          (else
           (set! NPATH NPATH1)
           (set! HOST "0"))))
  (define (down)
    (if (nil? (npath-minus NPATH2 PREF))
      (error "gaddr-next::down"))
    (if (== NPATH1 PREF)
      (cond ((== HOST1 "00")
             (if (== (npath-up NPATH2) PREF)
               (begin
                 (set! NPATH NPATH2)
                 (set! HOST (if (== HOST1 "00") "0" HOST2)))
               (let* ((LOC (npath->list (npath-minus NPATH2 PREF))))
                 (set! NPATH (string+ PREF "/" (car LOC)))
                 (set! HOST (if (> (list-length LOC) 1)
                                "00"
                                "0")))))
            (else
             (error "gaddr-next::down::HOST1")))
      Void))
  (set! A1 (gaddr-normalize A1))
  (set! A2 (gaddr-normalize A2))
  (set! NPATH1 (gaddr-npath A1))
  (set! HOST1 (gaddr-host A1))
  (set! NPATH2 (gaddr-npath A2))
  (set! HOST2 (gaddr-host A2))
  (set! PREF (npath-prefix NPATH1 NPATH2))
 ;(outraw* "  A1=" A1 "; A2=" A2 "\n")
 ;(outraw* "  P1=" NPATH1 "; H1=" HOST1 "\n")
 ;(outraw* "  P2=" NPATH2 "; H2=" HOST2 "\n")
 ;(outraw* "  PREF=" PREF "\n")
  (if (== NPATH1 NPATH2)
    (post)
    (if (or (nil? PREF)
            (not (nil? (npath-minus NPATH1 PREF))))
      (up)
      (down)))
  (gaddr NPATH HOST))

(define (gaddr-up ADDR)
  (gaddr-next ADDR (npath-first (gaddr-npath ADDR))))

;; Physical OS-allocated (possibly agglomerated) proc
(define tprocph0 (type "procph0"
                       '(GADDR    ;; Physical address of the current proc: VGADDR[:PROCNO]
                         INCHAN   ;; Server channels (incoming)
                         NONCE    ;; Next nonce
                         AWANSWS  ;; All nonces for which we await an (asynchronous) answer
                         ACTIONH  ;; Action handler (in the loop)
                         RECVH    ;; Receiving handler (per message)
                         EXTH     ;; Extended handler (additional actions per message)
                         IDLEH    ;; Idle handler
                         PROCPH   ;; The high-level avatar of a procph0 (aka. procph) [TODO: reunite later with procph0].
                        )))

(define (procph0? PROC)
  (== (typeof PROC) tprocph0))

(define (procph0 . PARM)
  (define RES (rexpr tprocph0 (list-group PARM)))
  (define PROCID Void)
  (define BIND Void)
  (:? RES 'INCHAN (empty))
  (:? RES 'NONCE 0)
  (:? RES 'AWANSWS (empty))
  (set! PROCID (<- RES 'PROCID))
  (if (specified? PROCID)
  (begin
    (if (specified? (: RES 'GADDR))
      (error "procph0::PROCID"))
    (:= RES 'GADDR (gaddr _VMACHINE_GADDR PROCID))))
  (set! BIND (<- RES 'BIND))
  (if (specified? BIND)
    (procph0-bind RES (if (symbol? BIND) BIND Void)))
  RES)

(define (procph0-bind PROC SOCKMODE)
  (define CHAN Void)
  (if (not (procph0? PROC))
    (error "procph0-bind" (typeof PROC)))
  (if (unspecified? (: PROC 'GADDR))
    (error "procph0-bind::no GADDR" (: PROC 'GADDR)))
  (set! CHAN (channel-srv (: PROC 'GADDR) PROC)) ;; TODO: check that it doesn't already exists
  (rcons (: PROC 'INCHAN) CHAN)                  ;; FIXME: previous line: don't bind only on PROC.GADDR
  (if (specified? SOCKMODE)
    (:= CHAN 'MODE SOCKMODE))) ;; TODO: check that SOCKMODE is actually a valid mode name

;; All procph0s
(define _PORT0 10002)     ;; Port of proc 00
(define _PROCPH0 (empty))

;; Global context (current procph0)
(define _CURPROCPH0 Nil)
(define (current-procph0)
  _CURPROCPH0)

(define (current-procph0! PROC)
  (if (not (or (nil? PROC) (== (typeof PROC) tprocph0)))
    (error "current-procph0! : not a procph0" (typeof PROC)))
  (set! _CURPROCPH0 PROC))

;; Chmsgs
(define tchmsg (type "chmsg"
                     '(MODE     ;; Async, Async*, Sync (Async & Async*: standard main loop ; Sync: accept() & handshaken)
                       FROM     ;; Physical address of the socket's sending endpoint (sender proc)
                       TO       ;; Physical address of the socket's receiving endpoint (recipient proc)
                       _FROM    ;; Physical address of the socket's sending endpoint (relay)
                       _TO      ;; Physical address of the socket's receiving endpoint (relay)
                       MSG      ;; The payload
                      )))

(define (chmsg? MSG)
  (== (typeof MSG) tchmsg))

(define (chmsg . PARM)
  (define RES (rexpr tchmsg (list-group PARM)))
  RES)

(define (chmsg-oob FROM TO _FROM _TO)
  (define RES (string+ FROM ";" TO ";" _FROM ";" _TO))
  (string+ (string (string-length RES)) "#" RES))

(define (rawmsg->chmsg STR)
  (define L (string-split STR #\#))
  (define LEN Void)
  (define HEAD Void)
  (define PAYLOAD Void)
  (set! LEN (number (car L)))
  (set! PAYLOAD (substring STR (+ (string-length (car L)) 1) (string-length STR)))
  (set! HEAD (substring PAYLOAD 0 LEN))
  (set! PAYLOAD (substring PAYLOAD LEN (string-length PAYLOAD)))
  (set! L (string-split HEAD #\;))
  (if (!= (list-length L) 4)
    (error "rawmsg->chmsg"))
  (chmsg 'FROM (car L) 'TO (cadr L) '_FROM (caddr L) '_TO (cadddr L) 'MSG PAYLOAD))

;; Channels
(define tchannel (type "channel"
                       '(CATEG     ;; Client (outcoming), Server, SrvCli (incoming socket)
                         MODE      ;; Async, Async*, Sync (Async & Async*: standard main loop ; Sync: accept() & handshaken)
                         PROTO     ;; Protocol: Binary, Text (newline-separated text), HTTP
                         _FROM     ;; Physical address of the socket's sending endpoint (IP)
                         FROM      ;; Physical address of the socket's sending endpoint (proc)
                         TO        ;; Physical address of the socket's receiving endpoint (proc)
                         KEEP      ;; Kept channel (long polling)
                         BLOCKING  ;; Blocking channel (long polling)
                         SOCK      ;; Socket (server socket for Server channels, client socket for Client channels)
                         INCHAN    ;; Incoming (client) channels (Server channels only)
                         PARENT    ;; Parent (the Server it belongs to, i.e., it appears in its parent's INCHAN)
                         CURCHAN   ;; Current channel (Server channels only)
                         PROC      ;; Process (Server channels only)
                        )))

(define (channel? CH)
  (== (typeof CH) tchannel))

(define (channel . PARM)
  (define RES (rexpr tchannel (list-group PARM)))
  (:? RES 'KEEP False)
  (:? RES 'BLOCKING True)
  (:? RES 'INCHAN (empty))
  (:? RES 'PARENT Nil)
  (:? RES 'CURCHAN Nil)
  (:? RES 'PROC Nil)
  RES)

(define (channel-categ CHAN)
  (: CHAN 'CATEG))

(define (channel-srv? CHAN)
  (== (channel-categ CHAN) 'Server))

(define (channel-cli? CHAN)
  (== (channel-categ CHAN) 'Client))

(define (channel-srvcli? CHAN)
  (== (channel-categ CHAN) 'SrvCli))

(define (channel-mode CHAN)
  (: CHAN 'MODE))

(define (channel-mode! CHAN MODE)
  (:= CHAN 'MODE MODE))

(define (channel-async? CHAN)
  (== (channel-mode CHAN) 'Async))

(define (channel-async*? CHAN)
  (== (channel-mode CHAN) 'Async*))

(define (channel-sync? CHAN)
  (== (channel-mode CHAN) 'Sync))

(define (channel-from CHAN)
  (: CHAN 'FROM))

(define (channel-to CHAN)
  (: CHAN 'TO))

(define (channel-keep? CHAN)
  (: CHAN 'KEEP))

(define (channel-busy? CHAN)
  (not (nil? (: CHAN 'CURCHAN))))

(define (channel-parent! CHAN SRV)
  (define PARENT (: CHAN 'PARENT))
  (if (nil? SRV)
    (begin
      (if (nil? PARENT)
        (error "channel-parent!::detach"))
      (:= PARENT 'INCHAN
          (filter (=> (X) (!= X CHAN)) (: PARENT 'INCHAN)))
      (:= CHAN 'PARENT Nil))
    (begin
      (:= CHAN 'PARENT SRV) ;; NOTE: 1st time to satisfy the integrity test above
      (channel-parent! CHAN Nil)
      (rpush (: SRV 'INCHAN) CHAN)
      (:= CHAN 'PARENT SRV))))

(define (channel-srv ADDR . PROC) ;; => Chan[GADDR:PORT PROXIED SRVSOCK INSOCKS]
                                  ;; if !(root?), opens a server socket at the appropriate place in the filesystem
                                  ;; if (root?) opens a server socket on LOCALIP:_PORT0
                                  ;;            if !(proxied?) LOCALIP:_PORT0 will be able to receive external messages
                                  ;;            if  (proxied?), we only have the incoming kept sockets
                                  ;;            in any case, the incoming kept sockets exist, when:
                                  ;;                         => they stem from subscribes (incoming kept sockets) ;
                                  ;;                         => they stem from accepts: during the time they are used
                                  ;;                                                    to communicate and are not yet closed ;
  (define RES (channel 'CATEG 'Server
                       'MODE 'Async*
                       'TO (gaddr-normalize ADDR)))
  (define TO Void)
  (set! TO (gaddr-naddr (: RES 'TO)))
  (catch ;; TODO: move that inside (sock-srv)
    True (=> ()
           (:= RES 'SOCK (sock-srv TO)))
         (=> (E . OPT)
           (if (_naddr-path? TO)
             (if (fexists? (naddr-path TO)) ;; TODO: retrofit that (along with the exception handler) into (sock-srv)
             (begin
               (file-delete (naddr-path TO))
               (:= RES 'SOCK (sock-srv TO)))))))
  (if (not (empty? PROC))
    (:= RES 'PROC (car PROC)))
  RES)

(define (channel-connect SRV CLI) ;; [Not sure we implement this] : connects a Server channel to its (accept)ed incoming
   ;Or: (channel-accept SRV CLI)  ;; connection CLI ; (channel-read) takes from this one only ; if we disconnect, then it
                                  ;; takes from all, and accepts new connections from the serversocket ; there is a way to
                                  ;; obtain the currently accepted socket (for resuming the conversation later).
  Void)

(define (channel-srvcli TO . MODE) ;; => SrvCliChan
  (define RES (channel 'CATEG 'SrvCli
                       'MODE 'Async*
                       'TO TO))
  (if (not (empty? MODE))
    (:= RES 'MODE (car MODE)))
  RES)

(define (channel-subscribe ADDR) ;; => Void
  Void)

(define (_chfrom)
  (define PROC (current-procph0))
  (if (nil? PROC)
     (string+ (gaddr (npath-up _VMACHINE_GADDR) ":00"))
     (: PROC 'GADDR)))

(define (channel-cli ADDR . MODE) ;; => CliChan (either kept, or either on a !(proxied?), so volatile in the latter case
                                  ;;    A local IP address (i.e. 127.0.0.[1-254]) _cannot_ connect to a nonlocal IP address
                                  ;;                                                          [ other than _PHMACHINE_GADDR
  (define RES Void)
  (define FROM Void)
  (set! RES (channel 'CATEG 'Client
                     'MODE 'Async*
                     'TO (gaddr-normalize ADDR)))
  (set! FROM (_chfrom))
  (:= RES 'FROM FROM)
  (set! ADDR (: RES 'TO))
  (catch True (=> ()
                (:= RES 'SOCK (sock-cli (gaddr-naddr ADDR))))
              (=> (E . OPT)
                (if (== CHAN_LOG 2)
                  (chlog2 (string+ ADDR " [" (gaddr-naddr ADDR) "]") "< " "!"))
                Void))
  (if (not (empty? MODE))
    (:= RES 'MODE (car MODE)))
  RES)

(define (channel-wet? CHAN) ;; => Bool [channel has data]
  Void)

(define (channel-blocking? CHAN) ;; => Bool
  (: CHAN 'BLOCKING))

(define (channel-blocking! CHAN B) ;; => Void [when blocking, does a select inside (read) if SrvChan, otherwise (sock-read)]
  (:= CHAN 'BLOCKING B))

(define (channel-accept CHAN) ;; => SrvCliChan (either kept, or either on a !(proxied?), so volatile in the latter case
  (define RES (channel-srvcli (: CHAN 'TO)))
  (define L Void)
  (define PORT Void)
  (define SOCK Void)
  (set! L (cons (: CHAN 'SOCK)
                (map (=> (CHAN)
                       (: CHAN 'SOCK))
                     (filter specified? (: CHAN 'INCHAN))))) ;; FIXME: fix this wart with (boxed-empty?) or not lists and (map)
  (set! PORT (if (channel-blocking? CHAN)
               (sock-select (map cadr L) '() '())
               (sock-select (map cadr L) '() '() 0 10)))
  (if (not (empty? (car PORT)))
    (begin
      (set! L (filter (=> (SOCK)
                        (== (cadr SOCK) (caar PORT)))
                      L))
      (set! PORT (car L))
      (set! PORT (if (== (: CHAN 'SOCK) PORT)
                   CHAN
                   (car (filter (=> (X) (== X PORT)) (: CHAN 'INCHAN))))))
    (set! PORT Nil))
  (set! SOCK
        (if (not (nil? PORT))
          (sock-accept (: PORT 'SOCK))
          False))
  (if SOCK
    (begin
      (channel-parent! RES CHAN)
      (:= RES 'MODE (: CHAN 'MODE)) ;; TODO: when the mode is at the level of the server channel ; otherwise the client sets this
      (:= RES '_FROM (ipaddr (sock-ip-address SOCK))) ;; NOTE: (sock-ip-address) is only for the 1st time ; TODO: check what
      (:= RES 'SOCK SOCK))                            ;;       we can about it (e.g. error if local address of a remote machine)
    (set! RES False))
  RES)

(define (gpath-unreachable? FROM TO)
  (outraw* TO " " (gaddr-proxied? TO) " " (gaddr-npath TO) " " _PHMACHINE_GADDR "\n")
  (and (gaddr-proxied? TO)
       (not (== (gaddr-npath TO) _PHMACHINE_GADDR))))

(let* ((L (conf-get2 "PROXIED"))) ;; Read PROXIED
  (if (not (pair? L))
    (set! L `(,L)))
  (set! L (map (=> (A) (gaddr-normalize (gaddr-expand (string+ (string A) ":00")))) L))
  (for-each gaddr-proxied! L))

(define (channel-write CHAN MSG . OPT) ;; => Void ; adds FROM, TO, NONCE, ASK, SYNC]
                                       ;;           if ISANSW, it must be the right FROM+TO, and an answer to an NONCE to
                                       ;;                      which there have been no answers yet
  (define FROM (: CHAN 'FROM))
  (define TO (: CHAN 'TO))
  (define FROM_ Void)
  (define TO_ Void)
  (cond ((channel-srv? CHAN)
         (if (not (channel-busy? CHAN))
           (error "channel-write::srv+!busy"))
         (apply channel-write `(,(: CHAN 'CURCHAN) ,MSG . ,OPT)))
        ((list-in? (: CHAN 'CATEG) '(Client SrvCli))
         (if (== (: CHAN 'CATEG) 'SrvCli)
         (begin
           (set! FROM TO)
           (set! TO (: CHAN 'FROM))))
         (set! FROM_ FROM)
         (set! TO_ TO)
         (if (not (empty? OPT))
         (begin
           (set! FROM_ (car OPT))
           (set! TO_ (cadr OPT))))
         (sock-write (: CHAN 'SOCK) (chmsg-oob FROM_ TO_ FROM TO) 0)
         (sock-write (: CHAN 'SOCK) MSG))
        (else
         (error "channel-write::CATEG"))))

(define (channel-send ADDR MSG . OPT) ;; => Void
  (define CLI Void)
  (define FROM_ Void)
  (define TO_ Void)
  (if (chmsg? MSG)
    (begin
      (set! FROM_ (: MSG 'FROM))
      (set! TO_ (: MSG 'TO))
      (if (and (specified? ADDR) (!= TO_ ADDR))
        (error "channel-send")
        (set! ADDR TO_))
      (set! MSG (: MSG 'MSG)))
    (begin
      (set! FROM_ (_chfrom))
      (set! TO_ ADDR)))
  (set! FROM_ (gaddr-normalize FROM_))
  (set! TO_ (gaddr-normalize TO_))
  (set! ADDR (gaddr-next (_chfrom) TO_))
  (set! CLI (channel-cli ADDR 'Async))
  (if (or (unspecified? (: CLI 'SOCK))
          (gpath-unreachable? FROM_ ADDR))
    (begin
      (if (== CHAN_LOG 2) ;; FIXME: move that test into (channel-cli) (otherwise it still tries to connect before we reach here)
        (chlog2 (string+ MSG " [" (gaddr-shorten FROM_) "=>" (gaddr-shorten TO_) "] via " (gaddr-shorten ADDR)) "< " "!"))
      Void)
    (begin
      (channel-write CLI MSG FROM_ TO_)
      (channel-eof! CLI))))

(define (channel-read CHAN) ;; => String <> False (no data) ; does (accept)+(read) at once => cli always does (connect)+(write)
                            ;;                                if (proxied?), does (read) on the first wet incoming kept socket
                            ;; if CliChan, one only reads on its socket
                            ;; decodes FROM+TO, NONCE, ASK&SYNC, and does what is appropriate
  (define FROMNP Void)
  (define MSG Void)
  (define CATEG (: CHAN 'CATEG))
  (define CLI Void)
  (cond ((== CATEG 'Server)
         (if (channel-busy? CHAN)
           (begin
             (if (not (channel-sync? CHAN))
               (error "channel-read::!sync busy"))
             (set! CLI (: CHAN 'CURCHAN))) ;; TODO: improve this: one active conversation at a time, plus a number of KEEPs
           (set! CLI (channel-accept CHAN)))
         (if (channel-sync? CHAN)
           (:= CHAN 'CURCHAN CLI))
         (set! MSG (channel-read CLI))
         (if (eof-object? MSG)
           (channel-eof! CHAN))
         MSG)
        (else
         (set! FROMNP (ipaddr (sock-ip-address (: CHAN 'SOCK))))
         (set! MSG (sock-read (: CHAN 'SOCK)))
         (if (== MSG "Unspecified")
           (sock-write (: CHAN 'SOCK) "1234")) ;; FIXME: temporary fix ; remove this asap
         (if (and (not (eof-object? MSG))
                  (!= MSG "Unspecified")) ;; FIXME: temporary fix ; remove this asap
           (begin
             (set! MSG (rawmsg->chmsg MSG))
             (if (and (== (: CHAN 'CATEG) 'SrvCli) (unspecified? (: CHAN 'FROM)))
               (:= CHAN 'FROM (: MSG '_FROM))) ;; TODO: manage combining proxied addresses (by meand of FROMNP)
                                               ;; TODO: raise an error if the announced address definitely cannot fit with FROMNP
          ;; TODO: set CHAN.MODE the first time (if there is one)
             Void))
         (if (== (: CHAN 'MODE) 'Async)
           (channel-eof! CHAN))
         MSG)))

(define (channel-close CHAN) ;; => Void
  (if (not (nil? (: CHAN 'PARENT)))
    (channel-parent! CHAN Nil))
  (sock-close (: CHAN 'SOCK)))

(define (channel-eof! CHAN) ;; => Void
  (define CLI Void)
  (cond ((channel-srv? CHAN)
         (if (not (channel-busy? CHAN))
           (error "channel-eof!::srv+!busy"))
         (set! CLI (: CHAN 'CURCHAN))
         (:= CHAN 'CURCHAN Nil)
         (channel-eof! CLI))
        (else
         (if (channel-keep? CHAN)
           Void ;; TODO: we don't close ; so send an actual EOF object
           (channel-close CHAN)))))

;; (channel-touch)
(define (channel-touch ADDR . FETCH)
  (define RES Void)
  (define SOCK (channel-cli ADDR))
  (set! FETCH (if (empty? FETCH)
                Void
                (car FETCH)))
  (if (specified? (: SOCK 'SOCK))
    (begin
      (if (specified? FETCH)
        (begin
          (channel-write SOCK "Unspecified")
          (set! RES (channel-read SOCK)))
        (set! RES True))
      (channel-close SOCK)))
  RES)

;; Chlog
(define (chlog OBJ) ;; FIXME: (outraw), (cr), etc., do not work well with redirects (1)
  (define (addr VAR)
    (gaddr-shorten (: OBJ VAR)))
  (cond ((channel? OBJ)
         (outraw (: OBJ 'CATEG))
         (outraw " ")
         (outraw (: OBJ 'MODE))
         (outraw " ")
         (outraw (: OBJ 'FROM))
         (outraw " ")
         (outraw (: OBJ 'TO)))
        ((chmsg? OBJ)
         (outraw (: OBJ 'MSG))
         (outraw " [")
         (outraw (addr 'FROM))
         (outraw "=>")
         (outraw (addr 'TO))
         (outraw "]")
         (outraw " {")
         (outraw (addr '_FROM))
         (outraw "=>")
         (outraw (addr '_TO))
         (outraw "}"))
        ((string? OBJ)
         (outraw OBJ)) ;; FIXME: not nice
        (else
         (>> OBJ)))) ;; TODO: display regular calls in a more concise manner

(define (chlog2 MSG . PREF)
  (define ERR (if (and (not (empty? PREF)) (> (list-length PREF) 1))
                (cadr PREF) Void))
  (set! PREF (if (not (empty? PREF))
               (car PREF) Void))
  (rawouts 1)
  (with-output-to-port
    (current-error-port)
    (=> ()
      (if (specified? PREF)
        (outraw PREF)) ;; FIXME: (outraw), (cr), etc., do not work well with redirects (2)
      (if (specified? ERR)
        (begin
          (color-red)
          (outraw ERR)
          (color-white)))
      (chlog MSG)
      (cr)
      (force-output)))
  (rawouts 0))

;; Logs
(define (com-log LEVEL) ;; TODO: move that below socks.ss, in such a way that it can be used there.
  (define LOGS (conf-get "COM_LOG"))
  (define RES Void)
  (if (specified? LOGS)
    (set! RES (: LOGS LEVEL)))
  (set! RES (if (specified? RES)
              (number RES)
              False))
  RES)

(define CHAN_LOG (com-log "chan"))

;; Procph0's main loop
(define (procph0-reroute PROC MSG)
  (define RES False)
  (if (!= (: MSG 'TO) (: PROC 'GADDR))
  (begin
    (channel-send Void MSG)
    (set! RES True)))
  RES)

(define (procph0-start PROC)
;; 2 incoming sockets:
;;   IN00:: _VMACHINE_GADDR:_PORT0 <= si SUBM==0, _PHMACHINE_LADDR:_PORT0, sinon 127.0.0.SUBM:_PORT0
;;                                 => UP:   faire les subscribe si IN00 est proxied (si SUBM==0 ; sinon on relaye vers IN00/0)
;;                                 => DOWN: relayer les messages vers le bon IN00
;;   IN01:: socks/SUBM/0           => UP:   aller vers IN00/SUBM ; allouer les PROCNO, et installer le demon 0 si ya personne
;;                                 => DOWN: relayer les messages vers le bon IN01
;;
;; while select _PROC_INCHAN
;; => IN00 && proxy  => renvoyer l'IP du proxy
;;    IN00
;; => IN01 && procno => allouer un procno
;;
;; 
  (define SRV (car (: PROC 'INCHAN))) ;; TODO: (select) the appropriate (wet) server channel
  (define RECVH Void)
  (define EXTH Void)
  (define IDLEH Void)
  (define MSG Void)
  (set! RECVH (: PROC 'RECVH))
  (set! EXTH (: PROC 'EXTH))
 ;(set! IDLEH (: PROC 'IDLEH)) ;; TODO: manage IDLEH by means of (select) & nonblocking socks
  (while True
    (set! MSG (channel-read SRV))
    (if CHAN_LOG
      (chlog2 MSG ">  "))
    (if (and (chmsg? MSG) ;; FIXME: temporary fix ; remove this asap
             (not (procph0-reroute PROC MSG)))
      (begin
        (if (specified? RECVH)
          (RECVH MSG))
        (if (specified? EXTH)
          (EXTH MSG))))))
