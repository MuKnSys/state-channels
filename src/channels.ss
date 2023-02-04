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

;; Addresses
(define (ipaddr->vector IP)
  (define RES (make-vector 4))
  (define L (string-split IP #\.))
  (vector-set! RES 0 (number (car L)))
  (vector-set! RES 1 (number (cadr L)))
  (vector-set! RES 2 (number (caddr L)))
  (vector-set! RES 3 (number (cadddr L)))
  RES)

(define (ipaddr-private? IP)
  (define V (ipaddr->vector IP))
  (or (== (vector-ref V 0) 10)       ;; Class A
      (and (== (vector-ref V 0) 172) ;; Class B
           (>= (vector-ref V 1) 16)
           (<= (vector-ref V 1) 31))
      (and (== (vector-ref V 0) 192) ;; Class C
           (== (vector-ref V 1) 168))))

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
        ((string? IP) ;; TODO: check that it is actually an IP
         IP)
        (else
         Void)))

;; Addrs ; TODO: remove calls to those from the code, fuse them with those below, & throw them away 
(define (addr-machine ADDR)
  (define RES (car (string-split ADDR #\:)))
  (if (!= RES "")
    RES
    Void))

(define (addr-netm ADDR)
  (define ADDRM (addr-machine ADDR))
  (if (string? ADDRM)
    (car (string-split ADDRM #\/))
    Void))

(define (addr-subm ADDR)
  (define ADDRM (addr-machine ADDR))
  (define L Void)
  (if (string? ADDRM)
    (begin
      (set! L (string-split ADDRM #\/))
      (if (> (list-length L) 1)
        (cadr L)
        "0"))
    "0"))

(define (addr-host ADDR) ;; Hosts == OS-allocated processes (i.e. procph0s)
  (define L (string-split ADDR #\:))
  (if (> (list-length L) 1)
    (cadr L)
    Void))

;; GAddrs
;; [PROXY*]CORE/SUBM:HOST
;;
(define (gaddr-proxied? ADDR)
  (define L (string-split (gaddr-netm ADDR) #\*))
  (> (list-length L) 1))

(define (gaddr-netm ADDR)
  (addr-netm ADDR))

(define (gaddr-proxy ADDR)
  (define L (string-split (gaddr-netm ADDR) #\*))
  (if (> (list-length L) 1)
    (car L)
    Void))

(define (gaddr-core ADDR)
  (define L Void)
  (set! ADDR (gaddr-netm ADDR))
  (if (string? ADDR)
    (begin
      (set! L (string-split ADDR #\*))
      (if (> (list-length L) 1)
        (set! ADDR (cadr L)))))
  ADDR)

(define (gaddr-subm ADDR)
  (addr-subm ADDR))

(define (gaddr-host ADDR) ;; Hosts == OS-allocated processes (i.e. procph0s)
  (addr-host ADDR))

(define (gipaddr PROXY IP)
  (define PROXIED (and (specified? PROXY) (!= PROXY IP)))
  (string+ (if PROXIED (string+ PROXY "*") "") IP))

(define (gaddr GIP PROCID . SUBM)
  (set! SUBM (if (empty? SUBM) "" (car SUBM)))
  (if (unspecified? SUBM)
    (set! SUBM "0"))
  (string+ GIP (if (string? SUBM) (string+ "/" SUBM) "")
               (if (unspecified? PROCID) "" (string+ ":" PROCID))))

(define (gaddr-npath ADDR . FILLIN) ;; GADDR/SUBM:PROCID => 127.0.0.SUBM:_PORT0 ou _PHMACHINE_LADDR:_PORT0 si 00
                                    ;;                   => PATH dans le filesystem sinon
  (define CORE (gaddr-core ADDR))
  (define SUBM (gaddr-subm ADDR))
  (define PROCID (gaddr-host ADDR))
  (if (not (empty? FILLIN))
    (set! FILLIN (car FILLIN)))
  (if (unspecified? CORE)
    (if FILLIN
      (set! CORE (gaddr-core _VMACHINE_GADDR))
      (error "gaddr-npath::CORE " ADDR)))
  (if (unspecified? SUBM)
    (error "gaddr-npath::SUBM " ADDR))
  (if (unspecified? PROCID)
    (error "gaddr-npath::PROCID " ADDR))
  (if (== PROCID "00")
    (if (== SUBM "0")
      (string+ CORE ":" (string _PORT0))
      (string+ "127.0.0." SUBM ":" (string _PORT0)))
    (string+ "./" (gaddr-subm ADDR) "/" PROCID)))

(define (gaddr-normalize ADDR) ;; 127.0.0.SUBM:PROCID => GADDR/SUBM:PROCID
  (define CORE (gaddr-core ADDR))
  (define SUBM (gaddr-subm ADDR))
  (define PROCID (gaddr-host ADDR))
  (if (unspecified? CORE)
    (set! CORE (gaddr-core _VMACHINE_GADDR)))
  (set! ADDR
        (if (== (substring CORE 0 8) "127.0.0.") ;; FIXME: refine this test
          (begin
            (if (specified? SUBM)
              (error "gaddr-normalize::SUBM[local]"))
            (gaddr (gaddr-netm _PHMACHINE_GADDR) PROCID (substring CORE 8 (string-length CORE))))
          (gaddr CORE PROCID SUBM)))
  ADDR)

;; Machines
(define _PHMACHINE_LADDR (ownip))  ;; Local IP address of the physical machine
(define _PHMACHINE_GADDR           ;; Global IP address of the physical machine ([PROXY*]LADDR)
        _PHMACHINE_LADDR)          ;; FIXME: depend de si on peut savoir qu'on est derriere un proxy ou non
(define _VMACHINE_GADDR            ;; Physical address of the machine: GADDR/SUBM ; (can also be 127.0.0.SUBM)
        (conf-get "MACHINE"        ;; TODO: check that it's actually a correct IP (_PHMACHINE_GADDR or 127.0.0.XY)
                  (addr-machine (gaddr-normalize _PHMACHINE_GADDR))))

;; Relays
(define (relay-out SRC DEST) ;; SRC & DEST are gaddrs
  (define CORES (gaddr-core SRC))
  (define SUBMS (gaddr-subm SRC))
  (define HOSTS (gaddr-host SRC))
  (define CORED (gaddr-core DEST))
  (define SUBMD (gaddr-subm DEST))
  (define HOSTD (gaddr-host DEST))
  (if (== CORES CORED)
    (if (== SUBMS SUBMD)
      (if (or (== HOSTS HOSTD) (== HOSTS "0"))
        DEST
        (gaddr CORED "0" SUBMD))
      (if (!= HOSTS "00")
        (gaddr CORES "00" SUBMS)
        (gaddr CORES "00" SUBMD)))
    (if (== HOSTS "00")
      (gaddr CORED "00" "0")
      (gaddr CORES "00" SUBMS))))

;; Physical OS-allocated (possibly agglomerated) proc
(define tprocph0 (type "procph0"
                       '(GADDR    ;; Physical address of the current proc: VGADDR[:PROCNO]
                         INCHAN   ;; Server channels (incoming)
                         NONCE    ;; Next nonce
                         AWANSWS  ;; All nonces for which we await an (asynchronous) answer
                         RECVH    ;; Receiving handler (per message)
                         EXTH     ;; Extended handler (additional actions per message)
                         IDLEH    ;; Idle handler
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
    (:= RES 'GADDR (gaddr (gaddr-netm _VMACHINE_GADDR)
                          PROCID
                          (gaddr-subm _VMACHINE_GADDR)))))
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
  (rcons (: PROC 'INCHAN) CHAN)
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

;; Proxied channels
(define _PROXIED (make-hashv-table)) ;; Physical (root) address => CliChans [outcoming kept sockets to (mainly proxied) procs]

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
                       '(CATEG    ;; Client (outcoming), Server, SrvCli (incoming socket)
                         MODE     ;; Async, Async*, Sync (Async & Async*: standard main loop ; Sync: accept() & handshaken)
                         _FROM    ;; Physical address of the socket's sending endpoint (IP)
                         FROM     ;; Physical address of the socket's sending endpoint (proc)
                         TO       ;; Physical address of the socket's receiving endpoint (proc)
                         SOCK     ;; Socket (server socket for Server channels, client socket for Client channels)
                         INSOCK   ;; Incoming (client) sockets
                         PROC     ;; Process (Server channels only)
                        )))

(define (channel? CH)
  (== (typeof CH) tchannel))

(define (channel . PARM)
  (define RES (rexpr tchannel (list-group PARM)))
  (:? RES 'INSOCK (empty))
  (:? RES 'PROC Nil)
  RES)

(define (channel-from CHAN)
  (: CHAN 'FROM))

(define (channel-to CHAN)
  (: CHAN 'TO))

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
  (set! TO (gaddr-npath (: RES 'TO)))
  (catch ;; TODO: move that inside (sock-srv)
    True (=> ()
           (:= RES 'SOCK (sock-srv TO)))
         (=> (E . OPT)
           (if (_npath-path? TO)
             (if (file-exists? (npath-path TO))
             (begin
               (file-delete (npath-path TO))
               (:= RES 'SOCK (sock-srv TO)))))))
  (if (not (empty? PROC))
    (:= RES 'PROC (car PROC)))
  RES)

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
     (string+ _VMACHINE_GADDR ":00")
     (: PROC 'GADDR)))

(define (channel-cli ADDR . MODE) ;; => CliChan (either kept, or either on a !(proxied?), so volatile in the latter case
                                  ;;    A local IP address (i.e. 127.0.0.[1-254]) _cannot_ connect to a nonlocal IP address
                                  ;;                                                          [ other than _PHMACHINE_GADDR
  (define RES (channel 'CATEG 'Client
                       'MODE 'Async*
                       'TO (gaddr-normalize ADDR)))
  (define FROM (_chfrom))
  (:= RES 'FROM FROM)
  (catch True (=> ()
                (:= RES 'SOCK (sock-cli (gaddr-npath ADDR))))
              (=> (E . OPT)
                (error "Can't connect to " ADDR "[" (gaddr-npath ADDR) "]")))
  (if (not (empty? MODE))
    (:= RES 'MODE (car MODE)))
  RES)

(define (channel-accept CHAN) ;; => SrvCliChan (either kept, or either on a !(proxied?), so volatile in the latter case
  (define RES (channel-srvcli (: CHAN 'TO)))
  (define SOCK (sock-accept (: CHAN 'SOCK)))
  (:= RES 'MODE (: CHAN 'MODE)) ;; TODO: when the mode is at the level of the server channel ; otherwise the client sets this
  (:= RES '_FROM (ipaddr (sock-ip-address SOCK))) ;; NOTE: (sock-ip-address) is only for the 1st time ; TODO: check what
  (:= RES 'SOCK SOCK)                             ;;       we can about it (e.g. error if local address of a remote machine)
  RES)

(define (channel-write CHAN MSG . OPT) ;; => Void ; adds FROM, TO, NONCE, ASK, SYNC]
                                       ;;           if ISANSW, it must be the right FROM+TO, and an answer to an NONCE to
                                       ;;                      which there have been no answers yet
  (define FROM (: CHAN 'FROM))
  (define TO (: CHAN 'TO))
  (define FROM_ Void)
  (define TO_ Void)
  (if (not (list-in? (: CHAN 'CATEG) '(Client SrvCli)))
    (error "channel-write::CATEG"))
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
  (set! ADDR (relay-out (_chfrom) TO_))
  (set! CLI (channel-cli ADDR))
  (channel-write CLI MSG FROM_ (gaddr-normalize TO_)))

(define (channel-wet? CHAN) ;; => Bool [channel has data]
  Void)

(define (channel-blocking? CHAN) ;; => Bool
  Void)

(define (channel-blocking! CHAN B) ;; => Void [when blocking, does a select inside (read) if SrvChan, otherwise (sock-read)]
  Void)

(define (channel-read CHAN) ;; => String <> False (no data) ; does (accept)+(read) at once => cli always does (connect)+(write)
                            ;;                                if (proxied?), does (read) on the first wet incoming kept socket
                            ;; if CliChan, one only reads on its socket
                            ;; decodes FROM+TO, NONCE, ASK&SYNC, and does what is appropriate
  (define FROMNP Void)
  (define MSG Void)
  (define CATEG (: CHAN 'CATEG))
  (cond ((== CATEG 'Server)
         (set! CHAN (channel-accept CHAN))
         (channel-read CHAN))
        (else
         (set! FROMNP (ipaddr (sock-ip-address (: CHAN 'SOCK))))
         (set! MSG (sock-read (: CHAN 'SOCK)))
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
  (sock-close (: CHAN 'SOCK)))

(define (channel-eof! CHAN) ;; => Void
  (channel-close CHAN)) ;; FIXME: do that only if CHAN.KEEP ; otherwise, we have to sent an actual EOF object

;; Chlog
(define (chlog OBJ)
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
         (outraw (: OBJ 'FROM))
         (outraw "=>")
         (outraw (: OBJ 'TO))
         (outraw "]")
         (outraw " {")
         (outraw (: OBJ '_FROM))
         (outraw "=>")
         (outraw (: OBJ '_TO))
         (outraw "}"))))

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
  (define SRV (car (: PROC 'INCHAN)))
  (define RECVH Void)
  (define EXTH Void)
  (define IDLEH Void)
  (define MSG Void)
  (set! RECVH (: PROC 'RECVH))
  (set! EXTH (: PROC 'EXTH))
 ;(set! IDLEH (: PROC 'IDLEH)) ;; TODO: manage IDLEH by means of (select) & nonblocking socks
  (while True
    (set! MSG (channel-read SRV))
    (if (and (chmsg? MSG) ;; FIXME: temporary fix ; remove this asap
             (not (procph0-reroute PROC MSG)))
      (begin
        (if (specified? RECVH)
          (RECVH MSG))
        (if (specified? EXTH)
          (RECVH MSG)))
      (begin
        (chlog MSG)
        (cr)))))
