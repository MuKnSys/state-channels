; eth.ss [TODO: turn that to a backend]
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (September 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./rexpr ./json
        ./aliases)

;; Geth (util funcs)
(define (number->hex N)
  (string+ "0x" (number->string N 16)))

(define (strnum-trim N)
  (define HEX (== (substring N 0 2) "0x"))
  (if HEX
    (set! N (substring N 2 (string-length N))))
  (while (and (> (string-length N) 0)
              (== (string-get N 0) #\0))
    (set! N (substring N 1 (string-length N))))
  (if (== (string-length N) 0)
    (set! N "0"))
  (string+ (if HEX "0x" "") N))

(define (bytes32->string S)
  (define L '())
  (define FINISHED False)
  (define CH Void)
  (while (and (not FINISHED) (> (string-length S) 1))
    (set! CH (substring S 0 2))
    (if (== CH "00")
      (set! FINISHED True)
      (set! L (cons CH L)))
    (set! S (substring S 2 (string-length S))))
  (list->string (map (=> (CH)
                       (integer->char (string->number CH 16)))
                     (reverse L))))

;; Geth
(define _ETHDIR Unspecified)
(define (eth-init DIR)
  (set! _ETHDIR (path-normalize DIR)))

(define (eth-execjs JS)
  (sh-cmd (string-append
            "geth --exec '" JS "' attach " _ETHDIR "/data/geth.ipc")))

(define (eth-execcmd PRG . CMD)
  (set! PRG (string-append "loadScript(\"" _ETHDIR "/scripts/" PRG ".js\")"))
  (if (not (null? CMD))
    (set! PRG (string+ PRG " ; " (car CMD))))
 ;(errlog PRG)
  (eth-execjs PRG))

(define (geth FUNC PARMS)
  (define RES (sh-cmd (string-append
                        "curl -H \"Content-Type: application/json\" -X POST --data '{\"jsonrpc\":\"2.0\",\"method\":\""
                        FUNC
                        "\",\"params\":"
                        PARMS
                        ",\"id\":0}' --url 127.0.0.1:8545 2>/dev/null")))
  (if (or (empty? RES) (== (car RES) ""))
    (rexpr Void '(result ()))
    (json-parse (car RES))))

;; Current block number
(define (eth-blockNumber)
  (define RES (: (geth "eth_blockNumber" "[]") 'result))
  (string->number (substring RES 2 (string-length RES)) 16))

(define (eth-waitNBlocks N) ;; FIXME: shitty (?) ; make that work with an interrupt (?)
  (define GOTIT False)
  (define BN0 (eth-blockNumber))
  (define BN1 Void)
  (define I 0)
 ;(outraw "Starting from ")
 ;(outraw BN0)
 ;(cr)
  (while (not GOTIT)
    (set! BN1 (eth-blockNumber))
    (outraw I)
    (outraw " ")
   ;(outraw I)
   ;(outraw ": Block ")
   ;(outraw BN1)
   ;(outraw " reached [")
   ;(outraw BN1)
   ;(outraw " ")
   ;(outraw (+ BN0 N))
   ;(outraw " ")
   ;(outraw (>= BN1 (+ BN0 N)))
   ;(outraw "]")
   ;(cr)
    (set! I (+ I 1))
    (if (>= BN1 (+ BN0 N))
      (begin
        (set! GOTIT True)
        (cr))
      (sleep 1))))

;; Accounts
(define (eth-accounts)
  (: (geth "eth_accounts" "[]") 'result))

;; Create an instance
(define (eth-createInstance ABI CODE)
  (define RCPT (car (eth-execcmd "runtime" (string+ "createInstanceHashOf(" ABI ",\"" CODE "\")"))))
  (define (receipt)
    (geth "eth_getTransactionReceipt" (json-serialize `(,(json-parse RCPT)))))
  (define RES Nil)
  (while (== RES Nil)
    (eth-blockNumber)
    (sleep 1) ;; FIXME: shitty ; make that work well
    (set! RES (: (receipt) 'result)))
  (: RES 'contractAddress))

(define _ETHCODEDIR Unspecified)
(define _ETHALIASES Unspecified)
(define (eth-create-init DIR)
  (set! _ETHCODEDIR (path-normalize DIR))
  (set! _ETHALIASES (aliases (string+ _ETHCODEDIR "/ALIASES"))))
 
(define (eth-addr NAME)
  (^ 'addr _ETHALIASES NAME))

(define (eth-cname ADDR)
  (car (string-split (^ 'alias _ETHALIASES ADDR) #\@)))

(define (eth-nextId CNAME)
  (: (: _ETHALIASES 'ROOT) CNAME))

(define (eth-abi CNAME) ;; TODO: memoize this
  (car (file-read (path-normalize (string+ _ETHCODEDIR "/" CNAME ".abi")) 1)))

(define (eth-bin CNAME)
  (car (file-read (path-normalize (string+ _ETHCODEDIR "/" CNAME ".bin")) 1)))

(define (eth-create CNAME)
  (define ABI (eth-abi CNAME))
  (define BIN (eth-bin CNAME))
  (define ADDR (eth-createInstance ABI (string+ "0x" BIN)))
  (^ 'new! _ETHALIASES CNAME ADDR)
  ADDR)

;; Call a method
(define (_eth-callMethod ABI ADDR FNAME PARM . LOGIN) ;; TODO: _BULLET_ _PROOF_ the parameter checking (indeed there is none, at the moment), otherwise transactions are simply ignored, it's super hard to find what's going on
  (if (null? LOGIN)
    (set! LOGIN "")
    (let* ((USER (car LOGIN))
           (PASS (cadr LOGIN)))
      (set! LOGIN (string+ "," (string USER) ",\"" PASS "\""))))
  (car (reverse (eth-execcmd "runtime" (string+ "callMethod(" ABI ",\""
                                                              ADDR "\",\""
                                                              FNAME "\","
                                                              (json-serialize PARM) LOGIN ")")))))
(define (eth-callMethod CNAME ADDR FNAME PARM . LOGIN)
  (define ABI (eth-abi CNAME)) ;; TODO: cache those
  (apply _eth-callMethod `(,ABI ,ADDR ,FNAME ,PARM . ,LOGIN)))

;; Get the logs of an instance
(define (eth-getFilterLogs FROMB ADDR) ;; TODO: add a filter for transactions
  (define FLT (: (geth "eth_newFilter" (json-serialize `(,(rexpr Unspecified
                                                                 `(fromBlock ,(number->hex FROMB)
                                                                   toBlock ,(number->hex (eth-blockNumber))
                                                                   address ,ADDR))))) 'result))
  (define RES Void)
  (if (unspecified? FLT)
    (error "eth-getFilterLogs(1)"))
  (set! RES (geth "eth_getFilterLogs" (json-serialize `(,FLT))))
  (geth "eth_uninstallFilter" (json-serialize `(,FLT)))
 (: RES 'result))

;; Geth (ABI) => TODO: needs to involve the actual ABI into that
(define (eth-decode-parms X)
  (define L '())
  (define PTR Void)
  (set! X (substring X 2 (string-length X)))
  (while (> (string-length X) 0)
    (set! L (cons (substring X 0 64) L))
    (set! X (substring X 64 (string-length X))))
  (set! L (reverse L))
  (set-car! L (bytes32->string (car L)))          ;; 1st parm (method name) => TODO: make that general (1)
  (set-car! (cdr L) (string->number (cadr L) 16)) ;; 2nd parm (an int)      => TODO: make that general (2)
  (set! PTR (cddr L))
  (for-each (=> (VAL)
              (set-car! PTR (bytes32->string (car PTR))) ;; other parms (strings) => TODO: make that general (3)
              (set! PTR (cdr PTR)))
            (cddr L))
  L)
