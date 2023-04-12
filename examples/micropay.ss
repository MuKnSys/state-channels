; micropay.ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (April 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ../src/runtime)
(import ../src/cli)
(import ../src/apimon)
(import ./mp2)

(export
  (import: ./mp2))

;; Micropays
(define tsc_micropay (type `("sc_micropay" ,tstatech) ;; State channel for micropayments
                           '()))

;; Constructor
(define (_sc_micropay ISCORE NAME . PETNAMES)
  (define GR (apply _gr `(,NAME ,cmicropay ,micropay . ,PETNAMES)))
  (define CPR (current-proc))
  (:= GR 'TYPE tsc_micropay)
  (if (not (nil? CPR))
    (begin
      (set! CPR (if (account? CPR) 'NAME 'USER))
      (_gre NAME)
      (for-each (=> (NM)
                  (if (!= NM NAME)
                    (^ 'endpoint GR NM)))
                PETNAMES)))
  (if (not ISCORE)
    (:= (: GR 'PARENT) 'ROLE 'Mapping)) ;; FIXME: doesn't work ; when created before, GR.PARENT can't be a pure mapping ; and we must create it with a self being an instance of cmicropay, because of the methods peerId & peerInit that are used in (procg::endpoint)
  GR)

(define (sc_micropay NAME . PETNAMES)
  (apply _sc_micropay `(,True ,NAME . ,PETNAMES)))

(define (sc_recv_micropay NAME . PETNAMES)
  (apply _sc_micropay `(,False ,NAME . ,PETNAMES)))

;; Basic methods
(method! tsc_micropay 'deposit (=> (MP AMOUNT)
  (^ 'send MP 'deposit AMOUNT)
  (step) ;; FIXME: doesn't do the full RSM immediately, even with autorun 1 ; needs one more step
  Void))

(method! tsc_micropay 'transfer (=> (MP USER AMOUNT)
  (^ 'send MP 'transfer USER AMOUNT)
  (step) ;; FIXME: cf. in (deposit) above
  Void))

(method! tsc_micropay 'withdraw (=> (X)
  Void))

(method! tsc_micropay 'close (=> (X)
  Void))

(method! tsc_micropay 'panic (=> (X)
  Void))

;; Global methods (that pertain to sc_lib) ; TODO: move them there
(define (sc_lib_account! NAME PASS UID)
  (define ACC (account 'NAME NAME 'PASSWORD PASS 'UID UID))
 ;(pralias! (: ACC 'UID) "ACC")
  ACC)

;; Corresponding CLI commands
(apimon "channel" sc_micropay '(str str str str str str str) 'VARGS True)
(apimon "recvg" sc_recv_micropay '(str str str str str str str) 'VARGS True)

(define (sc_deposit UIDG AMOUNT)
  (define GR (netrsv UIDG))
  (^ 'deposit GR AMOUNT))
(apimon "deposit" sc_deposit '(str num))

(define (sc_transfer UIDG USER AMOUNT)
  (define GR (netrsv UIDG))
  (^ 'transfer GR USER AMOUNT))
(apimon "transfer" sc_transfer '(str sy num))

(apimon "account" sc_lib_account! '(str str str))

;; Creating default accounts
(sc_lib_account! "Bob" "1234" "0xa16979a982b94200d61aede91f6cf2a0c0ac3613x")
(sc_lib_account! "Carol" "5678" "0xfe9d3038aa1e064e4bca147fa978dece561b91f1x")
(sc_lib_account! "Dave" "9abc" "0x51569535b832588d346d80af67c3341088cfd8fcx")
