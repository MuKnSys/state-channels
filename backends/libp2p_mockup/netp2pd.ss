; netp2pd.ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (November 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

;; NOTE: to be reimplemented in node or go (with the _same_ sexpr-based protocol),
;;       because there is no libp2p implementation that enables talking directly
;;       to a libp2p network from Scheme.
;; NOTE:
;;  => when linked to the mockup libp2p library, should run on a server, having
;;     a well-defined IP:PORT address, in a centralized fashion ;
;;  => when linked to a real libp2p library, should run on a local port, and
;;     act as a gateway to a real libp2p peer network ;
;;
(import ./runtime)

;; NetP2P-based naming daemon et al.
(define (handler MSG)
  (define RES Void)
  (>> MSG)
  (cr)
  (set! RES (cond ((== (car MSG) 'net-enter)
                   (_netp2p-net-enter (cadr MSG) (caddr MSG)))
                  ((== (car MSG) 'net-leave)
                   (_netp2p-net-enter (cadr MSG)))
                  ((== (car MSG) 'net-resolve)
                   (_netp2p-net-resolve (cadr MSG)))
                  ((== (car MSG) 'net-send)
                   (_netp2p-net-send (cadr MSG)))
                  ((== (car MSG) 'net-dispatch)
                   (_netp2p-net-dispatch (cadr MSG) (caddr MSG)))
                  (else
                    Void)))
  (outraw "=> ")
  (out RES)
  (cr)
  RES)

(define (netp2pd-main)
  (define ADDR (netp2pd-addr))
  (define SRV (sock-srv ADDR))
  (while True
    (let* ((SOCK (sock-accept SRV)))
     ;(outraw "New client: ")
     ;(out (sock-details SOCK))
     ;(cr)
     ;(outraw "Address: ")
     ;(out (sock-address SOCK))
     ;(cr)
      (sock-write SOCK (sexpr-serialize
                         (handler
                           (sexpr-parse (sock-read SOCK)))))
      (sock-close SOCK))))

(netp2p-connect _NETP2PD-ADDR)
(netp2pd-main)
