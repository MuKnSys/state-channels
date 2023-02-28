(load "./runtime.ss")

;; HTTP server
(define _MSGLOG False)
(define (http-msg L)
  (define RES (rexpr Void '()))
  (define L2 Void)
  (define S Void)
  (define (var S)
    (define L (string-split S #\space))
    (set! S (string-downcase (car L)))
    (cond ((== S "host:")
           (:= RES 'SRC (string-trim (cadr L))))
          ((== S "accept:")
           (:= RES 'ACCEPT (string-trim (cadr L))))
          ((== S "content-type:")
           (:= RES 'TY (string-trim (cadr L))))
          ((== S "content-length:")
           (:= RES 'LEN (number (string-trim (cadr L)))))
          (else
           Void)))
  (if _MSGLOG
  (begin
    (out L)
    (cr)))
  (set! L2 (string-split (car L) #\space))
  (:= RES 'METHOD (string-downcase (string-trim (car L2))))
  (set! S (string-trim (cadr L2)))
  (if (and (> (string-length S) 0) (== (string-ref S 0) #\/))
    (set! S (substring S 1 (string-length S))))
  (:= RES 'PATH S)
  (for-each (=> (ELT)
              (var ELT))
            (cdr L))
  RES)

(define _CTYPE Void)
(define (http-ctype! TY)
  (set! _CTYPE TY))

(define (http-answ S)
  (define CTYL (string+ "Content-type: "
                        (if (unspecified? _CTYPE)
                          "text/html; charset=utf-8" ;; TODO: check that we can actually put UTF8
                          _CTYPE)))
  (string+ "HTTP/1.1 200 OK\n"
           CTYL "\n"
           "Connection: closed\n"
           "Content-length: " (string (string-length S)) "\n\n"
           S))

(define HTTP_LOG (boolean (conf-get2 "HTTP_LOG" False)))
(define (http-srv ADDR HANDLER)
  (define ROOT (string+ (dirname (_getcf)) "/"))
  (define (handler MSG)
    (define FPATH Void)
    (if HTTP_LOG
      (begin
        (>> MSG)
        (cr)))
    (cond ((== (: MSG 'METHOD) "get")
           (set! FPATH (string+ ROOT (: MSG 'PATH)))
           (if (file-exists? FPATH)
             (string-join (file-read FPATH 1) "\n")
             (string+ "File '" FPATH "'not found")))
          ((== (: MSG 'METHOD) "post")
           (http-ctype! "application/json")
           (json-serialize (HANDLER (: MSG 'DATA))))
          (else
            "Doesn't understand")))

  (define SRV (sock-srv ADDR))
  (while True
    (let* ((SOCK (sock-accept SRV))
           (S Void)
           (L '())
           (MSG Void))
     ;(outraw "New client: ")
     ;(out (sock-details SOCK))
     ;(cr)
     ;(outraw "Address: ")
     ;(out (sock-address SOCK))
     ;(cr)
      (while (and (not (eof-object? S)) (!= S "")) ;; TODO: if there is a Content-length:, read the contents
        (set! S (sock-read SOCK))
        (if (not (eof-object? S))
          (set! S (string-trim S)))
        (if (and (not (eof-object? S)) (!= S ""))
          (set! L (cons S L))))
      (set! MSG (http-msg (reverse L)))
      (if (specified? (: MSG 'LEN))
        (begin
          (:= MSG 'DATA (json-parse (sock-read-n SOCK (: MSG 'LEN))))))
      (sock-write SOCK (http-answ (handler MSG)))
      (http-ctype! Void)
      (sock-close SOCK))))

;; Init
(loadi "../../examples/mp2.ss") ;; TODO: implement substracting paths (?)
(init0)
(define HOST (proch 'USER 'system
                    'UID "HOST1"))
(current-proch! HOST)                      
(netlist 1)(cr)

;; Start
(apimon "doesNotUnderstand" (=> (CMD PARM)
                              (outraw "Doesn't understand")
                              (cr)
                              (outraw "<= ")(outraw CMD)(outraw " ")(outraw (json-serialize PARM))
                              (cr)1235)
                            '(any any))
(define (handler MSG)
  (define (exec MSG)
    (define TAG (: MSG '_))
    (define PARM (: MSG 'PARM))
    (cond ((specified? TAG)
           (^ 'cmd APIMON TAG PARM))
          (else
           (rexpr Void '(_ "unknown" A 1)))))
  (if (rexpr? MSG)
    (exec MSG)
    (if (pair? MSG)
      (map exec MSG)
      (rexpr Void '(_ "unknown" B 2)))))

(http-srv "127.0.0.1:1234" handler)
