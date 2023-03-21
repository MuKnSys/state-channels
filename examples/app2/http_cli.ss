(import ./http)
(import ../../src/cli)
(import ../../src/apimon)

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
(apimon "func1" (=> (N S)
                  (out N)(spc)(out S)
                  (cr)
                  123456)
                '(num str))

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
