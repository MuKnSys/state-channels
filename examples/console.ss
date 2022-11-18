(export #t)
(import ../src/runtime)

;; Console 
(define tconsole (type "console" '() `(
                   print
                   (tconsole str
                   ,(=> (CON MSG)
                      (outraw MSG)
                    ))
                 )))

(define (console)
  (rexpr tconsole '()))
