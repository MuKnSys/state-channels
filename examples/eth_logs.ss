;; eth_logs.ss
;; Henri Lesourd, December 2022
;;
(import ../src/runtime)

;; Proc
(define (proc-log LOG)
  (map (=> (X)
         `(,(strnum-trim (cadr (: X 'topics))) ;; _from => TODO: is that solid ? (1)
           ,(strnum-trim (: X 'address))       ;; _to   => TODO: is that solid ? (2)
           .
           ,(eth-decode-parms (: X 'data))))
       LOG))

;; Displays
(define (lstre R)
  (outraw "=>")(cr)
  (for-each (=> (E)
              (if (or (not (pair? E)) (!= (car E) ':TYPE))
              (begin
                (outraw "    ")
                (>> E)
                (cr))))
            R))

(define (lst L . RE)
  (for-each (=> (E)
              (outraw "  ")
              (if (or (rexpr? E) (not (empty? RE)))
                (lstre E)
                (begin
                  (>> E)
                  (cr))))
            L))

(define (sep S)
  (outraw S)(cr))

;; Parsing the command line
(define L (command-line))

(if (< (list-length L) 2)
(begin
  (outraw "eth-logs <CNAME> expected")
  (exit2)))

(define CNAME (list-ref L 1))

;; Main
(eth-init "~/StateChannels/chain")

(define ACCOUNTS (: (geth "eth_accounts" "[]") 'result))
(outraw "Accounts:\n")
(lst ACCOUNTS)
(sep "")

(eth-create-init "~/StateChannels/a.out")
(define ADDR (eth-addr CNAME))

(outraw (string+ "Parsing the logs of:\n  " CNAME "\n  " ADDR))
(cr)
(sep "")

(define LOGS (eth-getFilterLogs 1 ADDR))
(outraw "Semi-parsed logs:\n")
(lst (proc-log LOGS) 1)
(cr)

(outraw "Raw logs:\n")
(lst LOGS)
(sep "")
