(load "buildlib.scm")

(define BACKEND (list-ref (command-line) 1))
(if (> (length (command-line)) 2)
  (set! _DIFF_FULL #t))

(if (or (equal? BACKEND "guile")
        (equal? BACKEND "gerbil"))
  (diff*! "llioruntime.ss" ;; TODO: read that directly from DIR's contents
          "llruntime.ss"
          "socks.ss"
          "scm"))

(if (equal? BACKEND "filesocks_unix")
  (diff*! "files.ss"))

(if (equal? BACKEND "filesocks_dummy")
  (diff*! "llioruntime.ss"
          "files.ss"
          "socks.ss"))

(if (equal? BACKEND "geth_dev")
  (diff*! "eth"
          "eth.ss"
          "solc"))

(if (equal? BACKEND "libp2p_mockup")
  (diff*! "libp2p.ss"))
