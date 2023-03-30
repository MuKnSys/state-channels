(load "buildlib.scm")

(define BACKEND (list-ref (command-line) 1))

(if (or (equal? BACKEND "guile")
        (equal? BACKEND "gerbil"))
  (diff*! "llioruntime.ss" ;; TODO: read that directly from DIR's contents
          "llruntime.ss"
          "socks.ss"
          "clish.ss"     ;; TODO: for commands, have the launcher automatically generated
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
