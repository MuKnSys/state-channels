(load "buildlib.scm")

;; TODO:
;; => read the list of files to copy directly from inside the backend's directory contents ;
;; => enable several backends to be built in the same time ;
;; => for commands, have the launcher automatically generated ;
;; => if directories like $SC_HOME/bin doesn't exists, create them automatically ;
;;
(define BACKEND (list-ref (command-line) 1))

(if (or (equal? BACKEND "guile")
        (equal? BACKEND "gerbil"))
  (build "llioruntime.ss"
         "llruntime.ss"
         "socks.ss"
         "scm"))

(if (equal? BACKEND "filesocks_unix")
  (build "files.ss"))

(if (equal? BACKEND "filesocks_dummy")
  (build "llioruntime.ss"
         "files.ss"
         "socks.ss"))

(if (equal? BACKEND "geth_dev")
  (build "eth.ss"
         "eth"
         "solc"))

(if (equal? BACKEND "libp2p_mockup") ;; Broken
  (build "libp2p.ss"))
