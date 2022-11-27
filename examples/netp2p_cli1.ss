(import ../src/runtime)

(define UID "PR1")
(define L (command-line))
(if (>= (list-length L) 2)
  (set! UID (list-ref L 1)))

(netp2p-net-enter UID)
(define L (netp2p-net-resolve UID))
(out L)
(cr)
