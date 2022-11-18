(import ../src/runtime)
(import ./libp2p)

(libp2pd '(net-enter "PR1" "12.34.56.78"))
(define L (libp2pd '(net-resolve "PR1")))
(out L)
(cr)
