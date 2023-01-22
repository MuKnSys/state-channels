(load "./runtime.ss")

(netlist 1)(cr)

(define FSOCK True)
(define ADDR (if FSOCK "./SOCK" 1234))
(define L (command-line))
(if (>= (list-length L) 2)
  (set! ADDR (list-ref L 1)))

(define SRV (sock-srv ADDR))
(while True
  (let* ((SOCK (sock-accept SRV)))
    (outraw "New client: ")
    (out (sock-details SOCK))
    (cr)
    (outraw "Address: ")
    (out (sock-address SOCK))
    (cr)
    (out (sock-read SOCK))
    (cr)
    (sock-write SOCK "Hello client")
    (sock-write SOCK "Hello client(2)")
    (sock-close SOCK)))
