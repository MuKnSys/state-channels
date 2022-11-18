(import ../src/runtime)

(netlist 1)(cr)

(define FSOCK True)
(define ADDR (if FSOCK "./SOCK" "127.0.0.1:1234"))
(define L (command-line))
(if (>= (list-length L) 2)
  (set! ADDR (list-ref L 1)))

(define SOCK (sock-cli ADDR))

(sock-write SOCK "Coucou")

(define L False)
(while (not (eof-object? L))
  (set! L (sock-read SOCK))
  (if (not (eof-object? L))
  (begin
    (outraw L)
    (cr))))
