(import ../src/runtime)
(import ./console)

;; Main
(outraw "I am process #")
(outraw (: (host-proc) 'HOSTID))
(cr)

(init "durand" "PR" (console))

(define PING (net-map "PING"))
(define S Void)

(^ 'send PING 'attach)
(start 'Once)

(define PREVS Void)
(while True
  (if (specified? PREVS)
  (begin
    (outraw "> ")
    (outraw PREVS)
    (cr)))
  (outraw "> ")
  (set! S (read-line (current-input-port)))
  (cursor-move 'Up)
  (clreol)
  (^ 'send PING 'echo S)
  (start 'Once)
  (set! PREVS S)
)
;; TODO: fork a process that takes the keystrokes, and sends them to 0d ; add a specialized version
;;       of the handler that directly feeds scheme code ; this way, everything will be inside (start),
;;       there won't by anymore 2 polling things which can block, i.e. the (read-line) and the (start).
;;       =>
;;       Verify that we can have that work without ever having to send messages to the keyboard-catching
;;       process. We just ignore its input in case we are changing the focus, or something. Would be nice
;;       to devise ways to reconfigure it dynamically, or at least to stop/restart it (probably if the
;;       exec-ed process is a C program, we can do select() inside, and thus, have it take _two_ inputs,
;;       rather than only one).
