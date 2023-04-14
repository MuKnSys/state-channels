(declare (standard-bindings) (extended-bindings))

(include "~~lib/_six/js#.scm")
(include "~~lib/_gambit#.scm")

(include "./gs_prelude.scm")
(include "../../backends/filesocks_dummy/llioruntime.ss")
(include "../../src/basics.ss")
(include "../../backends/filesocks_dummy/files.ss")
(include "../../src/rexpr.ss")
(include "../../backends/filesocks_dummy/socks.ss")
(include "../../src/json.ss")
(include "../../src/inits.ss")
(include "../../src/channels.ss")
(include "../../src/aliases.ss")
(include "../../src/scheds.ss")
(include "../../src/procs.ss")
(include "../../src/accounts.ss")
(include "../../src/ipc.ss")
(include "../../src/calls.ss")
(include "../../src/procl.ss")
(include "../../src/procg.ss")
(include "../../src/proch.ss")
(include "../../src/procph.ss")
(include "../../src/cli.ss")
(include "../../src/apimon.ss")
(include "../../examples/sc0.ss")
(include "../../examples/mp1.ss")
(include "../../examples/mp2.ss")
(include "../../examples/micropay.ss")

;; (init-tty)
(define (js_out X)
  \global.out(`X))

(define (js_color C)
  \global.color(`C))

(init-tty 
  (=> (X)
    (define OUT (open-output-string))
    (display X OUT)
    (js_out (get-output-string OUT)))
  (=> (X)
    (define OUT (open-output-string))
    (write X OUT)
    (js_out (get-output-string OUT)))
  (=> ()
    (js_out "\n")))

(init-tty-colors
  (=> ()
    (js_color "red"))
  (=> ()
    (js_color "black")))

\sc_cr=`cr

;; (tcli.cmd)
\sc_cmd=`(=> (TAG PARM)
           (set! PARM (vector->list PARM))
           (^ 'cmd APIMON TAG PARM))

;; (init-ipc)
(init-ipc
  (=> (UID MSG)
    \net_out(`UID,`MSG)))

\net_post=`net-post

;; Start
(thread-sleep! +inf.0)
