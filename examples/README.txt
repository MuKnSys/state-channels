  Examples
  (C) MUKN  2022, 2023


1) High-level layer

NOTE: except the 1st one, all the examples below show various parts of the
      micropayments' processing.

1.a) Sending a replicated message (RSM#1)

Open a first console, go to "~:SC" (the directory which contains the downloaded
state channels library's GIT repository), then do:
~:SC>bin/cli sc0 pr1_r

Open another console, go to "~:SC", then do:
~:SC>bin/cli sc0 pr2_r

In the first console, do:
> GR1 ^ set Salut

Then, do:
> netlist SC1

In the second console, do:
> netlist SC2

You obtain (e.g. for SC1, in the first console):
{{
  SC1 bob ^
  in:
    <0 0>set SC1=>GR1 (Salut) (S bob)
    <0 0>set SC1=>GR1 (Salut) (S bob carol)!!
  rl:
    <0 0>set SC1=>GR1 (Salut) (S bob carol)!*
  out:
    <0 _>set SC1=>GR1 (Salut) (S bob)
}}

showing the queues and the messages for the "set" replicated call sent by SC1 to GR1.

=> the Scheme code for the "sc0" state channel class used in this example can be
   found in examples/sc0.ss ;


1.b) mp1_1.ss (RSM#2)

bin/scm examples/mp1_1.ss

This example (written in Scheme) shows a "transfer", micropayment replicated
call between three processes (scheduled inside one physical UNIX process).

I.e., in mp1_1.ss, we have:
{{
................................
(define PR1 (procl 'USER "smith"   ;; Creation of 3 processes, PR1, PR2 and PR3
                   'UID "PR1"
                   'SELF MP1))
(define PR2 (procl 'USER "dupont"
                   'UID "PR2"
                   'SELF MP2))
(define PR3 (procl 'USER "durand"
                   'UID "PR3"
                   'SELF MP3))

..........................................
(define GR1 (proc-group Void PR1 PR2 PR3))  ;; Creation of a state channel "GR1" between them

.......................
;; Doing a micropayment
(current-proc! PR1)
(^ 'send (: PR1 'GROUP) 'transfer 'dupont 5)  ;; Replicated "transfer" call, sent by PR1 to GR1
}}

=> the corresponding output is ~:SC/tests/OUT1_1.txt


1.c) mp1_3.scsh (RSM#3)

~:SC>bin/cli mp1 mp1_3

Similar to the previous one (a replicated "transfer" between 3 processes bound
in a state channel), written as a script, rather than directly as Scheme code.

In this example (like in the previous one), the processes' execution is triggered
step by step, by means of explicit "step" method calls.

That's why the script is a bit long. But it also illustrates how one can,
in our system, obtain a very fine-grained view of an interaction's evolution.

=> the Scheme code for the micropayments' state channel class can be found
   in examples/mp1.ss ;

=> the corresponding output is ~:SC/tests/OUT1


1.d) mp1_3_scheduled (RSM#4)

~:SC>bin/cli mp1 mp1_3_scheduled

Exactly the same as the previous one, but this time, we call the "run" command
to saturate the scheduler, just after the "transfer" message has been sent.

=> the corresponding output is ~:SC/tests/OUT1_scheduled


1.e) mp2_3_scheduled (Synchronizing#1)

~:SC>bin/cli mp2 mp2_3_scheduled

Shows the initialization of a (simulated) contract by means of "deposit" method
calls, and the subsequent "sync" method calls to synchronize the state of the
state channel's local processes with the state of the contract.

=> the Scheme code for simulated contracts can be found in examples/mp2.ss ;

=> the corresponding output is ~:SC/tests/OUT2_scheduled


1.f) mp3_1_scheduled (Withdraw#1)

~:SC>bin/cli mp2 mp3_1_scheduled

At first, does the same as the previous one, and then, performs a "transfer" method
call between two participants in the state channel, then a "withdraw".

=> the corresponding output is ~:SC/tests/OUT3_scheduled


1.g) Panic#1

This example is yet to be implemented.


1.g) mp4 (Blockchain#1)

First, the micropayments' contract must have been compiled. To this end, go
to "~:SC" (the directory which contains the downloaded state channels library's
GIT repository), then do:
~:SC>bin/solc examples/mp2.sol

You then need to start the chain. To this end, open a first console, go
to "~:SC", then do:
~:SC>eth start

To see the chain's logs, open another console, go to "~:SC", then do:
~:SC>eth log

Finally, to start the "mp4" example, open another console, go to "~:SC", then do:
~:SC>bin/cli mp1 mp4

This example shows the creation of a "Micropay" contract (the corresponding Solidity
code is in examples/mp2.sol), its initialization (by means of "deposit" method calls),
and how it is mapped in the local memory of a UNIX process (by means of "sync" method
calls, applied to the local "proce" process which is the mapping of the remote contract
in the local memory).


2) communications layer

2.1) Simple client-server interaction

Open a first console, then do:
bin/scm examples/srv1.ss

Open another console, then do:
bin/scm examples/cli1.ss


2.2) A ping/echo server and clients sending echo messages to it

Open a first console, then do:
bin/scm examples/srvping.ss

Open another console, then do:
bin/scm examples/cliping.ss [stop it with ^C after you got the output, then restart another one]

The processes PR1, PR2, ... (i.e., the successive instances of cliping.ss)
send an "echo Coucou" message to the process PING (i.e., srvping.ss), which
simply echoes the message (this can be seen in the output of cliping.ss).

The cliping and srvping programs both make use of the "ping" state channel
class, which can be found in examples/ping.ss.


2.2) An CLI-based interactive client sending echo messages to the ping server

Open a first console, then do:
bin/scm examples/srvping.ss

Open another console, then do:
bin/scm examples/echo_cli.ss

The CLI interactive client is partially broken at the moment (it doesn't prints
the echoed messages).


2.3) Routing in between machines

export NETP2PD_ADDR=127.0.0.1:1234
bin/scm src/netp2pd.ss
bin/scm examples/netp2p_cli1.ss

This example is broken at the moment.
