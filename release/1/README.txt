  First release of the state channels library


1) Rationale

This first release of the state channel library (and of an example
that enables seeing it in action) is meant to provide the proof that
the core mechanism of state channels has been fully ported in the
Javascript-in-a-browser-tab environment.

It is based on a simple communication layer, which operates by means
of a server that acts as a mailbox for the messages.

It contains a clean and complete implementation of the main loop in
Javascript, plus the interfaces that are needed to plug the library
to the communication layer and to the main loop ; this interfaces
remain the same when we will reimplement the communication layer
on top of LibP2P.


2) Installation

Follow the installation procedure given at:
https://github.com/MuKnSys/state-channels/blob/main/README.txt


Don't configure the library for the dev Ethereum chain, it's not needed
for the current demo ; but you should run the test, to be sure that
everything went fine.


3) Running the example

The example is a webapp made of two HTML files (plus a number of Javascript
components) ; these files must be published by means of a webserver.

So for example with Apache (supposing that the library has been installed
in the directory /home/henri/tmp/mukn_sc), you would do:

/home/henri/tmp/mukn_sc>sudo bash
[sudo] password for henri:
/home/henri/tmp/mukn_sc>cd /var/www/html
/var/www/html>
/var/www/html>ln -s /home/henri/tmp/mukn_sc/examples/app2 demo1
/var/www/html>
/var/www/html>exit
/home/henri/tmp/mukn_sc>_


Then, start the communication's layer server:

/home/henri/tmp/mukn_sc>bin/scm examples/app2/http_srv0.ss
_


(It displays nothing at the moment: it's normal).


In the browser, open two tabs ; one that points at:
http://localhost/demo1/pr1.html

And another one that points at:
http://localhost/demo1/pr2.html


On each one of these two tabs, in the beginning, the list of existing
endpoints and groups that the peer running in the tab can see appears,
e.g., in the first tab we see:

PID      NAME   USER    PEER           SELF
0^       PR1    bob     ("PR1" "PR2")  @sc0@0
0procg^  GR1    nobody  (_)            _
0h^      HOST1  system  (_)            _
0m^      PR2    carol   ("PR1" "PR2")  _
0ph_     phys   system  (_)            _
5 procs


There are two endpoints PR1 and PR2, and a group/state channel GR1, plus
the host HOST1, and a process which represents the "system", i.e., the
low-level physical host (in this case, the browser tab itself).


Now, inside each one of these two tabs, open the Inspect pane of the browser (we
will use it as a CLI).

Inside each one of the two Inspect panes, do:

run();


These << run(); >> commands start the main loop in each one of the browser tabs.


Inside the Inspect pane of the first tab, do:

netlist("PR1");


And inside the Inspect pane of the second tab, do:

netlist("PR2");


That shows you that inside the first browser tab, the endpoint PR1 currently
did nothing, it sent nor received any message, and that inside the second
browser tab as well, the endpoint PR2 is in the same state, i.e. you should
see, for PR1 (in the first tab):

PR1 bob ^
in:
rl:
out:


And for PR2 (in the second tab):

PR2 carol ^
in:
rl:
out:


Now, inside the inspect pane of the first tab, do:

iam("PR1");
prcall("GR1","set","Salut");


The effect of this is that the endpoint PR1 will send a message "set" to the
group/state channel GR1 which binds the endpoints PR1 and PR2.

So, the group GR1 dispatches the message to PR1 and to PR2, and their states
are kept synchronized by means of the RSM protocol.


Now, if inside the Inspect pane of the first tab, you do:

netlist("PR1");


And if inside the Inspect pane of the second tab, you do:

netlist("PR2");


you now see that the incoming, outcoming and replay lists
of PR1 and PR2 are populated by a number of messages, i.e.
you should see, for PR1 (in the first tab):

PR1 bob ^?
in:
  <0 0>set PR1=>GR1 (Salut) (S bob)
rl:
  <0 0>set PR1=>GR1 (Salut) (S bob)
out:
  <0 _>set PR1=>GR1 (Salut) (S bob)


And for PR2 (in the second tab):

PR2 carol ^
in:
  <0 0>set PR1=>GR1 (Salut) (S bob)
  <0 0>set PR1=>GR1 (Salut) (S bob)!!
rl:
  <0 0>set PR1=>GR1 (Salut) (S bob carol)!*
out:


That's the result of the processing performed by the RSM
protocol.


In the logs of the communication layer's server, you should
see something like:
<<
send  {"USER":"bob","FROM":"PR1","OUTNB":0,"TO":"GR1","TO_":"#!void","PHYSTO":"#!void","INNB":"#!void","FUNC":"'set","PARM":["Salut"],"RESULT":"#!void","PATCH":"#!void","ACK":false,"ACK*":false,"REDIR":false,"SIGN_B":["'sign","bob"],"SIGN_E":["'sign","bob"],"MSGNO":1,"_TO":"PR2"}

send  {"USER":"bob","FROM":"PR1","OUTNB":0,"TO":"GR1","TO_":"#!void","PHYSTO":"#!void","INNB":0,"FUNC":"'set","PARM":["Salut"],"RESULT":true,"PATCH":"#!void","ACK":true,"ACK*":false,"REDIR":false,"SIGN_B":["'sign","bob"],"SIGN_E":["'sign","bob"],"MSGNO":1,"_TO":"PR2"}

poll! 03375ff0-cd78-44c8-88d8-0fe81e8a0f5f
fetch PR2

send  {"USER":"bob","FROM":"PR1","OUTNB":0,"TO":"GR1","TO_":"#!void","PHYSTO":"#!void","INNB":0,"FUNC":"'set","PARM":["Salut"],"RESULT":true,"PATCH":"#!void","ACK":true,"ACK*":false,"REDIR":false,"SIGN_B":["'sign","bob"],"SIGN_E":["'sign","bob","carol"],"MSGNO":2,"_TO":"PR1"}

poll! 49d76424-d17b-457a-b366-0bb3bf2d720a
fetch PR1
>>


This shows you the steps (i.e., the messages that went thru the communication
layer, which redispatched them) of the RSM protocol in this example.

And this provides the proof that the state channel library is able to fully process
the RSM mechanism that is at its core, in between two browser tabs.
