  First release of the state channels library


1) Rationale

This first release of the state channel library (and of an example
that enables seeing it in action) is meant to show a first version
of a state channel-based micropayment class, that works on top of LibP2P.

The methods deposit() and transfer() from this class perform a full RSM
handshake in between the browser tabs ; the connection between deposit()
and the (simulated) contract is disabled, at the moment.

It contains a (relatively) clean and complete implementation of the main
loop in Javascript, plus the interfaces that are needed to plug the library
to the LibP2P-based communication layer and to the main loop.

Once the 34 lines of code that mockup LibP2P will have been rewritten
against the actual LibP2P API, the example will work without the JSONP
server that is also contained in this release. The explanations below
are related to what you have to do when you use the JSONP server.


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
http://localhost/demo1/app.html

And another one that opens the same page:
http://localhost/demo1/app.html


On each one of these two tabs, in the beginning, the list of existing
endpoints and groups that the peer running in the tab can see appears,
e.g., in the first tab we see:

PID   NAME   USER    PEER  SELF
0h^   HOST1  system  (_)   _
0ph_  phys   system  (_)   _
2 procs


There are only the host HOST1, and a process which represents the "system",
i.e., the low-level physical host (in this case, the browser tab itself).


Now, inside each one of these two tabs, open the Inspect pane of the browser (we
will use it as a CLI).

Inside each one of the two Inspect panes, do:

run();


These << run(); >> commands start the main loop in each one of the browser tabs.


Inside the Inspect pane of the first tab, do:

iam("Bob");


And inside the Inspect pane of the second tab, do:

iam("Carol");


Then the first tab impersonates the user "Bob", and the second tab impersonates
the user "Carol".


Now, inside the inspect pane of the first tab, do:

channel("GR1","Bob","Carol");


The effect of this is that the micropayment channel GR2 will be created in the
first tab, and automatically propagated to the second tab. So, the creation is
distributed. Two endpoints GR1#1 and GR1#2 are created for Bob and Carol, respectively.


Now, if inside the Inspect pane of the first tab, you (i.e. Bob) do(es):

iam("GR1#1")
deposit("GR1",10)

then a deposit of 10 monetary units is done for Bob in the state channel's ledger.


To verify this, inside the inspect pane of the first tab, do:

netlist("GR1#1");


It gives:

STATE = Init
ACCOUNT = @rexpr
  Bob = 10
  Carol = 0


To see Bob's endpoint itself, you can do:

netlist("GR1#1");

you now see that the incoming, outcoming and replay lists
of GR1#1 are populated by a number of messages, i.e.  you
should see (still in the first tab):

GR1#1 Bob ^
in:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!!
rl:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!*
out:
  <0 _>deposit GR1#1=>GR1 (10) (S Bob)


And to see Carol's endpoint (in the second tab), you can do:

netlist("GR1#2");

you should see:

GR1#2 Carol ^
in:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)!!
rl:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!*
out:


That's the result of the processing performed by the RSM
protocol.


In the logs of the communication layer's server, you should
see something like:
<<
host! 9e5a95f5-19d0-47ad-99dc-e9784723a559  ## creation of the host for Bob
host! dc8953e9-9293-401f-a3b5-168848cad051  ## creation of the host for Carol


## creation and dispatch of the group, and of the 2 endpoints
##
all!  (enter-group ("9e5a95f5-19d0-47ad-99dc-e9784723a559" "ALL") "GR1" ("Bob" "Carol"))
recv! dc8953e9-9293-401f-a3b5-168848cad051
      (enter-group (9e5a95f5-19d0-47ad-99dc-e9784723a559 ALL) GR1 (Bob Carol))
recv! 9e5a95f5-19d0-47ad-99dc-e9784723a559
      (enter-group (9e5a95f5-19d0-47ad-99dc-e9784723a559 ALL) GR1 (Bob Carol))


## Bob emits a deposit() message
##
all!  ("sendto" ("9e5a95f5-19d0-47ad-99dc-e9784723a559" "GR1#2")
      ((:TYPE @call) (:ID @call@0) (:USER "Bob") (:FROM "GR1#1") (:TO "GR1")
       (:FUNC deposit) (:PARM (10))
       (:ACK #f) (:ACK* #f)
       (:SIGN_B (sign "Bob")) (:SIGN_E (sign "Bob"))
       (:_TO "GR1#2")))


## Carol receives it
##
recv! dc8953e9-9293-401f-a3b5-168848cad051
      (sendto (9e5a95f5-19d0-47ad-99dc-e9784723a559 GR1#2)
      ((:TYPE @call) (:ID @call@0) (:USER "Bob") (:FROM "GR1#1") (:TO "GR1")
       (:FUNC deposit) (:PARM (10))
       (:ACK #f) (:ACK* #f)
       (:SIGN_B (sign "Bob")) (:SIGN_E (sign "Bob"))
       (:_TO "GR1#2")))


## Carol signs the deposit() message, and resends it
##
all!  ("sendto" ("dc8953e9-9293-401f-a3b5-168848cad051" "GR1#1")
      ((:TYPE @call) (:ID 1234) (:USER "Bob") (:FROM "GR1#1") (:TO "GR1")
       (:FUNC deposit) (:PARM (10))
       (:ACK #t) (:ACK* #f)
       (:SIGN_B (sign "Bob")) (:SIGN_E (sign "Bob" "Carol"))
       (:_TO "GR1#1")))


## Bob receives it
##
recv! 9e5a95f5-19d0-47ad-99dc-e9784723a559
      (sendto (dc8953e9-9293-401f-a3b5-168848cad051 GR1#1)
      ((:TYPE @call) (:ID 1234) (:USER "Bob") (:FROM "GR1#1") (:TO "GR1")
       (:FUNC deposit) (:PARM (10))
       (:ACK #t) (:ACK* #f)
       (:SIGN_B (sign "Bob")) (:SIGN_E (sign "Bob" "Carol"))
       (:_TO "GR1#1")))
>>


This shows you the steps (i.e., the messages that went thru the communication
layer, which redispatched them) of the RSM protocol in this example.

And this provides the proof that the state channel library is able to fully process
the RSM mechanism that is at its core, in between two browser tabs.


Finally, after a transfer() operation performed by Bob in the first tab:

transfer("GR1","Carol","2")


The state of the ledger in the 2 tabs is:

STATE = Init
ACCOUNT = @rexpr
  Bob = 8
  Carol = 2


In the first tab, the state of Bob's endpoint is:

GR1#1 Bob ^
in:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!!
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob)
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob Carol)!!
rl:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!*
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob Carol)!*
out:
  <0 _>deposit GR1#1=>GR1 (10) (S Bob)
  <1 _>transfer GR1#1=>GR1 (Carol 2) (S Bob)


And in the second tab, the state of Carol's endpoint is:

GR1#2 Carol ^
in:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)
  <0 0>deposit GR1#1=>GR1 (10) (S Bob)!!
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob)
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob)!!
rl:
  <0 0>deposit GR1#1=>GR1 (10) (S Bob Carol)!*
  <1 1>transfer GR1#1=>GR1 (Carol 2) (S Bob Carol)!*
out:
