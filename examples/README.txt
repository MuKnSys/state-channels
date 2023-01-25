Faire par topic:

=> send d'un proc à l'autre
=> communications ;

=> RSM ;
=> mappings (d'un contrat ; entre contrat et procs du state channel) ;
=> withdraw ;
=> panic ;
=> blockchain ;

Dire à chaque fois quel est le OUT qui correspond dans les tests, ainsi que
le (ou les) programme(s).


1) high-level layer

guile --no-auto-compile --debug -l src/llruntime.ss examples/mp1_1.ss
bin/scm examples/mp1_1.ss
bin/scm examples/mp2_1.ss


Examples (.scsh files) [TODO: fix guile call, needs the -l src/llruntime.ss ; use scm ?]

src/clish.ss mp1 mp1_1 [Broken]
src/clish.ss mp1 mp1_2 [Broken]
src/clish.ss mp2 mp2_1 [Broken]


Demo (at the moment):

guile --no-auto-compile --debug -l src/llruntime.ss src/clish.ss mp0 mp0_1
src/clish.ss mp0 mp0_1
src/clish.ss mp1 mp1_3
src/clish.ss mp2 mp2_3

src/clish.ss mp0 mp0_1_0
src/clish.ss mp1 mp1_3_0
src/clish.ss mp2 mp2_3_0


2) communications layer

bin/scm examples/srv1.ss
bin/scm examples/cli1.ss

bin/scm examples/srvping.ss
bin/scm examples/cliping.ss

bin/scm examples/srvping.ss
bin/scm examples/echo_cli.ss

export NETP2PD_ADDR=127.0.0.1:1234
bin/scm src/netp2pd.ss
bin/scm examples/netp2p_cli1.ss
