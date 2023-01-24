1) Initial build

Create a directory, then from inside it, do:
git init
git pull https://github.com/MuKnSys/state-channels

to download the library ; and then:
./build guile

to build it for GNU Guile.

Or alternatively:
./build gerbil

To build it for Gerbil Scheme [this latter one is broken
at the moment].


Then, to configure the library for the dev Ethereum
chain, do:
./build eth_dev

The chain (i.e., geth with pcscd, along with solc) must
have been previously installed, by means of, e.g.:
add-apt-repository ppa:ethereum/ethereum
apt-get install geth
apt-get install pcscd
snap install solc

[Q: does go-ethereum absolutely depends on pcscd ?].


2) Examples (.ss files)

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


3) Sockets

bin/scm examples/srv1.ss
bin/scm examples/cli1.ss

bin/scm examples/srvping.ss
bin/scm examples/cliping.ss

bin/scm examples/srvping.ss
bin/scm examples/echo_cli.ss

export NETP2PD_ADDR=127.0.0.1:1234
bin/scm src/netp2pd.ss
bin/scm examples/netp2p_cli1.ss
