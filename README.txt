  State Channels Library
  (C) MUKN  2022, 2023


1) Initial build

Create a directory, then from inside it, do:
git init
git pull https://github.com/MuKnSys/state-channels

to download the library ; and then:
chmod 755 ./build
./build guile

to build it for GNU Guile.

Or alternatively:
./build gerbil

To build it for Gerbil Scheme.


Then, you can run the tests:
./tests/test1.sh

If there is nothing displayed, it means that everything
went okay.


Then, to configure the library for the dev Ethereum
chain (i.e., create a local chain with the appropriate
web3.js scripts, plus a number of Ethereum accounts),
do:
./build eth_dev


NOTES:
=> the Ethereum accounts all have their password set to "1234" ; this
   password is needed when you start the local chain ;

=> to start the local chain, do:
   ./bin/eth start

=> to see its live logs, do:
   ./bin/eth log

=> the Ethereum tools (i.e., geth with pcscd, along with solc)
   must have been previously installed, by means of, e.g.:

   add-apt-repository ppa:ethereum/ethereum
   apt-get install geth
   apt-get install pcscd
   snap install solc

   [Q: does go-ethereum absolutely depends on pcscd ?].


2) Examples and documentation

See:
examples/README.txt
doc/MANUAL.txt
