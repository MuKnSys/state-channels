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


2) Examples and documentation

See:
examples/README.txt
doc/MANUAL.txt
