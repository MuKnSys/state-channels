  Geth dev chain
  (C) MUKN  2022, 2023


0) Preliminary notes

More info about all what is discussed in this README is (in
a somehow more chaotic way) given in:
backends/geth_dev/ETH_NOTES.txt


Below, the pseudo-path "~:SC" is the path where the state
channel library is installed (this is only a notation, which
is not valid under the real environment ; it only exists to
be used in this documentation).

=> so "~:SC" has to be replaced in practice by a real
   path descriptor, e.g. "~/StateChannels", or either
   "$SC_PATH/<some directory>".


1) Installing Ethereum

snap install solc # installing the Solidity compiler

add-apt-repository ppa:ethereum/ethereum  # installing Ethereum libs
apt-get install geth                      # installing the Go-Ethereum daemon
apt-get install pcscd                     # installing PCSCD


2) Creating your local chain

Do:
cd ~:SC

do:
mkdir chain
cd chain
touch geth.log
mkdir data
mkdir data/keystore


Then, create some (at least one) Ethereum account:
geth account new --datadir ./data


Then, create a genesis.json file (in ~:SC/chain), e.g.:
<<
{
  "config": {
    "chainId": 10,
    "homesteadBlock": 0,
    "eip150Block": 0,
    "eip150Hash": "0x0000000000000000000000000000000000000000000000000000000000000000",
    "eip155Block": 0,
    "eip158Block": 0,
    "byzantiumBlock": 0,
    "constantinopleBlock": 0,
    "petersburgBlock": 0,
    "istanbulBlock": 0,
    "ethash": {}
  },
  "nonce": "0x0",
  "timestamp": "0x5e4a53b2",
  "extraData": "0x0000000000000000000000000000000000000000000000000000000000000000",
  "gasLimit": "0x47b760",
  "difficulty": "0x80000",
  "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
  "coinbase": "0x0000000000000000000000000000000000000000",
  "alloc": {
    "a16979a982b94200D61aEDe91F6CF2a0c0aC3613": {
  // ^^^ public key (and Ethereum address) obtained with geth account new
      "balance": "0x200000000000000000000000000000000000000000000000000000000000000"
               // ^^^ enough Ethereum, I suppose ...
    }
  },
  "number": "0x0",
  "gasUsed": "0x0",
  "parentHash": "0x0000000000000000000000000000000000000000000000000000000000000000"
}
>>

And then, create the chain:
geth --datadir "./data" init genesis.json


The whole creation process described above is implemented in ~:SC as:
./build eth_dev

Important: the aforementioned script creates a ~:SC/chain directory containing
  a data/keystore directory that has 3 predefined ethereum accounts (correctly,
  I hope, seeded in the provided genesis.json file), all of them have "1234" as
  their password. This password has to be provided when you start the chain.


3) Starting the chain

To start the chain:
cd ~:SC
geth --nodiscover --http --mine --miner.threads 1 --allow-insecure-unlock --unlock 0 \
     --datadir ./chain/data console 2>>./chain/geth.log

To see the interactive logs of the chain:
tail -f ./chain/geth.log


These two commands are implemented in ~:SC as:
bin/eth start  # start the chain
bin/eth log    # log the running chain
