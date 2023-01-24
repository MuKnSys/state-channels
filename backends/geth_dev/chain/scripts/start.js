eth.defaultAccount=eth.accounts[0]
personal.unlockAccount(eth.coinbase,"1234",0)
miner.setEtherbase(eth.accounts[0])
miner.start()
