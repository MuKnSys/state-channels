# Chenilles: Generalized State Channels in a Reflective Distributed System

## Reflective Distributed Systems for Blockchain Design

Chenilles implements Generalized State Channels on top of
a Reflective Distributed System architecture:
- Processes: in a Distributed System, we have autonomous processes that communicate asynchronously.
- Groups: In a Reflective Distributed System, some processes are themselves made of subprocesses—or
  conversely, some processes are organized in groups wherein they cooperate with each together
  to implement a larger notional process.
- A blockchain consensus can then be viewed as such a larger process made out of smaller processes:
  the individual nodes cooperate with each other such that the blockchain clients can view the
  blockchain “as if” it were a single centralized entity they can interact with.
  Building a centralized database on top of a decentralized network.
- A state channel is itself a such a blockchain consensus, wherein a group of members
  unanimously sign every message in their own private blockchain that manages the assets in the channel,
  and rely on an underlying blockchain as a service on which these assets are managed and settled.
- A lot of the "interesting" code in defining process groups is in the error handling
  for when communication fails.
- In the case of usual blockchain committees, error handling involves reaching a >2/3 majority
  over some mechanism, Proof of Work, Proof of Stake or Proof of Authority mechanism.
- In the case of a state channel, error handling involves using the underlying blockchain
  to handle breakdown of unanimity based on the latest agreed-upon state, which itself
  is determined through a challenge period on what the latest state of the channel is.
