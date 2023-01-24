  Backends

The backends are replaceable components that contain
various functionalities the library's code depends on.

=> communications ;
=> crypto ;
=> ethereum ;
=> atomic broadcast ;

And also:
=> functions related to miscellaneous datastructures (strings,
     lists, hash-tables, etc.) ;
=> files ;
=> sockets (bind, accept) ;

This way, reimplementing the backends, there's no
need to change anything to the code of the library.

When we change the langage (e.g. to Javascript), there
is also only to reimplement the backends, then translate
the Scheme code of the library to Javascript.

Additionally, for a given set of functionalities, there
can be several backends providing different implementations
of the same thing (including implementations that are pure
mockups).


Basic backends:

a) Guile Scheme backend : provides a compatibility layer
     that implements a number of usual Scheme functions
     and datastructures that are not very well standardized
     across Schemes, plus a simple socket API ;

b) Gerbil Scheme backend : same as the previous one, but
     for Gerbil Scheme ;

Production backends:

c) Geth dev backend : very simple implementation of the API
     that we use to connect to geth ;

d) Gerbil Ethereum-based geth backend : an implementation
     of the same API on top of Gerbil Ethereum ;

e) A real public/private key cryptographic library ;

Other backends:

f) NetP2P : wrapper to implement inter-machine communications in
     a genuine P2P fashion (probably by means of LibP2P) ;
