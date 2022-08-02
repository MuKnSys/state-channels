  Backends

The backends are loaded from llruntime.scm, and
contain all the low-level functions the library's
code depends on.

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


Basic backends (should word out-of-the box in Guile):

a) Guile Scheme backend : provides communications based
     on filesockets ; mock crypto, ethereum-like process,
     mock atomic broadcast (a simple token ring) ;

b) Geth basic backend : provides an extension of the ethereum-like
     process, that connect to geth ;

Other backends (only available in Gerbil Scheme):

c) LibP2P : provides a wrapper to LibP2P ;

d) Zab : provides a solid implementation of atomic broadcast, by
     means of Zab (using the appropriate components of Zookeeper) ;

e) Gerbil Ethereum : a more extended wrapper to Geth ;

f) A real public/private key cryptographic library ;
