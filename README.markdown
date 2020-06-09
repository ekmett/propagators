propagators
===========

[![Hackage](https://img.shields.io/hackage/v/propagators.svg)](https://hackage.haskell.org/package/propagators) [![Build Status](https://secure.travis-ci.org/ekmett/propagators.png?branch=master)](http://travis-ci.org/ekmett/propagators)

Propagators propagate increases in information from cell to cell.

They are described (using Scheme) in Alexey Radul and Gerald Sussman's ["The Art of the Propagator"](https://dspace.mit.edu/bitstream/handle/1721.1/44215/MIT-CSAIL-TR-2009-002.pdf) as well as in Alexey Radul's thesis on [Propagation Networks](http://groups.csail.mit.edu/genesis/papers/radul%202009.pdf).

This package explores design options for propagators in Haskell. The primary innovation here (beyond the published work) is the use of observable sharing to let us take a more direct form of programming and transform it back and forth to the propagator style.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
