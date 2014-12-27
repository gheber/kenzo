# Welcome to Kenzo

According to the title of its
[handbook](http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Kenzo-doc.pdf),
Kenzo is a "Symbolic Software for Effective Homology Computation" and
its main audience might be students and researchers in algebraic topology.
It is also a remarkable piece of LISP code, albeit in need of a little touch-up.

This is a repackaged version of the Kenzo program by Francis Sergeraert
and collaborators. The original version of the program can be found
at http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/ .
This version aims to update its infrastructure by providing the following:

1. Installation via the [Quicklisp](http://www.quicklisp.org/beta/) library
   manager
2. A simple regression test suite based on
   [FiveAM](http://common-lisp.net/project/fiveam/)
3. Support for [SBCL](http://www.sbcl.org/)

*!!! WARNING !!!*

This is work in progress. The entire code now compiles fine w/ SBCL, but that's
just the first step. There are plenty of examples in
[publications](http://www-fourier.ujf-grenoble.fr/~sergerar/Papers/) and scattered
throughout the source. Many work (most?), some don't. *Let's get to work! ...*
