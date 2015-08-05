# Welcome to Kenzo

[![GPLv3 Logo](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl-3.0.en.html)

[![Quicklisp badge](http://quickdocs.org/badge/kenzo.svg)](http://quickdocs.org/kenzo/)

According to the title of its
[handbook](http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Kenzo-doc.pdf),
Kenzo is a "Symbolic Software for Effective Homology Computation" and
its main audience might be students and researchers in algebraic topology.
It is also a remarkable piece of LISP code, albeit in need of a little touch-up.

This repository contains a repackaged version of the Kenzo program by Francis Sergeraert
and collaborators. The original version of the program can be found
at http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/ .
This version aims to update its infrastructure by providing the following:

1. A simple regression test suite based on [FiveAM](http://common-lisp.net/project/fiveam/)
2. Support for the great freely available Lisp compilers out there, including [CCL](http://ccl.clozure.com/), [ECL] (https://common-lisp.net/project/ecl/), [SBCL](http://www.sbcl.org/), etc.
3. Installation via the [Quicklisp](http://www.quicklisp.org/beta/) library manager
4. Updated documentation and examples

*!!! WARNING !!!*

This is work in progress and there are several outstanding [issues](https://github.com/gheber/kenzo/issues).

## Getting up and running

Here are two simple methods to get going: plain ASDF and Quicklisp.

### Plain ASDF

To load Kenzo as provided by this repo, make sure ASDF knowns where to find
the source, e.g. by creating a link to this directory at

      ~/.local/share/common-lisp/source/

Then in your Lisp (e.g., in ECL) type
```lisp
(require :asdf)
(require :kenzo)
```

### Quicklisp

Assuming you have [Quicklisp](http://www.quicklisp.org/beta/), there isn't really much to say here:

```
* (ql:quickload :kenzo)
To load "kenzo":
  Install 1 Quicklisp release:
    kenzo
; Fetching #<URL "http://beta.quicklisp.org/archive/kenzo/2015-08-04/kenzo-20150804-git.tgz">
; 1835.57KB
==================================================
1,879,625 bytes in 1.48 seconds (1236.91KB/sec)
; Loading "kenzo"
[package cat].....................................
..................................................
..............................
(:KENZO)
*
```

Verify that you're good to go by loading and running the Kenzo regression test suite. For example, in an SBCL prompt you should see something like this:
```
* (ql:quickload :kenzo-test)
To load "kenzo-test":
  Load 1 ASDF system:
    kenzo-test
; Loading "kenzo-test"
.
(:KENZO-TEST)
* (fiveam:run!)

Running test suite KENZO
 Running test F-CMPR ..........
 Running test L-CMPR ..........
 Running test S-CMPR .....
 ...
---done---
 Did 786 checks.
    Pass: 786 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

Similarly, you'd load Kenzo via `(ql:quickload :kenzo)`.
