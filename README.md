# Welcome to Kenzo

[![GPLv3 Logo](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl-3.0.en.html)
[![Travis Status](https://travis-ci.org/gheber/kenzo.svg?branch=master)](https://travis-ci.org/gheber/kenzo)

Kenzo is a *Symbolic Software for Effective Homology Computation* and
should be of interest to students and researchers in algebraic topology.

This repository contains a repackaged version of the Kenzo program developed by
Francis Sergeraert and collaborators. The original version of the program can
be found at http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/ . This version
updates its infrastructure by providing the following:

1. A simple regression test suite based on [FiveAM](http://common-lisp.net/project/fiveam/)
2. Support for the great freely available Lisp compilers out there, including [CCL](http://ccl.clozure.com/), [ECL](https://common-lisp.net/project/ecl/), [SBCL](http://www.sbcl.org/), etc.
3. Installation via the [Quicklisp](http://www.quicklisp.org/beta/) library manager
4. Updated documentation and examples runnable from [cl-jupyter](https://github.com/fredokun/cl-jupyter)

The primary source of documentation is the excellent [Kenzo Handbook](https://github.com/gheber/kenzo/blob/master/doc/Kenzo-Doc.pdf).
The easiest way to **get started with Kenzo** is to peruse a matching set of
[Jupyter notebooks](https://sur-l-analysis-sit.us/). In the `notebooks` folder,
there is one notebook for each chapter in the Kenzo handbook. **NOTE:** The
first time you try to access the
[Kenzo JupterHub](https://sur-l-analysis-sit.us/)
site, you will be prompted to authenticate via GitHub.

Please consult the [Wiki](https://github.com/gheber/kenzo/wiki) for
different installation methods and other Kenzo topics.

Enjoy!
