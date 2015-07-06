;;;; package.lisp

(defpackage #:cat
  (:use #:cl)
  (:export KENZO-VERSION

	   ;; kenzo.lisp

	   *BC*
	   *BDD*
	   *CMBN-CONTROL*
	   *DF-FD*
	   *DG-GD*
	   *FH*
	   *HG*
	   *HH*
	   *ID-FG*
	   *ID-GF-DH-HD*
           *LIST-LIST*
	   *TC*
	   *TDD*

	   ;; bicones.lisp

	   BICN-CMBN-CMBNB
	   BICN-CMBN-CMBNC
	   BICN-CMBN-CMBND
	   BICONE
	   BICONE-BASIS
	   BICONE-CMPR
	   DISPATCH-BICN-CMBN
	   MAKE-BICN-CMBN

	   ;; chain-complexes.lisp

	   ?2
	   ?3
	   +TOO-MUCH-TIME+
	   ALL-OBJECTS
	   BUILD-CHCM
	   BUILD-MRPH
	   CAT-INIT
	   CHCM
	   CMBN-?
	   DO-CONTROL
	   GNRT-?
	   HOW-MANY-OBJECTS
	   K
	   KD
	   KD2
	   MRPH

	   ;; chcm-elementary-op.lisp

	   ADD
	   CMPS
	   IDNT-MRPH
	   N-MRPH
	   OPPS
	   SBTR
	   Z-CHCM
	   ZERO-MRPH

	   ;; classes.lisp

	   BCC
	   CHAIN-COMPLEX
	   CMBN-CMBN
	   CMBN-DEGR
	   CMBN-LIST
	   DEGR
	   DFFR1
	   F
	   G
	   H
	   HOMOTOPY-EQUIVALENCE
	   IABSM
	   LRDCT
	   MAKE-CMBN
	   MAKE-RESULT
	   MORPHISM
	   ORGN
	   REDUCTION
	   RRDCT
	   TCC

	   ;; combinations.lisp

	   2CMBN-ADD
	   2CMBN-SBTR
	   2N-2CMBN
	   CHECK-CMBN
	   CMBN
	   CMBN-CMBN
	   CMBN-OPPS
	   DSTR-ADD-TERM-TO-CMBN
	   F-CMPR
	   L-CMPR
	   MAPLEXICO
	   N-CMBN
	   NCMBN-ADD
	   NTERM-ADD
	   S-CMPR
	   ZERO-CMBN
	   ZERO-INTR-DFFR

	   ;; delta.lisp

	   DELTA-N-BASIS

	   ;; effective-homology.lisp

	   BUILD-RDCT
	   CHECK-RDCT
	   CMPS
	   PRE-CHECK-RDCT
	   TRIVIAL-HMEQ
	   TRIVIAL-RDCT

	   ;; fibrations.lisp

	   FIBRATION-TOTAL

	   ;; k-pi-n.lisp

	   K-Z

	   ;; macros.lisp

	   -1-EXPT-N
	   ?
	   BASIS
	   BCC
	   BCNB
	   BCNC
	   BCND
	   BINOMIAL-P-Q
	   CFFC
	   CMBN-NON-ZERO-P
	   CMBN-ZERO-P
	   CMPR
	   DFFR
	   GNRT
	   I-SBTR
	   RBCC
	   TERM
	   TERM-CMBN
	   TCC

	   ;; searching-homology.lisp

	   HOMOLOGY

	   ;; simplicial-sets.lisp

	   DLOP-INT-EXT

	   ;; smith.lisp

	   CHML-CLSS

	   ;; special-smsts.lisp

	   SPHERE
	   BUILD-FINITE-SS

	   ;; various.lisp

	   +EMPTY-LIST+
	   <A-B<
	   <A-B>
	   >A-B<
	   >A-B>
	   BINOMIAL-N-P
	   CLOCK
	   DONE
	   SRANDOM
	   V<A-B>

	   ;; whitehead.lisp

	   Z-WHITEHEAD

	   ))

