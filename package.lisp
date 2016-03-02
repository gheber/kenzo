;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; package.lisp

(defpackage #:cat
  (:use #:cl)
  (:export #:KENZO-VERSION

           ;; kenzo.lisp

           #:*BC*
           #:*BDD*
           #:*CMBN-CONTROL*
           #:*DF-FD*
           #:*DG-GD*
           #:*FH*
           #:*HG*
           #:*HH*
           #:*HOMOLOGY-VERBOSE*
           #:*ID-FG*
           #:*ID-GF-DH-HD*
           #:*LIST-LIST*
           #:*RESULTS-VERBOSE*
           #:*TC*
           #:*TDD*
           #:*TNPR-WITH-DEGREES*

           ;; abbreviations.lisp

           #:+ABBREVIATIONS+
           #:WHAT-IS

           ;; algebras.lisp

           #:HOPF

           ;; bar.lisp

           #:+NULL-ABAR+
           #:ABAR
           #:BAR
           #:BAR-BASIS-LENGTH
           #:BAR-BASIS
           #:BAR-CMPR
           #:BAR-HRZN-DFFR
           #:BAR-INTR-HRZN-DFFR
           #:BAR-INTR-VRTC-DFFR
           #:HMTP-VRTC-BAR-INTR
           #:MRPH-VRTC-BAR-INTR
           #:NCMBN-BAR
           #:VRTC-BAR

           ;; bicones.lisp

           #:BICN-CMBN-CMBNB
           #:BICN-CMBN-CMBNC
           #:BICN-CMBN-CMBND
           #:BICONE
           #:BICONE-BASIS
           #:BICONE-CMPR
           #:DISPATCH-BICN-CMBN
           #:MAKE-BICN-CMBN

           ;; cartesian-products.lisp

           #:2ABSM-ACRPR
           #:CRTS-PRDC
           #:CRTS-PRDC-BASIS
           #:CRTS-PRDC-CMPR
           #:CRTS-PRDC-FACE
           #:CRTS-PRDC-FACE*
           #:EXTRACT-COMMON-DGOP

           ;; chain-complexes.lisp

           #:?2
           #:?3
           #:+TOO-MUCH-TIME+
           #:ALL-OBJECTS
           #:BUILD-CHCM
           #:BUILD-MRPH
           #:CAT-INIT
           #:CHCM
           #:CMBN-?
           #:DO-CONTROL
           #:GNRT-?
           #:HOW-MANY-OBJECTS
           #:K
           #:KD
           #:KD2
           #:MRPH

           ;; chcm-elementary-op.lisp

           #:ADD
           #:CMPS
           #:IDNT-MRPH
           #:N-MRPH
           #:OPPS
           #:SBTR
           #:Z-CHCM
           #:ZERO-MRPH

           ;; cl-space-efhm.lisp

           #:CS-HAT-T-U
           #:CS-HAT-U-T
           #:CS-LEFT-HMEQ
           #:CS-LEFT-HMEQ-HAT
           #:CS-LEFT-HMEQ-LEFT-REDUCTION
           #:CS-LEFT-HMEQ-RIGHT-REDUCTION
           #:CS-PRE-LEFT-HMEQ-LEFT-REDUCTION
           #:CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-F
           #:CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-G
           #:CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION
           #:CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-F
           #:CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-G

           ;; classes.lisp

           #:ABSM
           #:ABSM1
           #:ABSM2
           #:AB-SIMPLICIAL-GROUP
           #:ALLP-LIST
           #:APOWR
           #:BCC
           #:BRGN
           #:BSGN
           #:BSPN
           #:CBGN
           #:CHAIN-COMPLEX
           #:CMBN-CMBN
           #:CMBN-DEGR
           #:CMBN-LIST
           #:DEGR
           #:DGOP
           #:DGOP1
           #:DGOP2
           #:DFFR1
           #:EFHM
           #:F
           #:FACE
           #:G
           #:GBAR
           #:GMSM
           #:GMSM2
           #:H
           #:HOMOTOPY-EQUIVALENCE
           #:IABSM
           #:IDNM
           #:KAN
           #:LOOP-LIST
           #:LRDCT
           #:MAKE-ABAR
           #:MAKE-ALLP
           #:MAKE-CMBN
           #:MAKE-LOOP
           #:MAKE-RESULT
           #:MORPHISM
           #:ORGN
           #:REDUCTION
           #:RRDCT
           #:SIMPLICIAL-GROUP
           #:SINTR
           #:SORC
           #:TCC

           ;; classifying-space.lisp

           #:+NULL-GBAR+
           #:CLASSIFYING-SPACE
           #:CLASSIFYING-SPACE-BASIS
           #:CLASSIFYING-SPACE-CMPR
           #:CLASSIFYING-SPACE-FACE
           #:CLASSIFYING-SPACE-GRIN-SINTR
           #:CLASSIFYING-SPACE-GRML-SINTR
           #:GBAR
           #:NORMALIZE-GBAR
           #:UNNORMALIZE-GBAR

           ;; cobar.lisp

           #:ALLP
           #:COBAR
           #:COBAR-BASIS
           #:COBAR-BASIS-LENGTH
           #:COBAR-CMPR
           #:#:COBAR-HRZN-DFFR
           #:COBAR-INTR-HRZN-DFFR
           #:COBAR-INTR-VRTC-DFFR
           #:HMTP-VRTC-COBAR-INTR
           #:MRPH-VRTC-COBAR-INTR
           #:NCMBN-COBAR
           #:VRTC-COBAR

           ;; combinations.lisp

           #:2CMBN-ADD
           #:2CMBN-SBTR
           #:2N-2CMBN
           #:CHECK-CMBN
           #:CMBN
           #:CMBN-CMBN
           #:CMBN-OPPS
           #:DSTR-ADD-TERM-TO-CMBN
           #:F-CMPR
           #:L-CMPR
           #:MAPLEXICO
           #:N-CMBN
           #:NCMBN-ADD
           #:NTERM-ADD
           #:S-CMPR
           #:ZERO-CMBN
           #:ZERO-INTR-DFFR

           ;; cones.lisp

           #:CMBN-CON0
           #:CMBN-CON1
           #:CONE
           #:CONE-2CMBN-APPEND
           #:CONE-2MRPH-DIAG
           #:CONE-2MRPH-DIAG-IMPL
           #:CONE-3MRPH-TRIANGLE
           #:CONE-3MRPH-TRIANGLE-IMPL
           #:CONE-BASIS
           #:CONE-CMBN-SPLIT
           #:CONE-CMPR
           #:TERM-CON0
           #:TERM-CON1
           #:TERM-UNCON

           ;; cs-twisted-products.lisp

           #:SMGR-CRTS-CONTRACTION
           #:SMGR-FIBRATION
           #:SMGR-TNPR-CONTRACTION

           ;; delta.lisp

           #:DELTA
           #:DELTA-BNDR
           #:DELTA-DGNL
           #:DELTA-FACE
           #:DELTA-INFINITY
           #:DELTA-N-BASIS
           #:DELTAB
           #:DELTAB2
           #:DELTAB2-BNDR
           #:DELTAB2-DGNL
           #:SOFT-DELTA
           #:SOFT-DELTA-BNDR
           #:SOFT-DELTA-CMPR
           #:SOFT-DELTA-DGNL
           #:SOFT-DELTA-FACE
           #:SOFT-DELTA-INFINITY
           #:SOFT-DELTA-N-BASIS

           ;; disk-pasting.lisp

           #:CHCM-DISK-PASTING
           #:DISK-PASTING
           #:DISK-PASTING-BASIS
           #:DISK-PASTING-CMPR
           #:DISK-PASTING-FACE
           #:DISK-PASTING-INTR-DFFR
           #:HMEQ-DISK-PASTING
           #:MRPH-DISK-PASTING
           #:MRPH-DISK-PASTING-INTR

           ;; effective-homology.lisp

           #:BUILD-HMEQ
           #:BUILD-RDCT
           #:CHECK-RDCT
           #:CHECK-RDCT-NO-WAIT
           #:CMPS
           #:HMEQ
           #:PRE-CHECK-RDCT
           #:RDCT
           #:TRIVIAL-HMEQ
           #:TRIVIAL-RDCT

           ;; eilenberg-zilber.lisp

           #:AW
           #:EML
           #:EZ
           #:INTR-AW
           #:INTR-EML
           #:INTR-PHI
           #:PHI
           #:SHUFFLE-SIGN

           ;; fibrations.lisp

           #:FIBRATION-TOTAL

           ;; homology-groups

           #:CHCM-HOMOLOGY
           #:CHCM-HOMOLOGY-GEN
           #:CHCM-MAT
           #:HOMOLOGIE
           #:MAT-ALEAT

           ;; k-pi-n.lisp

           #:CIRCLE
           #:INTERESTING-FACES
           #:GMSM-COCYCLE
           #:K-Z
           #:K-Z-1
           #:K-Z-1-CMPR
           #:K-Z-1-FACE
           #:K-Z-1-GRIN
           #:K-Z-1-GRML
           #:K-Z2
           #:K-Z2-1
           #:KZ1-RDCT
           #:KZ1-RDCT-F-INTR
           #:KZ1-RDCT-H-INTR
           #:K-Z-FUNDAMENTAL-CLASS
           #:K-Z2-FUNDAMENTAL-CLASS
           #:Z-ABSM-BAR
           #:Z-BAR-ABSM
           #:Z-FUNDAMENTAL-GMSM
           #:Z2-ABSM-BAR
           #:Z2-BAR-ABSM
           #:Z2-FUNDAMENTAL-GMSM
           #:Z-COCYCLE-GBAR
           #:Z-COCYCLE-GBAR-HEAD
           #:Z2-COCYCLE-GBAR
           #:Z2-COCYCLE-GBAR-HEAD

           ;; kan.lisp

           #:CHECK-HAT
           #:CHECK-KAN
           #:SMST-KAN

           ;; loop-spaces.lisp

           #:+NULL-LOOP+
           #:APOWR-FACE4
           #:APOWR-LASTFACE4
           #:APOWR-NILOOP
           #:GDELTAB
           #:LOOP3
           #:LOOP-PRINT
           #:LOOP-SPACE
           #:LOOP-SPACE-CMPR
           #:LOOP-SPACE-FACE
           #:LOOP-SPACE-GRIN-SINTR
           #:LOOP-SPACE-GRML-SINTR
           #:NORMALIZE-LOOP
           #:UNNORMALIZE-LOOP

           ;; lp-space-efhm.lisp

           #:LS-HAT-T-U
           #:LS-HAT-U-T
           #:LS-LEFT-HMEQ
           #:LS-LEFT-HMEQ-HAT
           #:LS-LEFT-HMEQ-LEFT-REDUCTION
           #:LS-LEFT-HMEQ-LEFT-REDUCTION-G-INTR
           #:LS-LEFT-HMEQ-RIGHT-REDUCTION
           #:LS-PRE-LEFT-HMEQ-LEFT-REDUCTION
           #:LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-H-INTR
           #:LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-F
           #:LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION
           #:LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-F
           #:LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-G

           ;; ls-twisted-products.lisp

           #:ABSM-LOOPABSM
           #:CRTS-CONTRACTION
           #:CRTS-CONTRACTION-INTR
           #:DTAU-D-INTR
           #:POP-FIRST-ABSM
           #:SZCZARBA
           #:TNPR-CONTRACTION
           #:TWISTED-CRTS-PRDC

           ;; macros.lisp

           #:-1-EXPT-N
           #:-1-EXPT-N+1
           #:-1-EXPT-N-1
           #:?
           #:2-EXP
           #:ABAR-LIST
           #:APRD
           #:BASIS
           #:BCC
           #:BCNB
           #:BCNC
           #:BCND
           #:BINOMIAL-P-Q
           #:BNDR
           #:CFFC
           #:CMBN-NON-ZERO-P
           #:CMBN-ZERO-P
           #:CMPR
           #:COLUMN-MINUS-5
           #:COLUMN-OP-5
           #:COLUMN-SWAP-5
           #:CON0
           #:CON1
           #:CPRD
           #:CRPR
           #:D
           #:DEF
           #:F
           #:DFFR
           #:DGNL
           #:G
           #:GMSM1
           #:GNRT
           #:GNRT-NAME
           #:GRIN
           #:GRML
           #:H
           #:I-ADD
           #:I-CMPS
           #:I-SBTR
           #:KFLL
           #:LEXICO
           #:LF
           #:LG
           #:LINE-MINUS-5
           #:LINE-OP-5
           #:LINE-SWAP-5
           #:MASK
           #:MAKE-GBAR
           #:RBCC
           #:RF
           #:RG
           #:TERM
           #:TERM-CMBN
           #:TCC
           #:TNPR
           #:WITH--TERM
           #:WITH-ABSM
           #:WITH-CMBN
           #:WITH-CRPR
           #:WITH-TERM
           #:WITH-TNPR

           ;; searching-homology.lisp

           #:ECHCM
           #:HOMOLOGY

           ;; serre.lisp

           #:BROWN-REDUCTION
           #:FIBRATION-DTAU-D
           #:FIBRATION-DTAU-D-INTR
           #:RIGHT-SERRE-EFHM

           ;; simplicial-groups.lisp

           #:SMGR

           ;; simplicial-mrphs.lisp

           #:BUILD-SMMR
           #:TW-A-SINTR3

           ;; simplicial-sets.lisp

           #:1DGNR
           #:1DGOP*DGOP
           #:1DLOP-DGOP
           #:A-CMPR3
           #:A-FACE4
           #:BSPN-P
           #:CHECK-FACES
           #:CHECK-SMST
           #:DGOP*DGOP
           #:DGOP/DGOP
           #:DGOP-EXT-INT
           #:DGOP-INT-EXT
           #:DLOP-EXT-INT
           #:DLOP-INT-EXT
           #:FACE-BNDR
           #:FACE*-BNDR
           #:HYPHENIZE-LIST
           #:INTR-DIAGONAL
           #:NDGNR
           #:NFACE
           #:REMOVE-BIT
           #:SHOW-STRUCTURE
           #:SMST

           ;; smith.lisp

           #:CHCM-MTRX
           #:CHML-CLSS
           #:CHML-CLSS-INTR
           #:COLUMN-MINUS
           #:COLUMN-OP
           #:COLUMN-SWAP
           #:COPY-MTRX
           #:ECHCM-KILL-EPI
           #:ECHCM-KILL-EPI-F-INTR
           #:ECHCM-KILL-EPI-G-INTR
           #:ECHCM-KILL-EPI-H-INTR
           #:GNRT-NAME-BASIS
           #:IDNT-MTRX
           #:KILL-EPI
           #:KILL-EPIS
           #:LEFT-SUBMATRIX
           #:LINE-MINUS
           #:LINE-OP
           #:LINE-SWAP
           #:LIST-SMITH
           #:MINIMAL-REST-1
           #:MINIMAL-REST-2
           #:MINIMAL-TERM
           #:MINIMAL-TERM-TOP-LEFT
           #:MTRX-PRDC
           #:PIVOTT
           #:RANDOM-MATRIX

           ;; special-smsts.lisp

           #:BUILD-FINITE-SS
           #:FINITE-SS-PRE-TABLE
           #:FINITE-SS-PRE-TABLE-TABLE
           #:FINITE-SS-TABLE
           #:GMSMS-SUBSMST
           #:MOORE
           #:R-PROJ-SPACE
           #:R-PROJ-SPACE-BASIS
           #:SPHERE
           #:SPHERE-FACE
           #:SPHERE-WEDGE

           ;; suspensions.lisp

           #:SUSPENSION
           #:SUSPENSION-BASIS
           #:SUSPENSION-CMPR
           #:SUSPENSION-FACE
           #:SUSPENSION-INTR
           #:SUSPENSION-INTR-CPRD
           #:SUSPENSION-INTR-DFFR

           ;; tensor-products.lisp

           #:2CMBN-TNPR
           #:TNSR-PRDC
           #:TNSR-PRDC-BASIS
           #:TNSR-PRDC-CMPR
           #:TNSR-PRDC-INTR
           #:TNSR-PRDC-INTR-DFFR
           #:TNPR-PRINT

           ;; various.lisp

           #:+EMPTY-LIST+
           #:<A-B<
           #:<A-B>
           #:>A-B<
           #:>A-B>
           #:BINOMIAL-N-P
           #:CLOCK
           #:DONE
           #:SRANDOM
           #:V<A-B>

           ;; whitehead.lisp

           #:Z-WHITEHEAD
           #:Z-WHITEHEAD-SINTR
           #:Z2-WHITEHEAD
           #:Z2-WHITEHEAD-SINTR

           ))
