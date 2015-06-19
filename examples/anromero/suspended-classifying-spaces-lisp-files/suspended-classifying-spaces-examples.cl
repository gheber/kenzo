International Allegro CL Enterprise Edition
8.1 [Windows] (Nov 20, 2008 16:34)
Copyright (C) 1985-2007, Franz Inc., Oakland, CA, USA.  All Rights Reserved.

This development copy of Allegro CL is licensed to:
   [TC16573] Universidad de La Rioja

CG version 1.103.2.10 / IDE version 1.103.2.15
Loaded options from C:\Documents and Settings\Ana\Mis documentos\allegro-prefs-8-1.cl.

;; Optimization settings: safety 1, space 1, speed 1, debug 2.
;; For a complete description of all compiler switches given the current optimization settings evaluate
;; (EXPLAIN-COMPILER-SETTINGS).

[changing package from "COMMON-LISP-USER" to "COMMON-GRAPHICS-USER"]
CG-USER(1): (in-package :user)
#<The COMMON-LISP-USER package>
CL-USER(2): (progn
              (load "cat-init")
              (load-cfiles)
              (load "resolutions")
              (load "cylinders")
              (load "omparsers")
              (load "fundamental-classes")
              (load "permutation-groups")
              (load "homotopy"))

; Loading C:\Archivos de programa\acl81\ana\cat-init.cl

 FILE  1: macros
 FILE  2: various
 FILE  3: classes
 FILE  4: combinations
 FILE  5: chain-complexes
 FILE  6: chcm-elementary-op
 FILE  7: effective-homology
 FILE  8: homology-groups
 FILE  9: searching-homology
 FILE 10: cones
 FILE 11: bicones
 FILE 12: tensor-products
 FILE 13: coalgebras
 FILE 14: cobar
 FILE 15: algebras
 FILE 16: bar
 FILE 17: simplicial-sets
 FILE 18: simplicial-mrphs
 FILE 19: delta
 FILE 20: special-smsts
 FILE 21: suspensions
 FILE 22: disk-pasting
 FILE 23: cartesian-products
 FILE 24: eilenberg-zilber
 FILE 25: kan
 FILE 26: simplicial-groups
 FILE 27: fibrations
 FILE 28: loop-spaces
 FILE 29: ls-twisted-products
 FILE 30: lp-space-efhm
 FILE 31: classifying-spaces
 FILE 32: k-pi-n
 FILE 33: serre
 FILE 34: cs-twisted-products
 FILE 35: cl-space-efhm
 FILE 36: whitehead
 FILE 37: smith
; Fast loading C:\Archivos de programa\acl81\macros.fasl
; Fast loading C:\Archivos de programa\acl81\various.fasl
; Fast loading C:\Archivos de programa\acl81\classes.fasl
; Fast loading C:\Archivos de programa\acl81\combinations.fasl
; Fast loading C:\Archivos de programa\acl81\chain-complexes.fasl
; Fast loading C:\Archivos de programa\acl81\chcm-elementary-op.fasl
; Fast loading C:\Archivos de programa\acl81\effective-homology.fasl
; Fast loading C:\Archivos de programa\acl81\homology-groups.fasl
; Fast loading C:\Archivos de programa\acl81\searching-homology.fasl
; Fast loading C:\Archivos de programa\acl81\cones.fasl
; Fast loading C:\Archivos de programa\acl81\bicones.fasl
; Fast loading C:\Archivos de programa\acl81\tensor-products.fasl
; Fast loading C:\Archivos de programa\acl81\coalgebras.fasl
; Fast loading C:\Archivos de programa\acl81\cobar.fasl
; Fast loading C:\Archivos de programa\acl81\algebras.fasl
; Fast loading C:\Archivos de programa\acl81\bar.fasl
; Fast loading C:\Archivos de programa\acl81\simplicial-sets.fasl
; Fast loading C:\Archivos de programa\acl81\simplicial-mrphs.fasl
; Fast loading C:\Archivos de programa\acl81\delta.fasl
; Fast loading C:\Archivos de programa\acl81\special-smsts.fasl
; Fast loading C:\Archivos de programa\acl81\suspensions.fasl
; Fast loading C:\Archivos de programa\acl81\disk-pasting.fasl
; Fast loading C:\Archivos de programa\acl81\cartesian-products.fasl
; Fast loading C:\Archivos de programa\acl81\eilenberg-zilber.fasl
; Fast loading C:\Archivos de programa\acl81\kan.fasl
; Fast loading C:\Archivos de programa\acl81\simplicial-groups.fasl
; Fast loading C:\Archivos de programa\acl81\fibrations.fasl
; Fast loading C:\Archivos de programa\acl81\loop-spaces.fasl
; Fast loading C:\Archivos de programa\acl81\ls-twisted-products.fasl
; Fast loading C:\Archivos de programa\acl81\lp-space-efhm.fasl
; Fast loading C:\Archivos de programa\acl81\classifying-spaces.fasl
; Fast loading C:\Archivos de programa\acl81\k-pi-n.fasl
; Fast loading C:\Archivos de programa\acl81\serre.fasl
; Fast loading C:\Archivos de programa\acl81\cs-twisted-products.fasl
; Fast loading C:\Archivos de programa\acl81\cl-space-efhm.fasl
; Fast loading C:\Archivos de programa\acl81\whitehead.fasl
; Fast loading C:\Archivos de programa\acl81\smith.fasl
; Fast loading C:\Archivos de programa\acl81\resolutions.fasl
; Fast loading C:\Archivos de programa\acl81\cylinders.fasl
; Fast Loading C:\Archivos de programa\acl81\omparsers.fasl
; Fast Loading C:\Archivos de programa\acl81\symmetricgroup.fasl
; Fast Loading C:\Archivos de programa\acl81\fundamental-classes.fasl
; Fast Loading C:\Archivos de programa\acl81\permutation-groups.fasl
; Fast Loading C:\Archivos de programa\acl81\homotopy.fasl
T
CL-USER(3): (setf z5 (cyclicgroup 5))
[K1 Abelian-Group]
CL-USER(4): (setf k-z5-1 (k-g-1 z5))
[K2 Abelian-Simplicial-Group]
CL-USER(5): (efhm k-z5-1)
[K50 Homotopy-Equivalence K2 <= K40 => K31]
CL-USER(6): (homology k-z5-1 5)

Computing boundary-matrix in dimension 5.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 48m 45s.
Computing the boundary of the generator 1/1 (dimension 5) :
5 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 48m 45s.
Computing the boundary of the generator 1/1 (dimension 6) :
6 
End of computing.




Homology in dimension 5 :


Component Z/5Z


---done---

;; Clock -> 2011-07-12, 10h 48m 45s.

NIL
CL-USER(7): (setf s-k-z5-1 (suspension k-z5-1))
[K51 Simplicial-Set]
CL-USER(8): (efhm s-k-z5-1)
[K68 Homotopy-Equivalence K51 <= K56 => K62]
CL-USER(9): (homotopy s-k-z5-1 5)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
1 
End of computing.



;; Clock -> 2011-07-12, 10h 50m 9s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/1 (dimension 3) :
2 
End of computing.



;; Clock -> 2011-07-12, 10h 50m 9s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/1 (dimension 3) :
2 
End of computing.



;; Clock -> 2011-07-12, 10h 50m 9s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/3 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 2/3 (dimension 3) :
<TnPr 1 1> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 3/3 (dimension 3) :
<TnPr 2 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 4.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/4 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 2/4 (dimension 4) :
<TnPr 1 2> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 3/4 (dimension 4) :
<TnPr 2 1> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 4/4 (dimension 4) :
<TnPr 3 0> 
End of computing.



;; Clock -> 2011-07-12, 10h 50m 9s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 9.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/9 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 2/9 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 3/9 (dimension 4) :
<TnPr <TnPr S-BSGN 1> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 4/9 (dimension 4) :
<TnPr <TnPr S-BSGN 2> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 5/9 (dimension 4) :
<TnPr <TnPr 1 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 6/9 (dimension 4) :
<TnPr <TnPr S-BSGN 4> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 7/9 (dimension 4) :
<TnPr <TnPr 1 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 8/9 (dimension 4) :
<TnPr <TnPr 2 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 9/9 (dimension 4) :
<TnPr <TnPr 3 0> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 15.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 1/15 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 9s.
Computing the boundary of the generator 2/15 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 3/15 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 4/15 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 5/15 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 6/15 (dimension 5) :
<TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 7/15 (dimension 5) :
<TnPr <TnPr 1 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 8/15 (dimension 5) :
<TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 9/15 (dimension 5) :
<TnPr <TnPr 1 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 10/15 (dimension 5) :
<TnPr <TnPr 2 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 11/15 (dimension 5) :
<TnPr <TnPr S-BSGN 5> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 12/15 (dimension 5) :
<TnPr <TnPr 1 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 10s.
Computing the boundary of the generator 13/15 (dimension 5) :
<TnPr <TnPr 2 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 14/15 (dimension 5) :
<TnPr <TnPr 3 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 15/15 (dimension 5) :
<TnPr <TnPr 4 0> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 10h 50m 11s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 21.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 1/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 2/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 3/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 4/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 5/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 6/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 7/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 11s.
Computing the boundary of the generator 8/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 9/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 10/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 11/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 12/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 13/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 14/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 15/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 16/21 (dimension 5) :
<TnPr <TnPr <TnPr 2 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 12s.
Computing the boundary of the generator 17/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 5> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 13s.
Computing the boundary of the generator 18/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 13s.
Computing the boundary of the generator 19/21 (dimension 5) :
<TnPr <TnPr <TnPr 2 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 19s.
Computing the boundary of the generator 20/21 (dimension 5) :
<TnPr <TnPr <TnPr 3 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 19s.
Computing the boundary of the generator 21/21 (dimension 5) :
<TnPr <TnPr <TnPr 4 0> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 39.


;; Clock -> 2011-07-12, 10h 50m 19s.
Computing the boundary of the generator 1/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 19s.
Computing the boundary of the generator 2/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 19s.
Computing the boundary of the generator 3/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 4/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 5/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 6/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 7/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 8/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 9/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 10/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 11/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 12/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 13/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 14/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 20s.
Computing the boundary of the generator 15/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[6 5]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 21s.
Computing the boundary of the generator 16/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 21s.
Computing the boundary of the generator 17/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 22s.
Computing the boundary of the generator 18/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[4 3][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 22s.
Computing the boundary of the generator 19/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 22s.
Computing the boundary of the generator 20/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 23s.
Computing the boundary of the generator 21/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 24s.
Computing the boundary of the generator 22/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 24s.
Computing the boundary of the generator 23/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 25s.
Computing the boundary of the generator 24/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 26s.
Computing the boundary of the generator 25/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 26s.
Computing the boundary of the generator 26/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 26s.
Computing the boundary of the generator 27/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 28s.
Computing the boundary of the generator 28/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 30s.
Computing the boundary of the generator 29/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 32s.
Computing the boundary of the generator 30/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 4> <<Abar[2 1]>>> <<Abar>>> 
*CR* -- Cut = 6.6320505e-4 -- N = 1332
End of computing.


;; Clock -> 2011-07-12, 10h 50m 35s.
Computing the boundary of the generator 31/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 41s.
Computing the boundary of the generator 32/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 46s.
Computing the boundary of the generator 33/39 (dimension 6) :
<TnPr <TnPr <TnPr 3 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 47s.
Computing the boundary of the generator 34/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 6> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 50m 50s.
Computing the boundary of the generator 35/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 51m 14s.
Computing the boundary of the generator 36/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 3> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.0019053577 -- N = 3747
*CR* -- Cut = 0.0030287788 -- N = 5565
*CR* -- Cut = 0.0037655816 -- N = 7046
End of computing.


;; Clock -> 2011-07-12, 10h 52m 42s.
Computing the boundary of the generator 37/39 (dimension 6) :
<TnPr <TnPr <TnPr 3 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.0045507667 -- N = 8614
*CR* -- Cut = 0.005675921 -- N = 10577
End of computing.


;; Clock -> 2011-07-12, 10h 53m 42s.
Computing the boundary of the generator 38/39 (dimension 6) :
<TnPr <TnPr <TnPr 4 1> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.0063750423 -- N = 11880
End of computing.


;; Clock -> 2011-07-12, 10h 53m 55s.
Computing the boundary of the generator 39/39 (dimension 6) :
<TnPr <TnPr <TnPr 5 0> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 10h 53m 56s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 21.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 1/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 2/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 3/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 4/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 5/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 6/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 7/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 8/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 9/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 56s.
Computing the boundary of the generator 10/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 11/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 12/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 13/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 14/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 15/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 16/21 (dimension 5) :
<TnPr <TnPr <TnPr 2 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 17/21 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 5> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 57s.
Computing the boundary of the generator 18/21 (dimension 5) :
<TnPr <TnPr <TnPr 1 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 53m 58s.
Computing the boundary of the generator 19/21 (dimension 5) :
<TnPr <TnPr <TnPr 2 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 1s.
Computing the boundary of the generator 20/21 (dimension 5) :
<TnPr <TnPr <TnPr 3 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 21/21 (dimension 5) :
<TnPr <TnPr <TnPr 4 0> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 39.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 1/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 2/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 3/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 4/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 5/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 6/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 7/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 8/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 9/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 10/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 11/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 12/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 2s.
Computing the boundary of the generator 13/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 3s.
Computing the boundary of the generator 14/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 3s.
Computing the boundary of the generator 15/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[6 5]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 3s.
Computing the boundary of the generator 16/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 3s.
Computing the boundary of the generator 17/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 4s.
Computing the boundary of the generator 18/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[4 3][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 4s.
Computing the boundary of the generator 19/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 4s.
Computing the boundary of the generator 20/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 4s.
Computing the boundary of the generator 21/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 5s.
Computing the boundary of the generator 22/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 5s.
Computing the boundary of the generator 23/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 5s.
Computing the boundary of the generator 24/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 6s.
Computing the boundary of the generator 25/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 6s.
Computing the boundary of the generator 26/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 6s.
Computing the boundary of the generator 27/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 7s.
Computing the boundary of the generator 28/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 8s.
Computing the boundary of the generator 29/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 10s.
Computing the boundary of the generator 30/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 4> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 10s.
Computing the boundary of the generator 31/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 2> <<Abar[2 1]>>> <<Abar>>> 
*CR* -- Cut = 0.0068156803 -- N = 12771
End of computing.


;; Clock -> 2011-07-12, 10h 54m 14s.
Computing the boundary of the generator 32/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 17s.
Computing the boundary of the generator 33/39 (dimension 6) :
<TnPr <TnPr <TnPr 3 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 17s.
Computing the boundary of the generator 34/39 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 6> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 18s.
Computing the boundary of the generator 35/39 (dimension 6) :
<TnPr <TnPr <TnPr 1 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 54m 27s.
Computing the boundary of the generator 36/39 (dimension 6) :
<TnPr <TnPr <TnPr 2 3> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.007411653 -- N = 13953
*CR* -- Cut = 0.007767046 -- N = 14611
End of computing.


;; Clock -> 2011-07-12, 10h 54m 54s.
Computing the boundary of the generator 37/39 (dimension 6) :
<TnPr <TnPr <TnPr 3 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.00818243 -- N = 15505
End of computing.


;; Clock -> 2011-07-12, 10h 55m 9s.
Computing the boundary of the generator 38/39 (dimension 6) :
<TnPr <TnPr <TnPr 4 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 10h 55m 13s.
Computing the boundary of the generator 39/39 (dimension 6) :
<TnPr <TnPr <TnPr 5 0> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 10h 55m 13s.




Homotopy in dimension 5 :


Component Z/5Z

Component Z/5Z

NIL
CL-USER(10): (setf z3 (cyclicgroup 3))
[K627 Abelian-Group]
CL-USER(11): (setf k-z3-1 (k-g-1 z3))
[K628 Abelian-Simplicial-Group]
CL-USER(12): (setf k-z3+3-1 (crts-prdc k-z3-1 k-z3-1))
[K633 Simplicial-Set]
CL-USER(13): (setf s-k-z3+3-1 (suspension k-z3+3-1))
[K640 Simplicial-Set]
CL-USER(14): (homotopy s-k-z3+3-1 4)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/2 (dimension 2) :
<TnPr 0 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/2 (dimension 2) :
<TnPr 1 0> 
End of computing.



;; Clock -> 2011-07-12, 11h 26m 14s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/2 (dimension 2) :
<TnPr 0 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/2 (dimension 2) :
<TnPr 1 0> 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/3 (dimension 3) :
<TnPr 0 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/3 (dimension 3) :
<TnPr 1 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 3/3 (dimension 3) :
<TnPr 2 0> 
End of computing.



;; Clock -> 2011-07-12, 11h 26m 14s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/2 (dimension 2) :
<TnPr 0 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/2 (dimension 2) :
<TnPr 1 0> 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/3 (dimension 3) :
<TnPr 0 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/3 (dimension 3) :
<TnPr 1 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 3/3 (dimension 3) :
<TnPr 2 0> 
End of computing.



;; Clock -> 2011-07-12, 11h 26m 14s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 11.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 1/11 (dimension 3) :
<TnPr <TnPr S-BSGN 0> 3> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 2/11 (dimension 3) :
<TnPr <TnPr S-BSGN 1> 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 3/11 (dimension 3) :
<TnPr <TnPr S-BSGN 2> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 4/11 (dimension 3) :
<TnPr <TnPr <TnPr 0 1> 0> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 5/11 (dimension 3) :
<TnPr <TnPr <TnPr 1 0> 0> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 6/11 (dimension 3) :
<TnPr <TnPr S-BSGN 3> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 7/11 (dimension 3) :
<TnPr <TnPr <TnPr 0 1> 1> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 8/11 (dimension 3) :
<TnPr <TnPr <TnPr 1 0> 1> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 9/11 (dimension 3) :
<TnPr <TnPr <TnPr 0 2> 0> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 10/11 (dimension 3) :
<TnPr <TnPr <TnPr 1 1> 0> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 14s.
Computing the boundary of the generator 11/11 (dimension 3) :
<TnPr <TnPr <TnPr 2 0> 0> 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 21.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 1/21 (dimension 4) :
<TnPr <TnPr S-BSGN 0> 4> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 2/21 (dimension 4) :
<TnPr <TnPr S-BSGN 1> 3> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 3/21 (dimension 4) :
<TnPr <TnPr S-BSGN 2> 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 4/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 1> 0> 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 5/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 0> 0> 2> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 6/21 (dimension 4) :
<TnPr <TnPr S-BSGN 3> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 7/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 1> 1> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 8/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 0> 1> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 9/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 2> 0> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 10/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 1> 0> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 11/21 (dimension 4) :
<TnPr <TnPr <TnPr 2 0> 0> 1> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 12/21 (dimension 4) :
<TnPr <TnPr S-BSGN 4> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 13/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 1> 2> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 14/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 0> 2> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 15/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 2> 1> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 16/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 1> 1> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 17/21 (dimension 4) :
<TnPr <TnPr <TnPr 2 0> 1> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 18/21 (dimension 4) :
<TnPr <TnPr <TnPr 0 3> 0> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 19/21 (dimension 4) :
<TnPr <TnPr <TnPr 1 2> 0> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 20/21 (dimension 4) :
<TnPr <TnPr <TnPr 2 1> 0> 0> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 15s.
Computing the boundary of the generator 21/21 (dimension 4) :
<TnPr <TnPr <TnPr 3 0> 0> 0> 
End of computing.



;; Clock -> 2011-07-12, 11h 26m 15s.


*CR* -- Cut = 0.008484252 -- N = 16123
Computing boundary-matrix in dimension 4.
Rank of the source-module : 63.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 1/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 2/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 3/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 4/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 5/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 6/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 7/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 8/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 9/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 10/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 11/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 12/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 13/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 14/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 26s.
Computing the boundary of the generator 15/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 16/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 17/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 18/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 19/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 20/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 21/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 22/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 23/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 24/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 25/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 26/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 27s.
Computing the boundary of the generator 27/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 28/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 29/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 30/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 31/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 32/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 33/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 34/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 28s.
Computing the boundary of the generator 35/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 29s.
Computing the boundary of the generator 36/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 29s.
Computing the boundary of the generator 37/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 29s.
Computing the boundary of the generator 38/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 29s.
Computing the boundary of the generator 39/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 29s.
Computing the boundary of the generator 40/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 30s.
Computing the boundary of the generator 41/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 30s.
Computing the boundary of the generator 42/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 30s.
Computing the boundary of the generator 43/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 4> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 30s.
Computing the boundary of the generator 44/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 30s.
Computing the boundary of the generator 45/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.008873884 -- N = 16836
End of computing.


;; Clock -> 2011-07-12, 11h 26m 32s.
Computing the boundary of the generator 46/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 34s.
Computing the boundary of the generator 47/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 36s.
Computing the boundary of the generator 48/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 36s.
Computing the boundary of the generator 49/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 38s.
Computing the boundary of the generator 50/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 39s.
Computing the boundary of the generator 51/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 41s.
Computing the boundary of the generator 52/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 43s.
Computing the boundary of the generator 53/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 44s.
Computing the boundary of the generator 54/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 4> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 45s.
Computing the boundary of the generator 55/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 46s.
Computing the boundary of the generator 56/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 48s.
Computing the boundary of the generator 57/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.009364715 -- N = 17500
End of computing.


;; Clock -> 2011-07-12, 11h 26m 49s.
Computing the boundary of the generator 58/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 51s.
Computing the boundary of the generator 59/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 52s.
Computing the boundary of the generator 60/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 52s.
Computing the boundary of the generator 61/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 54s.
Computing the boundary of the generator 62/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 55s.
Computing the boundary of the generator 63/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 152.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 1/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 2/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 3/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 4/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 5/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 6/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 7/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 8/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 9/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 10/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 11/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 12/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 13/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 14/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 15/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 16/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 56s.
Computing the boundary of the generator 17/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 18/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 19/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 20/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 21/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 22/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 23/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 24/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 25/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 57s.
Computing the boundary of the generator 26/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 58s.
Computing the boundary of the generator 27/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 58s.
Computing the boundary of the generator 28/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 58s.
Computing the boundary of the generator 29/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 58s.
Computing the boundary of the generator 30/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 58s.
Computing the boundary of the generator 31/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 59s.
Computing the boundary of the generator 32/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 59s.
Computing the boundary of the generator 33/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 59s.
Computing the boundary of the generator 34/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 59s.
Computing the boundary of the generator 35/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 26m 59s.
Computing the boundary of the generator 36/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 0s.
Computing the boundary of the generator 37/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][3 2]>>> <<Abar>>> 
*CR* -- Cut = 0.009671103 -- N = 17912
End of computing.


;; Clock -> 2011-07-12, 11h 27m 0s.
Computing the boundary of the generator 38/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 0s.
Computing the boundary of the generator 39/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 0s.
Computing the boundary of the generator 40/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 0s.
Computing the boundary of the generator 41/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 1s.
Computing the boundary of the generator 42/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 1s.
Computing the boundary of the generator 43/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 1s.
Computing the boundary of the generator 44/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 1s.
Computing the boundary of the generator 45/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 2s.
Computing the boundary of the generator 46/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 2s.
Computing the boundary of the generator 47/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 3s.
Computing the boundary of the generator 48/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 3s.
Computing the boundary of the generator 49/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 4s.
Computing the boundary of the generator 50/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 4s.
Computing the boundary of the generator 51/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 5s.
Computing the boundary of the generator 52/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 5s.
Computing the boundary of the generator 53/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 5s.
Computing the boundary of the generator 54/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 5s.
Computing the boundary of the generator 55/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 6s.
Computing the boundary of the generator 56/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 6s.
Computing the boundary of the generator 57/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 7s.
Computing the boundary of the generator 58/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 8s.
Computing the boundary of the generator 59/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 9s.
Computing the boundary of the generator 60/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 10s.
Computing the boundary of the generator 61/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 10s.
Computing the boundary of the generator 62/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 11s.
Computing the boundary of the generator 63/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 12s.
Computing the boundary of the generator 64/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 13s.
Computing the boundary of the generator 65/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 14s.
Computing the boundary of the generator 66/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 15s.
Computing the boundary of the generator 67/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[5 4]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 15s.
Computing the boundary of the generator 68/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1][3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 15s.
Computing the boundary of the generator 69/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 16s.
Computing the boundary of the generator 70/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 16s.
Computing the boundary of the generator 71/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 17s.
Computing the boundary of the generator 72/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 17s.
Computing the boundary of the generator 73/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 18s.
Computing the boundary of the generator 74/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 18s.
Computing the boundary of the generator 75/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 20s.
Computing the boundary of the generator 76/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 22s.
Computing the boundary of the generator 77/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 23s.
Computing the boundary of the generator 78/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 24s.
Computing the boundary of the generator 79/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 25s.
Computing the boundary of the generator 80/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 26s.
Computing the boundary of the generator 81/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.010428162 -- N = 19091
End of computing.


;; Clock -> 2011-07-12, 11h 27m 27s.
Computing the boundary of the generator 82/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 27s.
Computing the boundary of the generator 83/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 28s.
Computing the boundary of the generator 84/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 30s.
Computing the boundary of the generator 85/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 32s.
Computing the boundary of the generator 86/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 36s.
Computing the boundary of the generator 87/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 38s.
Computing the boundary of the generator 88/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 39s.
Computing the boundary of the generator 89/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 41s.
Computing the boundary of the generator 90/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 43s.
Computing the boundary of the generator 91/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 45s.
Computing the boundary of the generator 92/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 47s.
Computing the boundary of the generator 93/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 49s.
Computing the boundary of the generator 94/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[5 4]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 49s.
Computing the boundary of the generator 95/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1][3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 50s.
Computing the boundary of the generator 96/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 51s.
Computing the boundary of the generator 97/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 52s.
Computing the boundary of the generator 98/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 53s.
Computing the boundary of the generator 99/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 54s.
Computing the boundary of the generator 100/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 55s.
Computing the boundary of the generator 101/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 27m 58s.
Computing the boundary of the generator 102/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 2s.
Computing the boundary of the generator 103/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 5s.
Computing the boundary of the generator 104/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.011627681 -- N = 20450
End of computing.


;; Clock -> 2011-07-12, 11h 28m 7s.
Computing the boundary of the generator 105/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 9s.
Computing the boundary of the generator 106/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 11s.
Computing the boundary of the generator 107/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 15s.
Computing the boundary of the generator 108/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 20s.
Computing the boundary of the generator 109/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 24s.
Computing the boundary of the generator 110/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 28s.
Computing the boundary of the generator 111/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 30s.
Computing the boundary of the generator 112/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 34s.
Computing the boundary of the generator 113/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 37s.
Computing the boundary of the generator 114/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 42s.
Computing the boundary of the generator 115/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 46s.
Computing the boundary of the generator 116/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 50s.
Computing the boundary of the generator 117/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 5> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 50s.
Computing the boundary of the generator 118/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 4> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 28m 53s.
Computing the boundary of the generator 119/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.013046071 -- N = 21789
End of computing.


;; Clock -> 2011-07-12, 11h 29m 1s.
Computing the boundary of the generator 120/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 29m 15s.
Computing the boundary of the generator 121/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 29m 27s.
Computing the boundary of the generator 122/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 29m 36s.
Computing the boundary of the generator 123/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.014317282 -- N = 22849
End of computing.


;; Clock -> 2011-07-12, 11h 30m 13s.
Computing the boundary of the generator 124/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.015753578 -- N = 16408
End of computing.


;; Clock -> 2011-07-12, 11h 30m 46s.
Computing the boundary of the generator 125/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.013701802 -- N = 17436
End of computing.


;; Clock -> 2011-07-12, 11h 31m 44s.
Computing the boundary of the generator 126/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.015032681 -- N = 18066
End of computing.


;; Clock -> 2011-07-12, 11h 32m 41s.
Computing the boundary of the generator 127/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.016601875 -- N = 4967
End of computing.


;; Clock -> 2011-07-12, 11h 33m 32s.
Computing the boundary of the generator 128/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 4> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 33m 37s.
Computing the boundary of the generator 129/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 34m 12s.
Computing the boundary of the generator 130/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.011981033 -- N = 6448
End of computing.


;; Clock -> 2011-07-12, 11h 34m 38s.
Computing the boundary of the generator 131/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 35m 18s.
Computing the boundary of the generator 132/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.014471108 -- N = 7937
End of computing.


;; Clock -> 2011-07-12, 11h 35m 58s.
Computing the boundary of the generator 133/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 36m 33s.
Computing the boundary of the generator 134/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.01644922 -- N = 6611
End of computing.


;; Clock -> 2011-07-12, 11h 36m 47s.
Computing the boundary of the generator 135/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 37m 29s.
Computing the boundary of the generator 136/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.017286744 -- N = 7003
End of computing.


;; Clock -> 2011-07-12, 11h 38m 18s.
Computing the boundary of the generator 137/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 38m 29s.
Computing the boundary of the generator 138/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 5> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 38m 30s.
Computing the boundary of the generator 139/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 38m 42s.
Computing the boundary of the generator 140/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 38m 50s.
Computing the boundary of the generator 141/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.018456804 -- N = 7390
End of computing.


;; Clock -> 2011-07-12, 11h 39m 23s.
Computing the boundary of the generator 142/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 39m 55s.
Computing the boundary of the generator 143/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.020442575 -- N = 7937
End of computing.


;; Clock -> 2011-07-12, 11h 40m 21s.
Computing the boundary of the generator 144/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 40m 31s.
Computing the boundary of the generator 145/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 41m 1s.
Computing the boundary of the generator 146/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.02240238 -- N = 8307
End of computing.


;; Clock -> 2011-07-12, 11h 41m 33s.
Computing the boundary of the generator 147/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 41m 39s.
Computing the boundary of the generator 148/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 4> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 41m 43s.
Computing the boundary of the generator 149/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 3> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 41m 54s.
Computing the boundary of the generator 150/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.023798611 -- N = 8615
End of computing.


;; Clock -> 2011-07-12, 11h 42m 29s.
Computing the boundary of the generator 151/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 37s.
Computing the boundary of the generator 152/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 4 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 11h 42m 40s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 63.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 1/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 2/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 3/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 4/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 5/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 6/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 7/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 8/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 9/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 10/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 11/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 12/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 13/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 14/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 15/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 40s.
Computing the boundary of the generator 16/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 17/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 18/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 19/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 20/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 21/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 22/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 23/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 24/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 25/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 41s.
Computing the boundary of the generator 26/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 27/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 28/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 29/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 30/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 31/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 32/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 42s.
Computing the boundary of the generator 33/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 43s.
Computing the boundary of the generator 34/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 43s.
Computing the boundary of the generator 35/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 43s.
Computing the boundary of the generator 36/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 43s.
Computing the boundary of the generator 37/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 44s.
Computing the boundary of the generator 38/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 44s.
Computing the boundary of the generator 39/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 45s.
Computing the boundary of the generator 40/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 45s.
Computing the boundary of the generator 41/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 46s.
Computing the boundary of the generator 42/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 46s.
Computing the boundary of the generator 43/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 4> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 46s.
Computing the boundary of the generator 44/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 46s.
Computing the boundary of the generator 45/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 47s.
Computing the boundary of the generator 46/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 48s.
Computing the boundary of the generator 47/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024053123 -- N = 8716
End of computing.


;; Clock -> 2011-07-12, 11h 42m 50s.
Computing the boundary of the generator 48/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 50s.
Computing the boundary of the generator 49/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 51s.
Computing the boundary of the generator 50/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 51s.
Computing the boundary of the generator 51/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 52s.
Computing the boundary of the generator 52/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 53s.
Computing the boundary of the generator 53/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 53s.
Computing the boundary of the generator 54/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 4> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 53s.
Computing the boundary of the generator 55/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 54s.
Computing the boundary of the generator 56/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 54s.
Computing the boundary of the generator 57/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 55s.
Computing the boundary of the generator 58/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 55s.
Computing the boundary of the generator 59/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 56s.
Computing the boundary of the generator 60/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 56s.
Computing the boundary of the generator 61/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 56s.
Computing the boundary of the generator 62/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 63/63 (dimension 4) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 152.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 1/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 2/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 3/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 4/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 5/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 6/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 7/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 8/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 57s.
Computing the boundary of the generator 9/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 10/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 11/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 12/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 13/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 14/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 15/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 16/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 17/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 18/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 58s.
Computing the boundary of the generator 19/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 20/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 21/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 22/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 23/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 24/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 42m 59s.
Computing the boundary of the generator 25/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 26/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 27/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 28/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 29/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 30/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 0s.
Computing the boundary of the generator 31/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 32/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 33/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 34/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 35/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 36/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 1s.
Computing the boundary of the generator 37/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][3 2]>>> <<Abar>>> 
*CR* -- Cut = 0.023999266 -- N = 8751
End of computing.


;; Clock -> 2011-07-12, 11h 43m 2s.
Computing the boundary of the generator 38/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 2s.
Computing the boundary of the generator 39/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 2s.
Computing the boundary of the generator 40/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 2s.
Computing the boundary of the generator 41/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 3s.
Computing the boundary of the generator 42/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 3s.
Computing the boundary of the generator 43/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 3s.
Computing the boundary of the generator 44/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 3s.
Computing the boundary of the generator 45/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 4s.
Computing the boundary of the generator 46/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 4s.
Computing the boundary of the generator 47/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 5s.
Computing the boundary of the generator 48/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 5s.
Computing the boundary of the generator 49/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 5s.
Computing the boundary of the generator 50/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 5s.
Computing the boundary of the generator 51/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 6s.
Computing the boundary of the generator 52/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 6s.
Computing the boundary of the generator 53/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 6s.
Computing the boundary of the generator 54/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 6s.
Computing the boundary of the generator 55/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 7s.
Computing the boundary of the generator 56/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 7s.
Computing the boundary of the generator 57/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 8s.
Computing the boundary of the generator 58/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 8s.
Computing the boundary of the generator 59/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 9s.
Computing the boundary of the generator 60/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 9s.
Computing the boundary of the generator 61/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 10s.
Computing the boundary of the generator 62/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 10s.
Computing the boundary of the generator 63/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 11s.
Computing the boundary of the generator 64/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 11s.
Computing the boundary of the generator 65/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 12s.
Computing the boundary of the generator 66/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 12s.
Computing the boundary of the generator 67/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[5 4]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 12s.
Computing the boundary of the generator 68/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[2 1][3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 12s.
Computing the boundary of the generator 69/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar>>> <<Abar[3 2][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 13s.
Computing the boundary of the generator 70/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 13s.
Computing the boundary of the generator 71/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 13s.
Computing the boundary of the generator 72/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 13s.
Computing the boundary of the generator 73/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar>>> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 13s.
Computing the boundary of the generator 74/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1]>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 14s.
Computing the boundary of the generator 75/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 14s.
Computing the boundary of the generator 76/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 15s.
Computing the boundary of the generator 77/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 16s.
Computing the boundary of the generator 78/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 17s.
Computing the boundary of the generator 79/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar>>> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 17s.
Computing the boundary of the generator 80/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 17s.
Computing the boundary of the generator 81/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 18s.
Computing the boundary of the generator 82/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1]>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 18s.
Computing the boundary of the generator 83/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 19s.
Computing the boundary of the generator 84/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 20s.
Computing the boundary of the generator 85/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 21s.
Computing the boundary of the generator 86/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 22s.
Computing the boundary of the generator 87/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 23s.
Computing the boundary of the generator 88/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 23s.
Computing the boundary of the generator 89/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 24s.
Computing the boundary of the generator 90/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024359837 -- N = 8862
End of computing.


;; Clock -> 2011-07-12, 11h 43m 25s.
Computing the boundary of the generator 91/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 25s.
Computing the boundary of the generator 92/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 26s.
Computing the boundary of the generator 93/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar>>> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 26s.
Computing the boundary of the generator 94/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[5 4]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 26s.
Computing the boundary of the generator 95/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[2 1][3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 27s.
Computing the boundary of the generator 96/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 0> <<Abar[3 2][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 27s.
Computing the boundary of the generator 97/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 27s.
Computing the boundary of the generator 98/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 1> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 27s.
Computing the boundary of the generator 99/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[4 3]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 28s.
Computing the boundary of the generator 100/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 0> <<Abar[2 1][2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 28s.
Computing the boundary of the generator 101/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 2> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 29s.
Computing the boundary of the generator 102/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 1> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 29s.
Computing the boundary of the generator 103/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 30s.
Computing the boundary of the generator 104/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 31s.
Computing the boundary of the generator 105/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 0> <<Abar[3 2]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 31s.
Computing the boundary of the generator 106/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 3> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 32s.
Computing the boundary of the generator 107/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 2> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 33s.
Computing the boundary of the generator 108/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 34s.
Computing the boundary of the generator 109/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 36s.
Computing the boundary of the generator 110/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 1> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 37s.
Computing the boundary of the generator 111/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 37s.
Computing the boundary of the generator 112/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 38s.
Computing the boundary of the generator 113/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 39s.
Computing the boundary of the generator 114/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 40s.
Computing the boundary of the generator 115/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 41s.
Computing the boundary of the generator 116/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 42s.
Computing the boundary of the generator 117/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> 5> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 42s.
Computing the boundary of the generator 118/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> 4> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 43s.
Computing the boundary of the generator 119/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 44s.
Computing the boundary of the generator 120/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 46s.
Computing the boundary of the generator 121/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 0> 3> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024477713 -- N = 8980
End of computing.


;; Clock -> 2011-07-12, 11h 43m 49s.
Computing the boundary of the generator 122/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 3> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 49s.
Computing the boundary of the generator 123/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 56s.
Computing the boundary of the generator 124/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 1> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 43m 57s.
Computing the boundary of the generator 125/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 4s.
Computing the boundary of the generator 126/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024651775 -- N = 9089
End of computing.


;; Clock -> 2011-07-12, 11h 44m 15s.
Computing the boundary of the generator 127/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 0> 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 16s.
Computing the boundary of the generator 128/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 4> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 17s.
Computing the boundary of the generator 129/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 22s.
Computing the boundary of the generator 130/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 2> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 26s.
Computing the boundary of the generator 131/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.02475143 -- N = 9176
End of computing.


;; Clock -> 2011-07-12, 11h 44m 30s.
Computing the boundary of the generator 132/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 35s.
Computing the boundary of the generator 133/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 1> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 38s.
Computing the boundary of the generator 134/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 41s.
Computing the boundary of the generator 135/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 44s.
Computing the boundary of the generator 136/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 50s.
Computing the boundary of the generator 137/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 0> 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024916824 -- N = 9269
End of computing.


;; Clock -> 2011-07-12, 11h 44m 52s.
Computing the boundary of the generator 138/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 5> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 52s.
Computing the boundary of the generator 139/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 1> 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 53s.
Computing the boundary of the generator 140/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 0> 3> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 54s.
Computing the boundary of the generator 141/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 2> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 56s.
Computing the boundary of the generator 142/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 1> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 58s.
Computing the boundary of the generator 143/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 0> 2> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 44m 59s.
Computing the boundary of the generator 144/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 3> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 0s.
Computing the boundary of the generator 145/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 2> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 3s.
Computing the boundary of the generator 146/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 1> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.024949644 -- N = 9336
End of computing.


;; Clock -> 2011-07-12, 11h 45m 10s.
Computing the boundary of the generator 147/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 0> 1> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 10s.
Computing the boundary of the generator 148/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 0 4> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 11s.
Computing the boundary of the generator 149/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 1 3> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 13s.
Computing the boundary of the generator 150/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 2 2> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 18s.
Computing the boundary of the generator 151/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 3 1> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 11h 45m 19s.
Computing the boundary of the generator 152/152 (dimension 5) :
<TnPr <TnPr <TnPr <TnPr <TnPr <TnPr <TnPr 4 0> 0> 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 11h 45m 19s.




Homotopy in dimension 4 :


Component Z/3Z

Component Z/3Z

Component Z/3Z

Component Z/3Z

Component Z/3Z

Component Z/3Z

Component Z/3Z

Component Z/3Z

NIL
CL-USER(15): (setf z (z-group))
[K1423 Abelian-Group]
CL-USER(16): (setf k-z-1 (k-g-1 z))
[K1424 Abelian-Simplicial-Group]
CL-USER(17): (setf s-k-z-1 (suspension k-z-1))
[K1436 Simplicial-Set]
CL-USER(18): (homotopy s-k-z-1 2)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 0s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 0s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 0s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 0s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 0s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 0s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 0s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 0s.




Homotopy in dimension 2 :


Component Z

NIL
CL-USER(19):  (homotopy s-k-z-1 3)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 4s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 4s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 4s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 4s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 4s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 4s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 4s.
Computing the boundary of the generator 1/1 (dimension 3) :
<TnPr S1 S1> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 4s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 4s.
Computing the boundary of the generator 1/1 (dimension 3) :
<TnPr S1 S1> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 4s.




Homotopy in dimension 3 :


Component Z

NIL
CL-USER(20):  (homotopy s-k-z-1 4)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 9s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 9s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 9s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/1 (dimension 3) :
<TnPr S1 S1> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 9s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/2 (dimension 4) :
<TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 2/2 (dimension 4) :
<TnPr <TnPr S1 *> <<Abar[2 S1]>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 1/2 (dimension 5) :
<TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 9s.
Computing the boundary of the generator 2/2 (dimension 5) :
<TnPr <TnPr S1 S1> <<Abar[2 S1]>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 10s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 10s.
Computing the boundary of the generator 1/2 (dimension 4) :
<TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 10s.
Computing the boundary of the generator 2/2 (dimension 4) :
<TnPr <TnPr S1 *> <<Abar[2 S1]>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 10s.
Computing the boundary of the generator 1/2 (dimension 5) :
<TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 10s.
Computing the boundary of the generator 2/2 (dimension 5) :
<TnPr <TnPr S1 S1> <<Abar[2 S1]>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 10s.




Homotopy in dimension 4 :


Component Z/2Z

NIL
CL-USER(21):  (homotopy s-k-z-1 5)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 16s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 16s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 16s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/1 (dimension 3) :
<TnPr S1 S1> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 16s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/2 (dimension 4) :
<TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 2/2 (dimension 4) :
<TnPr <TnPr S1 *> <<Abar[2 S1]>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/2 (dimension 5) :
<TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 2/2 (dimension 5) :
<TnPr <TnPr S1 S1> <<Abar[2 S1]>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 16s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 7.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 2/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 3/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 4/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 5/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 6/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 7/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 S1> <<Abar[2 S1]>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 12.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 1/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 2/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 3/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 4/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 5/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 6/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 7/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 8/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 9/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 10/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 11/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 16s.
Computing the boundary of the generator 12/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 17s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 7.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 1/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 2/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 3/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 4/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 5/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 6/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 7/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 S1> <<Abar[2 S1]>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 12.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 1/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 2/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 3/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 4/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 5/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 6/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 7/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 8/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 9/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 10/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 11/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 17s.
Computing the boundary of the generator 12/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 17s.




Homotopy in dimension 5 :


Component Z/2Z

NIL
CL-USER(22):  (homotopy s-k-z-1 6)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/1 (dimension 2) :
S1 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 1.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/1 (dimension 3) :
<TnPr S1 S1> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 0.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/2 (dimension 4) :
<TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 2/2 (dimension 4) :
<TnPr <TnPr S1 *> <<Abar[2 S1]>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 2.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/2 (dimension 5) :
<TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 2/2 (dimension 5) :
<TnPr <TnPr S1 S1> <<Abar[2 S1]>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 7.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 2/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 3/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 4/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 5/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 6/7 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 7/7 (dimension 5) :
<TnPr <TnPr <TnPr S1 S1> <<Abar[2 S1]>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 12.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 2/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 3/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 4/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 5/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 6/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 7/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 8/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 9/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 10/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 11/12 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 12/12 (dimension 6) :
<TnPr <TnPr <TnPr S1 *> <<Abar[2 S1][2 S1]>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-12, 12h 48m 26s.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 17.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 1/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 2/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 3/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 4/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 26s.
Computing the boundary of the generator 5/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 6/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 7/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 8/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 9/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 10/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 11/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 12/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 13/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 14/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 15/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 27s.
Computing the boundary of the generator 16/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 17/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar[2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 7.
Rank of the source-module : 32.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 1/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[5 4]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 2/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[2 1][3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 3/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[3 2][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 4/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 5/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 6/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 7/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 29s.
Computing the boundary of the generator 8/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 9/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 10/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 11/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 12/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[6 5]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 13/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[2 1][4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 14/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[3 2][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 15/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[4 3][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 30s.
Computing the boundary of the generator 16/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[2 1][2 1][2 1]>>]>>> <<Abar>>> 
*CR* -- Cut = 0.025002742 -- N = 9372
End of computing.


;; Clock -> 2011-07-12, 12h 48m 31s.
Computing the boundary of the generator 17/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 31s.
Computing the boundary of the generator 18/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 31s.
Computing the boundary of the generator 19/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 31s.
Computing the boundary of the generator 20/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 32s.
Computing the boundary of the generator 21/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 32s.
Computing the boundary of the generator 22/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 32s.
Computing the boundary of the generator 23/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 32s.
Computing the boundary of the generator 24/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 33s.
Computing the boundary of the generator 25/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 33s.
Computing the boundary of the generator 26/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 33s.
Computing the boundary of the generator 27/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 33s.
Computing the boundary of the generator 28/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 34s.
Computing the boundary of the generator 29/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 35s.
Computing the boundary of the generator 30/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 36s.
Computing the boundary of the generator 31/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 48m 41s.
Computing the boundary of the generator 32/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar[2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.025244262 -- N = 9428
*CR* -- Cut = 0.026013508 -- N = 9770
*CR* -- Cut = 0.025819128 -- N = 9868
End of computing.



;; Clock -> 2011-07-12, 12h 50m 21s.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 17.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 1/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 2/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 3/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 4/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 5/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 6/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 7/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 8/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 21s.
Computing the boundary of the generator 9/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 10/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 11/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 12/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 13/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 14/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 15/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 22s.
Computing the boundary of the generator 16/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 17/17 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar[2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 7.
Rank of the source-module : 32.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 1/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[5 4]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 2/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[2 1][3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 3/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[6 <<Abar[3 2][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 4/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar>>> <<Abar[7 <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 5/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 6/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 7/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 8/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 9/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 10/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 11/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 12/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[6 5]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 23s.
Computing the boundary of the generator 13/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[2 1][4 3]>>]>>> <<Abar>>> 
*CR* -- Cut = 0.02626452 -- N = 9977
End of computing.


;; Clock -> 2011-07-12, 12h 50m 24s.
Computing the boundary of the generator 14/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[3 2][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 24s.
Computing the boundary of the generator 15/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[4 3][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 24s.
Computing the boundary of the generator 16/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[7 <<Abar[2 1][2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 24s.
Computing the boundary of the generator 17/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 25s.
Computing the boundary of the generator 18/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar>>> <<Abar[4 <<Abar[3 2]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 25s.
Computing the boundary of the generator 19/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 25s.
Computing the boundary of the generator 20/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 26s.
Computing the boundary of the generator 21/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 26s.
Computing the boundary of the generator 22/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 27s.
Computing the boundary of the generator 23/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 27s.
Computing the boundary of the generator 24/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1]>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 27s.
Computing the boundary of the generator 25/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 27s.
Computing the boundary of the generator 26/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 28s.
Computing the boundary of the generator 27/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1]>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 28s.
Computing the boundary of the generator 28/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 29s.
Computing the boundary of the generator 29/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN *> <<Abar[2 S1][2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 29s.
Computing the boundary of the generator 30/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 *> <<Abar[2 S1]>>> <<Abar[3 <<Abar[2 1]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 30s.
Computing the boundary of the generator 31/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S-BSGN S1> <<Abar[2 S1][2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-12, 12h 50m 34s.
Computing the boundary of the generator 32/32 (dimension 7) :
<TnPr <TnPr <TnPr <TnPr S1 S1> <<Abar[2 S1][2 S1]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.02604392 -- N = 10039
*CR* -- Cut = 0.02635973 -- N = 10124
End of computing.



;; Clock -> 2011-07-12, 12h 51m 35s.




Homotopy in dimension 6 :


Component Z/12Z

NIL
CL-USER(23): (setf s1 (filetostring "resolutions3-8-first-part.txt"))



CL-USER(25): (setf s2 (filetostring "resolutions3-8-second-part.txt"))



			
CL-USER(26): (setf s (concatenate 'string   s1 s2))



			
CL-USER(27): (setf rsltn (OMparseNextObject s))
[K2362 Reduction K2356 => K17]
CL-USER(28): (setf s3 (group1 (tcc rsltn)))
[K2355 Group]
CL-USER(30): (setf (slot-value s3 'resolution) rsltn)
[K2362 Reduction K2356 => K17]
CL-USER(31): (setf k-s3-1 (k-g-1 s3))
[K2363 Simplicial-Group]
CL-USER(32): (setf s-k-s3-1 (suspension k-s3-1))
[K2375 Simplicial-Set]
CL-USER(33): (homotopy s-k-s3-1 3)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.



;; Clock -> 2011-07-14, 9h 54m 34s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 54m 34s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 54m 34s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 6.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 1/6 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 2/6 (dimension 3) :
<TnPr g11 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 3/6 (dimension 3) :
<TnPr g12 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 4/6 (dimension 3) :
<TnPr g21 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 5/6 (dimension 3) :
<TnPr g22 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 34s.
Computing the boundary of the generator 6/6 (dimension 3) :
<TnPr g23 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 10.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 1/10 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 2/10 (dimension 4) :
<TnPr g11 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 3/10 (dimension 4) :
<TnPr g12 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 4/10 (dimension 4) :
<TnPr g21 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 5/10 (dimension 4) :
<TnPr g22 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 6/10 (dimension 4) :
<TnPr g23 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 7/10 (dimension 4) :
<TnPr g31 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 8/10 (dimension 4) :
<TnPr g32 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 9/10 (dimension 4) :
<TnPr g33 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 10/10 (dimension 4) :
<TnPr g34 0> 
End of computing.



;; Clock -> 2011-07-14, 9h 54m 35s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 6.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 1/6 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 2/6 (dimension 3) :
<TnPr g11 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 3/6 (dimension 3) :
<TnPr g12 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 4/6 (dimension 3) :
<TnPr g21 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 5/6 (dimension 3) :
<TnPr g22 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 6/6 (dimension 3) :
<TnPr g23 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 10.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 1/10 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 2/10 (dimension 4) :
<TnPr g11 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 3/10 (dimension 4) :
<TnPr g12 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 4/10 (dimension 4) :
<TnPr g21 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 5/10 (dimension 4) :
<TnPr g22 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 6/10 (dimension 4) :
<TnPr g23 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 7/10 (dimension 4) :
<TnPr g31 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 8/10 (dimension 4) :
<TnPr g32 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 9/10 (dimension 4) :
<TnPr g33 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 54m 35s.
Computing the boundary of the generator 10/10 (dimension 4) :
<TnPr g34 0> 
End of computing.



;; Clock -> 2011-07-14, 9h 54m 35s.




Homotopy in dimension 3 :


Component Z/2Z

NIL
CL-USER(34): (homotopy s-k-s3-1 4)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 49s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 49s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 49s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 49s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 6.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 1/6 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 2/6 (dimension 3) :
<TnPr g11 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 3/6 (dimension 3) :
<TnPr g12 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 4/6 (dimension 3) :
<TnPr g21 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 5/6 (dimension 3) :
<TnPr g22 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 6/6 (dimension 3) :
<TnPr g23 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 10.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 1/10 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 2/10 (dimension 4) :
<TnPr g11 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 3/10 (dimension 4) :
<TnPr g12 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 4/10 (dimension 4) :
<TnPr g21 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 5/10 (dimension 4) :
<TnPr g22 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 6/10 (dimension 4) :
<TnPr g23 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 7/10 (dimension 4) :
<TnPr g31 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 8/10 (dimension 4) :
<TnPr g32 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 9/10 (dimension 4) :
<TnPr g33 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 10/10 (dimension 4) :
<TnPr g34 0> 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 50s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 16.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 1/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 50s.
Computing the boundary of the generator 2/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 3/16 (dimension 4) :
<TnPr <TnPr S-BSGN 1> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 4/16 (dimension 4) :
<TnPr <TnPr S-BSGN 2> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 5/16 (dimension 4) :
<TnPr <TnPr g11 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 6/16 (dimension 4) :
<TnPr <TnPr g12 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 7/16 (dimension 4) :
<TnPr <TnPr S-BSGN 4> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 8/16 (dimension 4) :
<TnPr <TnPr g11 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 9/16 (dimension 4) :
<TnPr <TnPr g12 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 10/16 (dimension 4) :
<TnPr <TnPr g21 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 11/16 (dimension 4) :
<TnPr <TnPr g22 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 12/16 (dimension 4) :
<TnPr <TnPr g23 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 13/16 (dimension 4) :
<TnPr <TnPr g31 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 14/16 (dimension 4) :
<TnPr <TnPr g32 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 15/16 (dimension 4) :
<TnPr <TnPr g33 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 16/16 (dimension 4) :
<TnPr <TnPr g34 0> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 29.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 1/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 2/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 3/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 4/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 5/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 6/29 (dimension 5) :
<TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 51s.
Computing the boundary of the generator 7/29 (dimension 5) :
<TnPr <TnPr g11 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 8/29 (dimension 5) :
<TnPr <TnPr g12 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 9/29 (dimension 5) :
<TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 10/29 (dimension 5) :
<TnPr <TnPr g11 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 11/29 (dimension 5) :
<TnPr <TnPr g12 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 12/29 (dimension 5) :
<TnPr <TnPr g21 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 13/29 (dimension 5) :
<TnPr <TnPr g22 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 14/29 (dimension 5) :
<TnPr <TnPr g23 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 15/29 (dimension 5) :
<TnPr <TnPr S-BSGN 5> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 16/29 (dimension 5) :
<TnPr <TnPr g11 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 17/29 (dimension 5) :
<TnPr <TnPr g12 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 18/29 (dimension 5) :
<TnPr <TnPr g21 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 19/29 (dimension 5) :
<TnPr <TnPr g22 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 52s.
Computing the boundary of the generator 20/29 (dimension 5) :
<TnPr <TnPr g23 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 21/29 (dimension 5) :
<TnPr <TnPr g31 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 22/29 (dimension 5) :
<TnPr <TnPr g32 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 23/29 (dimension 5) :
<TnPr <TnPr g33 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 24/29 (dimension 5) :
<TnPr <TnPr g34 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 25/29 (dimension 5) :
<TnPr <TnPr g41 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 26/29 (dimension 5) :
<TnPr <TnPr g42 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 27/29 (dimension 5) :
<TnPr <TnPr g43 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 28/29 (dimension 5) :
<TnPr <TnPr g44 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 29/29 (dimension 5) :
<TnPr <TnPr g45 0> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 53s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 16.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 1/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 2/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 3/16 (dimension 4) :
<TnPr <TnPr S-BSGN 1> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 4/16 (dimension 4) :
<TnPr <TnPr S-BSGN 2> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 5/16 (dimension 4) :
<TnPr <TnPr g11 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 6/16 (dimension 4) :
<TnPr <TnPr g12 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 7/16 (dimension 4) :
<TnPr <TnPr S-BSGN 4> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 8/16 (dimension 4) :
<TnPr <TnPr g11 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 9/16 (dimension 4) :
<TnPr <TnPr g12 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 10/16 (dimension 4) :
<TnPr <TnPr g21 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 11/16 (dimension 4) :
<TnPr <TnPr g22 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 12/16 (dimension 4) :
<TnPr <TnPr g23 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 13/16 (dimension 4) :
<TnPr <TnPr g31 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 14/16 (dimension 4) :
<TnPr <TnPr g32 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 15/16 (dimension 4) :
<TnPr <TnPr g33 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 16/16 (dimension 4) :
<TnPr <TnPr g34 0> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 29.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 1/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 2/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 3/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 4/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 5/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 6/29 (dimension 5) :
<TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 7/29 (dimension 5) :
<TnPr <TnPr g11 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 8/29 (dimension 5) :
<TnPr <TnPr g12 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 53s.
Computing the boundary of the generator 9/29 (dimension 5) :
<TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 10/29 (dimension 5) :
<TnPr <TnPr g11 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 11/29 (dimension 5) :
<TnPr <TnPr g12 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 12/29 (dimension 5) :
<TnPr <TnPr g21 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 13/29 (dimension 5) :
<TnPr <TnPr g22 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 14/29 (dimension 5) :
<TnPr <TnPr g23 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 15/29 (dimension 5) :
<TnPr <TnPr S-BSGN 5> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 16/29 (dimension 5) :
<TnPr <TnPr g11 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 17/29 (dimension 5) :
<TnPr <TnPr g12 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 18/29 (dimension 5) :
<TnPr <TnPr g21 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 19/29 (dimension 5) :
<TnPr <TnPr g22 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 20/29 (dimension 5) :
<TnPr <TnPr g23 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 21/29 (dimension 5) :
<TnPr <TnPr g31 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 22/29 (dimension 5) :
<TnPr <TnPr g32 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 23/29 (dimension 5) :
<TnPr <TnPr g33 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 24/29 (dimension 5) :
<TnPr <TnPr g34 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 25/29 (dimension 5) :
<TnPr <TnPr g41 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 26/29 (dimension 5) :
<TnPr <TnPr g42 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 27/29 (dimension 5) :
<TnPr <TnPr g43 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 28/29 (dimension 5) :
<TnPr <TnPr g44 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 55m 54s.
Computing the boundary of the generator 29/29 (dimension 5) :
<TnPr <TnPr g45 0> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 55m 54s.




Homotopy in dimension 4 :


Component Z/12Z

NIL
CL-USER(35): (homotopy s-k-s3-1 5)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.



;; Clock -> 2011-07-14, 9h 56m 7s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 56m 7s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 56m 7s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 6.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/6 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/6 (dimension 3) :
<TnPr g11 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 3/6 (dimension 3) :
<TnPr g12 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 4/6 (dimension 3) :
<TnPr g21 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 5/6 (dimension 3) :
<TnPr g22 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 6/6 (dimension 3) :
<TnPr g23 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 10.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 1/10 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 2/10 (dimension 4) :
<TnPr g11 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 3/10 (dimension 4) :
<TnPr g12 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 7s.
Computing the boundary of the generator 4/10 (dimension 4) :
<TnPr g21 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 5/10 (dimension 4) :
<TnPr g22 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 6/10 (dimension 4) :
<TnPr g23 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 7/10 (dimension 4) :
<TnPr g31 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 8/10 (dimension 4) :
<TnPr g32 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 9/10 (dimension 4) :
<TnPr g33 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 10/10 (dimension 4) :
<TnPr g34 0> 
End of computing.



;; Clock -> 2011-07-14, 9h 56m 8s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 16.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 1/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 2/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 3/16 (dimension 4) :
<TnPr <TnPr S-BSGN 1> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 4/16 (dimension 4) :
<TnPr <TnPr S-BSGN 2> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 5/16 (dimension 4) :
<TnPr <TnPr g11 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 6/16 (dimension 4) :
<TnPr <TnPr g12 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 7/16 (dimension 4) :
<TnPr <TnPr S-BSGN 4> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 8/16 (dimension 4) :
<TnPr <TnPr g11 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 9/16 (dimension 4) :
<TnPr <TnPr g12 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 10/16 (dimension 4) :
<TnPr <TnPr g21 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 11/16 (dimension 4) :
<TnPr <TnPr g22 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 12/16 (dimension 4) :
<TnPr <TnPr g23 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 13/16 (dimension 4) :
<TnPr <TnPr g31 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 14/16 (dimension 4) :
<TnPr <TnPr g32 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 15/16 (dimension 4) :
<TnPr <TnPr g33 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 16/16 (dimension 4) :
<TnPr <TnPr g34 0> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 29.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 1/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 2/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 8s.
Computing the boundary of the generator 3/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 4/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 5/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 6/29 (dimension 5) :
<TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 7/29 (dimension 5) :
<TnPr <TnPr g11 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 8/29 (dimension 5) :
<TnPr <TnPr g12 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 9/29 (dimension 5) :
<TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 10/29 (dimension 5) :
<TnPr <TnPr g11 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 11/29 (dimension 5) :
<TnPr <TnPr g12 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 12/29 (dimension 5) :
<TnPr <TnPr g21 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 13/29 (dimension 5) :
<TnPr <TnPr g22 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 14/29 (dimension 5) :
<TnPr <TnPr g23 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 15/29 (dimension 5) :
<TnPr <TnPr S-BSGN 5> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 16/29 (dimension 5) :
<TnPr <TnPr g11 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 17/29 (dimension 5) :
<TnPr <TnPr g12 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 18/29 (dimension 5) :
<TnPr <TnPr g21 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 19/29 (dimension 5) :
<TnPr <TnPr g22 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 20/29 (dimension 5) :
<TnPr <TnPr g23 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 21/29 (dimension 5) :
<TnPr <TnPr g31 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 22/29 (dimension 5) :
<TnPr <TnPr g32 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 23/29 (dimension 5) :
<TnPr <TnPr g33 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 24/29 (dimension 5) :
<TnPr <TnPr g34 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 25/29 (dimension 5) :
<TnPr <TnPr g41 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 26/29 (dimension 5) :
<TnPr <TnPr g42 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 27/29 (dimension 5) :
<TnPr <TnPr g43 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 28/29 (dimension 5) :
<TnPr <TnPr g44 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 29/29 (dimension 5) :
<TnPr <TnPr g45 0> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 56m 9s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 36.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 1/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 2/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 3/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 4/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 5/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 6/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 7/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 8/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 9/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 10/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 11/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 12/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 13/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 14/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 15/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 16/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 9s.
Computing the boundary of the generator 17/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 18/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 19/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 20/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 21/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 22/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 5> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 23/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 24/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 25/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 26/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 10s.
Computing the boundary of the generator 27/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 11s.
Computing the boundary of the generator 28/36 (dimension 5) :
<TnPr <TnPr <TnPr g31 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 11s.
Computing the boundary of the generator 29/36 (dimension 5) :
<TnPr <TnPr <TnPr g32 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 11s.
Computing the boundary of the generator 30/36 (dimension 5) :
<TnPr <TnPr <TnPr g33 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 11s.
Computing the boundary of the generator 31/36 (dimension 5) :
<TnPr <TnPr <TnPr g34 1> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.026471086 -- N = 10197
End of computing.


;; Clock -> 2011-07-14, 9h 56m 11s.
Computing the boundary of the generator 32/36 (dimension 5) :
<TnPr <TnPr <TnPr g41 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 12s.
Computing the boundary of the generator 33/36 (dimension 5) :
<TnPr <TnPr <TnPr g42 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 12s.
Computing the boundary of the generator 34/36 (dimension 5) :
<TnPr <TnPr <TnPr g43 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 12s.
Computing the boundary of the generator 35/36 (dimension 5) :
<TnPr <TnPr <TnPr g44 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 12s.
Computing the boundary of the generator 36/36 (dimension 5) :
<TnPr <TnPr <TnPr g45 0> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 69.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 1/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 2/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 3/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 4/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 5/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 6/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 7/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 8/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 9/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 13s.
Computing the boundary of the generator 10/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 11/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 12/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 13/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 14/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 15/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 16/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 17/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 18/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 19/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[6 5]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 20/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 21/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 22/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[4 3][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 23/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 14s.
Computing the boundary of the generator 24/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 25/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 26/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 27/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 28/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 29/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 30/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 31/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 32/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 33/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 15s.
Computing the boundary of the generator 34/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 35/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 36/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 37/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 38/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 39/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 4> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 16s.
Computing the boundary of the generator 40/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 18s.
Computing the boundary of the generator 41/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 18s.
Computing the boundary of the generator 42/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 19s.
Computing the boundary of the generator 43/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 20s.
Computing the boundary of the generator 44/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 21s.
Computing the boundary of the generator 45/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 22s.
Computing the boundary of the generator 46/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 22s.
Computing the boundary of the generator 47/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 22s.
Computing the boundary of the generator 48/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 22s.
Computing the boundary of the generator 49/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 6> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 23s.
Computing the boundary of the generator 50/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 25s.
Computing the boundary of the generator 51/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 25s.
Computing the boundary of the generator 52/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 3> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.026193824 -- N = 10254
End of computing.


;; Clock -> 2011-07-14, 9h 56m 30s.
Computing the boundary of the generator 53/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 33s.
Computing the boundary of the generator 54/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 36s.
Computing the boundary of the generator 55/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.026362488 -- N = 10311
End of computing.


;; Clock -> 2011-07-14, 9h 56m 43s.
Computing the boundary of the generator 56/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 46s.
Computing the boundary of the generator 57/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 56m 51s.
Computing the boundary of the generator 58/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.026545823 -- N = 10368
End of computing.


;; Clock -> 2011-07-14, 9h 57m 2s.
Computing the boundary of the generator 59/69 (dimension 6) :
<TnPr <TnPr <TnPr g41 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 3s.
Computing the boundary of the generator 60/69 (dimension 6) :
<TnPr <TnPr <TnPr g42 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 7s.
Computing the boundary of the generator 61/69 (dimension 6) :
<TnPr <TnPr <TnPr g43 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 11s.
Computing the boundary of the generator 62/69 (dimension 6) :
<TnPr <TnPr <TnPr g44 1> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.02681824 -- N = 10420
End of computing.


;; Clock -> 2011-07-14, 9h 57m 18s.
Computing the boundary of the generator 63/69 (dimension 6) :
<TnPr <TnPr <TnPr g45 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 25s.
Computing the boundary of the generator 64/69 (dimension 6) :
<TnPr <TnPr <TnPr g51 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 25s.
Computing the boundary of the generator 65/69 (dimension 6) :
<TnPr <TnPr <TnPr g52 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 26s.
Computing the boundary of the generator 66/69 (dimension 6) :
<TnPr <TnPr <TnPr g53 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 28s.
Computing the boundary of the generator 67/69 (dimension 6) :
<TnPr <TnPr <TnPr g54 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 28s.
Computing the boundary of the generator 68/69 (dimension 6) :
<TnPr <TnPr <TnPr g55 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 30s.
Computing the boundary of the generator 69/69 (dimension 6) :
<TnPr <TnPr <TnPr g56 0> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 57m 32s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 36.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 1/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 2/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 3/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 4/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 5/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 6/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 7/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 8/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 9/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 10/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 11/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 12/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> <<Abar>>> 
*CR* -- Cut = 0.027058253 -- N = 10497
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 13/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 14/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 15/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 16/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 17/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 18/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 19/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 20/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 21/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 22/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 5> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 32s.
Computing the boundary of the generator 23/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 33s.
Computing the boundary of the generator 24/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 33s.
Computing the boundary of the generator 25/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 33s.
Computing the boundary of the generator 26/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 33s.
Computing the boundary of the generator 27/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 33s.
Computing the boundary of the generator 28/36 (dimension 5) :
<TnPr <TnPr <TnPr g31 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 29/36 (dimension 5) :
<TnPr <TnPr <TnPr g32 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 30/36 (dimension 5) :
<TnPr <TnPr <TnPr g33 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 31/36 (dimension 5) :
<TnPr <TnPr <TnPr g34 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 32/36 (dimension 5) :
<TnPr <TnPr <TnPr g41 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 33/36 (dimension 5) :
<TnPr <TnPr <TnPr g42 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 34s.
Computing the boundary of the generator 34/36 (dimension 5) :
<TnPr <TnPr <TnPr g43 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 35s.
Computing the boundary of the generator 35/36 (dimension 5) :
<TnPr <TnPr <TnPr g44 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 35s.
Computing the boundary of the generator 36/36 (dimension 5) :
<TnPr <TnPr <TnPr g45 0> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 69.


;; Clock -> 2011-07-14, 9h 57m 35s.
Computing the boundary of the generator 1/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 2/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 3/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 4/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 5/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 6/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 7/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 8/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 9/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 10/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 11/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 12/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 13/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 14/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 15/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 16/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 37s.
Computing the boundary of the generator 17/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 18/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 19/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[6 5]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 20/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 21/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 22/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[4 3][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 23/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 24/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 25/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 26/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 27/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 28/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 38s.
Computing the boundary of the generator 29/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 30/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 31/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 32/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 33/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 34/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 39s.
Computing the boundary of the generator 35/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 40s.
Computing the boundary of the generator 36/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 40s.
Computing the boundary of the generator 37/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 40s.
Computing the boundary of the generator 38/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 40s.
Computing the boundary of the generator 39/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 4> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 40s.
Computing the boundary of the generator 40/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 41s.
Computing the boundary of the generator 41/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 41s.
Computing the boundary of the generator 42/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 42s.
Computing the boundary of the generator 43/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 42s.
Computing the boundary of the generator 44/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 44s.
Computing the boundary of the generator 45/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 44s.
Computing the boundary of the generator 46/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 44s.
Computing the boundary of the generator 47/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 44s.
Computing the boundary of the generator 48/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 45s.
Computing the boundary of the generator 49/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 6> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 45s.
Computing the boundary of the generator 50/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 46s.
Computing the boundary of the generator 51/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 46s.
Computing the boundary of the generator 52/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 3> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.027114734 -- N = 10505
End of computing.


;; Clock -> 2011-07-14, 9h 57m 50s.
Computing the boundary of the generator 53/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 52s.
Computing the boundary of the generator 54/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 54s.
Computing the boundary of the generator 55/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 56s.
Computing the boundary of the generator 56/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 57m 57s.
Computing the boundary of the generator 57/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 0s.
Computing the boundary of the generator 58/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.027164893 -- N = 10514
End of computing.


;; Clock -> 2011-07-14, 9h 58m 6s.
Computing the boundary of the generator 59/69 (dimension 6) :
<TnPr <TnPr <TnPr g41 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 7s.
Computing the boundary of the generator 60/69 (dimension 6) :
<TnPr <TnPr <TnPr g42 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 9s.
Computing the boundary of the generator 61/69 (dimension 6) :
<TnPr <TnPr <TnPr g43 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 11s.
Computing the boundary of the generator 62/69 (dimension 6) :
<TnPr <TnPr <TnPr g44 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 15s.
Computing the boundary of the generator 63/69 (dimension 6) :
<TnPr <TnPr <TnPr g45 1> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.027187303 -- N = 10525
End of computing.


;; Clock -> 2011-07-14, 9h 58m 18s.
Computing the boundary of the generator 64/69 (dimension 6) :
<TnPr <TnPr <TnPr g51 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 18s.
Computing the boundary of the generator 65/69 (dimension 6) :
<TnPr <TnPr <TnPr g52 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 18s.
Computing the boundary of the generator 66/69 (dimension 6) :
<TnPr <TnPr <TnPr g53 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 20s.
Computing the boundary of the generator 67/69 (dimension 6) :
<TnPr <TnPr <TnPr g54 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 20s.
Computing the boundary of the generator 68/69 (dimension 6) :
<TnPr <TnPr <TnPr g55 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 21s.
Computing the boundary of the generator 69/69 (dimension 6) :
<TnPr <TnPr <TnPr g56 0> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 23s.




Homotopy in dimension 5 :


Component Z/2Z

Component Z/2Z

NIL
CL-USER(36): (homotopy s-k-s3-1 6)

Computing boundary-matrix in dimension 1.
Rank of the source-module : 0.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 48s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 48s.


Computing boundary-matrix in dimension 2.
Rank of the source-module : 2.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/2 (dimension 2) :
"g11" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/2 (dimension 2) :
"g12" 
End of computing.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 3.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/3 (dimension 3) :
"g21" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/3 (dimension 3) :
"g22" 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/3 (dimension 3) :
"g23" 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 48s.


Computing boundary-matrix in dimension 3.
Rank of the source-module : 6.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/6 (dimension 3) :
<TnPr S-BSGN 3> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/6 (dimension 3) :
<TnPr g11 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/6 (dimension 3) :
<TnPr g12 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 4/6 (dimension 3) :
<TnPr g21 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 5/6 (dimension 3) :
<TnPr g22 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 6/6 (dimension 3) :
<TnPr g23 0> 
End of computing.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 10.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/10 (dimension 4) :
<TnPr S-BSGN 4> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/10 (dimension 4) :
<TnPr g11 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/10 (dimension 4) :
<TnPr g12 2> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 4/10 (dimension 4) :
<TnPr g21 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 5/10 (dimension 4) :
<TnPr g22 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 6/10 (dimension 4) :
<TnPr g23 1> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 7/10 (dimension 4) :
<TnPr g31 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 8/10 (dimension 4) :
<TnPr g32 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 9/10 (dimension 4) :
<TnPr g33 0> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 10/10 (dimension 4) :
<TnPr g34 0> 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 48s.


Computing boundary-matrix in dimension 4.
Rank of the source-module : 16.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/16 (dimension 4) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/16 (dimension 4) :
<TnPr <TnPr S-BSGN 1> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 4/16 (dimension 4) :
<TnPr <TnPr S-BSGN 2> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 5/16 (dimension 4) :
<TnPr <TnPr g11 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 6/16 (dimension 4) :
<TnPr <TnPr g12 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 7/16 (dimension 4) :
<TnPr <TnPr S-BSGN 4> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 8/16 (dimension 4) :
<TnPr <TnPr g11 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 9/16 (dimension 4) :
<TnPr <TnPr g12 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 10/16 (dimension 4) :
<TnPr <TnPr g21 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 11/16 (dimension 4) :
<TnPr <TnPr g22 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 12/16 (dimension 4) :
<TnPr <TnPr g23 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 13/16 (dimension 4) :
<TnPr <TnPr g31 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 14/16 (dimension 4) :
<TnPr <TnPr g32 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 15/16 (dimension 4) :
<TnPr <TnPr g33 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 16/16 (dimension 4) :
<TnPr <TnPr g34 0> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 29.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 1/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 2/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 3/29 (dimension 5) :
<TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 4/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 5/29 (dimension 5) :
<TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 6/29 (dimension 5) :
<TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 7/29 (dimension 5) :
<TnPr <TnPr g11 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 8/29 (dimension 5) :
<TnPr <TnPr g12 0> <<Abar[3 2]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 9/29 (dimension 5) :
<TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 10/29 (dimension 5) :
<TnPr <TnPr g11 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 11/29 (dimension 5) :
<TnPr <TnPr g12 1> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 12/29 (dimension 5) :
<TnPr <TnPr g21 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 13/29 (dimension 5) :
<TnPr <TnPr g22 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 14/29 (dimension 5) :
<TnPr <TnPr g23 0> <<Abar[2 1]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 15/29 (dimension 5) :
<TnPr <TnPr S-BSGN 5> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 48s.
Computing the boundary of the generator 16/29 (dimension 5) :
<TnPr <TnPr g11 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 17/29 (dimension 5) :
<TnPr <TnPr g12 3> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 18/29 (dimension 5) :
<TnPr <TnPr g21 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 19/29 (dimension 5) :
<TnPr <TnPr g22 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 20/29 (dimension 5) :
<TnPr <TnPr g23 2> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 21/29 (dimension 5) :
<TnPr <TnPr g31 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 22/29 (dimension 5) :
<TnPr <TnPr g32 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 23/29 (dimension 5) :
<TnPr <TnPr g33 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 24/29 (dimension 5) :
<TnPr <TnPr g34 1> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 25/29 (dimension 5) :
<TnPr <TnPr g41 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 26/29 (dimension 5) :
<TnPr <TnPr g42 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 27/29 (dimension 5) :
<TnPr <TnPr g43 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 28/29 (dimension 5) :
<TnPr <TnPr g44 0> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 29/29 (dimension 5) :
<TnPr <TnPr g45 0> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 58m 49s.


Computing boundary-matrix in dimension 5.
Rank of the source-module : 36.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 1/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 2/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 3/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 4/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 5/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 6/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 7/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 8/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 9/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 10/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 11/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 12/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 13/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 14/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 15/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 16/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 17/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 49s.
Computing the boundary of the generator 18/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 19/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 20/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 21/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 22/36 (dimension 5) :
<TnPr <TnPr <TnPr S-BSGN 5> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 23/36 (dimension 5) :
<TnPr <TnPr <TnPr g11 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 24/36 (dimension 5) :
<TnPr <TnPr <TnPr g12 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 25/36 (dimension 5) :
<TnPr <TnPr <TnPr g21 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 26/36 (dimension 5) :
<TnPr <TnPr <TnPr g22 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 27/36 (dimension 5) :
<TnPr <TnPr <TnPr g23 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 50s.
Computing the boundary of the generator 28/36 (dimension 5) :
<TnPr <TnPr <TnPr g31 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 29/36 (dimension 5) :
<TnPr <TnPr <TnPr g32 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 30/36 (dimension 5) :
<TnPr <TnPr <TnPr g33 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 31/36 (dimension 5) :
<TnPr <TnPr <TnPr g34 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 32/36 (dimension 5) :
<TnPr <TnPr <TnPr g41 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 33/36 (dimension 5) :
<TnPr <TnPr <TnPr g42 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 34/36 (dimension 5) :
<TnPr <TnPr <TnPr g43 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 35/36 (dimension 5) :
<TnPr <TnPr <TnPr g44 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 36/36 (dimension 5) :
<TnPr <TnPr <TnPr g45 0> <<Abar>>> <<Abar>>> 
End of computing.


Computing boundary-matrix in dimension 6.
Rank of the source-module : 69.


;; Clock -> 2011-07-14, 9h 58m 51s.
Computing the boundary of the generator 1/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 2/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 3/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 4/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 5/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 6/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[2 1][2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 7/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 8/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 9/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 10/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar[4 <<Abar[3 2]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 11/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 12/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1]>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 13/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 14/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 15/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 52s.
Computing the boundary of the generator 16/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 17/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 18/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 19/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[6 5]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 20/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 21/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[3 2][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 22/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[4 3][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 23/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1][2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 24/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[5 4]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 25/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[2 1][3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 53s.
Computing the boundary of the generator 26/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 1> <<Abar[3 2][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 27/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 28/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 2> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 29/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 30/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 31/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[4 3]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 32/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 0> <<Abar[2 1][2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 33/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 3> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 54s.
Computing the boundary of the generator 34/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 35/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 1> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 36/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 37/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 38/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 0> <<Abar[3 2]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 39/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 4> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 55s.
Computing the boundary of the generator 40/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 56s.
Computing the boundary of the generator 41/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 2> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 57s.
Computing the boundary of the generator 42/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 57s.
Computing the boundary of the generator 43/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 1> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 58m 58s.
Computing the boundary of the generator 44/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 1> <<Abar[2 1]>>> <<Abar>>> 
*CR* -- Cut = 0.02723752 -- N = 10538
End of computing.


;; Clock -> 2011-07-14, 9h 58m 59s.
Computing the boundary of the generator 45/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 0s.
Computing the boundary of the generator 46/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 0s.
Computing the boundary of the generator 47/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 0s.
Computing the boundary of the generator 48/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 0> <<Abar[2 1]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 0s.
Computing the boundary of the generator 49/69 (dimension 6) :
<TnPr <TnPr <TnPr S-BSGN 6> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 1s.
Computing the boundary of the generator 50/69 (dimension 6) :
<TnPr <TnPr <TnPr g11 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 1s.
Computing the boundary of the generator 51/69 (dimension 6) :
<TnPr <TnPr <TnPr g12 4> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 2s.
Computing the boundary of the generator 52/69 (dimension 6) :
<TnPr <TnPr <TnPr g21 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 4s.
Computing the boundary of the generator 53/69 (dimension 6) :
<TnPr <TnPr <TnPr g22 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 6s.
Computing the boundary of the generator 54/69 (dimension 6) :
<TnPr <TnPr <TnPr g23 3> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 8s.
Computing the boundary of the generator 55/69 (dimension 6) :
<TnPr <TnPr <TnPr g31 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 10s.
Computing the boundary of the generator 56/69 (dimension 6) :
<TnPr <TnPr <TnPr g32 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 12s.
Computing the boundary of the generator 57/69 (dimension 6) :
<TnPr <TnPr <TnPr g33 2> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.02728105 -- N = 10552
End of computing.


;; Clock -> 2011-07-14, 9h 59m 15s.
Computing the boundary of the generator 58/69 (dimension 6) :
<TnPr <TnPr <TnPr g34 2> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 19s.
Computing the boundary of the generator 59/69 (dimension 6) :
<TnPr <TnPr <TnPr g41 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 20s.
Computing the boundary of the generator 60/69 (dimension 6) :
<TnPr <TnPr <TnPr g42 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 22s.
Computing the boundary of the generator 61/69 (dimension 6) :
<TnPr <TnPr <TnPr g43 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 24s.
Computing the boundary of the generator 62/69 (dimension 6) :
<TnPr <TnPr <TnPr g44 1> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 26s.
Computing the boundary of the generator 63/69 (dimension 6) :
<TnPr <TnPr <TnPr g45 1> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.027316269 -- N = 10565
End of computing.


;; Clock -> 2011-07-14, 9h 59m 30s.
Computing the boundary of the generator 64/69 (dimension 6) :
<TnPr <TnPr <TnPr g51 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 30s.
Computing the boundary of the generator 65/69 (dimension 6) :
<TnPr <TnPr <TnPr g52 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 31s.
Computing the boundary of the generator 66/69 (dimension 6) :
<TnPr <TnPr <TnPr g53 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 32s.
Computing the boundary of the generator 67/69 (dimension 6) :
<TnPr <TnPr <TnPr g54 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 32s.
Computing the boundary of the generator 68/69 (dimension 6) :
<TnPr <TnPr <TnPr g55 0> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 9h 59m 33s.
Computing the boundary of the generator 69/69 (dimension 6) :
<TnPr <TnPr <TnPr g56 0> <<Abar>>> <<Abar>>> 
End of computing.



;; Clock -> 2011-07-14, 9h 59m 33s.


*CR* -- Cut = 0.027431589 -- N = 10574
*CR* -- Cut = 0.027409986 -- N = 10581
*CR* -- Cut = 0.027447063 -- N = 10601
*CR* -- Cut = 0.027479827 -- N = 10657
*CR* -- Cut = 0.02755706 -- N = 10710
*CR* -- Cut = 0.027726682 -- N = 10786
*CR* -- Cut = 0.027987706 -- N = 10898
*CR* -- Cut = 0.028594453 -- N = 11093
*CR* -- Cut = 0.028853327 -- N = 11175
*CR* -- Cut = 0.029098665 -- N = 11284
*CR* -- Cut = 0.02917352 -- N = 11327
*CR* -- Cut = 0.02930427 -- N = 11396
*CR* -- Cut = 0.030319087 -- N = 11615
*CR* -- Cut = 0.030405309 -- N = 11708
*CR* -- Cut = 0.030384291 -- N = 11746
*CR* -- Cut = 0.030515656 -- N = 11774
*CR* -- Cut = 0.031394616 -- N = 8479
*CR* -- Cut = 0.028224792 -- N = 8561
*CR* -- Cut = 0.028261755 -- N = 8604
*CR* -- Cut = 0.028326884 -- N = 8629
*CR* -- Cut = 0.028889015 -- N = 8819
*CR* -- Cut = 0.028893422 -- N = 8847
*CR* -- Cut = 0.029014897 -- N = 8881
*CR* -- Cut = 0.029757868 -- N = 9103
*CR* -- Cut = 0.029971622 -- N = 9210
*CR* -- Cut = 0.029928245 -- N = 9238
*CR* -- Cut = 0.030070832 -- N = 9262
*CR* -- Cut = 0.03054888 -- N = 9378
*CR* -- Cut = 0.030755755 -- N = 9475
*CR* -- Cut = 0.03078257 -- N = 9500
*CR* -- Cut = 0.030935898 -- N = 9529
*CR* -- Cut = 0.031507302 -- N = 9483
*CR* -- Cut = 0.03135413 -- N = 9549
*CR* -- Cut = 0.031555425 -- N = 9583
*CR* -- Cut = 0.031551644 -- N = 9600
*CR* -- Cut = 0.031776916 -- N = 9652
*CR* -- Cut = 0.032632053 -- N = 8387
*CR* -- Cut = 0.03133738 -- N = 8454
*CR* -- Cut = 0.031436592 -- N = 8525
*CR* -- Cut = 0.03143434 -- N = 8539
*CR* -- Cut = 0.031562917 -- N = 8575
*CR* -- Cut = 0.031557184 -- N = 8594
*CR* -- Cut = 0.031637106 -- N = 8596
*CR* -- Cut = 0.03194841 -- N = 8740
*CR* -- Cut = 0.032019418 -- N = 8688
*CR* -- Cut = 0.032713216 -- N = 8806
*CR* -- Cut = 0.033036035 -- N = 8895
*CR* -- Cut = 0.032949958 -- N = 8901
*CR* -- Cut = 0.033048354 -- N = 8908
*CR* -- Cut = 0.03341168 -- N = 8989
*CR* -- Cut = 0.033435553 -- N = 9025
*CR* -- Cut = 0.033545233 -- N = 9039
*CR* -- Cut = 0.034353748 -- N = 9204
*CR* -- Cut = 0.034458824 -- N = 9268
*CR* -- Cut = 0.034690924 -- N = 9348
*CR* -- Cut = 0.034693472 -- N = 9355
*CR* -- Cut = 0.03482469 -- N = 9367
*CR* -- Cut = 0.03489123 -- N = 9387
*CR* -- Cut = 0.03490389 -- N = 9415
*CR* -- Cut = 0.03562493 -- N = 9585
*CR* -- Cut = 0.035654992 -- N = 9635
*CR* -- Cut = 0.035713308 -- N = 9652
*CR* -- Cut = 0.035746217 -- N = 9667
*CR* -- Cut = 0.035940286 -- N = 9706
*CR* -- Cut = 0.036304988 -- N = 9849
*CR* -- Cut = 0.036636807 -- N = 9935
*CR* -- Cut = 0.037287433 -- N = 10126
Computing boundary-matrix in dimension 6.
Rank of the source-module : 83.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 1/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 2/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 3/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 4/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 5/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 6/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 7/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 8/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[4 3]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 9/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar>>> <<Abar[6 <<Abar[5 <<Abar[2 1][2 1]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 15s.
Computing the boundary of the generator 10/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar>>> <<Abar[5 <<Abar[4 <<Abar[3 2]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 16s.
Computing the boundary of the generator 11/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar[2 1]>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 16s.
Computing the boundary of the generator 12/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 2> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 16s.
Computing the boundary of the generator 13/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr g11 0> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 16s.
Computing the boundary of the generator 14/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr g12 0> <<Abar>>> <<Abar>>> <<Abar[4 <<Abar[3 <<Abar[2 1]>>]>>]>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 18m 16s.
Computing the boundary of the generator 15/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[5 4]>>]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.037479904 -- N = 10189
*CR* -- Cut = 0.037520714 -- N = 10227
*CR* -- Cut = 0.037702538 -- N = 10266
*CR* -- Cut = 0.03763576 -- N = 10288
End of computing.


;; Clock -> 2011-07-14, 10h 18m 50s.
Computing the boundary of the generator 16/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[2 1][3 2]>>]>>> <<Abar>>> <<Abar>>> 
*CR* -- Cut = 0.03776791 -- N = 10319
End of computing.


;; Clock -> 2011-07-14, 10h 19m 0s.
Computing the boundary of the generator 17/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[6 <<Abar[3 2][2 1]>>]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 19m 4s.
Computing the boundary of the generator 18/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 0> <<Abar>>> <<Abar[3 <<Abar[2 1]>>][3 <<Abar[2 1]>>]>>> <<Abar>>> <<Abar>>> 
End of computing.


;; Clock -> 2011-07-14, 10h 19m 4s.
Computing the boundary of the generator 19/83 (dimension 6) :
<TnPr <TnPr <TnPr <TnPr <TnPr S-BSGN 1> <<Abar>>> <<Abar[5 <<Abar[4 3]>>]>>> <<Abar>>> <<Abar>>> 
Error: `#<Closure (:INTERNAL (FLET BAR-CMPR RSLT) 0) @ #x2d0c02c2>' is not of the expected type `NUMBER'
[condition type: TYPE-ERROR]
CL-USER(37): 