;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS
;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS
;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS

(IN-PACKAGE #:cat-8)

(PROVIDE "cs-twisted-products")

(DEFUN SMGR-FIBRATION-SINTR (dmns gbar)
  (declare
    (ignore dmns)
    (type gbar gbar))
  (the absm
    (first (gbar-list gbar))))

(DEFUN SMGR-FIBRATION (smgr)
  (declare (type simplicial-group smgr))
  (the fibration
    (build-smmr
      :sorc (classifying-space smgr)
      :trgt smgr
      :degr -1
      :sintr #'smgr-fibration-sintr
      :orgn `(simplicial-group-fibration ,smgr))))

#|
(setf k1 (k-z-1))
(setf tw (smgr-fibration k1))
(setf k2 (sorc tw))
(setf tt (fibration-total tw))
(setf gmsm (crpr 0 (gbar 4 0 '(10 11 12) 0 '(20 21) 0 '(30) 0 '())
                 0 '(2 4 6 8)))
(dotimes (i 5)
  (print (face tt i 4 gmsm)))
(setf br (brown-reduction tw))
(setf tw-pr (bcc br))
(? tw-pr 4 (tnpr 4 (gbar 4 0 '(1 10 100) 0 '(1000 10000)
                         0 '(100000) 0 '())
                 0 '()))
|#

(DEFUN SMGR-CRTS-CONTRACTION-INTR (idnt)
  (declare
    (type gmsm idnt))
  (flet ((rslt (degr crpr
		     &aux (degr+1 (1+ degr)))
	   (declare
	    (fixnum degr degr+1)
	    (type crpr crpr))
	   (the cmbn
	     (with-crpr (absm1 absm2) crpr
	       (if (= (dgop absm2) (mask degr))
		 (zero-cmbn degr+1)
		 (term-cmbn degr+1
		   (-1-expt-n+1 degr)
		   (crpr 0 (make-gbar
			    :dmns degr+1
			    :list (cons absm2
					(rest (unnormalize-gbar absm1 idnt))))
			 (mask degr+1) idnt)))))))
     (the intr-mrph #'rslt)))

(DEFUN SMGR-CRTS-CONTRACTION (smgr)
  (declare (type simplicial-group smgr))
  (the morphism
    (build-mrph
      :sorc (fibration-total (smgr-fibration smgr))
      :trgt (fibration-total (smgr-fibration smgr))
      :degr +1
      :intr (smgr-crts-contraction-intr (bspn smgr))
      :strt :gnrt
      :orgn `(smgr-crts-contraction ,smgr))))

#|
(cat-init)
(setf chi (smgr-crts-contraction (k-z-1)))
(setf rdct (build-rdct :f (zero-mrph (sorc chi) (z-chcm))
                       :g (zero-mrph (z-chcm) (sorc chi))
                       :h chi))
(pre-check-rdct rdct)
(setf *tc* (cmbn 0 1 (crpr 0 +null-gbar+ 0 '())))
(setf *bc* (cmbn 0 1 :z-gnrt))
(check-rdct)
(setf *tc* (cmbn 1 1 (crpr 1 +null-gbar+ 0 '(1))))
(check-rdct)
(setf *tc* (cmbn 4 1 (crpr 0 (gbar 4 0 '(10 11 12) 0 '(20 21) 0 '(30) 0 '())
                           0 '(2 4 6 8))))
(check-rdct)
(setf *tc* (cmbn 3 1 (crpr 0 (gbar 3 0 '(10 11) 0 '(20) 0 '())
                           0 '(2 4 6))))
(check-rdct)
|#

(DEFUN SMGR-TNPR-CONTRACTION (smgr
			        &aux (fibration (smgr-fibration smgr))
				     (brown (brown-reduction fibration))
				     (f (f brown))
				     (g (g brown))
				     (chi (smgr-crts-contraction smgr)))
  (declare
    (type simplicial-group smgr)
    (type fibration fibration)
    (type reduction brown)
    (type morphism f g chi))
  (the morphism
    (i-cmps f chi g)))

#|
  (cat-init)
  (setf chi (smgr-tnpr-contraction (k-z-1)))
  (setf rdct (build-rdct :f (zero-mrph (sorc chi) (z-chcm))
                         :g (zero-mrph (z-chcm) (sorc chi))
                         :h chi))
  (pre-check-rdct rdct)
  (setf *tc* (cmbn 0 1 (tnpr 0 +null-gbar+ 0 '())))
  (setf *bc* (cmbn 0 1 :z-gnrt))
  (check-rdct)
  (setf *tc* (cmbn 1 1 (tnpr 0 +null-gbar+ 1 '(1))))
  (check-rdct)
  (setf *tc* (cmbn 3 1 (tnpr 0 +null-gbar+ 3 '(1 10 100))))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 3 1 (tnpr 2 (gbar 2 0 '(1) 0 '()) 1 '(10))))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 3 1 (tnpr 3 (gbar 3 0 '(1 10) 0 '(100) 0 '()) 0 '())))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 4 1 (tnpr 0 +null-gbar+ 4 '(1 10 100 1000))))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 4 1 (tnpr 2 (gbar 2 0 '(1) 0 '()) 2 '(10 100))))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 4 1 (tnpr 3 (gbar 3 0 '(1 10) 0 '(100) 0 '())
                             1 '(1000))))
  (? chi *tc*)
  (check-rdct)
  (setf *tc* (cmbn 4 1 (tnpr 4 (gbar 4 0 '(1 10 100)
                                       0 '(1000 10000)
                                       0 '(100000)
                                       0 '())
                              0 '())))
  (? chi *tc*)
  (check-rdct)
|#
