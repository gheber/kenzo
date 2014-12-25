;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD
;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD
;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD

(IN-PACKAGE #:cat)

(provide "whitehead")

;;;
;;; Z
;;;

(DEFUN Z-WWHITEHEAD-SINTR (smst n chml-clss
				&aux (face (face smst))
				     (k-z-n (k-z n))
				     (idnt (bspn k-z-n))
				     (efhm (efhm smst)))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss)
      (type face face)
      (type ab-simplicial-group k-z-n)
      (type gmsm idnt)
      (type homotopy-equivalence efhm))
   (setf chml-clss
	 (i-cmps chml-clss (rf efhm) (lg efhm)))
   (flet ((rslt (dmns gmsm)
	    (declare
	      (fixnum dmns)
	      (type gmsm gmsm))
	    (if (< dmns n)
	        (absm (mask dmns) idnt)
	      (z-cocycle-gbar n dmns
		(gmsm-cocycle face n dmns gmsm chml-clss)))))
     (the sintr #'rslt)))

#|
(cat-init)
(setf k (k-z 2))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 2
	:intr #'(lambda (degr gnrt)
		  (if (= degr 2)
		      (term-cmbn 0 1 :z-gnrt)
		    (zero-cmbn (- degr 2))))
	:strt :gnrt :orgn '(fundamental-class k-z-2)))
(setf tw (z-wwhitehead-sintr k 2 chml-clss))
(a-sintr3 tw 1 (absm 1 +null-gbar+))  ;;; cf old-macro cancelled
(a-sintr3 tw 2 (absm 3 +null-gbar+))  ;;; in macros.cl
(funcall tw 2 (gbar 2 0 '(55) 0 '()))
(funcall tw 3 (gbar 3 0 '(12 23)
                      0 '(34)
                      0 '()))
(funcall tw 4 (gbar 4 0 '(12 23 34)
                      2 '(45)
                      0 '(56)
                      0 '()))
(setf k (k-z 3))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 3
	:intr #'(lambda (degr gnrt)
		  (if (= degr 3)
		      (term-cmbn 0 -1 :z-gnrt)
		    (zero-cmbn (- degr 3))))
	:strt :gnrt :orgn '(fundamental-class k-z-3)))
(setf tw (z-wwhitehead-sintr k 3 chml-clss))
(a-sintr3 tw 1 (absm 1 +null-gbar+))
(a-sintr3 tw 2 (absm 3 +null-gbar+))
(a-sintr3 tw 3 (absm 7 +null-gbar+))
(funcall tw 3 (z-fundamental-gmsm 3 55))
(equal (funcall tw 3 (z-fundamental-gmsm 3 55))
       (absm 0 (z-fundamental-gmsm 3 55)))
(funcall tw 4 (gbar 4 0 (gbar 3 0 '(12 23) 0 '(34) 0 '())
                      0 (gbar 2 0 '(45) 0 '())
                      1 (gbar 0)
                      0 (gbar 0)))
|#

(DEFUN Z-WHITEHEAD-SINTR (smst n chml-clss
				&aux (face (face smst))
				     (k-z-n-1 (k-z (1- n)))
				     (idnt (bspn k-z-n-1))
				     (efhm (efhm smst)))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss)
      (type face face)
      (type ab-simplicial-group k-z-n-1)
      (type gmsm idnt)
      (type homotopy-equivalence efhm))
   (setf chml-clss
	 (i-cmps chml-clss (rf efhm) (lg efhm)))
   (flet ((rslt (dmns gmsm)
	    (declare
	      (fixnum dmns)
	      (type gmsm gmsm))
	    (if (< dmns n)
	        (absm (mask (1- dmns)) idnt)
	      (z-cocycle-gbar-head n dmns
		(gmsm-cocycle face n dmns gmsm chml-clss)))))
     (the sintr #'rslt)))

#|
(cat-init)
(setf k (k-z 2))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 2
	:intr #'(lambda (degr gnrt)
		  (if (= degr 2)
		      (term-cmbn 0 1 :z-gnrt)
		    (zero-cmbn (- degr 2))))
	:strt :gnrt :orgn '(fundamental-class k-z-2)))
(setf tw (z-whitehead-sintr k 2 chml-clss))
(tw-a-sintr3 tw 1 (absm 1 +null-gbar+) '())
(tw-a-sintr3 tw 2 (absm 3 +null-gbar+) '())
(funcall tw 2 (gbar 2 0 '(55) 0 '()))
(funcall tw 3 (gbar 3 0 '(12 23)
                      0 '(34)
                      0 '()))
(funcall tw 4 (gbar 4 0 '(12 23 34)
                      2 '(45)
                      0 '(56)
                      0 '()))
(setf k (k-z 3))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 3
	:intr #'(lambda (degr gnrt)
		  (if (= degr 3)
		      (term-cmbn 0 -1 :z-gnrt)
		    (zero-cmbn (- degr 3))))
	:strt :gnrt :orgn '(fundamental-class k-z-3)))
(setf tw (z-whitehead-sintr k 3 chml-clss))
(tw-a-sintr3 tw 1 (absm 1 +null-gbar+) +null-gbar+)
(tw-a-sintr3 tw 2 (absm 3 +null-gbar+) +null-gbar+)
(tw-a-sintr3 tw 3 (absm 7 +null-gbar+) +null-gbar+)
(funcall tw 3 (z-fundamental-gmsm 3 55))
(equal (funcall tw 3 (z-fundamental-gmsm 3 55))
       (absm 0 (z-fundamental-gmsm 2 55)))
(funcall tw 4 (gbar 4 0 (gbar 3 0 '(12 23) 0 '(34) 0 '())
                      0 (gbar 2 0 '(45) 0 '())
                      1 (gbar 0)
                      0 (gbar 0)))
|#

(DEFUN Z-WHITEHEAD (smst chml-clss &aux (n (- (degr chml-clss))))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss))
   (the fibration
      (build-smmr
         :sorc smst :trgt (k-z (1- n)) :degr -1
         :sintr (z-whitehead-sintr smst n chml-clss)
         :orgn `(z-whitehead ,smst))))

;;;
;;; Z/2Z
;;;

(DEFUN Z2-WWHITEHEAD-SINTR (smst n chml-clss
				&aux (face (face smst))
				     (k-z2-n (k-z2 n))
				     (idnt (bspn k-z2-n))
				     (efhm (efhm smst)))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss)
      (type face face)
      (type ab-simplicial-group k-z2-n)
      (type gmsm idnt)
      (type homotopy-equivalence efhm))
   (setf chml-clss
	 (i-cmps chml-clss (rf efhm) (lg efhm)))
   (flet ((rslt (dmns gmsm)
	    (declare
	      (fixnum dmns)
	      (type gmsm gmsm))
	    (if (< dmns n)
	        (absm (mask dmns) idnt)
	      (z2-cocycle-gbar n dmns
		(gmsm-cocycle face n dmns gmsm chml-clss)))))
     (the sintr #'rslt)))

#|
(cat-init)
(setf k (k-z2 2))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 2
	:intr #'(lambda (degr gnrt)
		  (if (= degr 2)
		      (term-cmbn 0 1 :z-gnrt)
		    (zero-cmbn (- degr 2))))
	:strt :gnrt :orgn '(fundamental-class k-z-2)))
(setf tw (Z2-wwhitehead-sintr k 2 chml-clss))
(a-sintr3 tw 1 (absm 1 +null-gbar+))
(a-sintr3 tw 2 (absm 3 +null-gbar+))
(funcall tw 2 (gbar 2 0 1 0 0))
(funcall tw 3 (gbar 3 0 2
                      0 1
                      0 0))
(funcall tw 4 (gbar 4 0 3
                      2 1
                      0 1
                      0 0))
(setf k (k-z2 3))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 3
	:intr #'(lambda (degr gnrt)
		  (if (= degr 3)
		      (term-cmbn 0 -1 :z-gnrt)
		    (zero-cmbn (- degr 3))))
	:strt :gnrt :orgn '(fundamental-class k-z-3)))
(setf tw (Z2-wwhitehead-sintr k 3 chml-clss))
(a-sintr3 tw 1 (absm 1 +null-gbar+))
(a-sintr3 tw 2 (absm 3 +null-gbar+))
(a-sintr3 tw 3 (absm 7 +null-gbar+))
(funcall tw 3 (Z2-fundamental-gmsm 3 1))
(equal (funcall tw 3 (Z2-fundamental-gmsm 3 1))
       (absm 0 (Z2-fundamental-gmsm 3 1)))
(funcall tw 4 (gbar 4 0 (gbar 3 0 2 0 1 0 0)
                      0 (gbar 2 0 1 0 0)
                      1 (gbar 0)
                      0 (gbar 0)))
|#

(DEFUN Z2-WHITEHEAD-SINTR (smst n chml-clss
				&aux (face (face smst))
				     (k-z2-n-1 (k-z2 (1- n)))
				     (idnt (bspn k-z2-n-1))
				     (efhm (efhm smst)))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss)
      (type face face)
      (type ab-simplicial-group k-z2-n-1)
      (type gmsm idnt)
      (type homotopy-equivalence efhm))
   (setf chml-clss
	 (i-cmps chml-clss (rf efhm) (lg efhm)))
   (flet ((rslt (dmns gmsm)
	    (declare
	      (fixnum dmns)
	      (type gmsm gmsm))
	    (if (< dmns n)
	        (absm (mask (1- dmns)) idnt)
	      (z2-cocycle-gbar-head n dmns
		(gmsm-cocycle face n dmns gmsm chml-clss)))))     
     (the sintr #'rslt)))

#|
(cat-init)
(setf k (k-z2 2))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 2
	:intr #'(lambda (degr gnrt)
		  (if (= degr 2)
		      (term-cmbn 0 1 :z-gnrt)
		    (zero-cmbn (- degr 2))))
	:strt :gnrt :orgn '(fundamental-class k-z-2)))
(setf tw (Z2-whitehead-sintr k 2 chml-clss))
(tw-a-sintr3 tw 1 (absm 1 +null-gbar+) 0)
(tw-a-sintr3 tw 2 (absm 3 +null-gbar+) 0)
(funcall tw 2 (gbar 2 0 1 0 '()))
(funcall tw 3 (gbar 3 0 2
                      0 1
                      0 0))
(funcall tw 4 (gbar 4 0 3
                      2 1
                      0 1
                      0 0))
(setf k (k-z2 3))
(setf ecc (echcm k))
(setf chml-clss
      (build-mrph
        :sorc ecc :trgt (z-chcm) :degr 3
	:intr #'(lambda (degr gnrt)
		  (if (= degr 3)
		      (term-cmbn 0 -1 :z-gnrt)
		    (zero-cmbn (- degr 3))))
	:strt :gnrt :orgn '(fundamental-class k-z-3)))
(setf tw (Z2-whitehead-sintr k 3 chml-clss))
(tw-a-sintr3 tw 1 (absm 1 +null-gbar+) 0)
(tw-a-sintr3 tw 2 (absm 3 +null-gbar+) 0)
(tw-a-sintr3 tw 3 (absm 7 +null-gbar+) 0)
(funcall tw 3 (Z2-fundamental-gmsm 3 1))
(equal (funcall tw 3 (Z2-fundamental-gmsm 3 1))
       (absm 0 (Z2-fundamental-gmsm 2 1)))
(funcall tw 4 (gbar 4 0 (gbar 3 0 2 0 1 0 0)
                      0 (gbar 2 0 1 0 0)
                      1 (gbar 0)
                      0 (gbar 0)))
|#

(DEFUN Z2-WHITEHEAD (smst chml-clss &aux (n (- (degr chml-clss))))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss))
   (the fibration
      (build-smmr
         :sorc smst :trgt (k-z2 (1- n)) :degr -1
         :sintr (Z2-whitehead-sintr smst n chml-clss)
         :orgn `(Z2-whitehead ,smst))))

#|
(setf m (moore 2 4))
(setf chml-clss
      (build-mrph
         :sorc m :trgt (z-chcm) :degr -4
         :intr #'(lambda (dmns gmsm)
                    (if (= 4 dmns)
                       (cmbn 0 1 :z-gnrt)
                       (zero-cmbn (- dmns 4))))
         :strt :gnrt
         :orgn '(chml-clss moore 2 4)))
(setf mf (z2-whitehead m 4 chml-clss))
(setf mt (fibration-total mf))
(homology mt 0 10)
|#
         
