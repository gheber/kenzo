;;;  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS
;;;  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS
;;;  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS  TENSOR-PRODUCTS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "tensor-products")

(DEFVAR *TNPR-WITH-DEGREES* nil)

(DEFMETHOD PRINT-OBJECT ((tnpr tnpr) stream)
  (declare (type stream stream))
  (the tnpr
    (progn
      (with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
        (format stream
            "<TnPr~:[~*~; ~D~] ~A~:[~*~; ~D~] ~A>"
          *tnpr-with-degrees* degr1 gnrt1
          *tnpr-with-degrees* degr2 gnrt2))
      tnpr)))



#|
  (tnpr 2 'a 3 'b)
  (setf *tnpr-with-degrees* t)    
  (tnpr 2 'a 3 'b)
  (setf *tnpr-with-degrees* nil)
|#

(DEFUN 2CMBN-TNPR (cmbn1 cmbn2)
   (declare (type cmbn cmbn1 cmbn2))
   (the cmbn
      (with-cmbn (degr1 list1) cmbn1
      (with-cmbn (degr2 list2) cmbn2
         (make-cmbn
            :degr (+ degr1 degr2)
            :list (mapcan
                     #'(lambda (term1)
                          (declare (type term term1))
                          (the list
                             (with-term (cffc1 gnrt1) term1
                                (mapcar
                                   #'(lambda (term2)
                                        (declare (type term term2))
                                        (the term
                                           (term
                                              (* cffc1 (cffc term2))
                                              (tnpr degr1 gnrt1 degr2 (gnrt term2)))))
                                   list2))))
                     list1))))))
#|
  (setf *tnpr-with-degrees* t)
  (2cmbn-tnpr (cmbn 2 3 'a 4 'b -5 'c) (cmbn 3 4 'x -3 'y 2 'z)))
|#


#|
(DEFUN TNSR-PRDC-CMPR (cmpr1 cmpr2)
   (declare (type cmprf cmpr1 cmpr2))
   (flet ((rslt (tnpr1 tnpr2)
             (declare (type tnpr tnpr1 tnpr2))
             (let ((left-cons-1 (cadr tnpr1))
                   (left-cons-2 (cadr tnpr2)))
                (declare (cons left-cons-1 left-cons-2))
                (lexico
                   (f-cmpr (car left-cons-1) (car left-cons-2))
                   (funcall cmpr1 (cdr left-cons-1) (cdr left-cons-2))
                   (funcall cmpr2 (gnrt2 tnpr1) (gnrt2 tnpr2))))))
      (the cmprf #'rslt)))
|#
(DEFUN TNSR-PRDC-CMPR (cmpr1 cmpr2)
  (declare (type cmprf cmpr1 cmpr2))
  (flet ((rslt (tnpr1 tnpr2)
           (declare (type tnpr tnpr1 tnpr2))
           (the cmpr
             (lexico
              (f-cmpr (degr1 tnpr1) (degr1 tnpr2))
              (funcall cmpr1 (gnrt1 tnpr1) (gnrt1 tnpr2))
              (funcall cmpr2 (gnrt2 tnpr1) (gnrt2 tnpr2))))))
    (the cmprf #'rslt)))


#|
  (setf cmpr (tnsr-prdc-cmpr #'s-cmpr #'s-cmpr))
  (funcall cmpr (tnpr 2 'a 3 'b) (tnpr 3 'a 2 'b))
  (funcall cmpr (tnpr 3 'a 2 'b) (tnpr 2 'a 3 'b))
  (funcall cmpr (tnpr 2 'a 3 'b) (tnpr 2 'b 3 'b))
  (funcall cmpr (tnpr 2 'b 3 'b) (tnpr 2 'a 3 'b))
  (funcall cmpr (tnpr 2 'a 3 'a) (tnpr 2 'a 3 'b))
  (funcall cmpr (tnpr 2 'a 3 'c) (tnpr 2 'a 3 'b))
  (funcall cmpr (tnpr 2 'a 3 'b) (tnpr 2 'a 3 'b)))
|#

(DEFUN TNSR-PRDC-BASIS (basis1 basis2)
   (declare (type basis basis1 basis2))
   (when (or (eq basis1 :locally-effective)
             (eq basis2 :locally-effective))
      (return-from tnsr-prdc-basis :locally-effective))
   (flet ((rslt (degr)
             (declare (fixnum degr))
             (the list
                (progn
                   (when (minusp degr)
                      (return-from rslt +empty-list+))
                   (mapcan
                      #'(lambda (degr1)
                           (declare (fixnum degr1))
                           (let* ((basis1 (funcall basis1 degr1))
                                  (degr2 (- degr degr1))
                                  (basis2 (funcall basis2 degr2)))
                              (declare
                                 (fixnum degr2)
                                 (list basis1 basis2))
                              (the list
                                 (mapcan
                                    #'(lambda (gnrt1)
                                         (declare (type gnrt gnrt1))
                                         (the list
                                            (mapcar
                                               #'(lambda (gnrt2)
                                                    (declare (type gnrt gnrt2))
                                                    (the tnpr
                                                       (tnpr degr1 gnrt1 degr2 gnrt2)))
                                               basis2)))
                                    basis1))))
                      (<a-b> 0 degr))))))
      (the basis #'rslt)))

#|
  (defun bas (degr)
     (case degr
        (0 '(a b c))
        (1 '(d))
        (2 nil)
        (3 '(x y))))
  (setf bas (tnsr-prdc-basis #'bas #'bas))
  (dotimes (i 8)
     (print (funcall bas i))))
|#

(DEFUN TNSR-PRDC-INTR-DFFR (dffr1 dffr2)
  (declare (type morphism dffr1 dffr2))
  (flet ((rslt (degr tnpr)
               (declare
                (fixnum degr)
                (type tnpr tnpr))
               (the cmbn
                 (with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                   (let ((degr1-1 (1- degr1))
                         (degr2-1 (1- degr2))
                         (sign (oddp degr1))
                         (list1 (cmbn-list (gnrt-? dffr1 degr1 gnrt1)))
                         (list2 (cmbn-list (gnrt-? dffr2 degr2 gnrt2))))
                     (declare
                      (type fixnum degr1-1 degr2-1)
                      (type boolean sign)
                      (type list list1 list2))
                     (make-cmbn
                      :degr (1- degr)
                      :list (nconc
                             (mapcar
                                 #'(lambda (term1)
                                     (declare (type term term1))
                                     (with-term (cffc1 gnrt1) term1
                                       (term cffc1
                                             (tnpr degr1-1 gnrt1 degr2 gnrt2))))
                               list1)
                             (mapcar
                                 #'(lambda (term2)
                                     (declare (type term term2))
                                     (with-term (cffc2 gnrt2) term2
                                       (term (if sign (- cffc2) cffc2)
                                             (tnpr degr1 gnrt1 degr2-1 gnrt2))))
                               list2))))))))
    (the intr-mrph #'rslt)))

#|
  (cat-init)
  (setf chcm (build-chcm :cmpr #'s-cmpr :strt :gnrt :orgn '(test-1)))
  (setf dffr (build-mrph :sorc chcm :trgt chcm :degr -1
                         :intr #'(lambda (degr gnrt)
                                    (ecase gnrt
                                       (a (cmbn (1- degr) 2 'd1a -3 'd2a))
                                       (b (cmbn (1- degr) 3 'd1b -4 'd2b))))
                         :strt :gnrt :orgn '(test-2)))
  (setf rslt (tnsr-prdc-intr-dffr dffr dffr))
  (funcall rslt 4 (tnpr 2 'a 2 'b))
  (funcall rslt 5 (tnpr 3 'a 2 'b))
  (funcall rslt 5 (tnpr 2 'a 3 'b))
  (funcall rslt 6 (tnpr 3 'a 3 'b)))
|#

(DEFMETHOD TNSR-PRDC ((chcm1 chain-complex) (chcm2 chain-complex))
   (the chain-complex
      (with-slots ((cmpr1 cmpr) (basis1 basis) (dffr1 dffr)) chcm1
         (declare
            (type cmprf cmpr1)
            (type basis basis1)
	    (type morphism dffr1))
      (with-slots ((cmpr2 cmpr) (basis2 basis) (dffr2 dffr)) chcm2
         (declare
            (type cmprf cmpr2)
            (type basis basis2)
	    (type morphism dffr2))
	 (let ((rslt
		(build-chcm
		 :cmpr (tnsr-prdc-cmpr cmpr1 cmpr2)
		 :basis (tnsr-prdc-basis basis1 basis2)
		 :intr-dffr (tnsr-prdc-intr-dffr dffr1 dffr2)
		 :strt :gnrt
		 :orgn `(tnsr-prdc ,chcm1 ,chcm2))))
	   (declare (type chain-complex rslt))
	   (if (and (slot-boundp chcm1 'bsgn)
		    (slot-boundp chcm2 'bsgn))
	      (setf (slot-value rslt 'bsgn)
		    (tnpr 0 (bsgn chcm1) 0 (bsgn chcm2)))
	      (slot-makunbound rslt 'bsgn))
	   rslt)))))

#|
  (setf dd (tnsr-prdc (delta 2) (delta 3)))
  (cmpr dd (tnpr 2 7 2 11) (tnpr 2 7 2 14))
  (basis dd 3)
  (? dd 4 (tnpr 2 7 2 14))
  (? dd 3 (tnpr 1 6 2 14))
  (? dd (? dd 4 (tnpr 2 7 2 14)))
  (? dd (? dd 3 (tnpr 1 6 2 14))))
|#

(DEFUN TNSR-PRDC-INTR (mrph1 mrph2)
   (declare
      (type morphism mrph1 mrph2))
   (let ((mrph2-sign (oddp (degr mrph2))))
      (declare (type boolean mrph2-sign))         
      (flet ((rslt (degr tnpr)
              (declare
                 (ignore degr)
                 (type tnpr tnpr))
              (with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                 (let ((rslt (2cmbn-tnpr
                                (gnrt-? mrph1 degr1 gnrt1)
                                (gnrt-? mrph2 degr2 gnrt2))))
                    (declare (type cmbn rslt))
                    (when (and mrph2-sign
                               (oddp degr1))
                       (mapc #'(lambda (term)
                                  (declare (type term term))
                                  (setf (the fixnum (cffc term))
                                        (the fixnum (- (cffc term)))))
                          (cmbn-list rslt)))
                    rslt))))
         (the intr-mrph #'rslt))))

#|
  (cat-init)
  (setf chcm (build-chcm :cmpr #'s-cmpr :strt :gnrt :orgn '(test-1)))
  (setf mrph (build-mrph :sorc chcm :trgt chcm :degr 0
                         :intr #'(lambda (degr gnrt)
                                    (ecase gnrt
                                       (a (cmbn degr 2 'd1a -3 'd2a))
                                       (b (cmbn degr 3 'd1b -4 'd2b))))
                         :strt :gnrt :orgn '(test-2)))
  (setf rslt (tnsr-prdc-intr mrph mrph))
  (funcall rslt 6 (tnpr 2 'a 4 'b))
  (funcall rslt 5 (tnpr 3 'a 2 'b))
  (setf mrph (build-mrph :sorc chcm :trgt chcm :degr 1
                         :intr #'(lambda (degr gnrt)
                                    (ecase gnrt
                                       (a (cmbn (1+ degr) 2 'd1a -3 'd2a))
                                       (b (cmbn (1+ degr) 3 'd1b -4 'd2b))))
                         :strt :gnrt :orgn '(test-3)))
  (setf rslt (tnsr-prdc-intr mrph mrph))
  (funcall rslt 6 (tnpr 2 'a 4 'b))
  (funcall rslt 5 (tnpr 3 'a 2 'b)))
|#

(DEFMETHOD TNSR-PRDC ((mrph1 morphism) (mrph2 morphism))
  (the morphism
   (with-slots ((sorc1 sorc) (trgt1 trgt) (degr1 degr)) mrph1
   (with-slots ((sorc2 sorc) (trgt2 trgt) (degr2 degr)) mrph2
      (the morphism
	 (progn
	   (when (and (eq 'idnt-mrph (first (orgn mrph1)))
		      (eq 'idnt-mrph (first (orgn mrph2))))
	      (return-from tnsr-prdc
		 (idnt-mrph (tnsr-prdc sorc1 sorc2))))
	   (when (or (eq 'zero-mrph (first (orgn mrph1)))
		     (eq 'zero-mrph (first (orgn mrph2))))
	      (return-from tnsr-prdc
		 (zero-mrph (tnsr-prdc sorc1 sorc2)
			    (tnsr-prdc trgt1 trgt2)
			    (+ degr1 degr2))))
	   (build-mrph
              :sorc (tnsr-prdc sorc1 sorc2)
	      :trgt (tnsr-prdc trgt1 trgt2)
	      :degr (+ degr1 degr2)
	      :intr (tnsr-prdc-intr mrph1 mrph2)
	      :strt :gnrt
	      :orgn `(tnsr-prdc ,mrph1 ,mrph2))))))))

#|
  (cat-init)
  (setf d (dffr (delta-infinity)))
  (setf dd (tnsr-prdc d d))
  (eq (sorc dd) (tnsr-prdc (delta-infinity) (delta-infinity)))
  (? dd 5 (tnpr 2 7 3 15))
  (? dd 5 (tnpr 3 15 2 7))
  (? dd (? dd 5 (tnpr 2 7 3 15)))
  (? dd (? dd 5 (tnpr 3 15 2 7)))
  (setf ddd (cmps dd dd))
  (? ddd 5 (tnpr 2 7 3 15))
  (? ddd 5 (tnpr 3 15 2 7))
|#

(DEFMETHOD TNSR-PRDC ((rdct1 reduction) (rdct2 reduction))
  (the reduction
     (progn
       (when (and (eq 'trivial-rdct (first (orgn rdct1)))
		  (eq 'trivial-rdct (first (orgn rdct2))))
          (return-from tnsr-prdc
	     (trivial-rdct (tnsr-prdc (bcc rdct1) (bcc rdct2)))))
       (with-slots ((tcc1 tcc) (bcc1 bcc) (f1 f) (g1 g) (h1 h)) rdct1
          (declare
             (type chain-complex tcc1 bcc1)
	     (type morphism f1 g1 h1))
       (with-slots ((tcc2 tcc) (bcc2 bcc) (f2 f) (g2 g) (h2 h)) rdct2
          (declare
             (type chain-complex tcc2 bcc2)
	     (type morphism f2 g2 h2))
	  (build-rdct
	     :f (tnsr-prdc f1 f2)
	     :g (tnsr-prdc g1 g2)
	     :h (add
	         (tnsr-prdc h1 (cmps g2 f2))
		 (tnsr-prdc (idnt-mrph tcc1) h2))
	     :orgn `(tnsr-prdc ,rdct1 ,rdct2)))))))

#|
  (cat-init)
  (setf r (ez (delta-infinity) (delta-infinity)))
  (setf r2 (tnsr-prdc r r))
  (setf *bc* (cmbn 4 1 (tnpr 2 (tnpr 1 3 1 3) 2 (tnpr 1 3 1 3)))
        *tc* (cmbn 2 1 (tnpr 1 (crpr 0 3 0 3) 1 (crpr 0 3 0 3))))
  (pre-check-rdct r2)
  (check-rdct)
|#

(DEFMETHOD TNSR-PRDC ((hmeq1 homotopy-equivalence)
		      (hmeq2 homotopy-equivalence))
  (the homotopy-equivalence
     (build-hmeq
        :lrdct (tnsr-prdc (lrdct hmeq1) (lrdct hmeq2))
	:rrdct (tnsr-prdc (rrdct hmeq1) (rrdct hmeq2))
	:orgn `(tnsr-prdc ,hmeq1 ,hmeq2))))

(DEFMETHOD SEARCH-EFHM (chcm (orgn (eql 'tnsr-prdc)))
  (declare (type chain-complex chcm))
  (the homotopy-equivalence
       (tnsr-prdc (efhm (second (orgn chcm)))
		  (efhm (third (orgn chcm))))))

#|
(cat-init)
(setf k (k-z 2))
(setf k2 (tnsr-prdc k k))
(homology k2 0 10)
|#