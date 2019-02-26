;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS
;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS
;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS

(IN-PACKAGE #:cat-8)

(PROVIDE "suspensions")

(DEFUN SUSPENSION-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (gnrt1 gnrt2)
	   (declare (type gnrt gnrt1 gnrt2))
	   (if (eq gnrt1 :s-bsgn)
	       :equal
	     (funcall cmpr gnrt1 gnrt2))))
    (the cmprf #'rslt)))

#|
  (setf cc (deltab))
  (setf cmpr (suspension-cmpr (cmpr cc)))
  (funcall cmpr :s-bsgn :s-bsgn)
  (funcall cmpr 6 5)
|#

(DEFUN SUSPENSION-BASIS (basis)
  (declare (type basis basis))
  (when (eq basis :locally-effective)
    (return-from suspension-basis :locally-effective))
  (flet ((rslt (degr)
	   (declare (fixnum degr))
	   (case degr
	     (0 (list :s-bsgn))
	     (1 +empty-list+)
	     (otherwise (funcall basis (1- degr))))))
    (the basis #'rslt)))

#|
  (setf cc (sphere 2))
  (setf basis (suspension-basis (basis cc)))
  (dotimes (i 6)
    (print (funcall basis i)))
  (suspension-basis :locally-effective)
|#

(DEFUN SUSPENSION-INTR-DFFR (dffr)
  (declare (type morphism dffr))
  (flet ((rslt (cmbn)
	   (declare (type cmbn cmbn))
	   (with-cmbn (degr list) cmbn
	     (decf degr)
	     (if (<= degr 0)
		 (zero-cmbn degr)
	       (make-cmbn :degr degr
			  :list (cmbn-list
				 (cmbn-? dffr
				   (make-cmbn :degr degr
					      :list list))))))))
    (the intr-mrph #'rslt)))

#|
  (setf cc (deltab))
  (setf intr (suspension-intr-dffr (dffr cc)))
  (funcall intr (cmbn 0 3 :s-bsgn))
  (funcall intr (cmbn 3 66 7))
|#

(DECLAIM (FTYPE function suspension-1))

(DEFUN SUSPENSION (obj &optional (n 1))
  (declare (type (or chain-complex morphism reduction
		     homotopy-equivalence) obj)
	   (fixnum n))
  (if (= n 0)
      obj
    (suspension-1 (suspension obj (1- n)))))    

(DEFMETHOD SUSPENSION-1 ((chcm chain-complex))
  (the chain-complex
    (with-slots (cmpr basis dffr) chcm
      (build-chcm
        :cmpr (suspension-cmpr cmpr)
	:basis (suspension-basis basis)
	:bsgn :s-bsgn
	:intr-dffr (suspension-intr-dffr dffr)
	:strt :cmbn
	:orgn `(suspension ,chcm)))))

#|
  (cat-init)
  (setf cc (deltab))
  (setf scc (suspension cc))
  (cmpr scc 7 11)
  (basis scc 3)  ;; error
  (bsgn scc)
  (? scc 3 11)
  (setf scc (suspension cc 2))
  (? scc 4 11)
|#

(DEFUN SUSPENSION-INTR-CPRD (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
    (with-cmbn (degr list) cmbn
      (if (zerop degr)
	  (make-cmbn :degr 0
		     :list (if list
			       (list (term (-cffc list)
					   (tnpr 0 :s-bsgn
						 0 :s-bsgn)))
			     +empty-list+))
	(make-cmbn :degr degr
		   :list (let ((list1 +empty-list+)
			       (list2 +empty-list+))
			   (declare (list list1 list2))
			   (mapc
			    #'(lambda (term)
				(declare (type term term))
				(with-term (cffc gnrt) term
				  (push (term cffc
					      (tnpr 0 :s-bsgn
						    degr gnrt))
					list1)
				  (push (term cffc
					      (tnpr degr gnrt
						    0 :s-bsgn))
					list2)))
			    list)
			   (nreconc list1 (nreverse list2))))))))

#|
 (suspension-intr-cprd (cmbn 0))
 (suspension-intr-cprd (cmbn 0 5 :s-bsgn))
 (suspension-intr-cprd (cmbn 3 4 7 5 11))
|#

(DEFMETHOD SUSPENSION-1 ((clgb coalgebra))
  (the coalgebra
    (let ((irslt (call-next-method)))
      (declare (type (or chain-complex coalgebra) irslt))
      (when (typep irslt 'coalgebra)
	(return-from suspension-1 irslt))
      (change-chcm-to-clgb irslt :intr-cprd #'suspension-intr-cprd
			         :cprd-strt :cmbn)
      (setf (slot-value irslt 'orgn) `(suspension ,clgb))
      (setf (slot-value (slot-value irslt 'cprd) 'orgn)
	    `(coproduct ,irslt))
      irslt)))

#|
  (cat-init)
  (setf coal (deltab))
  (setf scoal (suspension coal))
  (setf scoal (suspension coal))
  (? scoal 3 7)
  (cprd scoal 3 7)
|#

(DEFUN SUSPENSION-FACE (face)
  (declare (type face face))
  (flet ((rslt (indx dmns gmsm)
	   (declare
	     (fixnum indx dmns)
	     (type gmsm gmsm))
	   (assert (> dmns 1))
	   (if (= indx dmns)
	       (absm (mask (1- dmns)) :s-bsgn)
	     (let ((irslt (funcall face indx (1- dmns) gmsm)))
	       (declare (type absm irslt))
	       (if (= (dgop irslt) (mask (- dmns 2)))
		   (absm (mask (1- dmns)) :s-bsgn)
		 irslt)))))
     (the face #'rslt)))

#|
  (setf ss (deltab))
  (setf face (suspension-face (face ss)))
  (funcall face 4 4 15)
  (funcall face 0 4 15)
  (check-faces #'f-cmpr face 4 15)
  (check-faces #'f-cmpr face 3 7)
  (check-faces #'f-cmpr face 2 3)
  (setf m (moore 2 3))
  (setf face (suspension-face (face m)))
  (dotimes (i 5) (print (funcall face i 4 'm3)))
  (dotimes (i 6) (print (funcall face i 5 'mm4)))
  (check-faces #'s-cmpr face 4 'm3)
  (check-faces #'s-cmpr face 5 'mm4)
|#

(DEFMETHOD SUSPENSION-1 ((smst simplicial-set))
  (the simplicial-set
    (let ((irslt (call-next-method)))
      (declare (type coalgebra irslt))
      (when (typep irslt 'simplicial-set)
	(return-from suspension-1 irslt))
      (change-class irslt 'simplicial-set)
      (setf (slot-value irslt 'face)
	    (suspension-face (face smst)))
      (setf (slot-value (slot-value irslt 'cprd) 'orgn)
	    `(diagonal ,irslt))
      irslt)))

#|
  (cat-init)
  (setf d (deltab))
  (setf sd (suspension d))
  (setf sd (suspension d))
  (inspect sd)
  (? sd 3 7)
  (cprd sd 3 7)
  (face sd 3 3 7)
  (face sd 2 3 7)
  (setf m (moore 2 1))
  (setf sm (suspension m))
  (homology sm 0 5)
  (setf ssm (suspension sm))
  (homology ssm 0 6)
|#

(DEFUN SUSPENSION-INTR (mrph)
  (declare (type morphism mrph))
  (let ((mdegr (degr mrph)))
    (declare (fixnum mdegr))
    (flet ((rslt (cmbn)
	     (declare (type cmbn cmbn))
	     (with-cmbn (cdegr list) cmbn
	       (when (zerop cdegr)
		 (return-from rslt
		   (if (zerop mdegr)
		       cmbn
		     (zero-cmbn (+ mdegr cdegr)))))
	       (make-cmbn :degr (+ mdegr cdegr)
			  :list (cmbn-list
				 (cmbn-? mrph (make-cmbn :degr (1- cdegr)
							 :list list)))))))
    (the intr-mrph #'rslt))))

#|
  (setf f (idnt-mrph (deltab)))
  (setf sf (suspension-intr f))
  (funcall sf (cmbn 0 3 :s-bsgn))
  (funcall sf (cmbn 2 4 3))
  (setf d (dffr (deltab)))
  (setf sd (suspension-intr d))
  (funcall sd (cmbn 0 3 :s-bsgn))
  (funcall sd (cmbn 2 4 3))
  (funcall sd (cmbn 3 4 7))
|#
	 
(DEFMETHOD SUSPENSION-1 ((mrph morphism))
  (the morphism
    (let ((orgn (orgn mrph)))
      (declare (list orgn))
      (when (eq (first orgn) 'zero-mrph)
	(return-from suspension-1
	  (zero-mrph (suspension (sorc mrph))
		     (suspension (trgt mrph))
		     (degr mrph))))
      (when (eq (first orgn) 'idnt-mrph)
	(return-from suspension-1
	  (idnt-mrph (suspension (sorc mrph)))))
      (build-mrph
       :sorc (suspension (sorc mrph))
       :trgt (suspension (trgt mrph))
       :degr (degr mrph)
       :intr (suspension-intr mrph)
       :strt :cmbn
       :orgn `(suspension ,mrph)))))

#|
  (setf f (idnt-mrph (deltab)))
  (setf sf (suspension f))
  (? sf (cmbn 0 3 :s-bsgn))
  (? sf (cmbn 2 4 3))
  (setf d (dffr (deltab)))
  (setf sd (suspension d))
  (? sd (cmbn 0 3 :s-bsgn))
  (? sd (cmbn 2 4 3))
  (? sd (cmbn 3 4 7))
|#

(DEFMETHOD SUSPENSION-1 ((rdct reduction))
  (the reduction (progn
    (when (eq (first (orgn rdct)) 'trivial-rdct)
      (return-from suspension-1
	(trivial-rdct (suspension (second (orgn rdct))))))
    (build-rdct
      :f (suspension (f rdct))
      :g (suspension (g rdct))
      :h (suspension (h rdct))
      :orgn `(suspension ,rdct)))))

#|
  (setf rdct (ez (deltab) (deltab)))
  (setf srdct (suspension rdct))
  (pre-check-rdct srdct)
  (setf *tc* (cmbn 4 1 (crpr 0 15 0 15)))
  (setf *bc* (cmbn 4 1 (tnpr 0 1 3 15)
                     5 (tnpr 1 3 2 7)
                     10 (tnpr 2 7 1 3)
                     100 (tnpr 3 15 0 1)))
  (check-rdct)
|#

(DEFMETHOD SUSPENSION-1 ((hmeq homotopy-equivalence))
  (the homotopy-equivalence (progn
    (when (eq (first (orgn hmeq)) 'trivial-hmeq)
      (return-from suspension-1
	(trivial-hmeq (suspension (second (orgn hmeq))))))
    (build-hmeq
      :lrdct (suspension (lrdct hmeq))
      :rrdct (suspension (rrdct hmeq))
      :orgn `(suspension ,hmeq)))))

(DEFMETHOD SEARCH-EFHM (suspension (orgn (eql 'suspension)))
  (declare (type chain-complex suspension))
  (suspension (efhm (second (orgn suspension)))))

#|
  (setf sk (suspension (k-z 2)))
  (homology sk 0 10)
|#
