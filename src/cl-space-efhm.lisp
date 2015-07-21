;;;  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM
;;;  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM
;;;  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM  CL-SPACE-EFHM

(IN-PACKAGE #:cat)

(provide "cl-space-efhm")

(DEFUN CS-HAT-U-U (smgr
		   &aux (bar (bar smgr))
		     (cl-space (classifying-space smgr))
		     (clsp-tnsr-smgr (tnsr-prdc cl-space smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex bar clsp-tnsr-smgr)
   (type simplicial-set cl-space))
  (the chain-complex
       (tnsr-prdc clsp-tnsr-smgr bar)))


(DEFUN CS-HAT-RIGHT-PERTURBATION-INTR (smgr)
  (declare (type simplicial-group smgr))
  (let ((aprd (aprd smgr)))
    (flet ((rslt (degr tnpr)
	     (declare
	      (fixnum degr)
	      (type tnpr tnpr))
	     (with-tnpr
		 (degr1 tnpr1 degr2 abar2) tnpr
		 (when (zerop degr2)
		   (return-from rslt (zero-cmbn (1- degr))))
		 (with-tnpr
		     (degr11 gbar11 degr12 gmsm12) tnpr1
		     (let ((sign (-1-expt-n degr1))
			   (brgn21 (first (abar-list abar2)))
			   (bar22 (make-abar :list (rest (abar-list abar2)))))
		       (declare
			(fixnum sign)
			(type brgn brgn21)
			(type abar bar22))
		       (with-brgn
			   (degr21 gmsm21) brgn21
			   (let ((degr22 (- degr2 degr21)))
			     (declare (fixnum degr22))
			     (decf degr21) ;; = dmns(gmsm21)
			     (let ((aprd (gnrt-? aprd (+ degr12 degr21)
						 (tnpr degr12 gmsm12
						       degr21 gmsm21))))
			       (declare (type cmbn aprd))
			       (with-cmbn
				   (degr12+21 list) aprd
				   (make-cmbn
				    :degr (1- degr)
				    :list
				    (mapcar
				     #'(lambda (term)
					 (declare (type term term))
					 (with-term
					     (cffc gmsm) term
					     (term (* sign cffc)
						   (tnpr (+ degr1 degr21)
							 (tnpr degr11 gbar11
							       degr12+21 gmsm)
							 degr22 bar22))))
				     list)))))))))))
      (the intr-mrph #'rslt))))


(DEFUN CS-HAT-RIGHT-PERTURBATION (smgr
				  &aux (hat-u-u (cs-hat-u-u smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-u-u))
  (the morphism
       (build-mrph
        :sorc hat-u-u
	:trgt hat-u-u
	:degr -1
	:intr (cs-hat-right-perturbation-intr smgr)
	:strt :gnrt
	:orgn `(cs-hat-right-perturbation ,smgr))))


(DEFUN CS-HAT-U-T (smgr
		   &aux (hat-u-u (cs-hat-u-u smgr))
		     (hat-right-perturbation
		      (cs-hat-right-perturbation smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-u-u)
   (type morphism hat-right-perturbation))
  (the chain-complex
       (progn
	 (setf (slot-value hat-right-perturbation 'sorc) hat-u-u
	       (slot-value hat-right-perturbation 'trgt) hat-u-u)
	 ;; cf lp-space-efhm.lisp
	 (add hat-u-u hat-right-perturbation))))


(DEFUN CS-HAT-LEFT-PERTURBATION (smgr
				 &aux (fibration (smgr-fibration smgr))
				   (bar (bar smgr))
				   (hat-u-t (cs-hat-u-t smgr)))
  (declare
   (type simplicial-group smgr)
   (type fibration fibration)
   (type chain-complex bar hat-u-t))
  (the morphism
       (multiple-value-bind (brown bottom-perturbation)
	   (brown-reduction fibration)
	 (declare
	  (ignore brown)
	  (type morphism bottom-perturbation))
	 (let ((rslt (tnsr-prdc bottom-perturbation (idnt-mrph bar))))
	   (declare (type morphism rslt))
	   (setf (slot-value rslt 'sorc) hat-u-t
		 (slot-value rslt 'trgt) hat-u-t)
	   rslt))))


(DEFUN CS-HAT-T-U (smgr
		   &aux (bar (bar smgr))
		     (fibration (smgr-fibration smgr))
		     (brown (brown-reduction fibration))
		     (twisted-tnsr-prdc (bcc brown))
		     (hat-u-u (cs-hat-u-u smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex bar twisted-tnsr-prdc hat-u-u)
   (type fibration fibration)
   (type reduction brown))
  (the chain-complex
       (let ((rslt (tnsr-prdc twisted-tnsr-prdc bar)))
	 (declare (type chain-complex rslt))
	 (setf (slot-value rslt 'grmd) hat-u-u)
	 rslt)))


(DEFUN CS-LEFT-HMEQ-HAT (smgr
			 &aux (hat-t-u (cs-hat-t-u smgr))
			   (hat-right-perturbation
			    (cs-hat-right-perturbation smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-t-u)
   (type morphism hat-right-perturbation))
  (the chain-complex
       (add hat-t-u hat-right-perturbation)))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-F (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn
	   (degr list) cmbn
	   (make-cmbn
	    :degr degr
	    :list (mapcar
		   #'(lambda (term)
		       (declare (type term term))
		       (with-term
			   (cffc tnpr) term
			   (term cffc (gnrt1 (gnrt1 tnpr)))))
		   (member-if
		    #'(lambda (term)
			(declare (type term term))
			(with-term
			    (cffc tnpr) term
			    (declare (ignore cffc))
			    (with-tnpr
				(degr1 tnpr1 degr2 abar2) tnpr
				(declare (ignore degr1 abar2))
				(and (zerop degr2) (zerop (degr2 tnpr1))))))
		    list))))))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-F (smgr
					  &aux (hat-u-t (cs-hat-u-t smgr))
					    (classifying-space (classifying-space smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-u-t)
   (type simplicial-set classifying-space))
  (the morphism
       (build-mrph
	:sorc hat-u-t
	:trgt classifying-space
	:degr 0
	:intr #'cs-pre-left-hmeq-left-reduction-intr-f
	:strt :cmbn
	:orgn `(cs-pre-left-hmeq-left-reduction-f ,smgr))))

(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-G (idnt)
  (declare (type gmsm idnt))
  (flet ((rslt (cmbn)
	   (declare (type cmbn cmbn))
	   (the cmbn
		(with-cmbn
		    (degr list) cmbn
		    (make-cmbn
		     :degr degr
		     :list (mapcar
			    #'(lambda (term)
				(declare (type term term))
				(with-term
				    (cffc gbar) term
				    (term cffc
					  (tnpr degr
						(tnpr degr gbar 0 idnt)
						0 +null-abar+))))
			    list))))))
    (the intr-mrph #'rslt)))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-G (smgr
					  &aux (hat-u-t (cs-hat-u-t smgr))
					    (classifying-space
					     (classifying-space smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-u-t)
   (type simplicial-set classifying-space))
  (the morphism
       (build-mrph
	:sorc classifying-space
	:trgt hat-u-t
	:degr 0
	:intr (cs-pre-left-hmeq-left-reduction-intr-g (bspn smgr))
	:strt :cmbn
	:orgn `(cs-pre-left-hmeq-left-reduction-g ,smgr))))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-H (cmpr idnt)
  (declare (type gmsm idnt))
  (flet ((rslt (cmbn)
	   (declare (type cmbn cmbn))
	   (the cmbn
		(with-cmbn
		    (degr list) cmbn
		    (do ((rslt (zero-cmbn (1+ degr)))
			 (mark list (cdr mark)))
			((endp mark) rslt)
		      (declare
		       (type cmbn rslt)
		       (list mark))
		      (let ((term (car mark)))
			(declare (type term term))
			(with-term
			    (cffc tnpr) term
			    (with-tnpr
				(degr1 tnpr1 degr2 abar2) tnpr
				(declare (ignore degr1))
				(with-tnpr
				    (degr11 gbar11 degr12 gmsm12) tnpr1
				    (unless (zerop degr12)
				      (dstr-add-term-to-cmbn
				       cmpr
				       (if (evenp degr11) cffc (- cffc))
				       (tnpr degr11
					     (tnpr degr11 gbar11 0 idnt)
					     (+ 1 degr12 degr2)
					     (make-abar
					      :list
					      (cons (brgn (1+ degr12)
							  gmsm12)
						    (abar-list abar2))))
				       rslt)))))))))))
    (the intr-mrph #'rslt)))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION-H (smgr
					  &aux (hat-u-t (cs-hat-u-t smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-u-t))
  (the morphism
       (build-mrph
	:sorc hat-u-t
	:trgt hat-u-t
	:degr +1
	:intr (cs-pre-left-hmeq-left-reduction-intr-h (cmpr hat-u-t)
						      (bspn smgr))
	:strt :cmbn
	:orgn `(cs-pre-left-hmeq-left-reduction-h ,smgr))))


(DEFUN CS-PRE-LEFT-HMEQ-LEFT-REDUCTION (smgr)
  (declare (type simplicial-group smgr))
  (the reduction
       (build-rdct
	:f (cs-pre-left-hmeq-left-reduction-f smgr)
	:g (cs-pre-left-hmeq-left-reduction-g smgr)
	:h (cs-pre-left-hmeq-left-reduction-h smgr)
	:orgn `(cs-pre-left-hmeq-left-reduction ,smgr))))


(DEFUN CS-LEFT-HMEQ-LEFT-REDUCTION (smgr
				    &aux (pre-left-hmeq-left-reduction
					  (cs-pre-left-hmeq-left-reduction
					   smgr))
				      (hat-left-perturbation
				       (cs-hat-left-perturbation smgr)))
  (declare
   (type simplicial-group smgr)
   (type reduction pre-left-hmeq-left-reduction)
   (type morphism hat-left-perturbation))
  (the reduction
       (progn
	 (dstr-change-sorc-trgt hat-left-perturbation
				:new-sorc (tcc pre-left-hmeq-left-reduction)
				:new-trgt (tcc pre-left-hmeq-left-reduction))
	 (let ((rslt (special-bpl-2 pre-left-hmeq-left-reduction
				    hat-left-perturbation)))
	   (declare (type reduction rslt))
	   (with-slots (tcc f g h) rslt
	     (setf tcc (cs-left-hmeq-hat smgr)
		   (slot-value f 'sorc) tcc
		   (slot-value g 'trgt) tcc
		   (slot-value h 'sorc) tcc
		   (slot-value h 'trgt) tcc))
	   rslt))))


(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-F (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn (degr list) cmbn
		  (let ((rslt +empty-list+))
		    (declare (list rslt))
		    (do ((mark list (cdr mark)))
			((endp mark))
		      (declare (list mark))
		      (with--term (cffc tnpr) mark
				  (with-tnpr (degr1 nil nil abar2) tnpr
					     (if (zerop degr1)
						 (push (term cffc abar2) rslt)
						 (return)))))
		    (make-cmbn :degr degr
			       :list (nreverse rslt))))))


(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-F (smgr
					   &aux (hat-t-u (cs-hat-t-u smgr))
					     (bar (bar smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-t-u bar))
  (the morphism
       (build-mrph
	:sorc hat-t-u :trgt bar :degr 0
	:intr #'cs-pre-left-hmeq-right-reduction-intr-f
	:strt :cmbn
	:orgn `(cs-pre-left-hmeq-right-reduction-f ,smgr))))


(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-G (idnt)
  (declare (type gmsm idnt))
  (flet ((rslt (cmbn)
	   (declare (type cmbn cmbn))
	   (the cmbn
		(with-cmbn
		    (degr list) cmbn
		    (let ((bsgn (tnpr 0 +null-gbar+ 0 idnt)))
		      (declare (type tnpr bsgn))
		      (make-cmbn :degr degr
				 :list (mapcar
					#'(lambda (term)
					    (declare (type term term))
					    (with-term
						(cffc abar) term
						(term cffc
						      (tnpr 0 bsgn
							    degr abar))))
					list)))))))
    (the intr-mrph #'rslt)))


(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-G (smgr
					   &aux (hat-t-u (cs-hat-t-u smgr))
					     (bar (bar smgr))
					     (idnt (bspn smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex hat-t-u bar)
   (type gmsm idnt))
  (the morphism
       (build-mrph
	:sorc bar :trgt hat-t-u :degr 0
	:intr (cs-pre-left-hmeq-right-reduction-intr-g idnt)
	:strt :cmbn
	:orgn `(cs-pre-left-hmeq-right-reduction-g ,smgr))))

(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-H (smgr
					   &aux (bar (bar smgr))
					     (tnpr-contraction
					      (smgr-tnpr-contraction smgr)))
  (declare
   (type simplicial-group smgr)
   (type chain-complex bar)
   (type morphism tnpr-contraction))
  (the morphism
       (tnsr-prdc tnpr-contraction (idnt-mrph bar))))

(DEFUN CS-PRE-LEFT-HMEQ-RIGHT-REDUCTION (smgr)
  (declare (type simplicial-group smgr))
  (the reduction
       (build-rdct
	:f (cs-pre-left-hmeq-right-reduction-f smgr)
	:g (cs-pre-left-hmeq-right-reduction-g smgr)
	:h (cs-pre-left-hmeq-right-reduction-h smgr)
	:orgn `(cs-pre-left-hmeq-right-reduction ,smgr))))


(DEFUN CS-LEFT-HMEQ-RIGHT-REDUCTION (smgr
				     &aux (pre-reduction
					   (cs-pre-left-hmeq-right-reduction
					    smgr))
				       (perturbation
					(cs-hat-right-perturbation smgr)))
  (declare
   (type simplicial-group smgr)
   (type reduction pre-reduction)
   (type morphism perturbation))
  (the reduction
       (progn
	 (setf (slot-value perturbation 'sorc) (tcc pre-reduction)
	       (slot-value perturbation 'trgt) (tcc pre-reduction))
	 (special-bpl-2 pre-reduction perturbation))))


(DEFUN CS-LEFT-HMEQ (smgr)
  (declare (type simplicial-group smgr))
  (the homotopy-equivalence
       (build-hmeq
	:lrdct (cs-left-hmeq-left-reduction smgr)
	:rrdct (cs-left-hmeq-right-reduction smgr)
	:orgn `(cs-left-hmeq ,smgr))))


(DEFUN CLASSIFYING-SPACE-EFHM (smgr)
  (declare (type simplicial-group smgr))
  (let ((left-hmeq (cs-left-hmeq smgr))
	(right-hmeq (bar (efhm smgr))))
    (declare (type homotopy-equivalence left-hmeq right-hmeq))
    (cmps left-hmeq right-hmeq)))


(DEFMETHOD SEARCH-EFHM (classifying-space (orgn (eql 'classifying-space)))
  (declare (type simplicial-set classifying-space))
  (classifying-space-efhm (second (orgn classifying-space))))
