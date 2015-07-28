;;;  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE
;;;  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE
;;;  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE  SERRE

(IN-PACKAGE #:cat)

(PROVIDE "serre")

(DEFUN FIBRATION-DTAU-D-INTR
    (fibration &aux (base (sorc fibration))
		 (bface (face base))
		 (fibre (trgt fibration))
		 (fface (face fibre))
		 (sintr-grml (sintr (grml fibre)))
		 (fbspn (bspn fibre))
		 (sintr-twist (sintr fibration))
		 (total (fibration-total fibration))
		 (total-cmpr (cmpr total)))
  (declare
   (type fibration fibration)
   (type simplicial-set base total)
   (type face bface fface)
   (type simplicial-group fibre)
   (type gmsm fbspn)
   (type sintr sintr-grml sintr-twist)
   (type cmprf total-cmpr))
  (flet ((rslt (dmns crpr
                     &aux (dmns-1 (1- dmns)))
	   (declare
	    (fixnum dmns dmns-1)
	    (type crpr crpr))
	   (unless (plusp dmns)
	     (return-from rslt
	       (zero-cmbn dmns-1)))
	   (with-crpr
	       (b-absm f-absm) crpr
	       (let ((tau-b
		      (tw-a-sintr3 sintr-twist dmns
				   b-absm fbspn)))
		 (declare (type absm tau-b))
		 (when (= (dgop tau-b) (mask dmns-1))
		   (return-from rslt
		     (zero-cmbn dmns-1)))
		 (let ((deln-b
			(a-face4 bface dmns dmns
				 b-absm))
		       (deln-f
			(a-face4 fface dmns dmns
				 f-absm)))
		   (declare
		    (type absm deln-b deln-f tau-b))
		   (let ((acrpr1 (2absm-acrpr deln-b deln-f))
			 (acrpr2
			  (2absm-acrpr
			   deln-b
			   (a-grml4 sintr-grml (1- dmns)
				    tau-b deln-f))))
		     (declare (type absm acrpr1 acrpr2))
		     (with-absm
			 (dgop1 crpr1) acrpr1
			 (with-absm
			     (dgop2 crpr2) acrpr2
			     (if (zerop dgop1)
				 (if (zerop dgop2)
				     (dstr-add-term-to-cmbn
				      total-cmpr
				      (-1-expt-n+1 dmns) crpr1
				      (term-cmbn dmns-1
						 (-1-expt-n dmns)
						 crpr2))
				     (term-cmbn dmns-1
						(-1-expt-n+1 dmns)
						crpr1))
				 (if (zerop dgop2)
				     (term-cmbn dmns-1
						(-1-expt-n dmns)
						crpr2)
				     (zero-cmbn dmns-1)))))))))))
    (the intr-mrph #'rslt)))


(DEFUN FIBRATION-DTAU-D (fibration
			 &aux (base (sorc fibration))
			   (fibre (trgt fibration))
			   (utotal (crts-prdc base fibre)))
  (declare (type fibration fibration))
  (the morphism
       (build-mrph
        :sorc utotal
	:trgt utotal
	:degr -1
	:intr (fibration-dtau-d-intr fibration)
	:strt :gnrt
	:orgn `(fibration-dtau-d ,fibration))))


(DEFUN BROWN-REDUCTION (fibration
		        &aux (base (sorc fibration))
			  (fibre (trgt fibration))
			  (total (fibration-total fibration))
			  (ez (eilenberg-zilber base fibre))
			  (t-perturbation (fibration-dtau-d fibration)))
  (declare
   (type fibration fibration)
   (type simplicial-set base fibre total)
   (type reduction ez)
   (type morphism t-perturbation))
  (the (values reduction morphism)
       (multiple-value-bind (rslt b-perturbation) (add ez t-perturbation)
	 (declare
	  (type reduction rslt)
	  (type morphism b-perturbation))
	 (with-slots (f g h tcc) rslt
	   (setf tcc total)
	   (dstr-change-sorc-trgt f :new-sorc total)
	   (dstr-change-sorc-trgt g :new-trgt total)
	   (dstr-change-sorc-trgt h :new-sorc total :new-trgt total)
	   (values rslt b-perturbation)))))


(DEFUN LEFT-SERRE-EFHM (fibration)
  (declare (type fibration fibration))
  (the homotopy-equivalence
       (build-hmeq
        :lrdct (trivial-rdct (fibration-total fibration))
	:rrdct (brown-reduction fibration))))


(DEFUN RIGHT-SERRE-EFHM (fibration
			 &aux (base (sorc fibration))
			   (fibre (trgt fibration))
			   (base-efhm (efhm base))
			   (fibre-efhm (efhm fibre))
			   (uprdc-efhm (tnsr-prdc base-efhm fibre-efhm)))
  (declare
   (type fibration fibration)
   (type simplicial-set base fibre)
   (type homotopy-equivalence base-efhm fibre-efhm uprdc-efhm))
  (the homotopy-equivalence
       (multiple-value-bind (brown b-perturbation) (brown-reduction fibration)
	 (declare
	  (type reduction brown)
	  (type morphism b-perturbation))
	 (let ((rslt (add uprdc-efhm b-perturbation)))
	   (declare (type homotopy-equivalence rslt))
	   (with-slots (lbcc lf lg lrdct) rslt
	     (declare
	      (type chain-complex lbcc)
	      (type morphism lf lg)
	      (type reduction lrdct))
	     (setf lbcc (bcc brown))
	     (dstr-change-sorc-trgt lf :new-trgt lbcc)
	     (dstr-change-sorc-trgt lg :new-sorc lbcc)
	     (with-slots (bcc f g) lrdct
	       (declare
		(type chain-complex bcc)
		(type morphism f g))
	       (setf bcc lbcc)
	       (dstr-change-sorc-trgt f :new-trgt lbcc)
	       (dstr-change-sorc-trgt g :new-sorc lbcc)))
	   rslt))))


(DEFUN FIBRATION-TOTAL-EFHM (fibration)
  (declare (type fibration fibration))
  (the homotopy-equivalence
       (cmps (left-serre-efhm fibration)
	     (right-serre-efhm fibration))))


(DEFMETHOD SEARCH-EFHM (smst (orgn (eql 'fibration-total)))
  (declare
   (type simplicial-set smst))
  (the homotopy-equivalence
       (fibration-total-efhm (second (orgn smst)))))
