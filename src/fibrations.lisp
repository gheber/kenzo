;;;  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS
;;;  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS
;;;  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS  FIBRATIONS

(IN-PACKAGE #:cat)

(PROVIDE "fibrations")

(DEFUN FIBRATION-TOTAL-FACE (fibration
			     &aux (base (sorc fibration))
			       (bface (face base))
			       (fibr (trgt fibration))
			       (fbspn (bspn fibr))
			       (fface (face fibr))
			       (grml (grml fibr))
			       (sintr-grml (sintr grml))
			       (totl (crts-prdc base fibr))
			       (uface (face totl))
			       (sintr-twist (sintr fibration)))
  (declare
   (type fibration fibration)
   (type simplicial-mrph grml)
   (type simplicial-set base totl)
   (type simplicial-group fibr)
   (type gmsm fbspn)
   (type face bface fface uface)
   (type sintr sintr-grml sintr-twist))
  (flet ((rslt (indx dmns crpr)
	   (declare
	    (fixnum indx dmns)
	    (type crpr crpr))
	   (the absm
		(progn
		  (when (< indx dmns)
		    (return-from rslt
		      (funcall uface indx dmns crpr)))
		  (with-crpr (b-absm f-absm) crpr
			     (let ((deln-b
				    (a-face4 bface dmns dmns
					     b-absm))
				   (deln-f
				    (a-face4 fface dmns dmns
					     f-absm))
				   (tau-b
				    (tw-a-sintr3 sintr-twist dmns
						 b-absm fbspn)))
			       (declare
				( type absm deln-b deln-f tau-b))
			       (2absm-acrpr
				deln-b
				(a-grml4 sintr-grml (1- dmns)
					 tau-b deln-f))))))))
    (the face #'rslt)))


(DEFUN FIBRATION-TOTAL (fibration
			&aux (base (sorc fibration))
			  (fibre (trgt fibration))
			  (untwisted-prdc
			   (crts-prdc base fibre)))
  (declare (type fibration fibration))
  (the simplicial-set
       (let ((rslt (build-smst
                    :cmpr (cmpr untwisted-prdc)
		    :basis (basis untwisted-prdc)
		    :bspn (bspn untwisted-prdc)
		    :face (fibration-total-face fibration)
		    :orgn `(fibration-total ,fibration))))
	 (declare (type simplicial-set rslt))
	 (setf (slot-value rslt 'grmd) untwisted-prdc)
	 (when (and (typep base 'kan)
		    (typep fibre 'kan))
	   (change-class rslt 'kan)
	   (setf (slot-value rslt 'kfll)
		 (fibration-kfll fibration)))
	 rslt)))


(DEFUN FIBRATION-KFLL (twist
		       &aux (sintr-twist (sintr twist))
			 (base (sorc twist))
			 (fibre (trgt twist))
			 (fbspn (bspn fibre))
			 (bkfll (kfll base))
			 (fkfll (kfll fibre))
			 (sintr-grml (sintr (grml fibre)))
			 (sintr-grin (sintr (grin fibre))))
  (declare
   (type fibration twist)
   (type kan base fibre)
   (type gmsm fbspn)
   (type kfll bkfll fkfll)
   (type sintr sintr-grml sintr-grin))
  (flet ((rslt (indx dmns hat)
	   (declare
	    (fixnum indx dmns)
	    (list hat))
	   (let (bhat fhat)
	     (declare (type list bhat fhat))
	     (do ((mark hat (cdr mark)))
		 ((endp mark))
	       (declare (list mark))
	       (let ((absm (car mark)))
		 (declare (type absm absm))
		 (with-absm
		     (dgop crpr) absm
		     (with-crpr
			 (dgop1 gmsm1 dgop2 gmsm2) crpr
			 (push (absm (dgop*dgop dgop dgop1) gmsm1) bhat)
			 (push (absm (dgop*dgop dgop dgop2) gmsm2) fhat)))))
	     (setf bhat (nreverse bhat))
	     (let ((bkfll (funcall bkfll indx dmns bhat)))
	       (declare (type absm bkfll))
	       (when (< indx dmns)
		 (setf (first fhat)
		       (a-grml4 sintr-grml (1- dmns)
				(a-grin4 sintr-grin (1- dmns)
					 (tw-a-sintr3 sintr-twist dmns bkfll
						      fbspn))
				(first fhat))))
	       (setf fhat (nreverse fhat))
	       (let ((fkfll (funcall fkfll indx dmns fhat)))
		 (declare (type absm fkfll))
		 (2absm-acrpr bkfll fkfll))))))
    (the kfll #'rslt)))
