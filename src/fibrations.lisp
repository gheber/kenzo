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

#|
  (cat-init)
  (setf h (hopf 1))
  (setf sf (serre-face h))
  (funcall sf 0 2 (crpr 0 's2 0 '(2 4)))
  (funcall sf 1 2 (crpr 0 's2 0 '(2 4)))
  (funcall sf 2 2 (crpr 0 's2 0 '(2 4)))
  (funcall sf 3 3 (crpr 1 's2 2 '(2 4)))
|#

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

#|
  (cat-init)
  (setf s2 (sphere 2))
  (setf k (k-z-1))
  (setf tw (build-smmr
             :sorc s2
	     :trgt k
	     :degr -1
	     :sintr #'(lambda (dmns gmsm)
			(absm 0 (list 1)))
	     :orgn '(s2-tw-kz1)))
  (setf e (fibration-total tw))
  (homology e 2)
  (homology e 0 8)
  (cat-init)
  (setf s2 (sphere 2))
  (setf k (k-z2-1))
  (setf tw (build-smmr
             :sorc s2
	     :trgt k
	     :degr -1
	     :sintr #'(lambda (dmns gmsm)
			(absm 0 1))
	     :orgn '(s2-tw-kz2)))
  (setf e (fibration-total tw))
  (homology e 2)
  (homology e 0 8)
|#
		 
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
		  (with-absm (dgop crpr) absm
		  (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
		     (push (absm (dgop*dgop dgop dgop1) gmsm1) bhat)
		     (push (absm (dgop*dgop dgop dgop2) gmsm2) fhat)))))
	     (setf bhat (nreverse bhat))
	     (let ((bkfll (funcall bkfll indx dmns bhat)))
	       (declare (type absm bkfll))
	       (when (< indx dmns)
		 (setf (first fhat)
		       (a-grml4 sintr-grml (1- dmns)
				(a-grin4 sintr-grin (1- dmns)
					 (tw-a-sintr3 sintr-twist dmns bkfll fbspn))
				(first fhat))))
	       (setf fhat (nreverse fhat))
	       (let ((fkfll (funcall fkfll indx dmns fhat)))
		 (declare (type absm fkfll))
		 (2absm-acrpr bkfll fkfll))))))
	(the kfll #'rslt)))

#|
  (cat-init)
  (load "r-proj-space")
  (setf tw (kdivide-z2-twist opr opr-chml-clss))
  (setf pr-4 (fibration-total tw))
  (setf absm (absm 0 (crpr 0 (loop3 0 5 2) 0 4)))
  (setf hat (mapcar #'(lambda (i) (face pr-4 i 4 absm))
                    (<a-b> 0 4)))
  (dotimes (i 5)
    (print (kfll pr-4 i 4 (remove (nth i hat) hat)))
    (check-kan pr-4 i 4 (remove (nth i hat) hat)))
|#

#|
  (cat-init)
  (setf os3-fibration os3-fibration)
  (setf total (fibration-total os3-fibration))
  (setf absm (absm 1 (crpr 0 (loop3 1 's3 1 2 's3 -2)
				   0 '(-2 1 1))))
  (setf hat (mapcar #'(lambda (i) (face total i 4 absm))
                    '(0 1 2 3 4)))
  (dotimes (i 5)
    (print (kfll total i 4 (remove (nth i hat) hat)))
    (check-kan total i 4 (remove (nth i hat) hat)))
|#
