;;;  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1
;;;  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1
;;;  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1  K-PI-1

(IN-PACKAGE #:cat)

(PROVIDE "k-pi-1")

(DEFUN K-Z-1-CMPR (gnrt1 gnrt2)
  (declare (list gnrt1 gnrt2))
  (the cmpr
       (do ((mark1 gnrt1 (cdr mark1))
	    (mark2 gnrt2 (cdr mark2)))
	   ((endp mark1) :equal)
	 (case (f-cmpr (car mark1) (car mark2))
	   (:equal)
	   (:less (return-from k-z-1-cmpr :less))
	   (:greater (return-from k-z-1-cmpr :greater))))))


(DEFUN K-Z-1-FACE (indx dmns gmsm)
  (declare
   (fixnum indx dmns)
   (list gmsm))
  (the absm
       (cond ((zerop indx) (absm 0 (rest gmsm)))
	     ((= indx dmns) (absm 0 (butlast gmsm)))
	     (t
	      (do ((rslt +empty-list+ (cons (car mark) rslt))
		   (mark gmsm (cdr mark))
		   (i 1 (1+ i)))
		  ((= i indx) (let ((new-k (+ (car mark) (cadr mark))))
				(declare (fixnum new-k))
				(if (zerop new-k)
				    (absm (2-exp (1- indx))
					  (nreconc rslt (cddr mark)))
				    (absm 0
					  (nreconc rslt
						   (cons new-k
							 (cddr mark)))))))
		(declare
		 (list rslt mark)
		 (fixnum i)))))))


(DEFUN Z-ABSM-BAR (absm)
  (declare (type absm absm))
  (the list
       (with-absm (dgop bar1) absm
		  (do ((dgop dgop (ash dgop -1))
		       (rslt +empty-list+))
		      ((and (zerop dgop) (endp bar1)) (nreverse rslt))
		    (declare
		     (fixnum dgop)
		     (list rslt))
		    (push (if (oddp dgop)
			      0
			      (pop bar1))
			  rslt)))))


(DEFUN Z-BAR-ABSM (bar)
  (declare (list bar))
  (the absm
       (do ((rslt-dgop 0)
	    (rslt-bar +empty-list+)
	    (mark bar (cdr mark))
	    (bark 1 (+ bark bark)))
	   ((endp mark) (absm rslt-dgop (nreverse rslt-bar)))
	 (declare
	  (fixnum rslt-dgop bark)
	  (list rslt-bar mark))
	 (let ((k-i (car mark)))
	   (declare (fixnum k-i))
	   (if (zerop k-i)
	       (incf rslt-dgop bark)
	       (push k-i rslt-bar))))))


(DEFUN K-Z-1-GRML (dmns crpr)
  (declare
   (fixnum dmns)
   (type crpr crpr))
  (the absm
       (with-crpr (dgop1 bar1 dgop2 bar2) crpr
		  (do ((indx 1 (1+ indx))
		       (dgop1 dgop1 (ash dgop1 -1))
		       (dgop2 dgop2 (ash dgop2 -1))
		       (bark 1 (ash bark +1))
		       (rslt-dgop 0)
		       (rslt-bar +empty-list+))
		      ((> indx dmns) (absm rslt-dgop (nreverse rslt-bar)))
		    (declare
		     (fixnum indx dgop1 dgop2 bark rslt-dgop)
		     (list rslt-bar))
		    (let ((item (if (evenp dgop1)
				    (if (evenp dgop2)
					(+ (pop bar1) (pop bar2))
					(pop bar1))
				    (if (evenp dgop2)
					(pop bar2)
					0))))
		      (declare (fixnum item))
		      (if (zerop item)
			  (incf rslt-dgop bark)
			  (push item rslt-bar)))))))


(DEFUN K-Z-1-GRIN (dmns bar)
  (declare
   (ignore dmns)
   (list bar))
  (the absm
       (absm 0 (mapcar #'- bar))))


#|
(DEFUN K-Z-1-KFLL (indx dmns hat)
  (declare
   (fixnum indx dmns)
   (list hat))
  (the absm
       (cond ((= 1 dmns)
	      (absm 1 +empty-list+))
	     ((zerop indx)
	      (let ((del-1 (first hat))
		    (del-2 (second hat)))
		(declare (type absm del-1 del-2))
		(with-absm (dgop1 bar1) del-1
			   (with-absm (dgop2 bar2) del-2
				      (let ((bar01 (if (oddp dgop2)
						       0
						       (first bar2)))
					    (bar02 (if (oddp dgop1)
						       0
						       (first bar1))))
					(declare (fixnum bar01 bar02))
					(let ((bar12 (- bar02 bar01)))
					  (declare (fixnum bar02))
					  (if (zerop bar01)
					      (1dgnr 0 del-1)
					      (if (zerop bar12)
						  (1gnr 1 del-1)
						  (let (del-01 (k-z-1-face 0 (1- dmns)
									   del-1))
						    (declare (type absm del-01))
						    (with-absm (dgop01 gmsm01) del-01
							       (absm (* 4 dgop01)
								     (cons bar01
									   (cons bar02 gmsm01)))))))))))))
	     ((= 1 indx)
	      (let ((del-0 (first hat))
		    (del-2 (second hat)))
		(declare (type absm del-0 del-2))
		(with-absm (dgop0 gmsm0) del-0
			   (with-absm (dgop2 gmsm2) del-2
				      (if (oddp dgop2 )))))))))

|#

(DEFUN K-Z-1 ()
  (the ab-simplicial-group
       (build-ab-smgr
        :cmpr #'k-z-1-cmpr
	:basis :locally-effective
	:bspn +empty-list+
	:face #'k-z-1-face
	:sintr-grml #'k-z-1-grml
	:sintr-grin #'k-z-1-grin
	:orgn '(k-z-1))))


(DEFUN K-Z (n)
  (declare (fixnum n))
  (the ab-simplicial-group
       (if (= n 1)
	   (k-z-1)
	   (classifying-space (k-z (1- n))))))

(DEFUN CIRCLE-CMPR (gnrt1 gnrt2)
  (declare (ignore gnrt1 gnrt2))
  (the cmpr :equal))

(DEFUN CIRCLE-BASIS (dmns)
  (declare (fixnum dmns))
  (the list
       (case dmns
	 (0 '(*))
	 (1 '(s1))
	 (otherwise +empty-list+))))

(DEFUN CIRCLE ()
  (the chain-complex
       (build-chcm
	:cmpr #'circle-cmpr
	:basis #'circle-basis
	:bsgn '*
	:intr-dffr #'zero-intr-dffr
	:strt :cmbn
	:orgn '(circle))))


(DEFUN KZ1-RDCT-F-INTR (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn
	   (degr list) cmbn
	   (case degr
	     (0 (if list
		    (term-cmbn 0 (-cffc list) '*)
		    (zero-cmbn 0)))
	     (1 (let ((rslt
		       (apply #'+
			      (mapcar
			       #'(lambda (term)
				   (declare (type term term))
				   (with-term
				       (cffc gnrt) term
				       (the fixnum (* cffc (first gnrt)))))
			       list))))
		  (if (zerop rslt)
		      (zero-cmbn 1)
		      (term-cmbn 1 rslt 's1))))
	     (otherwise (zero-cmbn degr))))))


(DEFUN KZ1-RDCT-F ()
  (the morphism
       (build-mrph
        :sorc (k-z-1)
	:trgt (circle)
	:degr 0
	:intr #'kz1-rdct-f-intr
	:strt :cmbn
	:orgn '(kz1-rdct-f))))

(DEFUN KZ1-RDCT-G-INTR (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn (degr list) cmbn
		  (if list
		      (ecase degr
			(0 (term-cmbn 0 (-cffc list) +empty-list+))
			(1 (term-cmbn 1 (-cffc list) (list 1))))
		      (zero-cmbn degr)))))


(DEFUN KZ1-RDCT-G ()
  (the morphism
       (build-mrph
        :sorc (circle)
	:trgt (k-z-1)
	:degr 0
	:intr #'kz1-rdct-g-intr
	:strt :cmbn
	:orgn '(kz1-rdct-g))))


(DEFUN KZ1-RDCT-H-INTR (dmns bar)
  (declare
   (fixnum dmns)
   (list bar))
  (the cmbn
       (progn
	 (when (zerop dmns)
	   (return-from kz1-rdct-h-intr (zero-cmbn +1)))
	 (let ((k1 (first bar))
	       (tail (rest bar)))
	   (declare
	    (fixnum k1)
	    (list tail))
	   (if (plusp k1)
	       (do ((rslt +empty-list+
			  (cons (term -1
				      (cons 1 (cons i tail)))
				rslt))
		    (i (1- k1) (1- i)))
		   ((zerop i) (make-cmbn :degr (1+ dmns)
					 :list rslt))
		 (declare
		  (list rslt)
		  (fixnum i)))
	       (do ((rslt +empty-list+
			  (cons (term +1
				      (cons 1 (cons i tail)))
				rslt))
		    (i -1 (1- i)))
		   ((< i k1) (make-cmbn :degr (1+ dmns)
					:list rslt))))))))


(DEFUN KZ1-RDCT-H ()
  (the morphism
       (build-mrph
        :sorc (k-z-1)
	:trgt (k-z-1)
	:degr +1
	:intr #'kz1-rdct-h-intr
	:strt :gnrt
	:orgn '(kz1-rdct-h))))


(DEFUN KZ1-RDCT ()
  (the reduction
       (build-rdct
        :f (kz1-rdct-f)
	:g (kz1-rdct-g)
	:h (kz1-rdct-h)
	:orgn '(kz1-rdct))))


(DEFUN KZ1-EFHM ()
  (the homotopy-equivalence
       (build-hmeq
        :lrdct (trivial-rdct (k-z-1))
	:rrdct (kz1-rdct)
	:orgn '(kz1-efhm))))

(DEFMETHOD SEARCH-EFHM (k-z-1 (orgn (eql 'k-z-1)))
  (declare (ignore k-z-1))
  (kz1-efhm))


(DEFUN K-Z2-1-GRML-INTR (dmns crpr)
  (declare
   (fixnum dmns)
   (type crpr crpr))
  (the absm
       (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
		  (declare (ignore gmsm1 gmsm2))
		  (let ((dgop (- (mask dmns) (logxor dgop1 dgop2))))
		    (declare (fixnum dgop))
		    (absm dgop (- dmns (logcount dgop)))))))


(DEFUN K-Z2-1-GRML ()
  (the simplicial-mrph
       (build-smmr
        :sorc (crts-prdc (r-proj-space) (r-proj-space))
	:trgt (r-proj-space)
	:degr 0
	:sintr #'k-z2-1-grml-intr
	:orgn '(k-z2-1-grml))))


(DEFUN K-Z2-1-GRIN-INTR (dmns gmsm)
  (declare
   (ignore dmns)
   (fixnum gmsm))
  (the absm
       (absm 0 gmsm)))


(DEFUN K-Z2-1-GRIN ()
  (the simplicial-mrph
       (build-smmr
        :sorc (r-proj-space)
	:trgt (r-proj-space)
	:degr 0
	:sintr #'k-z2-1-grin-intr
	:intr #'identity
	:strt :cmbn
	:orgn '(k-z2-1-grin))))


(DEFUN K-Z2-1 ()
  (the ab-simplicial-group
       (let ((rslt (r-proj-space)))
	 (declare (type simplicial-set rslt))
	 (change-class rslt 'ab-simplicial-group)
	 (setf (slot-value rslt 'grml)
	       (k-z2-1-grml)
	       (slot-value rslt 'grin)
	       (k-z2-1-grin))
	 (pushnew rslt *smgr-list*)
	 rslt)))


(DEFUN K-Z2 (n)
  (declare (fixnum n))
  (the ab-simplicial-group
       (if (= n 1)
	   (k-z2-1)
	   (classifying-space (k-z2 (1- n))))))


(DEFUN Z2-ABSM-BAR (absm)
  (declare (type absm absm))
  (the list
       (with-absm (dgop gmsm) absm
		  (do ((bmark (1- (+ (logcount dgop) gmsm)) (1- bmark))
		       (rslt +empty-list+
			     (cons (if (logbitp bmark dgop)
				       0 1)
				   rslt)))
		      ((minusp bmark) rslt)
		    (declare
		     (fixnum bmark)
		     (list rslt))))))


(DEFUN Z2-BAR-ABSM (bar)
  (declare (list bar))
  (the absm
       (do ((dgop 0)
	    (2-pwr 1 (+ 2-pwr 2-pwr))
	    (gmsm 0)
	    (mark bar (cdr mark)))
	   ((endp mark) (absm dgop gmsm))
	 (declare
	  (fixnum dgop 2-pwr gmsm)
	  (list mark))
	 (if (zerop (car mark))
	     (incf dgop 2-pwr)
	     (incf gmsm)))))


(DEFUN HOPF-FIBRATION-SINTR (n)
  (declare (fixnum n))
  (flet ((rslt (dmns gmsm)
	   (declare (ignore dmns gmsm))
	   (if (zerop n)
	       (absm 1 +empty-list+)
	       (absm 0 (list n)))))
    (the sintr #'rslt)))


(DEFUN HOPF-FIBRATION (n)
  (declare (fixnum n))
  (the simplicial-mrph
       (build-smmr
        :sorc (sphere 2)
	:trgt (k-z-1)
	:degr -1
	:sintr (hopf-fibration-sintr n)
	:orgn `(hopf-fibration ,n))))


(DEFUN Z-FUNDAMENTAL-GMSM (dmns pi-elm)   ;;; pi-elm not equal to 0
  (declare (fixnum dmns pi-elm))
  (the gmsm
       (if (= 1 dmns)
	   (list pi-elm)
	   (let ((bspn (if (= 2 dmns) +empty-list+ +null-gbar+)))
	     (declare (type gmsm bspn))
	     (make-gbar :dmns dmns
			:list (cons (absm 0 (z-fundamental-gmsm (1- dmns)
								pi-elm))
				    (mapcar
				     #'(lambda (k)
					 (declare (fixnum k))
					 (absm (mask k) bspn))
				     (nreverse (<a-b> 0 (- dmns 2))))))))))


(DEFUN INTERESTING-FACES (face small-dmns high-dmns gmsm)
  (declare
   (type face face)
   (fixnum small-dmns high-dmns)
   (type gmsm gmsm))
  (the list
       (let ((max (mask high-dmns)))
	 (declare (fixnum max))
	 (if (= small-dmns high-dmns)
	     (list (cons max (absm 0 gmsm)))
	     (do ((prev-list (interesting-faces face (1+ small-dmns) high-dmns
						gmsm))
		  (k (1- max) (1- k))
		  (rslt +empty-list+)
		  (count (binomial-n-p high-dmns small-dmns)))
		 ((zerop count) rslt)
	       (declare
		(list prev-list rslt)
		(fixnum k count))
	       (when (= small-dmns (logcount k))
		 (decf count)
		 (do ((i 0 (1+ i)))
		     ((not (logbitp i k))
		      (push (cons k
				  (a-face4 face
					   (- small-dmns -1
					      (if (= i (integer-length k))
						  (1+ i)
						  i))
					   (1+ small-dmns)
					   (cdr (assoc (+ k (2-exp i))
						       prev-list))))
			    rslt)))))))))


(DEFUN GMSM-COCYCLE (face small-dmns high-dmns gmsm chml-clss)
  (declare
   (type face face)
   (fixnum small-dmns high-dmns)
   (type gmsm gmsm)
   (type morphism chml-clss))
  (the list
       (let ((int-faces (interesting-faces face small-dmns high-dmns gmsm)))
	 (declare (list int-faces))
	 (mapc
	  #'(lambda (cons)
	      (declare (cons cons))
	      (setf (cdr cons)
		    (let ((absm (cdr cons)))
		      (declare (type absm absm))
		      (with-absm (dgop gmsm2) absm
				 (if (plusp dgop)
				     0
				     (let ((cmbn-list
					    (cmbn-list (gnrt-? chml-clss
							       small-dmns
							       gmsm2))))
				       (declare (list cmbn-list))
				       (if cmbn-list
					   (-cffc cmbn-list)
					   0)))))))
	  int-faces)
	 int-faces)))


(DEFUN Z-COCYCLE-GBAR (n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z
  (declare
   (fixnum n dmns)
   (list cocycle))
  (the absm
       (cond ((= n 1)
	      (z-bar-absm (nreverse (mapcar #'cdr cocycle))))
	     ((= n dmns)
	      (let ((value (cdr (first cocycle))))
		(declare (fixnum value))
		(if (zerop value)
		    (absm (mask dmns) +null-gbar+)
		    (absm 0 (z-fundamental-gmsm dmns value)))))
	     (t
	      (let ((cocycle1 +empty-list+)
		    (cocycle2 +empty-list+))
		(declare (list cocycle1 cocycle2))
		(dolist (cons cocycle)
		  (declare (cons cons))
		  (if (evenp (car cons))
		      (push (cons (ash (car cons) -1) (cdr cons))
			    cocycle1)
		      (push (cons (ash (car cons) -1) (cdr cons))
			    cocycle2)))
		(setf cocycle1 (nreverse cocycle1)
		      cocycle2 (nreverse cocycle2))
		(mapc
		 #'(lambda (cons)
		     (declare (cons cons))
		     (when (evenp (car cons))
		       (decf (cdr cons)
			     (cdr (assoc (1+ (car cons)) cocycle1)))))
		 cocycle2)
		(let ((head-absm (z-cocycle-gbar (1- n) (1- dmns) cocycle2))
		      (tail-absm (z-cocycle-gbar n (1- dmns) cocycle1)))
		  (declare (type absm head-absm tail-absm))
		  (normalize-gbar
		   (cons dmns
			 (cons head-absm
			       (rest (unnormalize-gbar
				      tail-absm
				      (if (= n 2) () +null-gbar+))))))))))))


(DEFUN Z-COCYCLE-GBAR-HEAD (n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z
  (declare
   (fixnum n dmns)
   (list cocycle))
  (the absm
       (cond ((= n 1)
	      (error "In Z-COCYCLE-GBAR-HEAD, this point should not have been reached."))
	     ;; (z-bar-absm (nreverse (mapcar #'cdr cocycle))))
	     ((= n dmns)
	      (let ((value (cdr (first cocycle))))
		(declare (fixnum value))
		(if (zerop value)
		    (if (= n 2)
			(absm 1 +empty-list+)
			(absm (mask (1- dmns)) +null-gbar+))
		    (absm 0 (z-fundamental-gmsm (1- dmns) value)))))
	     (t
	      (let ((cocycle1 +empty-list+)
		    (cocycle2 +empty-list+))
		(declare (list cocycle1 cocycle2))
		(dolist (cons cocycle)
		  (declare (cons cons))
		  (if (evenp (car cons))
		      (push (cons (ash (car cons) -1) (cdr cons))
			    cocycle1)
		      (push (cons (ash (car cons) -1) (cdr cons))
			    cocycle2)))
		(setf cocycle1 (nreverse cocycle1)
		      cocycle2 (nreverse cocycle2))
		(mapc
		 #'(lambda (cons)
		     (declare (cons cons))
		     (when (evenp (car cons))
		       (decf (cdr cons)
			     (cdr (assoc (1+ (car cons)) cocycle1)))))
		 cocycle2)
		(z-cocycle-gbar (1- n) (1- dmns) cocycle2))))))


(DEFUN Z2-FUNDAMENTAL-GMSM (dmns pi-elm)  ;; pi-elm not "equal" to 0, therefore 1.
  (declare (fixnum dmns pi-elm))
  (the gmsm
       (if (= 1 dmns)
	   1
	   (let ((bspn (if (= 2 dmns) 0 +null-gbar+)))
	     (declare (type gmsm bspn))
	     (make-gbar :dmns dmns
			:list (cons (absm 0 (Z2-fundamental-gmsm (1- dmns)
								 pi-elm))
				    (mapcar
				     #'(lambda (k)
					 (declare (fixnum k))
					 (absm (mask k) bspn))
				     (nreverse (<a-b> 0 (- dmns 2))))))))))


(DEFUN Z2-COCYCLE-GBAR (n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z/2Z
  (declare
   (fixnum n dmns)
   (list cocycle))
  (the absm
       (progn
	 (mapc
	  #'(lambda (cons)
	      (declare (cons cons))
	      (setf (cdr cons)
		    (if (evenp (cdr cons)) 0 1)))
	  cocycle)
	 (cond ((= n 1)
		(z2-bar-absm (nreverse (mapcar #'cdr cocycle))))
	       ((= n dmns)
		(let ((value (cdr (first cocycle))))
		  (declare (fixnum value))
		  (if (zerop value)
		      (absm (mask dmns) +null-gbar+)
		      (absm 0 (z2-fundamental-gmsm dmns value)))))
	       (t
		(let ((cocycle1 +empty-list+)
		      (cocycle2 +empty-list+))
		  (declare (list cocycle1 cocycle2))
		  (dolist (cons cocycle)
		    (declare (cons cons))
		    (if (evenp (car cons))
			(push (cons (ash (car cons) -1) (cdr cons))
			      cocycle1)
			(push (cons (ash (car cons) -1) (cdr cons))
			      cocycle2)))
		  (setf cocycle1 (nreverse cocycle1)
			cocycle2 (nreverse cocycle2))
		  (mapc
		   #'(lambda (cons)
		       (declare (cons cons))
		       (when (evenp (car cons))
			 (setf (cdr cons)
			       (mod (- (cdr cons)
				       (cdr (assoc (1+ (car cons)) cocycle1)))
				    2))))
		   cocycle2)
		  (let ((head-absm (z2-cocycle-gbar (1- n) (1- dmns) cocycle2))
			(tail-absm (z2-cocycle-gbar n (1- dmns) cocycle1)))
		    (declare (type absm head-absm tail-absm))
		    (normalize-gbar
		     (cons dmns
			   (cons head-absm
				 (rest (unnormalize-gbar
					tail-absm
					(if (= n 2) 0 +null-gbar+)))))))))))))


(DEFUN Z2-COCYCLE-GBAR-HEAD (n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z/2Z
  (declare
   (fixnum n dmns)
   (list cocycle))
  (the absm
       (progn
	 (mapc
	  #'(lambda (cons)
	      (setf (cdr cons)
		    (if (evenp (cdr cons)) 0 1)))
	  cocycle)
	 (cond ((= n 1)
		(error "In Z2-COCYCLE-GBAR-HEAD, this point should not have been reached."))
	       ;; (z2-bar-absm (nreverse (mapcar #'cdr cocycle))))
	       ((= n dmns)
		(let ((value (cdr (first cocycle))))
		  (declare (fixnum value))
		  (if (zerop value)
		      (if (= n 2)
			  (absm 1 0)
			  (absm (mask (1- dmns)) +null-gbar+))
		      (absm 0 (z2-fundamental-gmsm (1- dmns) value)))))
	       (t
		(let ((cocycle1 +empty-list+)
		      (cocycle2 +empty-list+))
		  (declare (list cocycle1 cocycle2))
		  (dolist (cons cocycle)
		    (declare (cons cons))
		    (if (evenp (car cons))
			(push (cons (ash (car cons) -1) (cdr cons))
			      cocycle1)
			(push (cons (ash (car cons) -1) (cdr cons))
			      cocycle2)))
		  (setf cocycle1 (nreverse cocycle1)
			cocycle2 (nreverse cocycle2))
		  (mapc
		   #'(lambda (cons)
		       (declare (cons cons))
		       (when (evenp (car cons))
			 (setf (cdr cons)
			       (mod (- (cdr cons)
				       (cdr (assoc (1+ (car cons)) cocycle1)))
				    2))))
		   cocycle2)
		  (z2-cocycle-gbar (1- n) (1- dmns) cocycle2)))))))


(DEFUN K-Z-FUNDAMENTAL-CLASS (n)
  (declare (fixnum n))
  (the morphism
       (build-mrph
	:sorc (k-z n) :trgt (z-chcm) :degr (- n)
	:intr #'(lambda (dmns gmsm)
		  (declare
		   (fixnum dmns)
		   (type gmsm gmsm))
		  (if (= dmns n)
		      (do ((gmsm gmsm (gmsm (first (gbar-list gmsm))))
			   (dmns dmns (1- dmns)))
			  ((= 1 dmns)
			   (term-cmbn 0 (first gmsm) :z-gnrt))
			(declare
			 (type gmsm gmsm)
			 (fixnum dmns)))
		      (zero-cmbn (- dmns n))))
	:strt :gnrt
	:orgn `(k-z-fundamental-class ,n))))


(DEFUN K-Z2-FUNDAMENTAL-CLASS (n)
  (declare (fixnum n))
  (the morphism
       (build-mrph
	:sorc (k-z2 n) :trgt (z-chcm) :degr (- n)
	:intr #'(lambda (dmns gmsm)
		  (declare
		   (fixnum dmns)
		   (ignore gmsm))
		  (if (= dmns n)
		      (term-cmbn 0 1 :z-gnrt)
		      (zero-cmbn (- dmns n))))
	:strt :gnrt
	:orgn `(k-z2-fundamental-class ,n))))

#|
(cat-init)
(setf c1 (k-z2-fundamental-class 1))
(? c1 1 1)
(? c1 2 2)
(setf c3 (k-z2-fundamental-class 3))
(? c3 3 (z2-fundamental-gmsm 3 1))
|#

