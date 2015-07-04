
(in-package :kenzo-test)

(in-suite :kenzo)

(defun cdelta (dmns)
  (the cat:chain-complex
       (cat:build-chcm
	:cmpr #'cat:l-cmpr
	:basis :locally-effective
	:bsgn '(0)
	:intr-dffr #'(lambda (degr gmsm)
		       (cat:make-cmbn
			:degr (1- degr)
			:list (do ((rslt cat:+empty-list+
					 (cons (cons sign
						     (append
						      (subseq gmsm 0 nark)
						      (subseq gmsm (1+ nark))))
					       rslt))
				   (sign 1 (- sign))
				   (nark 0 (1+ nark)))
				  ((> nark degr) rslt))))
	:strt :gnrt
	:orgn `(locally effective version of C_* delta ,dmns))))

(test cdelta
      (progn
	(cat:cat-init)
	(let* ((cc (cdelta 5))
	       (hmeq (cat:trivial-hmeq cc)))
	  (declare (ignore hmeq))
	  (signals simple-error (equal (cat:basis cc 0) cat:+empty-list+))
	  (signals simple-error (equal (cat:basis cc 1) cat:+empty-list+))
	  (signals simple-error (equal (cat:basis cc 2) cat:+empty-list+))
	  (signals simple-error (equal (cat:basis cc 3) cat:+empty-list+))
	  (signals simple-error (equal (cat:basis cc 4) cat:+empty-list+))
	  (signals simple-error (equal (cat:basis cc 5) cat:+empty-list+)))))


(defun make-f (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
	     (let ((pos (position-if #'(lambda (vertex)
					 (>= vertex bdmns)) gmsm)))
	       (if pos
		   (if (< pos degr)
		       (cat:zero-cmbn degr)
		       (cat:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
		   (cat:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))

(defun make-g (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))

(defun make-h (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
	     (let ((pos (position-if #'(lambda (vertex)
					 (>= vertex bdmns)) gmsm)))
	       (if pos
		   (if (member bdmns gmsm)
		       (cat:zero-cmbn (1+ degr))
		       (cat:cmbn (1+ degr) (cat:-1-expt-n pos)
				 (append (subseq gmsm 0 pos) (list bdmns)
					 (subseq gmsm pos))))
		   (cat:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))

(defun make-rdct (tdmns bdmns)
  (let ((rdct (cat:build-rdct
	      :f (make-f tdmns bdmns)
	      :g (make-g tdmns bdmns)
	      :h (make-h tdmns bdmns)
	      :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))


(defun check-rdct ()
  (dolist (phi '(cat:*tdd* cat:*bdd* cat:*df-fd* cat:*dg-gd* cat:*id-fg*
		 cat:*id-gf-dh-hd* cat:*hh* cat:*fh* cat:*hg*))
    (declare (type symbol phi))
    (is (cat:cmbn-zero-p
	 (cat:cmbn-? (eval phi)
		     (if (member phi '(cat:*bdd* cat:*dg-gd* cat:*id-fg*
				       cat:*dg-gd* cat:*hg*))
			 cat:*bc*
			 cat:*tc*))))))

(test make-rdct
      (progn
	(cat:cat-init)
	(let ((rdct (make-rdct 6 3)))
	  (is (equal (cat:orgn rdct) '(REDUCTION DELTA 6 3)))
	  (cat:pre-check-rdct rdct)
	  (setf cat:*tc* (cat:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
				   1000 '(2 3 4)))
	  (setf cat:*bc* (cat:cmbn 3 4 '(0 1 2 3)))
	  (check-rdct))))

(test cmps
      (progn
	(cat:cat-init)
	(let* ((trdct (make-rdct 6 4))
	       (brdct (make-rdct 4 3))
	       (rdct (cat:cmps brdct trdct)))
	  (cat:pre-check-rdct rdct)
	  (setf cat:*tc* (cat:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
				    100 '(1 3 5) 10 '(1 4 5) 1 '(3 4 5)))
	  (setf cat:*bc* (cat:cmbn 2 1 '(0 1 3)))
	  (check-rdct))))

(test zero-mrph
      (progn
	(cat:cat-init)
	(let* ((rdct (make-rdct 6 3))
	       (perturb (cat:zero-mrph (cdelta 6) (cdelta 6) -1))
	       (new-rdct (cat:add rdct perturb)))
	  (cat:pre-check-rdct new-rdct)
	  (setf cat:*tc* (cat:cmbn 2 3 '(0 1 2)))
	  (setf cat:*bc* (cat:cmbn 3 4 '(0 1 2 3)))
	  (check-rdct))))

(test opps
      (progn
	(cat:cat-init)
	(let* ((rdct (make-rdct 6 3))
	       (perturb (cat:opps (cat:dffr (cat:tcc rdct))))
	       (new-rdct (cat:add rdct perturb)))
	  (cat:pre-check-rdct new-rdct)
	  (setf cat:*tc* (cat:cmbn 2 3 '(0 1 2)))
	  (setf cat:*bc* (cat:cmbn 3 4 '(0 1 2 3)))
	  (check-rdct))))

;; an absurd reduction ; just to test bpl-*-sigma
(test bpl-*-sigma
      (progn
	(cat:cat-init)
	(let* ((tcc (cat:build-chcm
		     :cmpr #'cat:l-cmpr
		     :basis :locally-effective
		     :bsgn '(0)
		     :intr-dffr #'(lambda (degr gnrt)
				    (cat:cmbn (1- degr) 1 gnrt))
		     :strt :gnrt
		     :orgn '(test1)))
	       (rdct (cat:trivial-rdct tcc))
	       (perturb (cat:build-mrph
			 :sorc tcc :trgt tcc :degr -1
			 :intr #'(lambda (degr gnrt)
				   (if (zerop (first gnrt))
				       (cat:zero-cmbn (1- degr))
				       (cat:cmbn (1- degr) 1
						 (list (1- (first gnrt))
						       (1+ (second gnrt))))))
			 :strt :gnrt
			 :orgn '(test3)))
	       (new-rdct))
	  (setf (slot-value rdct 'cat:h)
		(cat:build-mrph
		 :sorc tcc :trgt tcc :degr +1
		 :intr #'(lambda (degr gnrt)
			   (cat:cmbn (1+ degr) 1 gnrt))
		 :strt :gnrt
		 :orgn '(test2)))
	  (setf new-rdct (cat:add rdct perturb))
	  (cat:gnrt-? (cat:dffr (cat:tcc new-rdct)) 3 '(3 5))
	  (cat:gnrt-? (cat:dffr (cat:bcc new-rdct)) 3 '(3 5)))))

(test trivial-hmeq
      (progn
	(cat:cat-init)
	(let ((hmeq (cat:trivial-hmeq (cdelta 4))))
	  (cat:add (cat:lrdct hmeq) (cat:opps (cat:dffr (cdelta 4))))
	  (setf hmeq (cat:add hmeq (cat:opps (cat:dffr (cdelta 4)))))
	  (cat:gnrt-? (cat:dffr (cat:rbcc hmeq)) 3 '(0 1 2 3)))))
