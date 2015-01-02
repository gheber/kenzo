
(in-package :kenzo-test)

(in-suite :kenzo)

(defun cdelta (dmns)
  (cat:build-chcm
   :cmpr #'cat:l-cmpr
   :basis :locally-effective
   :bsgn '(0)
   :intr-dffr #'(lambda (degr gmsm)
		  (cat:make-cmbn
		   :degr (1- degr)
		   :list (do ((rslt cat:+empty-list+
				    (cons (cons sign (append
						      (subseq gmsm 0 nark)
						      (subseq gmsm (1+ nark))))
					  rslt))
			      (sign 1 (- sign))
			      (nark 0 (1+ nark)))
			     ((> nark degr) rslt))))
   :strt :gnrt
   :orgn `(locally effective version of C_* delta ,dmns)))

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

(test cdelta
      (progn
	(cat:cat-init)
	(let* ((rdct (make-rdct 6 3))
	       (f (cat:f rdct))
	       (g (cat:g rdct))
	       (h (cat:h rdct))
	       (tcc (cat:tcc rdct))
	       (bcc (cat:bcc rdct))
	       (dh (cat:cmps (cat:dffr tcc) h))
	       (hd (cat:cmps h (cat:dffr tcc)))
	       (gf (cat:cmps g f))
	       (id-gf-dh-hd (cat:i-sbtr (cat:idnt-mrph tcc) gf dh hd))
	       (c (cat:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
			    1000 '(2 3 4)))
	       (comb (cat:cmbn-? id-gf-dh-hd c)))

	  )

	))
