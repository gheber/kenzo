
(in-package :kenzo-test)

(def-suite :kenzo)

(defparameter *n* -1)

(defun ff (degr i)
  (declare (special *n*))
  (do ((2*n* (ash *n* 1))
       (rslt cat:+empty-list+
             (cons (cons (let ((cffc (- (random 2*n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat:make-cmbn :degr degr :list rslt))))


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

(defun cdelta1 (dmns)
  (cat:build-chcm
   :cmpr #'cat:l-cmpr
   :basis #'(lambda (n)
	      (mapcar #'cat:dlop-int-ext (funcall (cat:delta-n-basis dmns) n)))
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

(defun make-f1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 bdmns) :degr 0
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


(defun make-g1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 bdmns) :trgt (cdelta1 tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 tdmns) :degr +1
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


(defun make-rdct1 (tdmns bdmns)
  (let ((rdct (cat:build-rdct
	      :f (make-f1 tdmns bdmns)
	      :g (make-g1 tdmns bdmns)
	      :h (make-h1 tdmns bdmns)
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


(defun random-abar (tot-degr~ max-degr)
  (do ((rslt nil)
       (cum-degr 0 (+ cum-degr degr 1))
       (degr))
      ((>= cum-degr tot-degr~) (cat:make-abar :list rslt))
    (setf degr (1+ (random max-degr)))
    (push (cat:brgn (1+ degr)
		(let ((list (make-list degr)))
		  (mapl
		   #'(lambda (sublist)
		       (setf (car sublist) (- (random 21) 10)))
		   list)
		  list))
	  rslt)))
