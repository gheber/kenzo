;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY

(IN-PACKAGE #:cat)

(PROVIDE "searching-homology")

(DEFUN ECHCM (chcm)
  (rbcc (efhm chcm)))


(DEFGENERIC HOMOLOGY (chcm degr1 &optional degr2))


(DEFMETHOD HOMOLOGY ((chcm chain-complex) degr1 &optional (degr2 (1+ degr1)))
  (declare (fixnum degr1 degr2))
  (do ((degr degr1 (1+ degr)))
      ((>= degr degr2))
    (declare (fixnum degr))
    (chcm-homology (echcm chcm) degr)
    (terpri) (clock) (terpri)))


(DEFGENERIC SEARCH-EFHM (chcm orgn))


(DEFMETHOD SEARCH-EFHM (chcm orgn)
  (declare (ignore chcm orgn))
  nil)


(DEFMETHOD SLOT-UNBOUND (class (chcm chain-complex) (slot-name (eql 'efhm)))
  (declare (ignore class))
  (the homotopy-equivalence
       (let ((efhm (search-efhm chcm (first (orgn chcm)))))
	 (setf (efhm chcm)
	       (or efhm
		   (when (functionp (basis chcm))
		     (trivial-hmeq chcm))
		   (error "I don't know how to determine ~
                         the effective homology of:~@
                         ~A (Origin: ~A)." chcm (orgn chcm)))))))
