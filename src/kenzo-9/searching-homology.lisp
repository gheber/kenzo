;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY

(IN-PACKAGE #:cat-9)

(PROVIDE "searching-homology")

(DEFUN ECHCM (chcm)
  (declare (type chain-complex chcm))
  (the chain-complex
    (etypecase (efhm chcm)
      (chain-complex chcm)
      (reduction (bcc (efhm chcm)))
      (equivalence (rbcc (efhm chcm))))))

(DEFUN HOMOLOGY (chcm degr1 &optional (degr2 (1+ degr1)))
   (declare (type chain-complex chcm) (type fixnum degr1 degr2))
   (do ((degr degr1 (1+ degr)))
       ((>= degr degr2))
      (declare (type fixnum degr))
      (chcm-homology (echcm chcm) degr)
      (terpri) (clock) (terpri)))

(DEFGENERIC SEARCH-EFHM (chcm orgn))

(DEFMETHOD SEARCH-EFHM (chcm orgn)
  (declare (ignore chcm orgn))
  nil)

(DEFMETHOD SLOT-UNBOUND (class (chcm chain-complex) (slot-name (eql 'efhm)))
   (declare (ignore class))
   (the effective-homology
     (let ((efhm (search-efhm chcm (first (orgn chcm)))))
       (setf (efhm chcm)
	     (or efhm
		 (when (functionp (basis chcm))
		       chcm)
		 (error "I don't know how to determine ~
                         the effective homology of:~@
                         ~A (Origin: ~A)." chcm (orgn chcm)))))))

#|
  (cat-init)
  (setf d (delta 3))
  (homology d 0)
|#
