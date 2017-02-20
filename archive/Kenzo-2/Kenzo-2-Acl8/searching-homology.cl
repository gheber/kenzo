;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY
;;;  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY  SEARCHING-HOMOLOGY

(IN-PACKAGE "CAT")

(PROVIDE "searching-homology")

(DEFUN ECHCM (chcm)
   (rbcc (efhm chcm)))

(DEFMETHOD HOMOLOGY ((chcm chain-complex) degr1 &optional (degr2 (1+ degr1)))
   (declare (fixnum degr1 degr2))
   (do ((degr degr1 (1+ degr)))
       ((>= degr degr2))
      (declare (fixnum degr))
      (chcm-homology (echcm chcm) degr)
      (terpri) (clock) (terpri)))

(DEFGENERIC SEARCH-EFHM (chcm dfnt))

(DEFMETHOD SEARCH-EFHM (chcm dfnt)
  (declare (ignore chcm dfnt))
  nil)

(DEFMETHOD SLOT-UNBOUND (class (chcm chain-complex) (slot-name (eql 'efhm)))
  (declare (ignore class))
  (the equivalence
    (let ((efhm (search-efhm chcm (first (dfnt chcm)))))
      (declare (type (or null equivalence) efhm))
      (setf (efhm chcm)
        (or efhm
            (when (functionp (basis chcm))
              (trivial-eqvl chcm))
            (error "I don't know how to determine ~
                    the effective homology of:~@
                    ~A (Origin: ~A)." chcm (dfnt chcm)))))))

#|
  (cat-init)
  (setf d (delta 3))
  (homology d 0)
|#
