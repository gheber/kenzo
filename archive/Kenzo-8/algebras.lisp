;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "algebras")

(DEFVAR *ALGB-LIST*)
(SETF *ALGB-LIST* +empty-list+)
(PUSHNEW '*ALGB-LIST* *list-list*)

#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((algb algebra) stream)
  (declare (type stream stream))
  (the algebra
    (progn
      (format stream "[K~D Algebra]" (idnm algb))
      algb)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFUN ALGB (idnm)
   (declare (fixnum idnm))
   (the (or algebra null)
      (find idnm *algb-list* :key #'idnm)))

(DEFUN BUILD-ALGB
    (&key cmpr basis bsgn intr-dffr dffr-strt intr-aprd aprd-strt orgn)
   (declare
      (type cmprf cmpr)
      (type intr-mrph intr-dffr intr-aprd)
      (type basis basis)
      (type gnrt bsgn)
      (type strt dffr-strt aprd-strt)
      (list orgn))
   (the algebra
      (progn
         (let ((already (find orgn *algb-list* :key #'orgn :test #'equal)))
            (declare (type (or null algebra) already))
            (when already
               (return-from build-algb already)))
         (let* ((rslt (build-chcm :cmpr cmpr :basis basis :bsgn bsgn
                        :intr-dffr intr-dffr :strt dffr-strt
                        :orgn orgn))
		;; to be done before change-class
		(rslt-rslt (tnsr-prdc rslt rslt)))
            (declare (type chain-complex rslt rslt-rslt))
            (change-class rslt 'algebra)  
            (setf (slot-value rslt 'aprd)
                  (build-mrph :sorc rslt-rslt :trgt rslt :degr 0
                        :intr intr-aprd :strt aprd-strt
                        :orgn `(algebra-product ,rslt)))
               (push rslt *algb-list*)
               rslt))))

(DEFUN CHANGE-CHCM-TO-ALGB (chcm &key intr-aprd aprd-strt orgn)
   (declare
      (type chain-complex chcm)
      (type intr-mrph intr-aprd)
      (type strt aprd-strt)
      (list orgn))
   (the algebra
     (let ((chcm-chcm (tnsr-prdc chcm chcm)))
       (declare (type chain-complex chcm-chcm))
       (change-class chcm 'algebra)
       (setf orgn (list (orgn chcm) 'then orgn))
       (let ((already (find orgn *algb-list* :key #'orgn :test #'equal)))
	 (declare (type (or null algebra) already))
	 (when already
               (return-from change-chcm-to-algb already)))
       (setf (slot-value chcm 'aprd) (build-mrph
				      :sorc chcm-chcm :trgt chcm
				      :degr 0
				      :intr intr-aprd :strt aprd-strt
				      :orgn orgn))
       (push chcm *algb-list*)
       chcm)))

(DEFVAR *HOPF-LIST*)
(SETF *HOPF-LIST* +empty-list+)
(PUSHNEW '*HOPF-LIST* *list-list*)

#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((hopf hopf-algebra) stream)
  (declare (type stream stream))
  (the hopf-algebra
    (progn
      (format stream "[K~D Hopf-Algebra]" (idnm hopf))
      hopf)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFUN HOPF (idnm)
   (declare (fixnum idnm))
   (the (or hopf-algebra null)
      (find idnm *hopf-list* :key #'idnm)))

