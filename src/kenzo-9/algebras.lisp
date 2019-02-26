;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS

(IN-PACKAGE #:cat-9)

(PROVIDE "algebras")

(DEFVAR *ALGB-LIST*)
(SETF *ALGB-LIST* +empty-list+)
(PUSHNEW '*ALGB-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((algb algebra) stream)
 (the algebra
   (progn
      (format stream "[K~D Algebra]" (idnm algb))
      algb)))

(DEFMETHOD PRINT-OBJECT ((algb ab-algebra) stream)
 (the algebra
   (progn
      (format stream "[K~D Abelian-Algebra]" (idnm algb))
      algb)))

(DEFUN ALGB (idnm)
   (declare (type fixnum idnm))
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

(DEFVAR *HOPF-LIST*)
(SETF *HOPF-LIST* +empty-list+)
(PUSHNEW '*HOPF-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((hopf hopf-algebra) stream)
 (the hopf-algebra
   (progn
      (format stream "[K~D Hopf-Algebra]" (idnm hopf))
      hopf)))

(DEFUN HOPF (idnm)
   (declare (type fixnum idnm))
   (the (or hopf-algebra null)
      (find idnm *hopf-list* :key #'idnm)))

