;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS

(IN-PACKAGE #:cat-8)

(PROVIDE "coalgebras")

(DEFVAR *CLGB-LIST*)
(SETF *CLGB-LIST* +empty-list+)
(PUSHNEW '*CLGB-LIST* *list-list*)

#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((clgb coalgebra) stream)
  (declare (type stream stream))
  (the coalgebra
    (progn
      (format stream "[K~D Coalgebra]" (idnm clgb))
      clgb)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFUN CLGB (idnm)
   (declare (fixnum idnm))
   (the (or coalgebra null)
      (find idnm *clgb-list* :key #'idnm)))

(DEFUN BUILD-CLGB
    (&key cmpr basis bsgn intr-dffr dffr-strt intr-cprd cprd-strt orgn)
   (declare
      (type cmprf cmpr)
      (type intr-mrph intr-dffr intr-cprd)
      (type basis basis)
      (type gnrt bsgn)
      (type strt dffr-strt cprd-strt)
      (list orgn))
   (the coalgebra
      (progn
         (let ((already (find orgn *clgb-list* :key #'orgn :test #'equal)))
            (declare (type (or null coalgebra) already))
            (when already
               (return-from build-clgb already)))
         (let ((rslt (build-chcm :cmpr cmpr :basis basis :bsgn bsgn
                        :intr-dffr intr-dffr :strt dffr-strt
                        :orgn orgn)))
            (declare (type chain-complex rslt))
            (change-class rslt 'coalgebra)
            (setf (slot-value rslt 'cprd)
                  (build-mrph :sorc rslt :trgt (tnsr-prdc rslt rslt) :degr 0
                        :intr intr-cprd :strt cprd-strt
                        :orgn `(coalgebra-coproduct ,rslt)))
               (push rslt *clgb-list*)
               rslt))))

(DEFUN CHANGE-CHCM-TO-CLGB (chcm &key intr-cprd cprd-strt orgn)
   (declare
      (type chain-complex chcm)
      (type intr-mrph intr-cprd)
      (type strt cprd-strt)
      (list orgn))
   (the coalgebra
      (progn
         (change-class chcm 'coalgebra)
         (setf orgn (list (orgn chcm) 'then orgn))
         (let ((already (find orgn *clgb-list* :key #'orgn :test #'equal)))
            (declare (type (or null coalgebra) already))
            (when already
               (return-from change-chcm-to-clgb already)))
         (setf (slot-value chcm 'cprd) (build-mrph
                                          :sorc chcm :trgt (tnsr-prdc chcm chcm)
                                          :degr 0
                                          :intr intr-cprd :strt cprd-strt
                                          :orgn orgn))
         (push chcm *clgb-list*)
         chcm)))

