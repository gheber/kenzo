;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS

(IN-PACKAGE "CAT")

(PROVIDE "coalgebras")

(DEFMETHOD PRINT-OBJECT ((clgb coalgebra) stream)
 (the coalgebra
   (progn
      (format stream "[K~D Coalgebra]" (idnm clgb))
      clgb)))

(DEFUN COALGEBRA (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already coalgebra dfnt)
  (the coalgebra
    (apply #'make-instance 'coalgebra rest)))

(DEFMETHOD SHARED-INITIALIZE ((clgb coalgebra) slot-names &key cprd-intr cprd-strt)
  (declare
   (type (or (eql t) list) slot-names)
   (type intr cprd-intr)
   (type strt cprd-strt))
  (call-next-method)
  (when-slot cprd
             (with-slots (cprd) clgb
               (declare (type morphism cprd))
               (when cprd-intr
                 (unless cprd-strt
                   (error "IN COALGEBRA, a CPRD-INTR is given, but not its CPRD-STRT."))
                 (setf cprd (morphism :sorc clgb :trgt (tnsr-prdc clgb clgb) :degr 0
                                      :intr cprd-intr :strt cprd-strt
                                      :dfnt `(coalgebra-coproduct ,clgb))))))
  clgb)

(DEFUN CHANGE-CHCM-TO-CLGB (chcm &key cprd-intr cprd-strt dfnt)
  (declare
   (type chain-complex chcm)
   (type intr cprd-intr)
   (type strt cprd-strt)
   (list dfnt))
  (the coalgebra
    (progn
      (change-class chcm 'coalgebra)
      (setf dfnt (list (dfnt chcm) 'then dfnt))
      (let ((already (find dfnt *k-list* :key #'dfnt :test #'equal)))
        (declare (type (or null coalgebra) already))
        (when already
          (return-from change-chcm-to-clgb already)))
      (setf (slot-value chcm 'cprd) (morphism
                                     :sorc chcm :trgt (tnsr-prdc chcm chcm)
                                     :degr 0
                                     :intr cprd-intr :strt cprd-strt
                                     :dfnt dfnt))
      chcm)))

