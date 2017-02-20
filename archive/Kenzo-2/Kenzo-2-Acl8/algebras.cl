;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS
;;;  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS  ALGEBRAS

(IN-PACKAGE "CAT")

(PROVIDE "algebras")

(DEFMETHOD PRINT-OBJECT ((algb algebra) stream)
 (the algebra
   (progn
      (format stream "[K~D Algebra]" (idnm algb))
      algb)))

(DEFUN ALGEBRA (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already algebra dfnt)
  (the algebra
    (apply #'make-instance 'algebra rest)))

(DEFMETHOD SHARED-INITIALIZE
    ((algb algebra) slot-names &key aprd-intr aprd-strt)
  (declare
   (type (or (eql t) list) slot-names)
   (type intr aprd-intr)
   (type strt aprd-strt))
  (call-next-method)
  (when-slot aprd
             (with-slots (aprd) algb
               (declare (type morphism aprd))
               (when aprd-intr
                 (unless aprd-strt
                   (error "In ALGEBRA, an APRD-INTR is given, but not its APRD-STRT."))
                 (setf aprd (morphism :sorc (tnsr-prdc algb algb) :trgt algb :degr 0
                                      :intr aprd-intr :strt aprd-strt
                                      :dfnt `(algebra-product ,algb))))))
  algb)

(DEFMETHOD PRINT-OBJECT ((hopf hopf-algebra) stream)
 (the hopf-algebra
   (progn
      (format stream "[K~D Hopf-Algebra]" (idnm hopf))
      hopf)))

(DEFUN HOPF-ALGEBRA (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already hopf-algebra dfnt)
  (the hopf-algebra
    (apply #'make-instance 'hopf-algebra rest)))

(DEFMETHOD SHARED-INITIALIZE ((hplg hopf-algebra) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (call-next-method))