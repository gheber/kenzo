;;;  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF
;;;  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF
;;;  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF  FIBRATIONS-DVF

(IN-PACKAGE #:cat-9)

(DEFUN TWISTED-EZ
    (twisted-product &aux
                     (base-space (sorc (second (orgn twisted-product))))
                     (fibre-space (trgt (second (orgn twisted-product)))))
  (declare (type simplicial-set twisted-product base-space fibre-space))
  (the reduction
    (chcm-vf-reduction twisted-product #'ez-vf
                       (tnsr-prdc base-space fibre-space)
                       #'ez-isof #'ez-isog)))

#|
(cat-init)
(setf s2 (sphere 2))
(setf cc2 (chml-clss s2 2))
(setf f2 (z2-whitehead s2 cc2))
(setf x2 (fibration-total f2))
(setf tez (twisted-ez x2))
(homology (bcc tez) 0 10)

(setf f2 (z-whitehead s2 cc2))
(setf x2 (fibration-total f2))
(setf tez (twisted-ez x2))
(homology (bcc tez) 0 10)
|#

(DEFUN TWISTED-PRODUCT-EFHM-BRDCT
    (twisted-product &aux
                     (twisted-ez (twisted-ez twisted-product))
                     (twisted-tnpr (bcc twisted-ez)))
  (declare (type simplicial-set twisted-product) (type chain-complex twisted-tnpr))
  (the effective-homology
    (let ((base-space (sorc (second (orgn twisted-product))))
          (fibre-space (trgt (second (orgn twisted-product)))))
      (declare (type simplicial-set base-space fibre-space))
      (let ((efhmb (efhm base-space))
            (efhmf (efhm fibre-space)))
        (declare (type effective-homology efhmb efhmf))
        (cond ((and (typep efhmb 'chain-complex) (typep efhmf 'chain-complex))
               twisted-tnpr)
              ((and (typep efhmb 'chain-complex) (typep efhmf 'reduction))
               (sbtr twisted-tnpr
                     (tnsr-prdc base-space efhmf)))
              ((and (typep efhmb 'reduction) (typep efhmf 'chain-complex))
               (setf efhmf (trivial-rdct efhmf))
               (sbtr twisted-tnpr (tnsr-prdc efhmb efhmf)))
              ((and (typep efhmb 'reduction) (typep efhmf 'reduction))
               (sbtr twisted-tnpr
                     (tnsr-prdc efhmb efhmf)))
              ((and (typep efhmb 'equivalence) (typep efhmf 'reduction))
               (setf efhmf
                 (build-hmeq :lrdct (trivial-rdct fibre-space)
                             :rrdct efhmf))
               (sbtr twisted-tnpr
                     (tnsr-prdc efhmb efhmf)))
              ((and (typep efhmb 'equivalence) (typep efhmf 'chain-complex))
               (setf efhmf (trivial-hmeq efhmf))
               (sbtr twisted-tnpr
                     (tnsr-prdc efhmb efhmf)))
              (t (error "Case of TWISTED-PRODUCT-EFHM-BRDCT not yet implemented.")))))))

(DEFUN TWISTED-PRODUCT-EFHM
    (twisted-product &aux
                     (twisted-ez (twisted-ez twisted-product))
                     (twisted-product-efhm-brdct
                      (twisted-product-efhm-brdct twisted-product)))
  (declare (type reduction twisted-ez)
           (type effective-homology twisted-product-efhm-brdct))
  (the effective-homology
    (etypecase twisted-product-efhm-brdct
      (chain-complex twisted-ez)
      (reduction (cmps twisted-product-efhm-brdct twisted-ez)))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf cc3 (chml-clss s3 3))
(setf f3 (z-whitehead s3 cc3))
(setf x4 (fibration-total f3))
(efhm x4)
(homology x4 0 8)
(setf cc4 (chml-clss x4 4))
(setf f4 (z2-whitehead x4 cc4))
(setf x5 (fibration-total f4))
(efhm x5)
(homology x5 5)
(setf cc5 (chml-clss x5 5))
(setf f5 (z2-whitehead x5 cc5))
(setf x6 (fibration-total f5))
(efhm x6)
(homology x6 6)
|#

(DEFMETHOD SEARCH-EFHM (twisted-product (orgn (eql 'fibration-total)))
  (declare (type simplicial-set twisted-product))
  (the effective-homology
    (twisted-product-efhm twisted-product)))
