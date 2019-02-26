;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS
;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS
;;;  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS  CS-TWISTED-PRODUCTS

(IN-PACKAGE #:cat-7)

(PROVIDE "cs-twisted-products")

(DEFUN SMGR-FIBRATION-SINTR (dmns gbar)
  (declare
   (ignore dmns)
   (type gbar gbar))
  (the absm
       (first (gbar-list gbar))))

(DEFUN SMGR-FIBRATION (smgr)
  (declare (type simplicial-group smgr))
  (the fibration
       (build-smmr
        :sorc (classifying-space smgr)
        :trgt smgr
        :degr -1
        :sintr #'smgr-fibration-sintr
        :orgn `(simplicial-group-fibration ,smgr))))


(DEFUN SMGR-CRTS-CONTRACTION-INTR (idnt)
  (declare
   (type gmsm idnt))
  (flet ((rslt (degr crpr
                     &aux (degr+1 (1+ degr)))
           (declare
            (fixnum degr degr+1)
            (type crpr crpr))
           (the cmbn
                (with-crpr
                    (absm1 absm2) crpr
                    (if (= (dgop absm2) (mask degr))
                        (zero-cmbn degr+1)
                        (term-cmbn degr+1
                                   (-1-expt-n+1 degr)
                                   (crpr 0 (make-gbar
                                            :dmns degr+1
                                            :list (cons absm2
                                                        (rest
                                                         (unnormalize-gbar
                                                          absm1 idnt))))
                                         (mask degr+1) idnt)))))))
    (the intr-mrph #'rslt)))


(DEFUN SMGR-CRTS-CONTRACTION (smgr)
  (declare (type simplicial-group smgr))
  (the morphism
       (build-mrph
        :sorc (fibration-total (smgr-fibration smgr))
        :trgt (fibration-total (smgr-fibration smgr))
        :degr +1
        :intr (smgr-crts-contraction-intr (bspn smgr))
        :strt :gnrt
        :orgn `(smgr-crts-contraction ,smgr))))


(DEFUN SMGR-TNPR-CONTRACTION (smgr
                              &aux (fibration (smgr-fibration smgr))
                                (brown (brown-reduction fibration))
                                (f (f brown))
                                (g (g brown))
                                (chi (smgr-crts-contraction smgr)))
  (declare
   (type simplicial-group smgr)
   (type fibration fibration)
   (type reduction brown)
   (type morphism f g chi))
  (the morphism
       (i-cmps f chi g)))
