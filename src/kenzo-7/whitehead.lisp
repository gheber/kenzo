;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD
;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD
;;;  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD  WHITEHEAD

(IN-PACKAGE #:cat-7)

(provide "whitehead")

;;;
;;; Z
;;;

(DEFUN Z-WWHITEHEAD-SINTR (smst n chml-clss
                           &aux (face (face smst))
                             (k-z-n (k-z n))
                             (idnt (bspn k-z-n))
                             (efhm (efhm smst)))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss)
   (type face face)
   (type ab-simplicial-group k-z-n)
   (type gmsm idnt)
   (type homotopy-equivalence efhm))
  (setf chml-clss
        (i-cmps chml-clss (rf efhm) (lg efhm)))
  (flet ((rslt (dmns gmsm)
           (declare
            (fixnum dmns)
            (type gmsm gmsm))
           (if (< dmns n)
               (absm (mask dmns) idnt)
               (z-cocycle-gbar n dmns
                               (gmsm-cocycle face n dmns gmsm chml-clss)))))
    (the sintr #'rslt)))


(DEFUN Z-WHITEHEAD-SINTR (smst n chml-clss
                          &aux (face (face smst))
                            (k-z-n-1 (k-z (1- n)))
                            (idnt (bspn k-z-n-1))
                            (efhm (efhm smst)))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss)
   (type face face)
   (type ab-simplicial-group k-z-n-1)
   (type gmsm idnt)
   (type homotopy-equivalence efhm))
  (setf chml-clss
        (i-cmps chml-clss (rf efhm) (lg efhm)))
  (flet ((rslt (dmns gmsm)
           (declare
            (fixnum dmns)
            (type gmsm gmsm))
           (if (< dmns n)
               (absm (mask (1- dmns)) idnt)
               (z-cocycle-gbar-head n dmns
                                    (gmsm-cocycle face n dmns gmsm chml-clss)))))
    (the sintr #'rslt)))


(DEFUN Z-WHITEHEAD (smst chml-clss &aux (n (- (degr chml-clss))))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss))
  (the fibration
       (build-smmr
        :sorc smst :trgt (k-z (1- n)) :degr -1
        :sintr (z-whitehead-sintr smst n chml-clss)
        :orgn `(z-whitehead ,smst))))

;;;
;;; Z/2Z
;;;

(DEFUN Z2-WWHITEHEAD-SINTR (smst n chml-clss
                            &aux (face (face smst))
                              (k-z2-n (k-z2 n))
                              (idnt (bspn k-z2-n))
                              (efhm (efhm smst)))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss)
   (type face face)
   (type ab-simplicial-group k-z2-n)
   (type gmsm idnt)
   (type homotopy-equivalence efhm))
  (setf chml-clss
        (i-cmps chml-clss (rf efhm) (lg efhm)))
  (flet ((rslt (dmns gmsm)
           (declare
            (fixnum dmns)
            (type gmsm gmsm))
           (if (< dmns n)
               (absm (mask dmns) idnt)
               (z2-cocycle-gbar n dmns
                                (gmsm-cocycle face n dmns gmsm chml-clss)))))
    (the sintr #'rslt)))


(DEFUN Z2-WHITEHEAD-SINTR (smst n chml-clss
                           &aux (face (face smst))
                             (k-z2-n-1 (k-z2 (1- n)))
                             (idnt (bspn k-z2-n-1))
                             (efhm (efhm smst)))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss)
   (type face face)
   (type ab-simplicial-group k-z2-n-1)
   (type gmsm idnt)
   (type homotopy-equivalence efhm))
  (setf chml-clss
        (i-cmps chml-clss (rf efhm) (lg efhm)))
  (flet ((rslt (dmns gmsm)
           (declare
            (fixnum dmns)
            (type gmsm gmsm))
           (if (< dmns n)
               (absm (mask (1- dmns)) idnt)
               (z2-cocycle-gbar-head n dmns
                                     (gmsm-cocycle face n dmns gmsm
                                                   chml-clss)))))
    (the sintr #'rslt)))


(DEFUN Z2-WHITEHEAD (smst chml-clss &aux (n (- (degr chml-clss))))
  (declare
   (type simplicial-set smst)
   (fixnum n)
   (type morphism chml-clss))
  (the fibration
       (build-smmr
        :sorc smst :trgt (k-z2 (1- n)) :degr -1
        :sintr (Z2-whitehead-sintr smst n chml-clss)
        :orgn `(Z2-whitehead ,smst))))
