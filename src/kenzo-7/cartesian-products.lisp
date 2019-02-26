;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS
;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS
;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS

(IN-PACKAGE #:cat)

(PROVIDE "cartesian-products")


(DEFUN CRPR-PRINT (crpr stream depth)
  (declare
   (type crpr crpr)
   (stream stream)
   (ignore depth))
  (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
             (format stream "<CrPr ~A ~A ~A ~A>"
                     (hyphenize-list (dgop-int-ext dgop1)) gmsm1
                     (hyphenize-list (dgop-int-ext dgop2)) gmsm2)
             crpr))


(DEFUN EXTRACT-COMMON-DGOP (dgop1 dgop2)
  (declare (fixnum dgop1 dgop2))
  (let ((dgop (logand dgop1 dgop2)))
    (declare (fixnum dgop))
    (do ((indx (1- (integer-length dgop)) (1- indx)))
        ((minusp indx))
      (declare (fixnum indx))
      (when (logbitp indx dgop)
        (setf dgop1 (remove-bit dgop1 indx)
              dgop2 (remove-bit dgop2 indx))))
    (values dgop dgop1 dgop2)))


(DEFUN 2ABSM-ACRPR (absm1 absm2)
  (declare (type absm absm1 absm2))
  (the absm
       (with-absm (dgop1 gmsm1) absm1
                  (with-absm (dgop2 gmsm2) absm2
                             (multiple-value-bind (dgop dgop11 dgop22)
                                 (extract-common-dgop dgop1 dgop2)
                               (declare (fixnum dgop dgop11 dgop22))
                               (absm dgop
                                     (crpr dgop11 gmsm1 dgop22 gmsm2)))))))


#|
(DEFUN CRTS-PRDC-CMPR (cmpr1 cmpr2)
  (declare (type cmprf cmpr1 cmpr2))
  (flet ((rslt (crpr1 crpr2)
           (declare (type crpr crpr1 crpr2))
           (the cmpr
                (let ((left-cons-1 (cadr crpr1))
                      (left-cons-2 (cadr crpr2)))
                  (declare (cons left-cons-1 left-cons-2))
                  (lexico
                   (f-cmpr (car left-cons-1) (car left-cons-2))
                   (let ((right-cons-1 (cddr crpr1))
                         (right-cons-2 (cddr crpr2)))
                     (declare (cons right-cons-1 right-cons-2))
                     (lexico
                      (f-cmpr (car right-cons-1) (car right-cons-2))
                      (funcall cmpr1 (cdr left-cons-1) (cdr left-cons-2))
                      (funcall cmpr2 (cdr right-cons-1) (cdr right-cons-2)))))))))
    (the cmprf #'rslt)))
|#

(DEFUN CRTS-PRDC-CMPR (cmpr1 cmpr2)
  (declare (type cmprf cmpr1 cmpr2))
  (flet ((rslt (crpr1 crpr2)
           (declare (type crpr crpr1 crpr2))
           (the cmpr
                (lexico
                 (f-cmpr (dgop1 crpr1) (dgop1 crpr2))
                 (f-cmpr (dgop2 crpr1) (dgop2 crpr2))
                 (funcall cmpr1 (gmsm1 crpr1) (gmsm1 crpr2))
                 (funcall cmpr2 (gmsm2 crpr1) (gmsm2 crpr2))))))
    (the cmprf #'rslt)))


(DEFUN CRTS-PRDC-BASIS (basis1 basis2)
  (declare (type basis basis1 basis2))
  (when (or (eq basis1 :locally-effective)
            (eq basis2 :locally-effective))
    (return-from crts-prdc-basis :locally-effective))
  (flet ((rslt (dmns)
           (declare (fixnum dmns))
           (the list
                (progn
                  (when (minusp dmns)
                    (return-from rslt +empty-list+))
                  (let ((array1 (make-array (1+ dmns)))
                        (array2 (make-array (1+ dmns)))
                        (rslt +empty-list+)
                        (mask (mask dmns)))
                    (declare
                     (simple-vector array1 array2)
                     (list rslt)
                     (fixnum mask))
                    (dotimes (i (1+ dmns))
                      (setf (svref array1 i) (funcall basis1 i)
                            (svref array2 i) (funcall basis2 i)))
                    (do ((dgop1 mask (1- dgop1)))
                        ((minusp dgop1))
                      (declare (fixnum dgop1))
                      (let ((dmns1 (- dmns (logcount dgop1))))
                        (declare (fixnum dmns1))
                        (do ((dgop2 mask (1- dgop2)))
                            ((minusp dgop2))
                          (declare (fixnum dgop2))
                          (unless (plusp (logand dgop1 dgop2))
                            (setf rslt
                                  (nconc
                                   (mapcan
                                    #'(lambda (item1)
                                        (declare (type gmsm item1))
                                        (mapcar
                                         #'(lambda (item2)
                                             (declare (type gmsm item2))
                                             (crpr dgop1 item1 dgop2 item2))
                                         (svref array2 (- dmns
                                                          (logcount dgop2)))))
                                    (svref array1 dmns1))
                                   rslt))))))
                    rslt)))))
    (the basis #'rslt)))


(DEFUN CRTS-PRDC-FACE (face1 face2)
  (declare (type face face1 face2))
  (flet ((rslt (indx dmns crpr)
           (declare
            (fixnum indx dmns)
            (type crpr crpr))
           (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                      (multiple-value-bind (dgop1 del1) (1dlop-dgop indx dgop1)
                        (declare
                         (fixnum dgop1)
                         (type (or null fixnum) del1))
                        (multiple-value-bind (dgop2 del2) (1dlop-dgop indx
                                                                      dgop2)
                          (declare
                           (fixnum dgop2)
                           (type (or null fixnum) del2))
                          (when del1
                            (let ((absm1 (funcall face1 del1 (- dmns (logcount
                                                                      dgop1))
                                                  gmsm1)))
                              (declare (type absm absm1))
                              (setf dgop1 (dgop*dgop dgop1 (dgop absm1))
                                    gmsm1 (gmsm absm1))))
                          (when del2
                            (let ((absm2 (funcall face2 del2 (- dmns (logcount
                                                                      dgop2))
                                                  gmsm2)))
                              (declare (type absm absm2))
                              (setf dgop2 (dgop*dgop dgop2 (dgop absm2))
                                    gmsm2 (gmsm absm2))))
                          (multiple-value-bind (dgop dgop1 dgop2)
                              (extract-common-dgop dgop1 dgop2)
                            (declare (fixnum dgop dgop1 dgop2))
                            (absm dgop (crpr dgop1 gmsm1 dgop2 gmsm2))))))))
    (the face #'rslt)))


(DEFUN CRTS-PRDC-FACE* (face1 face2)
  (declare (type face face1 face2))
  (flet ((rslt (indx dmns crpr)
           (declare
            (fixnum indx dmns)
            (type crpr crpr))
           (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                      (multiple-value-bind (dgop1 del1) (1dlop-dgop indx dgop1)
                        (declare
                         (fixnum dgop1)
                         (type (or null fixnum) del1))
                        (multiple-value-bind (dgop2 del2) (1dlop-dgop indx
                                                                      dgop2)
                          (declare
                           (fixnum dgop2)
                           (type (or null fixnum) del2))
                          (when del1
                            (let ((absm1 (funcall face1 del1 (- dmns (logcount
                                                                      dgop1))
                                                  gmsm1)))
                              (declare (type absm absm1))
                              (setf dgop1 (dgop*dgop dgop1 (dgop absm1))
                                    gmsm1 (gmsm absm1))))
                          (when del2
                            (let ((absm2 (funcall face2 del2 (- dmns (logcount
                                                                      dgop2))
                                                  gmsm2)))
                              (declare (type absm absm2))
                              (setf dgop2 (dgop*dgop dgop2 (dgop absm2))
                                    gmsm2 (gmsm absm2))))
                          (if (plusp (logand dgop1 dgop2))
                              :degenerate
                              (crpr dgop1 gmsm1 dgop2 gmsm2)))))))
    (the intr-mrph #'rslt)))


(DEFUN CRTS-PRDC (smst1 smst2)
  (declare (type simplicial-set smst1 smst2))
  (with-slots ((cmpr1 cmpr) (basis1 basis) (face1 face) (bspn1 bsgn)) smst1
    (declare
     (type cmprf cmpr1)
     (type basis basis1)
     (type face face1))
    (with-slots ((cmpr2 cmpr) (basis2 basis) (face2 face) (bspn2 bsgn)) smst2
      (declare
       (type cmprf cmpr2)
       (type basis basis2)
       (type face face2))
      (the simplicial-set
           (build-smst
            :cmpr (crts-prdc-cmpr cmpr1 cmpr2)
            :basis (crts-prdc-basis basis1 basis2)
            :bspn (crpr 0 bspn1 0 bspn2)
            :face (crts-prdc-face face1 face2)
            :face* (crts-prdc-face* face1 face2)
            :orgn `(crts-prdc ,smst1 ,smst2))))))
