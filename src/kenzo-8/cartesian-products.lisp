;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS
;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS
;;;  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS  CARTESIAN-PRODUCTS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "cartesian-products")

(DEFMETHOD PRINT-OBJECT ((crpr crpr) stream)
  (declare (type stream stream))
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

#|
  (dotimes (i 100)
     (dotimes (j 100)
        (multiple-value-bind (dgop dgop1 dgop2) (extract-common-dgop i j)
           (unless (and (= i (dgop*dgop dgop dgop1))
                        (= j (dgop*dgop dgop dgop2)))
              (error "i = ~D, j = ~D, dgop = ~D, dgop1 = ~D, dgop2 = ~D"
                 i j dgop dgop1 dgop2)))))
|#

(DEFUN 2ABSM-ACRPR (absm1 absm2)
   (declare (type absm absm1 absm2))
   (the absm
      (with-absm (dgop1 gmsm1) absm1
      (with-absm (dgop2 gmsm2) absm2
         (multiple-value-bind (dgop dgop11 dgop22) (extract-common-dgop dgop1 dgop2)
            (declare (fixnum dgop dgop11 dgop22))
            (absm dgop
               (crpr dgop11 gmsm1 dgop22 gmsm2)))))))

#|
  (2absm-acrpr (absm 5 'a) (absm 3 'b))
|#


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


#|
()
(setf c (cmpr (delta-infinity)))
(setf rslt (crts-prdc-cmpr c c))
(funcall rslt (crpr 0 3 0 3) (crpr 1 1 0 3))
(funcall rslt (crpr 4 3 0 7) (crpr 3 1 0 7))
(funcall rslt (crpr 0 3 0 3) (crpr 0 3 1 1))
(funcall rslt (crpr 0 3 0 3) (crpr 0 5 0 3))
(funcall rslt (crpr 0 3 0 3) (crpr 0 3 0 5))
(funcall rslt (crpr 0 3 0 3) (crpr 0 3 0 3)))
|#
           
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
                                                      (svref array2 (- dmns (logcount dgop2)))))
                                              (svref array1 dmns1))
                                           rslt))))))
                      rslt)))))
          (the basis #'rslt)))

#|
  (setf b (basis (delta 1)))
  (setf r (crts-prdc-basis b b))
  (funcall r 0)
  (funcall r 1)
  (funcall r 2)
  (funcall r 3)
  (setf d3 (basis (delta 3)))
  (setf r (crts-prdc-basis d3 d3))
  (time (dotimes (i 7)
           (print (length (funcall r i)))))
  (setf s3 (basis (sphere 3)))
  (setf p (crts-prdc-basis s3 s3))
  (dotimes (i 8)
     (print (funcall p i)))
|#

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
                (multiple-value-bind (dgop2 del2) (1dlop-dgop indx dgop2)
                   (declare
                      (fixnum dgop2)
                      (type (or null fixnum) del2))
                   (when del1
                      (let ((absm1 (funcall face1 del1 (- dmns (logcount dgop1)) gmsm1)))
                         (declare (type absm absm1))
                         (setf dgop1 (dgop*dgop dgop1 (dgop absm1))
                               gmsm1 (gmsm absm1))))
                   (when del2
                      (let ((absm2 (funcall face2 del2 (- dmns (logcount dgop2)) gmsm2)))
                         (declare (type absm absm2))
                         (setf dgop2 (dgop*dgop dgop2 (dgop absm2))
                               gmsm2 (gmsm absm2))))
                   (multiple-value-bind (dgop dgop1 dgop2) (extract-common-dgop dgop1 dgop2)
                            (declare (fixnum dgop dgop1 dgop2))
                            (absm dgop (crpr dgop1 gmsm1 dgop2 gmsm2))))))))
         (the face #'rslt)))

#|
  (setf d2 (delta 2))
  (setf b2 (basis d2))
  (setf f2 (face d2))
  (setf b (crts-prdc-basis b2 b2))
  (setf r (crts-prdc-face f2 f2))
  (dotimes (i 5)
     (unless (zerop i)
        (dolist (item (funcall b i))
           (dotimes (j (1+ i))
              (format t "~%del-~D ~A = ~A"
                 j item (funcall r j i item))))))
|#

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
                (multiple-value-bind (dgop2 del2) (1dlop-dgop indx dgop2)
                   (declare
                      (fixnum dgop2)
                      (type (or null fixnum) del2))
                   (when del1
                      (let ((absm1 (funcall face1 del1 (- dmns (logcount dgop1)) gmsm1)))
                         (declare (type absm absm1))
                         (setf dgop1 (dgop*dgop dgop1 (dgop absm1))
                               gmsm1 (gmsm absm1))))
                   (when del2
                      (let ((absm2 (funcall face2 del2 (- dmns (logcount dgop2)) gmsm2)))
                         (declare (type absm absm2))
                         (setf dgop2 (dgop*dgop dgop2 (dgop absm2))
                               gmsm2 (gmsm absm2))))
                   (if (plusp (logand dgop1 dgop2))
                      :degenerate
                      (crpr dgop1 gmsm1 dgop2 gmsm2)))))))
         (the intr-mrph #'rslt)))

#|
  (setf d2 (delta 2))
  (setf b2 (basis d2))
  (setf f2 (face d2))
  (setf b (crts-prdc-basis b2 b2))
  (setf r (crts-prdc-face* f2 f2))
  (dotimes (i 5)
     (unless (zerop i)
        (dolist (item (coerce (funcall b i) 'list))
           (dotimes (j (1+ i))
              (format t "~%del-~D ~A = ~A"
                 j item (funcall r j i item))))))
  (setf s3 (sphere 3))
  (setf b3 (basis s3))
  (setf f3 (face s3))
  (setf b (crts-prdc-basis b3 b3))
  (setf r (crts-prdc-face* f3 f3))
  (dotimes (i 7)
     (unless (zerop i)
        (dolist (item (coerce (funcall b i) 'list))
           (dotimes (j (1+ i))
              (format t "~%del-~D ~A = ~A"
                 j item (funcall r j i item))))))
|#

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

#|
  (setf p (crts-prdc (delta 3) (delta 3)))
  (? p 0 (crpr 0 1 0 2))
  (? p 1 (crpr 0 3 0 3))
  (? p 2 (crpr 0 7 0 7))
  (? p 3 (crpr 0 15 0 15))
  (? p 4 (crpr 1 15 2 15))
|#

#|
  ;; For comparison with EAT.
  ;; In CAT.
  (setf s5 (sphere 5))
  (setf p (crts-prdc s5 s5))
  (setf b (basis p))
  (setf d (bndr p))
  (setf basis (funcall b 10))
  (length basis)
  (setf c (make-cmbn :degr 10 :list (mapcar #'(lambda (item)
                                                (term (1+ (random 5)) item))
                                      basis)))
  (setf +too-much-time+ -1)
  (cmbn-? d (cmbn-? d c))
  (time (dotimes (i 5) (cmbn-? d (cmbn-? d c))))
|#

#|
  ;; In EAT.
  (setf s5 (sphere 5))
  (setf p (cpr-2ss s5 s5))
  (setf d (cc-d (ss-cc p)))
  (setf basis (sbs p 10))
  (length basis)
  (setf c (make-cmb :dgr 10 :lst (mapcar #'(lambda (item)
                                                (mnm (1+ (random 5)) item))
                                     basis)))
  (??? d (??? d c))
  (time (dotimes (i 5) (??? d (??? d c))))
|#
