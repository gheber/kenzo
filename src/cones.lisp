;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES
;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES
;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES

(IN-PACKAGE #:cat)

(PROVIDE "cones")

(DEFUN CONE-PRINT (cone stream depth)
  (declare
   (type cone cone)
   (stream stream)
   (ignore depth))
  (the cone
       (with-cone (conx icon) cone
                  (format stream "<CONE ~D" conx)
                  (do ((mark icon))
                      ((not (typep mark 'cone))
                       (format stream " ~A>" mark))
                    (declare (type gnrt icon))
                    (with-cone (conx icon) mark
                               (format stream "-~D" conx)
                               (setf mark icon)))
                  cone)))


(DEFUN CONE-CMPR (cmpr0 cmpr1)
  (declare (type cmprf cmpr0 cmpr1))
  (the cmprf
       (flet ((rslt (gnrt1 gnrt2)
                (declare (type cone gnrt1 gnrt2))
                (the cmpr
                     (let ((ind1 (conx gnrt1))
                           (ind2 (conx gnrt2))
                           (gnrt11 (icon gnrt1))
                           (gnrt22 (icon gnrt2)))
                       (declare
                        (fixnum ind1 ind2)
                        (type gnrt gnrt11 gnrt22))
                       (ecase ind1
                         (0 (ecase ind2
                              (0 (funcall cmpr0 gnrt11 gnrt22))
                              (1 :less)))
                         (1 (ecase ind2
                              (0 :greater)
                              (1 (funcall cmpr1 gnrt11 gnrt22)))))))))
         (declare (ftype (function (cone cone) cmpr) rslt))
         #'rslt)))


(DEFUN CONE-BASIS (basis0 basis1)
  (declare (type basis basis0 basis1))
  (the basis
       (progn
         (when (or (eq basis0 :locally-effective)
                   (eq basis1 :locally-effective))
           (return-from cone-basis :locally-effective))
         (flet ((rslt (degr)
                  (declare (type fixnum degr))
                  (the list
                       (append
                        (mapcar #'(lambda (item) (con0 item))
                                (funcall basis0 degr))
                        (mapcar #'(lambda (item) (con1 item))
                                (funcall basis1 (1- degr)))))))
           (declare (ftype (function (fixnum) list) rslt))
           #'rslt))))


(DEFUN TERM-CON0 (term)
  (declare (type term term))
  (the term
       (term (cffc term)
             (con0 (gnrt term)))))


(DEFUN TERM-CON1 (term)
  (declare (type term term))
  (the term
       (term (cffc term)
             (con1 (gnrt term)))))


(DEFUN TERM-UNCON (term)
  (declare (type term term))
  (the term
       (term (cffc term) (icon (gnrt term)))))


(DEFUN CMBN-CON0 (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (make-cmbn
        :degr (cmbn-degr cmbn)
        :list (mapcar #'term-con0 (cmbn-list cmbn)))))


(DEFUN CMBN-CON1 (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (make-cmbn
        :degr (cmbn-degr cmbn)
        :list (mapcar #'term-con1 (cmbn-list cmbn)))))


(DEFUN CONE-CMBN-SPLIT (cmbn)
  (declare (type cmbn cmbn))
  (the (values cmbn cmbn)
       (with-cmbn (degr list) cmbn
                  (let ((list0 +empty-list+)
                        (list1 +empty-list+))
                    (declare (type list list0 list1))
                    (do ((mark list (cdr mark)))
                        ((endp mark)
                         (setf list0 (nreverse list0)
                               list1 +empty-list+))
                      (declare (type list mark))
                      (when (eql 1 (conx (cdar mark)))
                        (setf list0 (nreverse list0)
                              list1 (mapcar #'term-uncon mark))
                        (return))
                      (push (term-uncon (car mark)) list0))
                    (values (make-cmbn :degr degr :list list0)
                            (make-cmbn :degr (1- degr) :list list1))))))


(DEFUN CONE-2CMBN-APPEND (cmbn0 cmbn1)
  (declare (type cmbn cmbn0 cmbn1))
  (the cmbn
       (make-cmbn
        :degr (cmbn-degr cmbn0)
        :list (append (mapcar #'term-con0 (cmbn-list cmbn0))
                      (mapcar #'term-con1 (cmbn-list cmbn1))))))


(DEFUN CONE-2MRPH-DIAG-IMPL (mrph0 mrph1)
  (declare (type morphism mrph0 mrph1))
  (the intr-mrph
       (flet ((rslt (cmbn)
                (declare (type cmbn cmbn))
                (the cmbn
                     (multiple-value-bind (cmbn0 cmbn1)
                         (cone-cmbn-split cmbn)
                       (declare (type cmbn cmbn0 cmbn1))
                       (cone-2cmbn-append (? mrph0 cmbn0)
                                          (? mrph1 cmbn1))))))
         #'rslt)))


(DEFUN CONE-3MRPH-TRIANGLE-IMPL (cmpr0 mrph0 mrph1 phi)
  (declare (ignore cmpr0) (type morphism mrph0 mrph1 phi))
  (the intr-mrph
       (flet ((rslt (degr gnrt)
                (declare (type fixnum degr) (type gnrt gnrt))
                (the cmbn
                     (ecase (conx gnrt)
                       (0 (cmbn-con0 (? mrph0 degr (icon gnrt))))
                       (1
                        (let ((gnrt (icon gnrt)))
                          (declare (type gnrt gnrt))
                          (cone-2cmbn-append
                           (? phi (1- degr) gnrt)
                           (? mrph1 (1- degr) gnrt))))))))
         #'rslt)))


(DEFUN CONE (mrph)
  (declare (type morphism mrph))
  (the chain-complex
       (let ((chcm0 (trgt mrph))
             (chcm1 (sorc mrph)))
         (declare (type chain-complex chcm0 chcm1))
         (build-chcm
          :cmpr (cone-cmpr (cmpr chcm0) (cmpr chcm1))
          :basis (cone-basis (basis chcm0) (basis chcm1))
          :bsgn (con0 (bsgn chcm0))
          :intr-dffr (cone-3mrph-triangle-impl (cmpr chcm0)
                                               (dffr chcm0)
                                               (n-mrph -1 (dffr chcm1))
                                               mrph)
          :strt :gnrt
          :orgn `(cone ,mrph)))))


(DEFUN CONE-2MRPH-DIAG (sorc-cone trgt-cone mrph0 mrph1)
  (declare
   (type chain-complex sorc-cone trgt-cone)
   (type morphism mrph0 mrph1))
  (the morphism
       (progn
         (unless (= (degr mrph0) (degr mrph1))
           (error "Non-coherent degrees in CONE-2MRPH-DIAG."))
         (build-mrph
          :sorc sorc-cone
          :trgt trgt-cone
          :degr (degr mrph0)
          :intr (cone-2mrph-diag-impl mrph0 mrph1)
          :strt :cmbn
          :orgn `(cone-2mrph-diag ,sorc-cone ,trgt-cone ,mrph0 ,mrph1)))))


(DEFUN CONE-3MRPH-TRIANGLE (sorc-cone trgt-cone mrph0 mrph1 phi)
  (declare
   (type chain-complex sorc-cone trgt-cone)
   (type morphism mrph0 mrph1 phi))
  (the morphism
       (progn
         (unless (= (degr mrph0) (degr mrph1))
           (error "Non-coherent degrees in CONE-3MRPH-TRIANGLE."))
         (unless (= (1+ (degr mrph0)) (degr phi))
           (error "Non-coherent-degrees in CONE-3MRPH-TRIANGLE."))
         (build-mrph
          :sorc sorc-cone
          :trgt trgt-cone
          :degr (degr mrph0)
          :intr (cone-3mrph-triangle-impl
                 (cmpr (trgt phi))
                 mrph0 mrph1 phi)
          :strt :gnrt
          :orgn `(cone-3mrph-triangle ,sorc-cone ,trgt-cone
                                      ,mrph0 ,mrph1 ,phi)))))


(DEFUN CONE-EFHM (cone)
  (declare (type chain-complex cone))
  (the homotopy-equivalence
       (let* ((phi (second (orgn cone)))
              (chcm0 (trgt phi)) (chcm1 (sorc phi))
              (efhm0 (efhm chcm0)) (efhm1 (efhm chcm1))
              (lf0 (lf efhm0)) (lg0 (lg efhm0)) (lh0 (lh efhm0))
              (rf0 (rf efhm0)) (rg0 (rg efhm0)) (rh0 (rh efhm0))
              (lf1 (lf efhm1)) (lg1 (lg efhm1)) (lh1 (lh efhm1))
              (rf1 (rf efhm1)) (rg1 (rg efhm1)) (rh1 (rh efhm1))
              (hphi (cmps lg0 (cmps phi lf1)))
              (ephi (cmps rf0 (cmps hphi rg1)))
              (hcone (cone hphi))
              (econe (cone ephi))
              (LF (cone-2mrph-diag hcone cone lf0 lf1))
              (LG (cone-2mrph-diag cone hcone lg0 lg1))
              (LH (cone-2mrph-diag hcone hcone lh0 (n-mrph -1 lh1)))
              (RF (cone-3mrph-triangle hcone econe rf0 rf1
                                       (cmps rf0 (cmps hphi rh1))))
              (RG (cone-3mrph-triangle econe hcone rg0 rg1
                                       (n-mrph -1 (cmps rh0 (cmps hphi rg1)))))
              (RH (cone-3mrph-triangle hcone hcone rh0 (n-mrph -1 rh1)
                                       (cmps rh0 (cmps hphi rh1)))))
         (declare
          (type chain-complex chcm0 chcm1 hcone econe)
          (type morphism phi lf0 lg0 lh0 rf0 rg0 rh0 lf1 lg1 lh1 rf1 rg1 rh1
                hphi ephi LF LG LH RF RG RH))
         (build-hmeq
          :lrdct (build-rdct :f LF :g LG :h LH
                             :orgn `(cone-efhm ,cone lrdct))
          :rrdct (build-rdct :f RF :g RG :h RH
                             :orgn `(cone-efhm ,cone rrdct))
          :orgn `(cone-efhm ,cone)))))

(DEFMETHOD SEARCH-EFHM (chcm (orgn (eql 'cone)))
  (declare (type chain-complex chcm))
  (the homotopy-equivalence
       (cone-efhm chcm)))
