;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  EILENBERG-ZILBER  EILENBERG-ZILBER  EILENBERG-ZILBER
;;;  EILENBERG-ZILBER  EILENBERG-ZILBER  EILENBERG-ZILBER
;;;  EILENBERG-ZILBER  EILENBERG-ZILBER  EILENBERG-ZILBER

(IN-PACKAGE #:cat-7)

(PROVIDE "eilenberg-zilber")

(DEFUN SHUFFLE-SIGN (dgop)
  (declare (fixnum dgop))
  (the fixnum
       (do ((rslt +1)
            (bit-position (- (integer-length dgop) 2) (1- bit-position))
            (factor -1))
           ((minusp bit-position) rslt)
         (declare (fixnum rslt bit-position factor))
         (if (logbitp bit-position dgop)
             (setf factor (- factor))
             (setf rslt (* rslt factor))))))


(DEFUN INTR-EML (cmbn)
  (declare (type cmbn cmbn))
  (when (cmbn-zero-p cmbn)
    (return-from intr-eml cmbn))
  (the cmbn
       (with-cmbn
           (degr list) cmbn
           (let* ((limits (make-array (+ degr 2)))
                  (rslt (list nil))
                  (rslt-mark rslt)
                  (mask (mask degr)))
             (declare
              (simple-vector limits)
              (list rslt rslt-mark)
              (fixnum mask))
             (do ((mark list (member i mark
                                     :test #'<
                                     :key #'(lambda (term)
                                              (declare (type term term))
                                              (degr1 (gnrt term)))))
                  (i 0 (1+ i)))
                 ((> i degr) (setf (svref limits i) nil))
               (declare
                (list mark)
                (fixnum i))
               (setf (svref limits i) mark))
             (dotimes (dgop1 (1+ mask))
               (declare (fixnum dgop1))
               (let* ((i (- degr (logcount dgop1)))
                      (dgop2 (- mask dgop1))
                      (sign (shuffle-sign dgop2)))
                 (declare (fixnum i dgop2 sign))
                 (do ((mark (svref limits i) (cdr mark))
                      (stop (svref limits (1+ i))))
                     ((eq mark stop))
                   (declare (list mark stop))
                   (with--term
                       (cffc tnpr) mark
                       (setf rslt-mark
                             (setf (cdr rslt-mark)
                                   (list (term
                                          (* sign cffc)
                                          (crpr
                                           dgop1 (gnrt1 tnpr)
                                           dgop2 (gnrt2 tnpr))))))))))
             (make-cmbn
              :degr degr
              :list (rest rslt))))))


(DEFUN EML (smst1 smst2)
  (build-mrph
   :sorc (tnsr-prdc smst1 smst2)
   :trgt (crts-prdc smst1 smst2)
   :degr 0
   :intr #'intr-eml
   :strt :cmbn
   :orgn `(eml ,smst1 ,smst2)))


(DEFUN INTR-PHI (smst1 smst2
                 &aux (crts-prdc (crts-prdc smst1 smst2))
                   (cmpr (cmpr crts-prdc))
                   (face1 (face smst1))
                   (face2 (face smst2)))
  (declare
   (type simplicial-set smst1 smst2 crts-prdc)
   (type cmprf cmpr)
   (type face face1 face2))
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (when (cmbn-zero-p cmbn)
             (return-from rslt (zero-cmbn (1+ (cmbn-degr cmbn)))))
           (the cmbn
                (let* ((n (cmbn-degr cmbn))
                       (n+1 (1+ n))
                       (array-pq (make-array (list (1+ n+1) (1+ n+1)))))
                  (declare
                   (fixnum n n+1)
                   (simple-array array-pq))
                  (do ((i 1 (1+ i)))
                      ((> i n+1))
                    (declare (fixnum i))
                    (do ((p i (1+ p))
                         (q n+1 (1- q)))
                        ((> p n+1))
                      (declare (fixnum p q))
                      (setf (aref array-pq p q) (zero-cmbn i))))
                  (dolist (term (cmbn-list cmbn))
                    (declare (type term term))
                    (with-term
                        (cffc crpr) term
                        (let ((absm1 (absm1 crpr))
                              (absm2 (absm2 crpr)))
                          (declare (type absm absm1 absm2))
                          (do ((n-alpha n (1- n-alpha))
                               (left-d absm1 (a-face4 face1 n-alpha n-alpha
                                                      left-d)))
                              ((zerop n-alpha))
                            (declare
                             (fixnum n-alpha)
                             (type absm left-d))
                            (do ((beta 0 (1+ beta))
                                 (i n-alpha (1- i))  ;; pseudo-degree
                                 (sign (-1-expt-n n-alpha) (- sign))
                                 (right-d absm2
                                          (a-face4 face2 (1- i) dim-right-d
                                                   right-d))
                                 (dim-right-d n (1- dim-right-d)))
                                ((= beta n-alpha))
                              (declare
                               (fixnum beta i sign dim-right-d)
                               (type absm right-d))
                              (let ((absm1 (1dgnr (1- i) left-d)))
                                (declare (type absm absm1))
                                (with-absm
                                    (dgop1 gmsm1) absm1
                                    (with-absm
                                        (dgop2 gmsm2) right-d
                                        (or (> (integer-length dgop1) i)
                                            (> (integer-length dgop2) i)
                                            (plusp (logand dgop1 dgop2))
                                            (dstr-add-term-to-cmbn
                                             cmpr
                                             (* sign cffc)
                                             (crpr dgop1 gmsm1 dgop2 gmsm2)
                                             (aref array-pq (1+ n-alpha)
                                                   (- n beta))))))))))))
                  ;;                 array-pq)))  ;; in case of debugging
                  (intr-phi-2 array-pq cmpr)
                  ))))
    (the intr-mrph #'rslt)))

#|

(DEFUN INTR-PHI-22 (array-pq cmpr)
  (declare
   (type (simple-array cmbn (* *)) array-pq)
   (type cmprf cmpr))
;   (format t "  -22-  ")		;       ;
  (the cmbn
       (let* ((n+1 (1- (array-dimension array-pq 0)))
              (n (1- n+1)))
         (declare (fixnum n n+1))
         (flet ((step-i (i)
                  (declare (fixnum i))
                  (do ((p (1+ i) (1+ p))
                       (p-1 i p)
                       (q n+1 q-1)
                       (q-1 n (1- q-1))
                       (sign t (not sign))
                       (ibit (2-exp i)))
                      ((> p n+1))
                    (declare
                     (fixnum p p-1 q q-1 ibit)
                     (type boolean sign))
                    (setf (aref array-pq p q)
                          (2cmbn-add cmpr
                                     (aref array-pq p q)
                                     (make-cmbn
                                      :degr (1+ i)
                                      :list (nconc
                                             (mapcar
                                              #'(lambda (term)
                                                  (declare (type term term))
                                                  (with-term (cffc crpr) term
                                                             (with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                                                                        (term cffc (crpr
                                                                                    dgop1 gmsm1
                                                                                    (+ ibit dgop2) gmsm2)))))
                                              (cmbn-list (aref array-pq p q-1)))
                                             (let ((list (cmbn-list (aref array-pq p-1 q))))
                                               (declare (list lst))
                                               (unless sign
                                                 (dolist (term list)
                                                   (declare (type term term))
                                                   (setf (cffc term) (- (cffc term)))))
                                               (dolist (term list)
                                                 (declare (type term term))
                                                 (incf (dgop1 (gnrt term)) ibit))
                                               list))))))))
           (do ((i 1 (1+ i)))
               ((= i n+1))
             (declare (type fixnum i))
             (step-i i))
           (aref array-pq n+1 n+1)))))
|#

(DEFUN INTR-PHI-2 (array-pq cmpr)
  (declare
   (type (simple-array cmbn (* *)) array-pq)
   (type cmprf cmpr))
                                        ;   (format t "  -2-  ")
  (the cmbn
       (let* ((n+1 (1- (array-dimension array-pq 0)))
              (n (1- n+1))
              (cmbn-list +empty-list+))
         (declare
          (fixnum n n+1)
          (list cmbn-list))
         (do ((i 1 (1+ i))
              (cmbn-i (list nil) (list nil))
              (mask (mask n) (ash mask -1)))
             ((= i n+1))
           (declare
            (fixnum i mask)
            (list cmbn-i))
           (let ((cmbn-i-tail cmbn-i))
             (declare (list cmbn-i-tail))
             (do ((dgop1 0 (1+ dgop1)))
                 ((> dgop1 mask)
                  (push (make-cmbn
                         :degr n+1
                         :list (rest cmbn-i))
                        cmbn-list))
               (declare (fixnum dgop1))
               (let ((p (- n+1 (logcount dgop1)))
                     (dgop2 (- mask dgop1)))
                 (declare (fixnum p dgop2))
                 (unless (= p i)
                   (let ((q (- (+ n+1 i) p))
                         (sign (shuffle-sign dgop2))
                         (dgop1-ashed (ash dgop1 i))
                         (dgop2-ashed (ash dgop2 i)))
                     (declare (fixnum q sign dgop1-ashed dgop2-ashed))
                     (dolist (term (cmbn-list (aref array-pq p q)))
                       (declare (type term term))
                       (with-term
                           (cffc crpr) term
                           (with-crpr
                               (dgop1 gmsm1 dgop2 gmsm2) crpr
                               (setf cmbn-i-tail
                                     (setf (cdr cmbn-i-tail)
                                           (list (term (* sign cffc)
                                                       (crpr
                                                        (+ dgop1 dgop1-ashed)
                                                        gmsm1
                                                        (+ dgop2 dgop2-ashed)
                                                        gmsm2))))))))))))))
         (if cmbn-list
             (apply #'ncmbn-add cmpr cmbn-list)
             (zero-cmbn n+1)))))


(DEFUN PHI (smst1 smst2)
  (declare (type simplicial-set smst1 smst2))
  (the morphism
       (build-mrph
        :sorc (crts-prdc smst1 smst2)
        :trgt (crts-prdc smst1 smst2)
        :degr +1
        :intr (intr-phi smst1 smst2)
        :strt :cmbn
        :orgn `(phi ,smst1 ,smst2))))


(DEFUN INTR-AW (face1 face2)
  (declare (type face face1 face2))
  (flet ((rslt (dmns crpr)
           (declare
            (fixnum dmns)
            (type crpr crpr))
           (the cmbn
                (let ((absm1 (absm1 crpr))
                      (absm2 (absm2 crpr))
                      (rslt +empty-list+)
                      (del-0-s +empty-list+))
                  (declare
                   (type absm absm1 absm2)
                   (list rslt del-0-s))
                  (do ((face absm2 (a-face4 face2 0 dmns2 face))
                       (dmns2 dmns (1- dmns2)))
                      ((zerop dmns2) (push face del-0-s))
                    (declare
                     (type absm face)
                     (fixnum dmns))
                    (push face del-0-s))
                  (do ((face absm1 (a-face4 face1 dmns1 dmns1 face))
                       (dmns1 dmns (1- dmns1))
                       (dmns2 0 (1+ dmns2))
                       (mark del-0-s (cdr mark)))
                      ((zerop dmns1)
                       (unless (degenerate-p absm2)
                         (push
                          (term 1
                                (tnpr 0 (gmsm face) dmns (gmsm absm2)))
                          rslt)))
                    (declare
                     (type absm face)
                     (fixnum dmns1 dmns2)
                     (list mark))
                    (unless (or (degenerate-p face)
                                (degenerate-p (car mark)))
                      (push
                       (term 1
                             (tnpr dmns1 (gmsm face) dmns2 (gmsm (car mark))))
                       rslt)))
                  (make-cmbn
                   :degr dmns
                   :list rslt)))))
    (the intr-mrph #'rslt)))


(DEFUN AW (smst1 smst2
           &aux (crts-prdc (crts-prdc smst1 smst2))
             (tnsr-prdc (tnsr-prdc smst1 smst2))
             (face1 (face smst1))
             (face2 (face smst2)))
  (declare
   (type simplicial-set smst1 smst2)
   (type face face1 face2))
  (the morphism
       (build-mrph
        :sorc crts-prdc
        :trgt tnsr-prdc
        :degr 0
        :intr (intr-aw face1 face2)
        :strt :gnrt
        :orgn `(aw ,smst1 ,smst2))))


(DEFUN EZ (smst1 smst2)
  (declare (type simplicial-set smst1 smst2))
  (the reduction
       (build-rdct
        :f (aw smst1 smst2)
        :g (eml smst1 smst2)
        :h (phi smst1 smst2)
        :orgn `(eilenberg-zilber ,smst1 ,smst2))))


(DEFUN LEFT-CRTS-PRDC-EFHM (smst1 smst2)
  (declare (type simplicial-set smst1 smst2))
  (the homotopy-equivalence
       (build-hmeq
        :lrdct (trivial-rdct (crts-prdc smst1 smst2))
        :rrdct (ez smst1 smst2))))


(DEFMETHOD SEARCH-EFHM (smst (orgn (eql 'crts-prdc)))
  (declare (type simplicial-set smst))
  (the homotopy-equivalence
       (cmps
        (left-crts-prdc-efhm (second (orgn smst)) (third (orgn smst)))
        (tnsr-prdc (efhm (second (orgn smst)))
                   (efhm (third (orgn smst)))))))
