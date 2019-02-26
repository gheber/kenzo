;;;  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP
;;;  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP
;;;  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP  CHCM-ELEMENTARY-OP

(IN-PACKAGE #:cat-8)

(PROVIDE "chcm-elementary-op")

(DEFUN Z-CHCM ()
   (the chain-complex
      (build-chcm
         :cmpr #'(lambda (gnrt1 gnrt2)
                    (declare (ignore gnrt1 gnrt2))
                    (the cmpr :equal))
         :basis #'(lambda (n)
                     (declare (type fixnum n))
                     (the list
                        (if (zerop n)
                           (list :Z-gnrt)
                           +empty-list+)))
         :bsgn :Z-gnrt
         :intr-dffr #'zero-intr-dffr
         :strt :cmbn
         :orgn '(Z-chcm))))

(DEFUN ZERO-MRPH (chcm1 &optional (chcm2 chcm1) (degr 0))
   (declare (type chain-complex chcm1 chcm2)
            (type fixnum degr))
   (the morphism
      (build-mrph
         :sorc chcm1 :trgt chcm2 :degr degr
         :intr #'(lambda (cmbn)
                    (declare (type cmbn cmbn))
                    (zero-cmbn (+ (cmbn-degr cmbn) degr)))
         :strt :cmbn
         :orgn `(zero-mrph ,chcm1 ,chcm2 ,degr))))

#|
  (cat-init)
  (setf z (zero-mrph (Z-chcm) (Z-chcm) 2))
  (gnrt-? z 0 :z-gnrt)
  (cmbn-? z (cmbn 3))
  (setf z2 (zero-mrph (Z-chcm) (Z-chcm) 2))
  (eq z z2)
  (setf z3 (zero-mrph (Z-chcm) (Z-chcm) 3))
  (gnrt-? z3 0 :z-gnrt)
  (eq z z3))
|#    



(DEFUN IDNT-MRPH (chcm)
   (declare (type chain-complex chcm))
   (the morphism
      (build-mrph
         :sorc chcm :trgt chcm :degr 0
         :intr #'identity
         :strt :cmbn
         :orgn `(idnt-mrph ,chcm))))

#|
  (setf zi (idnt-mrph (Z-chcm)))
  (gnrt-? zi 0 :z-gnrt)
  (setf zi2 (idnt-mrph (Z-chcm)))
  (eq zi zi2))
|#

(DEFUN OPPS (mrph)
   (declare (type morphism mrph))
   (the morphism
      (with-slots (sorc trgt degr) mrph
         (build-mrph
            :sorc sorc :trgt trgt :degr degr
            :intr #'(lambda (cmbn)
                       (declare (type cmbn cmbn))
                       (cmbn-opps (cmbn-? mrph cmbn)))
            :strt :cmbn
            :orgn `(opps ,mrph)))))

#|
  (cat-init)
  (setf -zi (opps (idnt-mrph (Z-chcm))))
  (gnrt-? -zi 0 :z-gnrt)
  (setf -zi2 (opps (idnt-mrph (Z-chcm))))
  (eq -zi -zi2))
|#

(DEFMETHOD CMPS ((chcm1 chain-complex) (chcm2 chain-complex) &optional strt)
   (declare (type (or strt null) strt))
   (the morphism
      (cmps (dffr chcm1) (dffr chcm2) strt)))

(DEFMETHOD CMPS ((chcm1 chain-complex) (mrph2 morphism) &optional strt)
   (declare (type (or strt null) strt))
   (the morphism
      (cmps (dffr chcm1) mrph2 strt)))

(DEFMETHOD CMPS ((mrph1 morphism) (chcm2 chain-complex) &optional strt)
   (declare (type (or strt null) strt))
   (the morphism
      (cmps mrph1 (dffr chcm2) strt)))

(DEFMETHOD CMPS ((mrph1 morphism) (mrph2 morphism) &optional strt)
   (declare (type (or strt null) strt))
   (the morphism
      (with-slots ((sorc1 sorc) (trgt1 trgt) (degr1 degr) (strt1 strt)) mrph1
         (declare
            (type chain-complex sorc1 trgt1)
            (fixnum degr1)
            (type strt strt1))
      (with-slots ((sorc2 sorc) (trgt2 trgt) (degr2 degr) (strt2 strt)) mrph2
         (declare
            (type chain-complex sorc2 trgt2)
            (fixnum degr2)
            (type strt strt2))
         (unless (eq (grmd sorc1) (grmd trgt2))
            (error "In 2MRPH-CMPS, the morphisms ~A and ~A may not be composed (cf source and target)."
               mrph1 mrph2))
         (unless strt
            (setf strt (if (and (eq strt1 :gnrt)
                                (eq strt2 :gnrt))
                          :gnrt
                          :cmbn)))
         (when (or (eq (first (orgn mrph1)) 'zero-mrph)
                   (eq (first (orgn mrph2)) 'zero-mrph))
            (return-from cmps
               (zero-mrph sorc2 trgt1 (+ degr1 degr2))))
         (when (eq (first (orgn mrph1)) 'idnt-mrph)
            (return-from cmps mrph2))
         (when (eq (first (orgn mrph2)) 'idnt-mrph)
            (return-from cmps mrph1))
         (ecase strt
            (:cmbn (build-mrph
                      :sorc sorc2 :trgt trgt1 :degr (+ degr1 degr2)
                      :intr #'(lambda (cmbn)
                                 (declare (type cmbn cmbn))
                                 (the cmbn
                                    (cmbn-? mrph1 (cmbn-? mrph2 cmbn))))
                      :strt :cmbn :orgn `(2mrph-cmps ,mrph1 ,mrph2 ,strt)))
            (:gnrt (build-mrph
                      :sorc sorc2 :trgt trgt1 :degr (+ degr1 degr2)
                      :intr #'(lambda (degr gnrt)
                                 (declare
                                    (type fixnum degr)
                                    (type gnrt gnrt))
                                 (the cmbn
                                    (cmbn-? mrph1 (gnrt-? mrph2 degr gnrt))))
                      :strt :gnrt :orgn `(2mrph-cmps ,mrph1 ,mrph2 ,strt))))))))
  
#|
  (cat-init)
  (setf cc (build-chcm :cmpr #'f-cmpr
                       :basis :locally-effective
                       :bsgn 0
                       :intr-dffr #'(lambda (cmbn)
                                       (cmbn (1- (cmbn-degr cmbn))))
                       :strt :cmbn
                       :orgn '(Z of Z)))
  (setf *n* 5)
  (defun ff (degr i)
     (do ((*2n* (ash *n* 1))
          (rslt +empty-list+
                (cons (cons (let ((cffc (- (random *2n*) *n*)))
                               (if (minusp cffc) cffc (1+ cffc)))
                            (decf gnrt (1+ (random *n*))))
                      rslt))
          (gnrt i)
          (k 0 (1+ k)))
         ((= k *n*)
          (make-cmbn
             :degr 0
             :list rslt))))
  (setf mrph (build-mrph :sorc cc :trgt cc :degr 0
			 :intr #'ff :strt :gnrt :orgn '(test)))
  (setf mrph2 (2mrph-cmps mrph mrph :gnrt))
  (gnrt-? mrph2 0 0) 
  (dotimes (i 5)
     (print (gnrt-? mrph2 0 i)))
  (setf mrph (build-mrph :sorc cc :trgt cc :degr 0
			 :intr #'ff :strt :gnrt :orgn '(test)))
  (setf mrph3 (cmps mrph mrph :cmbn))
  (gnrt-? mrph3 0 0) 
  (dotimes (i 5)
     (print (gnrt-? mrph3 0 i)))
  (setf mrph33 (cmps mrph mrph :cmbn))
  (eq mrph3 mrph33))
|# 

(DEFUN N-MRPH-INTR (n mrph)
  (declare
    (fixnum n)
    (type morphism mrph))
  (flet ((rslt (cmbn)
	   (declare (type cmbn cmbn))
	   (n-cmbn n (cmbn-? mrph cmbn))))
     (the intr-mrph #'rslt)))

(DEFUN N-MRPH (n mrph)
  (declare
    (fixnum n)
    (type morphism mrph))
  (the morphism
    (with-slots (sorc trgt degr) mrph
      (declare
        (type chain-complex sorc trgt)
	(fixnum degr))
      (build-mrph
        :sorc sorc :trgt trgt :degr degr
	:intr (n-mrph-intr n mrph) :strt :cmbn
	:orgn `(n-mrph ,n ,mrph)))))

#|
  (setf s3 (sphere 3))
  (setf ch3 (chml-clss s3 3))
  (setf 2ch3 (n-mrph 2 ch3))
  (setf f3 (z-whitehead s3 2ch3))
  (setf x (fibration-total f3))
  (homology x 0 10)
  (setf k (k-z 3))
  (setf ch3 (chml-clss k 3))
  (setf 2ch3 (n-mrph 2 ch3))
  (setf f3 (z-whitehead k 2ch3))
  (setf x (fibration-total f3))
  (homology x 0 10)
|#  

(DEFMETHOD ADD ((mrph1 morphism) (mrph2 morphism) &optional strt)
   (declare (type (or null strt) strt))
   (the morphism
      (with-slots ((sorc1 sorc) (trgt1 trgt) (degr1 degr) (strt1 strt)) mrph1
         (declare
            (type chain-complex sorc1 trgt1)
            (fixnum degr1)
            (type strt strt1))
      (with-slots ((sorc2 sorc) (trgt2 trgt) (degr2 degr) (strt2 strt)) mrph2
         (declare
            (type chain-complex sorc2 trgt2)
            (fixnum degr2)
            (type strt strt2))
         (unless (and (eq (grmd sorc1) (grmd sorc2))
                      (eq (grmd trgt1) (grmd trgt2))
                      (= degr1 degr2))
            (error "In 2MRPH-ADD, ~A and ~A may not be added." mrph1 mrph2))
         (when (eq (first (orgn mrph1)) 'zero-mrph)
            (return-from add mrph2))
         (when (eq (first (orgn mrph2)) 'zero-mrph)
            (return-from add mrph1))
         (unless strt
            (setf strt
                  (if (and (eq strt1 :gnrt)
                           (eq strt2 :gnrt))
                     :gnrt
                     :cmbn)))
         (when (or (eq (first (orgn mrph1)) 'idnt-mrph)
                   (eq (first (orgn mrph2)) 'idnt-mrph))
            (setf strt :cmbn))
         (let ((cmpr (cmpr trgt1)))
            (declare (type cmprf cmpr))
            (ecase strt
               (:gnrt (build-mrph
                         :sorc sorc1 :trgt trgt1 :degr degr1
                         :intr #'(lambda (degr gnrt)
                                    (declare (type fixnum degr) (type gnrt gnrt))
                                    (2cmbn-add cmpr
                                       (gnrt-? mrph1 degr gnrt)
                                       (gnrt-? mrph2 degr gnrt)))
                         :strt :gnrt
                         :orgn `(2mrph-add ,mrph1 ,mrph2 ,strt)))
               (:cmbn (build-mrph
                         :sorc sorc1 :trgt trgt1 :degr degr1
                         :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (2cmbn-add cmpr
                                       (cmbn-? mrph1 cmbn)
                                       (cmbn-? mrph2 cmbn)))
                         :strt :cmbn
                         :orgn `(2mrph-add ,mrph1 ,mrph2 ,strt)))))))))

#|
  (cat-init)
  (setf cc (build-chcm :cmpr #'f-cmpr
                       :basis :locally-effective
                       :bsgn 0
                       :intr-dffr #'(lambda (cmbn)
                                       (cmbn (1- (cmbn-degr cmbn))))
                       :strt :cmbn
                       :orgn '(Z of Z)))
  (setf *n* 10)
  (defun ff (degr i)
     (do ((*2n* (ash *n* 1))
          (rslt +empty-list+
                (cons (cons (let ((cffc (- (random *2n*) *n*)))
                               (if (minusp cffc) cffc (1+ cffc)))
                            (decf gnrt (1+ (random *n*))))
                      rslt))
          (gnrt i)
          (k 0 (1+ k)))
         ((= k *n*)
          (make-cmbn
             :degr 0
             :list rslt))))
  (setf mrph1 (build-mrph :sorc cc :trgt cc :degr 0
                          :intr #'ff :strt :gnrt :orgn '(test)))
  (setf mrph2 (build-mrph :sorc cc :trgt cc :degr 0
                          :intr #'ff :strt :gnrt :orgn '(test2)))
  (setf mrph3 (add mrph1 mrph2 :gnrt))
  (setf +too-much-time+ -1)
  (gnrt-? mrph1 0 0)
  (gnrt-? mrph2 0 0)
  (gnrt-? mrph3 0 0)
  (setf mrph4 (add mrph1 mrph2 :cmbn))
  (gnrt-? mrph4 0 0)
  (setf mrph44 (add mrph1 mrph2 :cmbn))
  (eq mrph4 mrph44))
|#

(DEFMETHOD SBTR ((mrph1 morphism) (mrph2 morphism) &optional strt)
   (declare (type (or null strt) strt))
   (the morphism
      (with-slots ((sorc1 sorc) (trgt1 trgt) (degr1 degr) (strt1 strt)) mrph1
         (declare
            (type chain-complex sorc1 trgt1)
            (fixnum degr1)
            (type strt strt1))
      (with-slots ((sorc2 sorc) (trgt2 trgt) (degr2 degr) (strt2 strt)) mrph2
         (declare
            (type chain-complex sorc2 trgt2)
            (fixnum degr2)
            (type strt strt2))
         (unless (and (eq (grmd sorc1) (grmd sorc2))
                      (eq (grmd trgt1) (grmd trgt2))
                      (= degr1 degr2))
            (error "In 2MRPH-SBTR, ~A and ~A may not be subtracteded." mrph1 mrph2))
         (when (eq (first (orgn mrph1)) 'zero-mrph)
            (return-from sbtr mrph2))
         (when (eq (first (orgn mrph2)) 'zero-mrph)
            (return-from sbtr mrph1))
         (unless strt
            (setf strt
                  (if (and (eq strt1 :gnrt)
                           (eq strt2 :gnrt))
                     :gnrt
                     :cmbn)))
         (when (or (eq (first (orgn mrph1)) 'idnt-mrph)
                   (eq (first (orgn mrph2)) 'idnt-mrph))
            (setf strt :cmbn))
         (let ((cmpr (cmpr trgt1)))
            (declare (type cmprf cmpr))
            (ecase strt
               (:gnrt (build-mrph
                         :sorc sorc1 :trgt trgt1 :degr degr1
                         :intr #'(lambda (degr gnrt)
                                    (declare (type fixnum degr) (type gnrt gnrt))
                                    (2cmbn-sbtr cmpr
                                       (gnrt-? mrph1 degr gnrt)
                                       (gnrt-? mrph2 degr gnrt)))
                         :strt :gnrt
                         :orgn `(2mrph-sbtr ,mrph1 ,mrph2 ,strt)))
               (:cmbn (build-mrph
                         :sorc sorc1 :trgt trgt1 :degr degr1
                         :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (2cmbn-sbtr cmpr
                                       (cmbn-? mrph1 cmbn)
                                       (cmbn-? mrph2 cmbn)))
                         :strt :cmbn
                         :orgn `(2mrph-sbtr ,mrph1 ,mrph2 ,strt)))))))))

#|
()
(cat-init)
(setf cc (build-chcm :cmpr #'f-cmpr
                     :basis :locally-effective
                     :bsgn 0
                     :intr-dffr #'(lambda (cmbn)
                                    (cmbn (1- (cmbn-degr cmbn))))
                     :strt :cmbn
                     :orgn '(Z of Z)))
(setf *n* 10)
(defun ff (degr i)
  (do ((*2n* (ash *n* 1))
       (rslt +empty-list+
             (cons (cons (let ((cffc (- (random *2n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (make-cmbn
        :degr 0
        :list rslt))))
(setf mrph1 (build-mrph :sorc cc :trgt cc :degr 0
                        :intr #'ff :strt :gnrt :orgn '(test)))
(setf mrph2 (build-mrph :sorc cc :trgt cc :degr 0
                        :intr #'ff :strt :gnrt :orgn '(test2)))
(setf mrph3 (sbtr mrph1 mrph2 :gnrt))
(setf +too-much-time+ -1)
(gnrt-? mrph1 0 0)
(gnrt-? mrph2 0 0)
(gnrt-? mrph3 0 0)
(setf mrph4 (sbtr mrph1 mrph2 :cmbn))
(gnrt-? mrph4 0 0)
(setf mrph44 (sbtr mrph1 mrph2 :cmbn))
(eq mrph4 mrph44))
|#

(DEFUN CHANGE-SORC-TRGT (mrph &key new-sorc new-trgt)
   (declare
      (type morphism mrph)
      (type (or null chain-complex) new-sorc new-trgt))
   (the morphism
      (with-slots (sorc trgt degr intr strt) mrph
         (build-mrph
            :sorc (or new-sorc sorc)
            :trgt (or new-trgt trgt)
            :degr degr
            :intr intr
            :strt strt
            :orgn `(change-sorc-trgt ,mrph ,new-sorc ,new-trgt)))))

(DEFUN DSTR-CHANGE-SORC-TRGT (mrph &key new-sorc new-trgt)
   (declare
      (type morphism mrph)
      (type (or null chain-complex) new-sorc new-trgt))
   (the morphism
      (with-slots (sorc trgt) mrph
         (when new-sorc 
            (setf sorc new-sorc))
         (when new-trgt
            (setf trgt new-trgt))
         mrph)))

(DEFMETHOD ADD ((chcm chain-complex) (perturbation morphism) &optional strt)
   (declare (type (or strt null) strt))
   (the chain-complex
      (with-slots (cmpr basis bsgn dffr) chcm
         (declare
            (type cmprf cmpr)
            (type basis basis)
            (type morphism dffr))
      (with-slots (sorc trgt degr) perturbation
         (declare
            (type chain-complex sorc trgt)
            (fixnum degr))
         (unless (and (eq (grmd sorc) (grmd chcm))
                      (eq (grmd trgt) (grmd chcm))
                      (= degr -1))
            (error "In (METHOD ADD CHAIN-COMPLEX MORPHISM), the data are not coherent."))
         (let ((new-dffr (add dffr perturbation strt)))
            (declare (type morphism new-dffr))
            (let ((new-chcm (build-chcm
                               :cmpr cmpr
                               :basis basis
                               :intr-dffr (intr new-dffr)
                               :strt (strt new-dffr)
                               :orgn  `(add ,chcm ,perturbation))))
	      (declare (type chain-complex new-chcm))
	      (if (slot-boundp chcm 'bsgn)
		 (setf (slot-value new-chcm 'bsgn) bsgn)
		 (slot-makunbound new-chcm 'bsgn))
	      (setf (slot-value new-chcm 'grmd) (grmd chcm))
	      new-chcm))))))
