;;; CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP
;;; CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP
;;; CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP CHCM-ELEMENTARY-OP

(IN-PACKAGE #:cat)

(PROVIDE "chcm-elementary-op")

(DEFUN Z-CHCM ()
  "--------------------------------------------------------------[function-doc]
Z-CHCM
Args: ()
Build the unit chain complex.
------------------------------------------------------------------------------"
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


(DEFUN IDNT-MRPH (chcm)
  (declare (type chain-complex chcm))
  (the morphism
       (build-mrph
	:sorc chcm :trgt chcm :degr 0
	:intr #'identity
	:strt :cmbn
	:orgn `(idnt-mrph ,chcm))))


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


(DEFGENERIC CMPS (chcm-or-mrph1 chcm-or-mrph2 &optional strt)
  (:documentation
  "---------------------------------------------------------------[generic-doc]
CMPS
Args: (arg1 arg2 &optional opt)
A generic function whose specializations implement the composition
ARG2 ° ARG1 modified by an optional argument OPT. A typical example is the
composition of two morphisms, where the optional argument is a strategy.
-----------------------------------------------------------------------------"))

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

(DEFGENERIC ADD (chcm-or-mrph morphism &optional strt))

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

(DEFGENERIC SBTR (chcm-or-mrph morphism &optional strt))

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
	     (error "In 2MRPH-SBTR, ~A and ~A may not be subtracteded."
		    mrph1 mrph2))
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
