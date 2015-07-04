
(in-package :kenzo-test)

(in-suite :kenzo)

(test zero-mrph
      (progn
	(cat:cat-init)
	(let* ((z (cat:zero-mrph (cat:Z-chcm) (cat:Z-chcm) 2))
	       (z2 (cat:zero-mrph (cat:Z-chcm) (cat:Z-chcm) 2))
	       (z3 (cat:zero-mrph (cat:Z-chcm) (cat:Z-chcm) 3))
	       (comb (cat:gnrt-? z 0 :z-gnrt))
	       (comb1 (cat:cmbn-? z (cat:cmbn 3)))
	       (comb2 (cat:gnrt-? z3 0 :z-gnrt)))
	  (is (equal (cat:cmbn-degr comb) 2))
	  (is (equal (cat:cmbn-list comb) nil))
	  (is (equal (cat:cmbn-degr comb1) 5))
	  (is (equal (cat:cmbn-list comb1) nil))
	  (is (eq z z2))
	  (is (equal (cat:cmbn-degr comb2) 3))
	  (is (equal (cat:cmbn-list comb2) nil))
	  (is (not (eq z z3))))))


(test idnt-mrph
      (progn
	(cat:cat-init)
	(let* ((zi (cat:idnt-mrph (cat:Z-chcm)))
	       (comb (cat:gnrt-? zi 0 :z-gnrt))
	       (zi2 (cat:idnt-mrph (cat:Z-chcm))))
	  (is (equal (cat:cmbn-degr comb) 0))
	  (is (equal (cat:cmbn-list comb) (cons (cons 1 :z-gnrt) nil)))
	  (is (eq zi zi2)))))


(test opps
      (progn
	(cat:cat-init)
	(let* ((-zi (cat:opps (cat:idnt-mrph (cat:Z-chcm))))
	       (comb (cat:gnrt-? -zi 0 :z-gnrt))
	       (-zi2 (cat:opps (cat:idnt-mrph (cat:Z-chcm)))))
	  (is (equal (cat:cmbn-degr comb) 0))
	  (is (equal (cat:cmbn-list comb) (cons (cons -1 :z-gnrt) nil)))
	  (is (eq -zi -zi2)))))


(defparameter *n* 5)
(defun ff (degr i)
  (do ((2*n* (ash *n* 1))
       (rslt cat:+empty-list+
	     (cons (cons (let ((cffc (- (random 2*n*) *n*)))
			   (if (minusp cffc) cffc (1+ cffc)))
			 (decf gnrt (1+ (random *n*))))
		   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat:make-cmbn
	:degr degr
	:list rslt))))

(test cmps
      (progn
	(cat:cat-init)
	(let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
				   :basis :locally-effective
				   :bsgn 0
				   :intr-dffr #'(lambda (cmbn)
						  (cat:cmbn
						   (1- (cat:cmbn-degr cmbn))))
				   :strt :cmbn
				   :orgn '(Z of Z)))
	       (mrph (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test)))
	       (mrph2 (cat:cmps mrph mrph :gnrt))
	       (comb (cat:gnrt-? mrph2 0 0))
	       (mrph3 (cat:cmps mrph mrph :cmbn))
               (comb1 (cat:gnrt-? mrph3 0 0))
	       (mrph33 (cat:cmps mrph mrph :cmbn)))
	  (dotimes (i 5)
	    (setq comb (cat:gnrt-? mrph2 0 i))
	    (is (equal (cat:cmbn-degr comb) 0)))
	  (dotimes (i 5)
	    (setq comb1 (cat:gnrt-? mrph3 0 i))
	    (is (equal (cat:cmbn-degr comb1) 0)))
	  (is (eq mrph3 mrph33)))))

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

(test n-mrph
      (progn
	(cat:cat-init)
	(let* ((s3 (cat:sphere 3))
	       (ch3 (cat:chml-clss s3 3))
	       (2ch3 (cat:n-mrph 2 ch3))
;;	       (f3 (cat:z-whitehead s3 2ch3))
;;	       (x (cat:fibration-total f3))
	       (k (cat:k-z 3)))
	  (declare (ignore k 2ch3))
;;	  (cat:homology x 0 10)
;;	  (setf ch3 (cat:chml-clss k 3))
;;	  (setf 2ch3 (cat:n-mrph 2 ch3))
;;	  (setf f3 (cat:z-whitehead k 2ch3))
;;	  (setf x (cat:fibration-total f3))
;;	  (cat:homology x 0 10)
	  )))


(setf *n* 10)
(setf cat:+too-much-time+ -1)
(test add
      (progn
	(cat:cat-init)
	(let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
				   :basis :locally-effective
				   :bsgn 0
				   :intr-dffr #'(lambda (cmbn)
						  (cat:cmbn
						   (1- (cat:cmbn-degr cmbn))))
				   :strt :cmbn
				   :orgn '(Z of Z)))
	       (mrph1 (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test)))
	       (mrph2 (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test2)))
	       (mrph3 (cat:add mrph1 mrph2 :gnrt))

	       (comb1 (cat:gnrt-? mrph1 0 0))
	       (comb2 (cat:gnrt-? mrph2 0 0))
	       (comb3 (cat:gnrt-? mrph3 0 0))
	       (mrph4 (cat:add mrph1 mrph2 :cmbn))
	       (comb4 (cat:gnrt-? mrph4 0 0))
	       (mrph44 (cat:add mrph1 mrph2 :cmbn)))
	  (is (equal (cat:cmbn-degr comb1) 0))
	  (is (equal (length (cat:cmbn-list comb1)) 10))
	  (is (equal (cat:cmbn-degr comb2) 0))
	  (is (equal (length (cat:cmbn-list comb2)) 10))
	  (is (equal (cat:cmbn-degr comb3) 0))
	  (is (<= (length (cat:cmbn-list comb3)) 20))
	  (is (equal (cat:cmbn-degr comb4) 0))
	  (is (<= (length (cat:cmbn-list comb4)) 20))
	  (is (eq mrph4 mrph44)))))


(test sbtr
      (progn
	(cat:cat-init)
	(let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
				   :basis :locally-effective
				   :bsgn 0
				   :intr-dffr #'(lambda (cmbn)
						  (cat:cmbn
						   (1- (cat:cmbn-degr cmbn))))
				   :strt :cmbn
				   :orgn '(Z of Z)))
	       (mrph1 (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test)))
	       (mrph2 (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test2)))
	       (mrph3 (cat:sbtr mrph1 mrph2 :gnrt))
	       (comb1 (cat:gnrt-? mrph1 0 0))
	       (comb2 (cat:gnrt-? mrph2 0 0))
	       (comb3 (cat:gnrt-? mrph3 0 0))
	       (mrph4 (cat:sbtr mrph1 mrph2 :cmbn))
	       (comb4 (cat:gnrt-? mrph4 0 0))
	       (mrph44 (cat:sbtr mrph1 mrph2 :cmbn)))
	  (is (equal (cat:cmbn-degr comb1) 0))
	  (is (equal (length (cat:cmbn-list comb1)) 10))
	  (is (equal (cat:cmbn-degr comb2) 0))
	  (is (equal (length (cat:cmbn-list comb2)) 10))
	  (is (equal (cat:cmbn-degr comb3) 0))
	  (is (<= (length (cat:cmbn-list comb3)) 20))
	  (is (equal (cat:cmbn-degr comb4) 0))
	  (is (<= (length (cat:cmbn-list comb4)) 20))
	  (is (eq mrph4 mrph44)))))
