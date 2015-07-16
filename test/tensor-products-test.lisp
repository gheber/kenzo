
(in-package :kenzo-test)

(in-suite :kenzo)


(test tnpr
      (cat:tnpr 2 'a 3 'b)
      (setf cat:*tnpr-with-degrees* t)
      (cat:tnpr 2 'a 3 'b)
      (setf cat:*tnpr-with-degrees* nil))


(test 2cmbn-tnpr
      (setf cat:*tnpr-with-degrees* t)
      (cat:2cmbn-tnpr (cat:cmbn 2 3 'a 4 'b -5 'c)
		      (cat:cmbn 3 4 'x -3 'y 2 'z)))


(test tnsr-prdc-cmpr
      (let ((cmpr (cat:tnsr-prdc-cmpr #'cat:s-cmpr #'cat:s-cmpr)))
	(is (equal :less (funcall cmpr
				  (cat:tnpr 2 'a 3 'b)
				  (cat:tnpr 3 'a 2 'b))))
	(is (equal :greater (funcall cmpr
				     (cat:tnpr 3 'a 2 'b)
				     (cat:tnpr 2 'a 3 'b))))
	(is (equal :less (funcall cmpr
				   (cat:tnpr 2 'a 3 'b)
				   (cat:tnpr 2 'b 3 'b))))
	(is (equal :greater (funcall cmpr
				     (cat:tnpr 2 'b 3 'b)
				     (cat:tnpr 2 'a 3 'b))))
	(is (equal :less (funcall cmpr
				  (cat:tnpr 2 'a 3 'a)
				  (cat:tnpr 2 'a 3 'b))))
	(is (equal :greater (funcall cmpr
				     (cat:tnpr 2 'a 3 'c)
				     (cat:tnpr 2 'a 3 'b))))
	(is (equal :equal (funcall cmpr
				   (cat:tnpr 2 'a 3 'b)
				   (cat:tnpr 2 'a 3 'b))))))


(defun bas (degr)
  (case degr
    (0 '(a b c))
    (1 '(d))
    (2 nil)
    (3 '(x y))))


(test tnsr-prdc-basis
      (let ((bas (cat:tnsr-prdc-basis #'bas #'bas)))
	(dotimes (i 8)
	  (print (funcall bas i)))))


(test tnsr-prdc-intr-dffr
      	(cat:cat-init)
	(let* ((chcm (cat:build-chcm :cmpr #'cat:s-cmpr
				     :basis :locally-effective
				     :intr-dffr #'cat:zero-mrph
				     :strt :gnrt :orgn '(test-1)))
	       (dffr (cat:build-mrph
		       :sorc chcm :trgt chcm :degr -1
		       :intr #'(lambda (degr gnrt)
				 (ecase gnrt
				   (a (cat:cmbn (1- degr) 2 'd1a -3 'd2a))
				   (b (cat:cmbn (1- degr) 3 'd1b -4 'd2b))))
		       :strt :gnrt :orgn '(test-2)))
	       (rslt (cat:tnsr-prdc-intr-dffr dffr dffr)))
	  (funcall rslt 4 (cat:tnpr 2 'a 2 'b))
	  (funcall rslt 5 (cat:tnpr 3 'a 2 'b))
	  (funcall rslt 5 (cat:tnpr 2 'a 3 'b))
	  (funcall rslt 6 (cat:tnpr 3 'a 3 'b))))


(test tnsr-prdc
      (let ((dd (cat:tnsr-prdc (cat:delta 2) (cat:delta 3))))
	(cat:cmpr dd (cat:tnpr 2 7 2 11) (cat:tnpr 2 7 2 14))
	(cat:basis dd 3)
	(cat:? dd 4 (cat:tnpr 2 7 2 14))
	(cat:? dd 3 (cat:tnpr 1 6 2 14))
	(cat:? dd (cat:? dd 4 (cat:tnpr 2 7 2 14)))
	(cat:? dd (cat:? dd 3 (cat:tnpr 1 6 2 14)))))


(test tnsr-prdc-intr
      (cat:cat-init)
      (let* ((chcm (cat:build-chcm :cmpr #'cat:s-cmpr
				   :basis :locally-effective
				   :intr-dffr #'cat:zero-mrph
				   :strt :gnrt
				   :orgn '(test-1)))

	     (mrph (cat:build-mrph
		    :sorc chcm :trgt chcm :degr 0
		    :intr #'(lambda (degr gnrt)
			      (ecase gnrt
				(a (cat:cmbn degr 2 'd1a -3 'd2a))
				(b (cat:cmbn degr 3 'd1b -4 'd2b))))
		    :strt :gnrt :orgn '(test-2)))
	     (rslt (cat:tnsr-prdc-intr mrph mrph)))
	(funcall rslt 6 (cat:tnpr 2 'a 4 'b))
	(funcall rslt 5 (cat:tnpr 3 'a 2 'b))
	(setf mrph (cat:build-mrph
		    :sorc chcm :trgt chcm :degr 1
		    :intr #'(lambda (degr gnrt)
			      (ecase gnrt
				(a (cat:cmbn (1+ degr) 2 'd1a -3 'd2a))
				(b (cat:cmbn (1+ degr) 3 'd1b -4 'd2b))))
		    :strt :gnrt :orgn '(test-3)))
	(setf rslt (cat:tnsr-prdc-intr mrph mrph))
	(funcall rslt 6 (cat:tnpr 2 'a 4 'b))
	(funcall rslt 5 (cat:tnpr 3 'a 2 'b))))


(test tnsr-prdc
      (cat:cat-init)
      (let* ((d (cat:dffr (cat:delta-infinity)))
	     (dd (cat:tnsr-prdc d d))
	     (ddd (cat:cmps dd dd)))
	(is (eq (cat:sorc dd) (cat:tnsr-prdc (cat:delta-infinity)
					     (cat:delta-infinity))))
	(cat:? dd 5 (cat:tnpr 2 7 3 15))
	(cat:? dd 5 (cat:tnpr 3 15 2 7))
	(cat:? dd (cat:? dd 5 (cat:tnpr 2 7 3 15)))
	(cat:? dd (cat:? dd 5 (cat:tnpr 3 15 2 7)))
	(cat:? ddd 5 (cat:tnpr 2 7 3 15))
	(cat:? ddd 5 (cat:tnpr 3 15 2 7))))


(test tnsr-prdc1
      (cat:cat-init)
      (let* ((r (cat:ez (cat:delta-infinity) (cat:delta-infinity)))
	     (r2 (cat:tnsr-prdc r r)))
	(setf cat:*bc* (cat:cmbn 4 1 (cat:tnpr 2 (cat:tnpr 1 3 1 3) 2
					       (cat:tnpr 1 3 1 3)))
	      cat:*tc* (cat:cmbn 2 1 (cat:tnpr 1 (cat:crpr 0 3 0 3) 1
					       (cat:crpr 0 3 0 3))))
	(cat:pre-check-rdct r2)
	(check-rdct)))


(test tnsr-prdc2
      (cat:cat-init)
      (let* ((k (cat:k-z 2))
	     (k2 (cat:tnsr-prdc k k)))
	(cat:homology k2 0 10)))
