
(in-package :kenzo-test)

(in-suite :kenzo)

(test bicn-cmbn
      (let* ((comb-bic (cat:cmbn 3 2 (cat:bcnb 'b1) 4 (cat:bcnb 'b2)
				 6 (cat:bcnb 'b3) 3 (cat:bcnc 'c1)
				 5 (cat:bcnc 'c2) 7 (cat:bcnd 'd1))))
	(cat:cmbn-list comb-bic)
	(cat:dispatch-bicn-cmbn (cat:cmbn 3 3 (cat:bcnb 'b1)
					  4 (cat:bcnb 'b2)
					  33 (cat:bcnc 'c)
					  333 (cat:bcnd 'd)))))

(test bicn-cmbn-cmbn*
      (is (cat:cmbn-non-zero-p
	   (cat:bicn-cmbn-cmbnb (cat:cmbn 3 1 (cat:bcnb 'a) 2 (cat:bcnc 'b)))))
      (is (cat:cmbn-zero-p
	   (cat:bicn-cmbn-cmbnb (cat:cmbn 3 2 (cat:bcnc 'b)))))
      (is (cat:cmbn-non-zero-p
	   (cat:bicn-cmbn-cmbnc (cat:cmbn 3 1 (cat:bcnb 'a) 2 (cat:bcnc 'b)
					  3 (cat:bcnd 'c)))))
      (is (cat:cmbn-zero-p
	   (cat:bicn-cmbn-cmbnc (cat:cmbn 3 1 (cat:bcnb 'a) 3 (cat:bcnd 'c)))))
      (is (cat:cmbn-non-zero-p
	   (cat:bicn-cmbn-cmbnd (cat:cmbn 3 1 (cat:bcnb 'a) 2 (cat:bcnc 'b)
					  3 (cat:bcnd 'c)))))
      (is (cat:cmbn-zero-p
	   (cat:bicn-cmbn-cmbnd (cat:cmbn 3 2 (cat:bcnb 'b))))))

(test make-bicn-cmbn
      (multiple-value-call #'cat:make-bicn-cmbn
	(cat:dispatch-bicn-cmbn
	 (cat:cmbn 3 3 (cat:bcnb 'b1) 4 (cat:bcnb 'b2) 33 (cat:bcnc 'c)
		   333 (cat:bcnd 'd)))))

(test bicn-print
      (let ((a (format nil "~A" (cat:bcnb 'a)))
	    (b (format nil "~A" (cat:bcnc 'b)))
	    (c (format nil "~A" (cat:bcnd 'c))))
	(is (string= a "<BcnB A>"))
	(is (string= b "<BcnC B>"))
	(is (string= c "<BcnD C>"))))

(test bicone-cmpr
      (let ((r (cat:bicone-cmpr #'cat:f-cmpr #'cat:f-cmpr #'cat:f-cmpr)))
	(is (equal :less (funcall r (cat:bcnb 1) (cat:bcnc 0))))
	(is (equal :less (funcall r (cat:bcnb 1) (cat:bcnb 2))))))

(test bicone-basis
      (is (equal :locally-effective
		 (cat:bicone-basis :locally-effective
				   :locally-effective
				   :locally-effective)))
      (let* ((b #'(lambda (degr)
		    (mapcar #'(lambda (item)
				(cons degr item)) (cat:<a-b> 0 degr))))
	     (r (cat:bicone-basis b b b)))
	(is (not (null (funcall r 3))))))


(test bicone
      (progn
	(cat:cat-init)
	(let ((delta3 (cdelta1 3))
	      (bic (cat:bicone (make-rdct1 3 2) (make-rdct1 4 2))))
	  #|
	  (princ (cat:basis delta3 0))
	  (princ (cat:basis delta3 1))
	  (princ (cat:basis delta3 2))
	  (princ (cat:basis delta3 3))
	  (princ (cat:basis delta3 4))
	  |#
	  (princ (cat:basis bic 0))
	  (princ (cat:basis bic 1))
	  (princ (cat:basis bic 4))
	  (princ (cat:? bic (cat:cmbn 2 3 (cat:bcnb '(0 1 3))
				      4 (cat:bcnc '(0 1 2 3))
				      5 (cat:bcnd '(0 1 4)))))
	  (is (cat:cmbn-zero-p
	       (cat:? bic
		      (cat:? bic (cat:cmbn 2 3 (cat:bcnb '(0 1 3))
					   4 (cat:bcnc '(0 1 2 3))
					   5 (cat:bcnd '(0 1 4))))))))))


(test cmps
      (progn
	(cat:cat-init)
	(let* ((c (cat:build-chcm
		  :cmpr #'cat:s-cmpr
		  :basis #'(lambda (dmns) '(a))
		  :bsgn 'a
		  :intr-dffr #'cat:zero-intr-dffr
		  :strt :cmbn
		  :orgn '(c)))
	       (h1 (cat:trivial-hmeq c))
	       (h2 (cat:cmps h1 h1))
	       (h3 (cat:cmps h2 h2)))
	  (cat:pre-check-rdct (cat:lrdct h2))
	  (setf cat:*tc* (cat:cmbn 3 1 (cat:bcnB 'a) 10 (cat:bcnC 'a)
			       100 (cat:bcnD 'a)))
	  (setf cat:*bc* (cat:cmbn 3 1 'a))
	  (check-rdct)
	  (cat:pre-check-rdct (cat:rrdct h2))
	  (check-rdct)
	  (setf cat:*tc* (cat:cmbn 3 1 (cat:bcnB (cat:bcnB 'a))
				   10 (cat:bcnB (cat:bcnC 'a))
				   100 (cat:bcnB (cat:bcnD 'a))
				   1000 (cat:bcnC 'a)
				   10000 (cat:bcnD (cat:bcnB 'a))
				   5234 (cat:bcnD (cat:bcnC 'a))
				   223 (cat:bcnD (cat:bcnD 'a))))
	  (cat:pre-check-rdct (cat:lrdct h3))
	  (check-rdct)
	  (cat:pre-check-rdct (cat:rrdct h3))
	  (check-rdct))))
