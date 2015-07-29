
(in-package :kenzo-test)

(in-suite :kenzo)

#|
(test ls-hat-t-u
      (cat:cat-init)
      (let ((c (cat:ls-hat-t-u (cat:deltab)))
	    allp allp-degr gnrt)
	(dotimes (i 10) (print (random-allp 5)))
	(setf allp (random-allp 4))  ;; the degree could be too big
	;; then redo
	(setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	(setf gnrt (cat:tnpr allp-degr allp 4
			     (cat:tnpr 2 7 2 (cat:loop3 0 15 2))))
	(cat:? c (+ 4 allp-degr) gnrt)
	(cat:? c *)
	(dotimes (i 10)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 4
				 (cat:tnpr 2 7 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 11)
	      (print (cat:? c (+ 4 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 4 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
				 (cat:tnpr 3 15 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
				 (cat:tnpr 2 7 3 (cat:loop3 0 31 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))))
|#

#|
(test ls-hat-u-t
      (cat:cat-init)
      (let ((c (cat:ls-hat-u-t (cat:deltab)))
	    allp allp-degr gnrt)
	(dotimes (i 10) (print (random-allp 5)))
	(setf allp (random-allp 3))
	(setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	(setf gnrt (cat:tnpr allp-degr allp 4
			     (cat:tnpr 2 7 2 (cat:loop3 0 15 2))))
	(cat:? c (+ 4 allp-degr) gnrt)
	(cat:? c *)
	(dotimes (i 10)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 4
				 (cat:tnpr 2 7 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 11)
	      (print (cat:? c (+ 4 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 4 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
				 (cat:tnpr 3 15 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
				 (cat:tnpr 2 7 3 (cat:loop3 0 31 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))))
|#

#|
(test ls-left-hmeq-hat
      (cat:cat-init)
      (let ((c (cat:ls-left-hmeq-hat (cat:deltab)))
	    allp allp-degr gnrt)
	(dotimes (i 10) (print (random-allp 5)))
	(setf allp (random-allp 3))
	(setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	(setf gnrt (cat:tnpr allp-degr allp 4 (cat:tnpr 2 7 2
							(cat:loop3 0 15 2))))
 	(cat:? c (+ 4 allp-degr) gnrt)
	(cat:? c (cat:? c (+ 4 allp-degr) gnrt))
	(dotimes (i 10)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 4
				 (cat:tnpr 2 7 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 11)
	      (print (cat:? c (+ 4 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 4 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
				 (cat:tnpr 3 15 2 (cat:loop3 0 15 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))
	(dotimes (i 20)
	  (let ((allp (random-allp 3)))
	    (setf allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	    (setf gnrt (cat:tnpr allp-degr allp 5
			     (cat:tnpr 2 7 3 (cat:loop3 0 31 2))))
	    (unless (>= allp-degr 10)
	      (print (cat:? c (+ 5 allp-degr) gnrt))
	      (print (cat:? c (cat:? c (+ 5 allp-degr) gnrt))))))))
|#

(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat:ls-pre-left-hmeq-left-reduction-intr-f
       (cat:cmbn 6 100 (cat:tnpr 0 'a 6 (cat:tnpr 0 'b 6 'c))
	     50 (cat:tnpr 0 'a 6 (cat:tnpr 0 'b 6 'cc))
	     10 (cat:tnpr 0 'a 6 (cat:tnpr 2 'b 4 'c))
	     1 (cat:tnpr 2 'a 4 (cat:tnpr 2 'b 2 'cc)))))


(test ls-left-hmeq-left-reduction-g-intr
      (let ((r (cat:ls-left-hmeq-left-reduction-g-intr '*)))
	(funcall r (cat:cmbn 3 4 'loop))))


(test ls-pre-left-hmeq-left-reduction-h-intr
      (cat:cat-init)
      (let ((r (cat:ls-pre-left-hmeq-left-reduction-h-intr (cat:deltab))))
	(setf cat:*tnpr-with-degrees* t)
	(funcall r (cat:cmbn 10 1 (cat:tnpr 3 (cat:allp 2 'a 1 'b)
					    7 (cat:tnpr 5 'c 2 'd))
			     1 (cat:tnpr 7 (cat:allp 2 'a 5 'b)
					 3 (cat:tnpr 0 '* 3 'c))
			     10 (cat:tnpr 8 (cat:allp 4 'aa 4 'bb)
					  2 (cat:tnpr 0 '* 2 'cc))))
	(funcall r (cat:cmbn 10 1 (cat:tnpr 3 (cat:allp 2 'a 1 'b)
					    7 (cat:tnpr 5 'c 2 'd))
			     1 (cat:tnpr 7 (cat:allp 4 'a 3 'b)
					 3 (cat:tnpr 0 '* 3 'c))
			     10 (cat:tnpr 8 (cat:allp 3 'aa 5 'bb)
					  2 (cat:tnpr 0 '* 2 'cc))))
	(setf cat:*tnpr-with-degrees* nil)))

#|
(test ls-pre-left-hmeq-left-reduction
      (cat:cat-init)
      (let* ((rdct (cat:ls-pre-left-hmeq-left-reduction (cat:deltab)))
	     (allp (random-allp 3))
	     (allp-degr (apply #'+ (mapcar #'car (cat:allp-list allp))))
	     (gnrt (cat:tnpr allp-degr allp 4 (cat:tnpr 2 7 2
						       (cat:loop3 0 15 2)))))
	(cat:pre-check-rdct rdct)
	(setf cat:*tc* (cat:cmbn (+ 4 allp-degr) 1 gnrt))
	(setf cat:*bc* (cat:cmbn 2 1 (cat:loop3 0 15 2)))
	(check-rdct)
	(setf gnrt (cat:tnpr allp-degr allp 4
			     (cat:tnpr 0 1 4 (cat:loop3 0 (cat:mask 6) 2))))
	(setf cat:*tc* (cat:cmbn (+ 4 allp-degr) 1 gnrt))
	(check-rdct)
	(setf cat:*bc* (cat:cmbn 0 1 (cat:bspn (cat:bcc rdct))))
	(setf cat:*tc* (cat:cmbn 0 1 (cat:bsgn (cat:tcc rdct))))
	(check-rdct)))
|#

(defun a (d1 d2 d3)
  (setf cat:*tc* (cat:cmbn (+ d1 d2 d3)
			   1 (cat:tnpr
			      d1 (cat:allp d1 (cat:mask (+ d1 2)))
			      (+ d2 d3)
			      (cat:tnpr d2 (cat:mask (1+ d2))
					d3 (cat:loop3
					    0 (cat:mask (+ d3 2)) 1))))
	cat:*bc* (cat:cmbn d3 1 (cat:loop3 0 (cat:mask (+ d3 2)) -1)))
  (check-rdct))


(test ls-left-hmeq-left-reduction
      (cat:cat-init)
      (let ((rdct (cat:ls-left-hmeq-left-reduction (cat:deltab2))))
	(cat:pre-check-rdct rdct)
	#|
	(a 1 0 1)
	(a 1 1 1)  ;; error because 3 does not exist in deltab2
	(a 1 2 1)
	(a 2 2 1)
	|#
	)
      )


(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat:ls-pre-left-hmeq-right-reduction-intr-f
       (cat:cmbn 3 12 (cat:tnpr 2 'a 1 'b) 14 (cat:tnpr 3 'aa 0 '*))))


(test ls-pre-left-hmeq-right-reduction-intr-g
      (let ((r (cat:ls-pre-left-hmeq-right-reduction-intr-g '*)))
	(funcall r (cat:cmbn 3 4 'a))))


(test ls-pre-left-hmeq-right-reduction
      (cat:cat-init)
      (let ((r (cat:ls-pre-left-hmeq-right-reduction (cat:deltab2))))
	(cat:pre-check-rdct r)
	(setf cat:*tc* (cat:cmbn 0 1 (cat:bsgn (cat:tcc r))))
	(setf cat:*bc* (cat:cmbn 0 1 (cat:bsgn (cat:bcc r))))
	(check-rdct)
	#|
	(setf cat:*tc* (cat:cmbn
			3 1 (cat:tnpr 3 (cat:allp 3 (cat:mask 5))
				      0 (cat:tnpr 0 1 0 cat:+null-loop+))))
	(setf cat:*bc* (cat:cmbn 3 1 (cat:allp 1 7 2 15)))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn
			6 1 (cat:tnpr 3 (cat:allp 1 7 2 15)
				      3 (cat:tnpr 2 7 1 (cat:loop3 0 7 2)))))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn
			6 1 (cat:tnpr
			     3 (cat:allp 1 7 2 15)
			     3 (cat:tnpr 0 1 3
					 (cat:loop3 0 (cat:mask 5) 2)))))
	(check-rdct)
	|#
	))


(test ls-left-hmeq-right-reduction
      (cat:cat-init)
      (let ((r (cat:ls-left-hmeq-right-reduction (cat:deltab2))))
	(cat:pre-check-rdct r)
	(setf cat:*tc* (cat:cmbn 0 1 (cat:bsgn (cat:tcc r))))
	(setf cat:*bc* (cat:cmbn 0 1 (cat:bsgn (cat:bcc r))))
	(check-rdct)
	#|
	(setf cat:*tc* (cat:cmbn 3 1 (cat:tnpr 3 (cat:allp 3 (cat:mask 5))
					       0 (cat:tnpr 0 1 0
							   cat:+null-loop+))))
	(setf cat:*bc* (cat:cmbn 3 1 (cat:allp 1 7 2 15)))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn
			6 1 (cat:tnpr 3 (cat:allp 1 7 2 15)
				      3 (cat:tnpr 2 7 1 (cat:loop3 0 7 2)))))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn
			6 1 (cat:tnpr 3 (cat:allp 1 7 2 15)
				      3 (cat:tnpr 0 1 3
						  (cat:loop3 0 (mask 5) 2)))))
	(check-rdct)
	|#
	))


#|
(test ls-left-hmeq
      (cat:cat-init)
      (let* ((h (cat:ls-left-hmeq (cat:deltab2)))
	     (loop (cat:loop3 0 (cat:mask 5) 2))
	     (allp (cat:allp 2 (cat:mask 4) 3 (cat:mask 5)))
	     x)
	(setf x (cat:lg h 3 loop))
	(setf x (cat:rf h x))
	(setf x (cat:rg h x))
	(setf x (cat:lf h x))
	(setf x (cat:rg h 5 allp))
	(setf x (cat:lf h x))
	(setf x (cat:lg h x))
	(setf x (cat:rf h x)) ;; = allp, but why ?
	))
|#

(test loop-space
      (cat:cat-init)
      (let ((l (cat:loop-space (cat:sphere 2)))
	    (oos3 (cat:loop-space (cat:loop-space (cat:sphere 3))))
	    (ooos4 (cat:loop-space (cat:loop-space (cat:loop-space
						    (cat:sphere 4))))))
	(cat:homology l 6)
	(cat:homology oos3 3)
	(cat:homology ooos4 2)))
