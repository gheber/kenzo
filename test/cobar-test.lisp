
(in-package :kenzo-test)

(in-suite :kenzo)

(test allp
      (cat:allp )
      (cat:allp '(2 a 3 b))
      (cat:allp 2 'a 3 'b)
      (signals simple-error (cat:allp 2 'a 3)))


(test cobar-cmpr
      (let ((r (cat:cobar-cmpr #'cat:s-cmpr)))
	(is (equal :equal (funcall r (cat:allp) (cat:allp))))
	(is (equal :greater (funcall r (cat:allp 3 'a) (cat:allp))))
	(is (equal :less (funcall r (cat:allp 3 'a) (cat:allp 2 'a 1 'b))))
	(is (equal :less (funcall r (cat:allp 3 'a) (cat:allp 3 'b))))
	(is (equal :equal (funcall r (cat:allp 3 'a) (cat:allp 3 'a))))))


(test cobar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
	(cat:cobar-basis-length basis 1 1)
	(cat:cobar-basis-length basis 2 1)
	(cat:cobar-basis-length basis 2 2)
	(cat:cobar-basis-length basis 3 1)
	(cat:cobar-basis-length basis 3 2)
	(cat:cobar-basis-length basis 3 3)))


(test cobar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
	     (r (cat:cobar-basis basis)))
	(funcall r 0)
	(funcall r 1)
	(funcall r 2)
	(dotimes (i 7)
	  (print (funcall r i)))
	(cat:cobar-basis :locally-effective)))


(test cobar-intr-vrtc-dffr
      (let* ((d (cat:soft-delta-infinity))
	     (r (cat:cobar-intr-vrtc-dffr (cat:dffr d))))
	(funcall r 0 (cat:allp))
	(funcall r 3 (cat:allp 3 (cat:d 15)))
	(funcall r 5 (cat:allp 3 (cat:d (cat:mask 5)) 2 (cat:d (cat:mask 4))))
	(funcall r 5 (cat:allp 2 (cat:d (cat:mask 4))
			       3 (cat:d (cat:mask 5))))))


(defun random-allp (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat:mask 9)))
	     (dmns (1- (logcount gmsm))))
	(when (plusp dmns)
	  (push (cat:cbgn (1- dmns) (cat:d gmsm)) rslt))))
    (cat:make-allp :list rslt)))


(test vrtc-cobar
      (cat:cat-init)
      (let ((v (cat:vrtc-cobar (cat:soft-delta-infinity))))
	(dotimes (i 10) (print (random-allp 5)))
	(dotimes (i 10)
	  (let ((allp (random-allp 3)))
	    (print allp)
	    (print (cat:? v (apply #'+ (mapcar #'car (cat:allp-list allp)))
			  allp))
	    (print (cat:? v (cat:? v (apply #'+ (mapcar #'car
							(cat:allp-list allp)))
				   allp)))))))


(test cobar-intr-hrzn-dffr
      (let* ((d (cat:soft-delta-infinity))
	     (r (cat:cobar-intr-hrzn-dffr (cat:cprd d))))
	(funcall r 0 (cat:allp))
	(funcall r 3 (cat:allp 3 (cat:d (cat:mask 5))))
	(funcall r 5 (cat:allp 3 (cat:d (cat:mask 5)) 2 (cat:d (cat:mask 4))))
	(funcall r 5 (cat:allp 2 (cat:d (cat:mask 4))
			       3 (cat:d (cat:mask 5))))))


#|
(test cobar-hrzn-dffr
      (cat:cat-init)
      (let ((h (cat:cobar-hrzn-dffr (cat:soft-delta-infinity)))
	    (allp (random-allp 4)))
	(dotimes (i 10) (print (random-allp 5)))
	(cat:? h (apply #'+ (mapcar #'car (cat:allp-list allp))) allp)
	(cat:? h (cat:? h (apply #'+ (mapcar #'car (cat:allp-list allp)))
			allp))
	(dotimes (i 10)
	  (let ((allp (random-allp 3)))
	    (print allp)
	    (print (cat:? h (apply #'+ (mapcar #'car (cat:allp-list allp)))
			  allp))
	    (print (cat:? h (cat:? h (apply #'+ (mapcar #'car
							(cat:allp-list allp)))
				   allp)))))))
|#

(defun random-allp1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat:mask 9)))
	     (dmns (1- (logcount gmsm))))
	(when (plusp dmns)
	  (push (cat:cbgn (1- dmns) gmsm) rslt))))
    (cat:make-allp :list rslt)))


#|
(test cobar
      (cat:cat-init)
      (let ((c (cat:cobar (cat:deltab)))
	    (allp (random-allp1 4)))
	(dotimes (i 10) (print (random-allp1 5)))
	(cat:? c (apply #'+ (mapcar #'car (cat:allp-list allp))) allp)
	(cat:? c (cat:? c (apply #'+ (mapcar #'car (cat:allp-list allp)))
			allp))
	(dotimes (i 10)
	  (let ((allp (random-allp1 3)))
	    (print allp)
	    (print (cat:? c (apply #'+ (mapcar #'car (cat:allp-list allp)))
			  allp))
	    (print (cat:? c (cat:? c (apply #'+ (mapcar #'car
							(cat:allp-list allp)))
				   allp)))))))
|#

(test ncmbn-cobar
      (cat:ncmbn-cobar nil)
      (cat:ncmbn-cobar (list (cat:cmbn 3 2 'a 3 'b)))
      (cat:ncmbn-cobar (list (cat:cmbn 1 2 'a 3 'b) (cat:cmbn 2 4 'c 5 'd)))
      (cat:ncmbn-cobar (list (cat:cmbn 1 2 'a 3 'b) (cat:cmbn 1 4 'c 5 'd)
			     (cat:cmbn 1 6 'e 7 'f))))


(test mrph-vrtc-cobar-intr
      (let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr :strt :cmbn))
	     (m (cat:build-mrph :sorc cc :trgt cc :degr 0 :intr
				#'(lambda (degr gnrt)
				    (cat:cmbn degr 2 gnrt 3 (1+ gnrt)))
				:strt :gnrt :orgn '(test)))
	     (r (cat:mrph-vrtc-cobar-intr m)))
	(funcall r 4 (cat:allp 2 3 2 4))))


(test vrtc-cobar
      (cat:cat-init)
      (let* ((f (cat:aw (cat:soft-delta-infinity) (cat:soft-delta-infinity)))
	     (cf (cat:vrtc-cobar f)))
	(cat:? cf 2 (cat:allp 1 (cat:crpr 0 (cat:d 7) 0 (cat:d 7))
			      1 (cat:crpr 0 (cat:d 56) 0 (cat:d 56))))))


(test hmtp-vrtc-cobar-intr
      (cat:cat-init)
      (let* ((ez (cat:ez (cat:delta-infinity) (cat:delta-infinity)))
	     (h (cat:h ez))
	     (gf (cat:cmps (cat:g ez) (cat:f ez)))
	     (r (cat:hmtp-vrtc-cobar-intr h gf)))
	(funcall r 3 (cat:allp 1 (cat:crpr 0 7 0 7)
			       1 (cat:crpr 0 14 0 14)
			       1 (cat:crpr 0 14 0 14)))))


#|
(test rdct
      (cat:cat-init)
      (let* ((tcc (cat:build-chcm
		   :cmpr #'cat:s-cmpr
		   :basis #'(lambda (degr) '(a b c d))
		   :bsgn 'd
		   :intr-dffr #'(lambda (degr gnrt)
				  (ecase gnrt
				    (a (cat:cmbn (1- degr) 1 'b 1 'd))
				    ((b d) (cat:cmbn (1- degr)))
				    (c (cat:cmbn (1- degr) 1 'd))))
		   :strt :gnrt
		   :orgn '(tcc)))
	     (bcc (cat:build-chcm
		   :cmpr #'cat:s-cmpr
		   :basis #'(lambda (degr) '(c d))
		   :bsgn 'd
		   :intr-dffr #'(lambda (degr gnrt)
				  (ecase gnrt
				    (d (cat:cmbn (1- degr)))
				    (c (cat:cmbn (1- degr) 1 'd))))
		   :strt :gnrt
		   :orgn '(bcc)))
	     (f (cat:build-mrph :sorc tcc :trgt bcc :degr 0
				:intr #'(lambda (degr gnrt)
					  (ecase gnrt
					    (a (cat:cmbn degr 1 'c 1 'd))
					    (b (cat:cmbn degr))
					    ((c d) (cat:cmbn degr 1 gnrt))))
				:strt :gnrt :orgn '(f)))
	     (g (cat:build-mrph :sorc bcc :trgt tcc :degr 0
				:intr #'identity :strt :cmbn :orgn '(g)))
	     (h (cat:build-mrph :sorc tcc :trgt tcc :degr +1
				:intr #'(lambda (degr gnrt)
					  (ecase gnrt
					    ((a b) (cat:cmbn (1+ degr) 1 'a
							     -1 'b -1 'c
							     -1 'd))
					    ((c d) (cat:cmbn (1+ degr)))))
				:strt :gnrt :orgn '(h)))
	     (rdct (cat:build-rdct :f f :g g :h h :orgn '(rdct)))
	     cobar)
	(cat:tcc rdct 3 'a)
	(cat:g rdct (cat:f rdct 3 'a))
	(cat:h rdct 3 'a)
	(setf cobar (cat:vrtc-cobar rdct))
	(cat:pre-check-rdct cobar)
	(aleat-tc)
	;;(aleat-bc)
	;;(loop (c))
	;; degrees >= 15 is possible => error.
	))
|#

#|
(cat-init)
(setf tcc (build-chcm
	   :cmpr #'s-cmpr
	   :basis #'(lambda (degr) '(a b c d))
	   :bsgn 'd
	   :intr-dffr #'(lambda (degr gnrt)
			  (ecase gnrt
			    (a (cmbn (1- degr) 1 'b 1 'd))
			    ((b d) (cmbn (1- degr)))
			    (c (cmbn (1- degr) 1 'd))))
	   :strt :gnrt
	   :orgn '(tcc)))
(setf bcc (build-chcm
	   :cmpr #'s-cmpr
	   :basis #'(lambda (degr) '(c d))
	   :bsgn 'd
	   :intr-dffr #'(lambda (degr gnrt)
			  (ecase gnrt
			    (d (cmbn (1- degr)))
			    (c (cmbn (1- degr) 1 'd))))
	   :strt :gnrt
	   :orgn '(bcc)))
(setf f (build-mrph :sorc tcc :trgt bcc :degr 0
		    :intr #'(lambda (degr gnrt)
			      (ecase gnrt
				(a (cmbn degr 1 'c 1 'd))
				(b (cmbn degr))
				((c d) (cmbn degr 1 gnrt))))
		    :strt :gnrt :orgn '(f)))
(setf g (build-mrph :sorc bcc :trgt tcc :degr 0
		    :intr #'identity :strt :cmbn :orgn '(g)))
(setf h (build-mrph :sorc tcc :trgt tcc :degr +1
		    :intr #'(lambda (degr gnrt)
			      (ecase gnrt
				((a b) (cmbn (1+ degr) 1 'a -1 'b -1 'c -1 'd))
				((c d) (cmbn (1+ degr)))))
		    :strt :gnrt :orgn '(h)))
(setf rdct (build-rdct :f f :g g :h h :orgn '(rdct)))
(tcc rdct 3 'a)
(g rdct (f rdct 3 'a))
(h rdct 3 'a)
(setf cobar (vrtc-cobar rdct))
(pre-check-rdct cobar)
(defun aleat-tc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (+ 65 (random 4))) 'string))
	     (intern (coerce (vector (+ 65 (random 4))) 'string)))
       (rslt nil (cons (cbgn degr gnrt) rslt)))
      ((> tdegr 10) (setf *tc* (cmbn tdegr 1 (make-allp :list rslt))))))
(aleat-tc)
(defun aleat-bc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (+ 67 (random 2))) 'string))
	     (intern (coerce (vector (+ 67 (random 2))) 'string)))
       (rslt nil (cons (cbgn degr gnrt) rslt)))
      ((> tdegr 10) (setf *bc* (cmbn tdegr 1 (make-allp :list rslt))))))
(aleat-bc)
(defun c ()
  (aleat-tc)
  (aleat-bc)
  (check-rdct))
(loop (c))  ;; degrees >= 15 is possible => error.
|#


#|
(test left-hmeq
      (cat:cat-init)
      (let* ((h (cat:left-hmeq (cat:sphere 3)))
	     (c (cat:cobar h)))
	(inspect c)))
|#
