
(in-package :kenzo-test)

(in-suite :kenzo)

(test abar
      (cat:abar )
      (cat:abar '(2 a 3 b))
      (cat:abar 2 'a 3 'b)
      (signals simple-error (cat:abar 2 'a 3)))

(test bar-cmpr
      (let ((r (cat:bar-cmpr #'cat:s-cmpr)))
	(is (equal :equal (funcall r (cat:abar) (cat:abar))))
	(is (equal :greater (funcall r (cat:abar 3 'a) (cat:abar))))
	(is (equal :less (funcall r (cat:abar 3 'a) (cat:abar 2 'a 1 'b))))
	(is (equal :less (funcall r (cat:abar 3 'a) (cat:abar 3 'b))))
	(is (equal :equal (funcall r (cat:abar 3 'a) (cat:abar 3 'a))))))

(test bar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
	(cat:bar-basis-length basis 2 1)
	(cat:bar-basis-length basis 2 2)
	(cat:bar-basis-length basis 3 1)
	(cat:bar-basis-length basis 3 2)
	(cat:bar-basis-length basis 4 1)
	(cat:bar-basis-length basis 4 2)
	(cat:bar-basis-length basis 4 3)
	(cat:bar-basis-length basis 4 4)
	(cat:bar-basis-length basis 8 1)
	(cat:bar-basis-length basis 8 2)
	(cat:bar-basis-length basis 8 3)
	(cat:bar-basis-length basis 8 4)
	(cat:bar-basis-length basis 8 5)
	(cat:bar-basis-length basis 8 6)
	(cat:bar-basis-length basis 8 11)))


(test bar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
	     (r (cat:bar-basis basis)))
	(funcall r 0)
	(funcall r 1)
	(funcall r 2)
	(dotimes (i 7)
	  (print (funcall r i)))
	(cat:bar-basis :locally-effective)))


(test bar-intr-vrtc-dffr
      (let* ((d (cat:soft-delta-infinity))
	     (r (cat:bar-intr-vrtc-dffr (cat:dffr d))))
	(funcall r 0 (cat:abar))
	(funcall r 3 (cat:abar 3 (cat:d 7)))
	(funcall r 5 (cat:abar 3 (cat:d 7) 2 (cat:d 3)))
	(funcall r 5 (cat:abar 2 (cat:d 3) 3 (cat:d 7)))))


(defun random-abar (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat:mask 7)))
	     (dmns (1- (logcount gmsm))))
	(when (plusp dmns)
	  (push (cat:brgn (1+ dmns) (cat:d gmsm)) rslt))))
    (cat:make-abar :list rslt)))


(test vrtc-bar
      (progn
	(cat:cat-init)
	(let ((v (cat:vrtc-bar (cat:soft-delta-infinity))))
	  (dotimes (i 10)
	    (print (random-abar 5)))
	  (dotimes (i 10)
	    (let ((abar (random-abar 3)))
	      (print abar)
	      (print (cat:? v (apply #'+ (mapcar #'car (cat:abar-list abar)))
			    abar))
	      (print (cat:? v (cat:? v (apply #'+ (mapcar #'car
							  (cat:abar-list abar)))
				     abar))))))))

#|
(test bar-intr-hrzn-dffr
      (let* ((k (cat:k-z-1))
	     (r (cat:bar-intr-hrzn-dffr (cat:aprd k))))
	(funcall r 0 (cat:abar ))
	(funcall r 3 (cat:abar 3 '(2 3)))
	(funcall r 6 (cat:abar 3 '(2 3) 3 '(-2 -3)))
	(funcall r 9 (cat:abar 3 '(2 3) 3 '(-2 -3) 3 '(2 3)))
	(funcall r 11 (cat:abar 3 '(2 3) 3 '(-2 -3) 2 '(-2) 3 '(-2 -3)))))
|#
