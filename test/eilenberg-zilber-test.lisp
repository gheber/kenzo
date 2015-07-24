
(in-package :kenzo-test)

(in-suite :kenzo)


(test shuffle-sign
      (dotimes (i 30)
	(format t "~%~A => ~@D" (cat:hyphenize-list (cat:dlop-int-ext i))
		(cat:shuffle-sign i))))


(test intr-eml
      (cat:intr-eml (cat:cmbn 3 1 (cat:tnpr 0 'a 3 'b)
			      2 (cat:tnpr 2 'a 1 'b)
			      3 (cat:tnpr 2 'aa 1 'bb)
			      4 (cat:tnpr 3 'a 0 'b)))
      (cat:intr-eml (cat:cmbn 3 1 (cat:tnpr 1 'c 2 'd)
			      10 (cat:tnpr 2 'a 1 'b)))
      (cat:intr-eml (cat:cmbn 3 10 (cat:tnpr 1 'c 2 'd)
			      1 (cat:tnpr 2 'a 1 'b)
			      100 (cat:tnpr 2 'aa 1 'bb)))
      (cat:intr-eml (cat:cmbn 3 10 (cat:tnpr 1 'c 2 'd)
			      100 (cat:tnpr 1 'cc 2 'dd)
			      1 (cat:tnpr 2 'a 1 'b))))


(test eml
      (cat:cat-init)
      (let* ((d (cat:delta-infinity))
	     (eml (cat:eml d d)))
	(cat:? eml (cat:cmbn 3 1 (cat:tnpr 0 1 3 30)
			     10 (cat:tnpr 1 3 2 28)
			     100 (cat:tnpr 2 7 1 24)
			     1000 (cat:tnpr 3 15 0 16)))))


(test intr-phi
      (cat:cat-init)
      (let* ((d (cat:delta-infinity))
	     (rslt (cat:intr-phi d d)))
	(funcall rslt (cat:cmbn 3 1 (cat:crpr 0 15 0 15)))
	(funcall rslt (cat:cmbn 2 1 (cat:crpr 0 7 0 7)))
	(funcall rslt (cat:cmbn 1 1 (cat:crpr 0 3 0 3)))
	(funcall rslt (cat:cmbn 0 1 (cat:crpr 0 1 0 1)))))


(test phi
      (cat:cat-init)
      (let ((phi (cat:phi (cat:delta-infinity) (cat:delta-infinity))))
	(cat:? phi 3 (cat:crpr 0 15 0 15))))


(test eat
      (cat:cat-init)
      (let* ((s (cat:sphere 5))
	     (p (cat:crts-prdc s s))
	     (b (cat:basis p 8))
	     (c (cat:make-cmbn :degr 8
			       :list (mapcar #'(lambda (item)
						 (cat:term (1+ (random 5))
							   item))
					     b)))
	     (phi (cat:phi s s)))
	(time (cat:? phi c))))


(test intr-aw
      (cat:cat-init)
      (let* ((r (cat:intr-aw #'cat:delta-face #'cat:delta-face))
	     (s (cat:sphere 3))
	     (f (cat:face s)))
	(funcall r 3 (cat:crpr 0 15 0 15))
	(setf r (cat:intr-aw f f))
	(funcall r 3 (cat:crpr 0 's3 0 's3))))


(test aw
      (cat:cat-init)
      (let ((aw (cat:aw (cat:delta-infinity) (cat:delta-infinity))))
	(cat:? aw (cat:cmbn 3 1 (cat:crpr 0 15 0 30)
			    -1 (cat:crpr 0 23 0 29)))))


(test ez
      (cat:cat-init)
      (let ((ez (cat:ez (cat:delta-infinity) (cat:delta-infinity))))
	(setf cat:*bc* (cat:cmbn 3 1 (cat:tnpr 0 1 3 30)
				 10 (cat:tnpr 1 96 2 896)
				 100 (cat:tnpr 2 7168 1 3)
				 1000 (cat:tnpr 3 15 0 4096)))
	(setf cat:*tc* (cat:cmbn 3 1 (cat:crpr 0 15 0 480)
				 100 (cat:crpr 0 15 5 96)
				 10 (cat:crpr 6 3 0 480)
				 1000 (cat:crpr 6 3 1 224)))
	(cat:pre-check-rdct ez)
	(check-rdct)))


(test eat1
      (cat:cat-init)
      (let* ((s (cat:sphere 5))
	     (ez (cat:ez s s))
	     (b (cat:basis (cat:tcc ez) 8))
	     (c (cat:make-cmbn :degr 8
			       :list (mapcar #'(lambda (item)
						 (cat:term (1+ (random 5))
							   item))
					     b))))
	(cat:pre-check-rdct ez)
	(setf cat:+too-much-time+ -1)
	(time (cat:? cat:*id-gf-dh-hd* c))
	(time (cat:? cat:*id-gf-dh-hd* c))))


(test crts-prdc
      (cat:cat-init)
      (let* ((k (cat:k-z 2))
	     (k2 (cat:crts-prdc k k)))
	(cat:homology k2 0 10)))
