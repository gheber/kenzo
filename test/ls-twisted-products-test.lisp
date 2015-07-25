
(in-package :kenzo-test)

(in-suite :kenzo)


(test absm-loopabsm
      (let ((cmpr (cat:cmpr (cat:deltab))))
	(cat:absm-loopabsm cmpr 3 (cat:absm 4 7) (cat:absm 7 cat:+null-loop+))
	(cat:absm-loopabsm cmpr 3 (cat:absm 0 15) (cat:absm 7 cat:+null-loop+))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 7 4)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 7 4 1 12 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 7 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 7 -1 1 12 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 7 4)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 0 (cat:loop3 2 7 4 1 14 -1)))
	;; in principle illegal but works
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 0 (cat:loop3 2 7 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 0 (cat:loop3 2 7 -1 1 14 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 0 (cat:loop3 0 15 -1 1 14 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 0 15)
			   (cat:absm 2 (cat:loop3 0 7 -1 0 14 -1)))
	(cat:absm-loopabsm cmpr 3 (cat:absm 2 7)
			   (cat:absm 2 (cat:loop3 0 28 -1 0 14 -1)))))


(test twisted-crts-prdc
      (cat:cat-init)
      (let* ((p (cat:r-proj-space 3))
	     (tw (cat:twisted-crts-prdc p))
	     (cmpr (cat:cmpr tw))
	     (c (random-cmbn cmpr 8 10 4 4 100)))
	(time (cat:? tw (cat:? tw c)))
	(time (cat:? tw (cat:? tw c)))))


(test dtau-d-intr
      (cat:cat-init)
      (let* ((d (cat:deltab))
	     (p (cat:crts-prdc d (cat:loop-space d)))
	     (tw (cat:twisted-crts-prdc d))
	     (crts-prdc-cmpr (cat:cmpr p))
	     (crts-prdc-face (cat:face p))
	     (twisted-crts-prdc-face (cat:face tw))
	     (delta (cat:dtau-d-intr crts-prdc-cmpr crts-prdc-face
				     twisted-crts-prdc-face)))
	(funcall delta 3 (cat:crpr 0 15 0 (cat:loop3 0 (- 2048 64) 1)))
	(funcall delta 5 (cat:crpr 10 15 5 (cat:loop3 0 (- 2048 64) 1)))
	(funcall delta 5 (cat:crpr 5 15 10 (cat:loop3 0 (- 2048 64) 1)))))


(test szczarba
      (cat:cat-init)
      (let ((rdct (cat:szczarba (cat:deltab)))
	    bcc)
	(cat:pre-check-rdct rdct)
	(setf cat:*tc* (cat:cmbn 2 1 (cat:crpr 0 7 0
					       (cat:loop3 0 (- 512 32) 1)))
	      cat:*bc* (cat:cmbn 2 1 (cat:tnpr 0 1 2
					       (cat:loop3 0 (- 512 32) 1))
				 10 (cat:tnpr 1 (- 4096 1024) 1
					      (cat:loop3 0 7 1))
				 100 (cat:tnpr 2 7 0
					       (cat:loop3 0 (- 4096 1024) 1))))
	(check-rdct)
	(setf bcc (cat:bcc rdct))
	(time (cat:? bcc 5 (cat:tnpr 5 (cat:mask 6) 0 cat:+null-loop+)))
	(time (cat:? bcc 6 (cat:tnpr 6 (cat:mask 7) 0 cat:+null-loop+)))))


(test pop-first-absm
      (cat:pop-first-absm 4 (cdr (cat:loop-list cat:+null-loop+)))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a -2))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a +2))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a -1))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a +1))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 1 'a -2 2 'b 3))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a +2 2 'b -3))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 1 'a -1 2 'b 3))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 0 'a +1 2 'b 5))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 2 'a -1 1 'b 3))))
      (cat:pop-first-absm 4 (cdr (cat:loop-list (cat:loop3 2 'a +1 1 'b 3)))))


(test crts-contraction-intr
      (cat:cat-init)
      (let* ((delta (cat:deltab))
	     (tw (cat:twisted-crts-prdc delta))
	     (d (cat:bndr tw))
	     (base-cmpr (cat:cmpr delta))
	     (base-bspn (cat:bspn delta))
	     (base-face (cat:face delta))
	     (crts-cmpr (cat:cmpr tw))
	     (r (cat:crts-contraction-intr base-cmpr base-bspn base-face
					   crts-cmpr))
	     (h (cat:build-mrph :sorc tw :trgt tw :degr +1
				:intr r :strt :gnrt
				:orgn `(crts-contraction ,delta)))
	     (z (cat:i-sbtr (cat:idnt-mrph tw) (cat:cmps d h) (cat:cmps h d))))
	(cat:? z 0 (cat:crpr 0 1 0 cat:+null-loop+))
	(funcall r 0 (cat:crpr 0 1 0 (cat:loop3 0 96 1)))
	(cat:? z 0 (cat:crpr 0 1 0 (cat:loop3 0 96 1)))
	(cat:? z 0 (cat:crpr 0 1 0 (cat:loop3 0 96 1 0 (+ 256 128) 1)))
	(cat:? z 0 (cat:crpr 0 1 0 (cat:loop3 0 96 1 0 (+ 256 128)
					      1 0 (+ 512 1024) 1)))
	(cat:? z 0 (cat:crpr 0 1 0 (cat:loop3 0 96 2)))
	(cat:? z 1 (cat:crpr 0 3 0 (cat:loop3 0 (+ 32 64 128) 1)))
	(cat:? z 2 (cat:crpr 0 7 0 (cat:loop3 0 (+ 32 64 128 256) 1)))
	(cat:? z 3 (cat:crpr 2 7 1 (cat:loop3 2 (+ 32 64 128)
					      2 4 (+ 32 64 128) -2)))
	(cat:? z 3 (cat:crpr 2 7 4 (cat:loop3 2 (+ 32 64 128)
					      2 1 (+ 32 64 128) -2)))
	(cat:? z 3 (cat:crpr 2 7 1 (cat:loop3 2 (+ 32 64 128)
					      -2 4 (+ 32 64 128) 2)))
	(cat:? z 3 (cat:crpr 2 7 4 (cat:loop3 2 (+ 32 64 128)
					      -2 1 (+ 32 64 128) 2)))
	(cat:? z 3 (cat:crpr 1 7 2 (cat:loop3 2 (+ 32 64 128)
					      2 4 (+ 32 64 128) -2)))
	(cat:? z 3 (cat:crpr 4 7 2 (cat:loop3 2 (+ 32 64 128)
					      2 1 (+ 32 64 128) -2)))
	(cat:? z 3 (cat:crpr 1 7 2 (cat:loop3 2 (+ 32 64 128)
					      -2 4 (+ 32 64 128) 2)))
	(cat:? z 3 (cat:crpr 4 7 2 (cat:loop3 2 (+ 32 64 128)
					      -2 1 (+ 32 64 128) 2)))))


(test crts-contraction
      (cat:cat-init)
      (let* ((p (cat:r-proj-space 3))
	     (tw (cat:twisted-crts-prdc p))
	     (cmpr (cat:cmpr tw))
	     (r (cat:crts-contraction-intr (cat:cmpr p) (cat:bspn p)
					   (cat:face p) (cat:cmpr tw)))
	     (h (cat:build-mrph :sorc tw :trgt tw :degr +1
				:intr r :strt :gnrt
				:orgn `(cat:crts-contraction ,p)))
	     (d (cat:bndr tw))
	     (z (cat:i-sbtr (cat:idnt-mrph tw) (cat:cmps h d) (cat:cmps d h)))
	     (c (random-cmbn cmpr 5 10 2 2 1)))
	(cat:? z c)
	(setf c (random-cmbn cmpr 6 10 3 3 3))
	(cat:? z c)
	(setf c (random-cmbn cmpr 7 10 4 4 6))
	(cat:? z c)
	(setf c (random-cmbn cmpr 8 10 4 4 20))
	(cat:? z c)
	(cat:? h c)
	(cat:? d (cat:? h c))
	(print (length (cat:cmbn-list (cat:? d (cat:? h c)))))))


(test crts-contraction1
      (let* ((h (cat:crts-contraction (cat:deltab)))
	     (d (cat:bndr (cat:twisted-crts-prdc (cat:deltab))))
	     (z (cat:i-sbtr (cat:idnt-mrph (cat:sorc d)) (cat:cmps d h)
			    (cat:cmps h d))))
	(cat:? z 3 (cat:crpr 4 7 2 (cat:loop3 2 (+ 32 64 128)
					      -2 1 (+ 32 64 128) 2)))))


(test tnpr-contraction
      (cat:cat-init)
      (let* ((delta (cat:deltab))
	     (sz (cat:szczarba (cat:deltab)))
	     (tw (cat:bcc sz))
	     (h (cat:tnpr-contraction delta))
	     (z (cat:i-sbtr (cat:idnt-mrph tw) (cat:cmps tw h)
			    (cat:cmps h tw))))
	(time (cat:? z 3 (cat:tnpr 0 1 3 (cat:loop3 0 (cat:mask 5)
						    2 0 (* 32 (cat:mask 5))
						    -1))))
	(cat:? h 4 (cat:tnpr 0 1 0 (cat:loop3 0 (cat:mask 6) 1)))))
