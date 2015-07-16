
(in-package :kenzo-test)

(in-suite :kenzo)

(test k-z-1-cmpr
      (is (equal :less (cat:k-z-1-cmpr '(1 1 2) '(1 2 2))))
      (is (equal :equal (cat:k-z-1-cmpr '(1 1 2) '(1 1 2))))
      (is (equal :greater (cat:k-z-1-cmpr '(1 1 2) '(1 1 -1)))))

(test k-z-1-face
      (cat:k-z-1-face 0 1 '(3))
      (cat:k-z-1-face 1 1 '(3))
      (cat:k-z-1-face 0 3 '(1 2 3))
      (cat:k-z-1-face 1 3 '(1 2 3))
      (cat:k-z-1-face 2 3 '(1 2 3))
      (cat:k-z-1-face 2 3 '(1 2 -2))
      (cat:k-z-1-face 3 3 '(1 2 3)))

(test z-absm-bar
      (cat:z-absm-bar (cat:absm 0 '()))
      (cat:z-absm-bar (cat:absm 1 '()))
      (cat:z-absm-bar (cat:absm 0 '(2)))
      (dotimes (i 8)
	(print (cat:z-absm-bar (cat:absm i '(3 6))))))


(test z-bar-absm
      (cat:z-bar-absm (cat:z-absm-bar (cat:absm 0 '())))
      (cat:z-bar-absm (cat:z-absm-bar (cat:absm 1 '())))
      (cat:z-bar-absm (cat:z-absm-bar (cat:absm 0 '(2))))
      (dotimes (i 8)
	(print (cat:z-bar-absm (cat:z-absm-bar (cat:absm i '(3 6)))))))


(test k-z-1-grml
      (cat:k-z-1-grml 0 (cat:crpr 0 nil 0 nil))
      (cat:k-z-1-grml 1 (cat:crpr 0 '(3) 0 '(4)))
      (cat:k-z-1-grml 1 (cat:crpr 0 '(3) 0 '(-3)))
      (cat:k-z-1-grml 1 (cat:crpr 0 '(3) 1 '()))
      (cat:k-z-1-grml 1 (cat:crpr 1 '() 0 '(3)))
      (cat:k-z-1-grml 1 (cat:crpr 1 '() 1 '()))
      (cat:k-z-1-grml 5 (cat:crpr 24 '(2 2 3) 20 '(-2 2 4))))


(test k-z-1-grin
      (cat:k-z-1-grin 3 '(2 3 -4))
      (let ((gmsm '(2 3 -4)))
	(cat:k-z-1-grml 3 (cat:crpr 0 gmsm 0
				    (cat:gmsm (cat:k-z-1-grin 3 gmsm))))))


(defun aleat-list (max length)
  (let ((rslt nil)
	(2max (+ max max)))
    (dotimes (i length)
      (push (let ((k (- (random 2max) max)))
	      (if (zerop k)
		  max
		  k))
	    rslt))
    rslt))


(test k-z-1
      (let ((k (cat:k-z-1))
	    zero?)
	(cat:? k (cat:? k 14 (aleat-list 200 14)))
	(setf zero? (cat:add (cat:idnt-mrph k) (cat:grin k)))
	(cat:? zero? 14 (aleat-list 200 14))))


(test circle
      (let ((c (cat:circle)))
	(cat:cmpr c '* '*)
	(cat:basis c 1)
	(cat:? c 1 's1)))


(test kz1-rdct-f-intr
      (cat:kz1-rdct-f-intr (cat:cmbn 0))
      (cat:kz1-rdct-f-intr (cat:cmbn 0 4 '()))
      (cat:kz1-rdct-f-intr (cat:cmbn 1))
      (cat:kz1-rdct-f-intr (cat:cmbn 1 4 '(3)))
      (cat:kz1-rdct-f-intr (cat:cmbn 1 4 '(3) 5 '(2)))
      (cat:kz1-rdct-f-intr (cat:cmbn 1 4 '(3) -3 '(4)))
      (cat:kz1-rdct-f-intr (cat:cmbn -3)))


(test kz1-rdct-h-intr
      (cat:kz1-rdct-h-intr 0 nil)
      (cat:kz1-rdct-h-intr 1 '(-4))
      (cat:kz1-rdct-h-intr 1 '(-1))
      (cat:kz1-rdct-h-intr 1 '(1))
      (cat:kz1-rdct-h-intr 1 '(4))
      (cat:kz1-rdct-h-intr 3 '(-4 3 5))
      (cat:kz1-rdct-h-intr 3 '(-1 3 -1))
      (cat:kz1-rdct-h-intr 3 '(1 2 2))
      (cat:kz1-rdct-h-intr 3 '(4 3 5)))


(test kz1-rdct
      (cat:cat-init)
      (cat:pre-check-rdct (cat:kz1-rdct))
      (setf cat:*tc* (cat:cmbn 0 1 '()))
      (setf cat:*bc* (cat:cmbn 0 1 '*))
      (check-rdct)
      (setf cat:*tc* (cat:cmbn 1 1 '(-4) 10 '(-1) 100 '(1) 1000 '(5)))
      (setf cat:*bc* (cat:cmbn 1 1 's1))
      (check-rdct)
      (setf cat:*tc* (cat:cmbn 2 1 '(-4 2) 10 '(-1 3) 100 '(1 -4) 1000 '(5 5)))
      (check-rdct)
      (setf cat:*tc* (cat:cmbn 3 1 '(-4 -4 5) 10 '(-1 2 1)
			       100 '(1 4 2) 1000 '(5 1 -1)))
      (check-rdct))


(test k-z-1-homology
      (cat:cat-init)
      (cat:homology (cat:k-z-1) 1))


(test k-z2-1-homology
      (let ((k (cat:k-z2-1)))
	(cat:homology k 0 4)))


(test k-z2-homology
      (let ((k3 (cat:k-z2 3)))
	(cat:homology k3 7)))


(test z2-bar-absm
      (dotimes (i 8)    ;;; not really legal
	(dotimes (j 8)
	  (print (cat:z2-bar-absm (cat:z2-absm-bar (cat:absm i j)))))))


(test z-fundamental-gmsm
      (cat:z-fundamental-gmsm 1 33)
      (cat:z-fundamental-gmsm 2 33)
      (cat:z-fundamental-gmsm 3 33)
      (cat:z-fundamental-gmsm 4 33))


(test interesting-faces
      (let* ((d (cat:delta 14))
	     (f (cat:face d)))
	(do ((i 5 (1- i)))
	    ((minusp i))
	  (print (cat:interesting-faces f i 5 (cat:mask 6))))))


(test gmsm-cocycle
      (cat:cat-init)
      (let* ((d (cat:delta 10))
	     (chml-clss (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -2
					:intr #'(lambda (dmns gmsm)
						  (cat:term-cmbn 0 gmsm
								 :gnrt-z))
					:strt :gnrt
					:orgn '(essai-1))))
	(cat:gmsm-cocycle (cat:face d) 2 4 31 chml-clss)
	(setf chml-clss (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -1
					:intr #'(lambda (dmns gmsm)
						  (cat:term-cmbn 0 gmsm
								 :gnrt-z))
					:strt :gnrt
					:orgn '(essai-2)))
	(cat:gmsm-cocycle (cat:face d) 1 4 31 chml-clss)))

(test z-cocycle-gbar
      (cat:cat-init)
      (let* ((d (cat:delta 10))
	     (chml-clss (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -1
					:intr #'(lambda (dmns gmsm)
						  (cat:term-cmbn 0 gmsm
								 :gnrt-z))
					:strt :gnrt
					:orgn '(essai-1))))
	(cat:gmsm-cocycle (cat:face d) 1 4 31 chml-clss)
	(cat:z-cocycle-gbar 1 4 (cat:gmsm-cocycle (cat:face d)
						  1 4 31 chml-clss))
	(setf chml-clss
	      (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -2
			      :intr #'(lambda (dmns gmsm)
					(cat:term-cmbn 0 gmsm :gnrt-z))
			      :strt :gnrt
			      :orgn '(essai-2)))
	(cat:gmsm-cocycle (cat:face d) 2 2 7 chml-clss)
	(cat:z-cocycle-gbar 2 2 (cat:gmsm-cocycle (cat:face d)
						  2 2 7 chml-clss))
	(cat:gmsm-cocycle (cat:face d) 2 2 0 chml-clss) ;; normally illegal
	(cat:z-cocycle-gbar 2 2 (cat:gmsm-cocycle (cat:face d)
						  2 2 0 chml-clss))
	(cat:gmsm-cocycle (cat:face d) 2 3 15 chml-clss)
	(cat:z-cocycle-gbar 2 3 (cat:gmsm-cocycle (cat:face d)
						  2 3 15 chml-clss))
	(cat:gmsm-cocycle (cat:face d) 2 4 31 chml-clss)
	(cat:z-cocycle-gbar 2 4 (cat:gmsm-cocycle (cat:face d)
						  2 4 31 chml-clss))
	(setf chml-clss
	      (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -3
			      :intr #'(lambda (dmns gmsm)
					(cat:term-cmbn 0 gmsm :gnrt-z))
			      :strt :gnrt
			      :orgn '(essai-3)))
	(cat:gmsm-cocycle (cat:face d) 3 4 31 chml-clss)
	(cat:z-cocycle-gbar 3 4 (cat:gmsm-cocycle (cat:face d)
						  3 4 31 chml-clss))
	(setf chml-clss
	      (cat:build-mrph :sorc d :trgt (cat:z-chcm) :degr -3
			      :intr #'(lambda (dmns gmsm)
					(cat:zero-cmbn 0))
			      :strt :gnrt
			      :orgn '(essai-33)))
	(cat:gmsm-cocycle (cat:face d) 3 4 31 chml-clss)
	(cat:z-cocycle-gbar 3 4 (cat:gmsm-cocycle (cat:face d)
						  3 4 31 chml-clss))))
