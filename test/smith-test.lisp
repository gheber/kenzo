
(in-package :kenzo-test)

(in-suite :kenzo)

(test random-matrix
      (cat:random-matrix 2 3 10))


(test idnt-mtrx
      (cat:idnt-mtrx 3))


(test copy-mtrx
      (let ((m (cat:random-matrix 3 4 10)))
	(is (equalp m (cat:copy-mtrx m)))))


(test left-submatrix
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:left-submatrix m 2)))


(test mtrx-prdc
      (let ((m1 (cat:random-matrix 2 3 10))
	    (m2 (cat:random-matrix 3 2 10)))
	(cat:mtrx-prdc m1 m2)
	(cat:mtrx-prdc m2 m1)))


(test chcm-mtrx
      (cat:cat-init)
      (let ((d (cat:delta 5))
	    (m (cat:moore 2 2)))
	(cat:chcm-mtrx d 3)
	(dotimes (i 5)
	  (print (cat:chcm-mtrx m i)))
	(dotimes (i 6)
	  (print (array-dimensions (cat:chcm-mtrx m i))))))


(test line-op
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:line-op m 1 3 2 0)))

;; mtrx-list = (P P^-1 M Q Q^-1)

(test line-op-5
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5) )
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:line-op-5 list 0 3 1 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)))


(test column-op
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:column-op m 1 3 2 0)))


(test column-op-5
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:column-op-5 list 0 3 1 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test line-swap
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:line-swap m 1 0 2)))


(test line-swap-5
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:line-swap-5 list 0 1 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test column-swap
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:column-swap m 1 0 2)))


(test column-swap-5
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:column-swap-5 list 0 1 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test column-minus
      (let ((m (cat:random-matrix 3 4 10)))
	(cat:line-minus m 1 2)
	(cat:column-minus m 1 2)))


(test column-minus-5
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:line-minus-5 list 0 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)
	(cat:column-minus-5 list 0 2)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test minimal-term
      (let ((m (cat:random-matrix 4 5 10)))
	(cat:minimal-term m 1)))


#|
;; potential divide by 0
(test minimal-rest-1
      (let ((m (cat:random-matrix 4 5 10)))
	(cat:minimal-rest-1 m 1)))

(test minimal-rest-2
      (let ((m (cat:random-matrix 4 5 10)))
	(cat:minimal-rest-2 m 1)))
|#


(test minimal-term-top-left
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:minimal-term-top-left list 0 1 3)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test minimal-term-top-left
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:pivott list 0)
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))

(test list-smith
      (let* ((p (cat:idnt-mtrx 4))
	     (p-1 (cat:idnt-mtrx 4))
	     (m (cat:random-matrix 4 5 10))
	     (q (cat:idnt-mtrx 5))
	     (q-1 (cat:idnt-mtrx 5))
	     (list (list p p-1 m q q-1))
	     (t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(third (cat:list-smith list))
	(is (equalp t1 (cat:mtrx-prdc p (cat:mtrx-prdc m q-1))))
	(cat:mtrx-prdc p p-1)
	(cat:mtrx-prdc q q-1)))


(test gnrt-name-basis
      (cat:gnrt-name 4)
      (cat:gnrt-name-basis 4))


(test echcm-kill-epi-f-intr
      (let* ((q-1 (cat:random-matrix 5 5 10))
	     (f (cat:echcm-kill-epi-f-intr #'cat:s-cmpr 2 2 5 '(a b c d e)
					   (list 0 0 0 0 q-1))))
	(funcall f (cat:cmbn 2 1 'a))
	(funcall f (cat:cmbn 4 1 'a))
	(funcall f (cat:cmbn 3 1 'a 10 'b 100 'c 1000 'd 10000 'e))))

#|
(test echcm-kill-epi-g-intr
      (let* ((q (cat:random-matrix 5 5 10))
	     (g (cat:echcm-kill-epi-g-intr 2 2 5 '(a b c d e)
					   (list 0 0 0 q 0))))
	(funcall g (cat:cmbn 2))
	(funcall g (cat:cmbn 4 1 'a))
	(funcall g (cat:cmbn 3 1 :gn-0 10 :gn-1 100 :gn-2))))
|#

(test echcm-kill-epi-h-intr
      (let* ((p-1 (cat:random-matrix 2 2 10))
	     (q (cat:random-matrix 5 5 10))
	     (h (cat:echcm-kill-epi-h-intr #'cat:s-cmpr 2 2 5 '(a b)
					   '(a b c d e)
					   (list 0 p-1 0 q 0))))
	(funcall h (cat:cmbn 2 1 'a 1000 'b))
	(funcall h (cat:cmbn 4 1 'a))))


(test echcm-kill-epi
      (cat:cat-init)
      (let* ((s3 (cat:sphere 3))
	     (s3-chml-clss (cat:chml-clss s3 3))
	     (s3-fibration (cat:z-whitehead s3 s3-chml-clss))
	     (s3-4 (cat:fibration-total s3-fibration))
	     (ecc (cat:echcm s3-4))
	     (rdct (cat:echcm-kill-epi ecc 2))
	     s3-4-chml-clss s3-4-fibration s3-5
	     rdct1 rdct2 rdct3 rdct12 rdct123)
	(cat:pre-check-rdct rdct)
	(setf cat:*tc* (cat:cmbn 0 1 (cat:bsgn ecc))
	      cat:*bc* cat:*tc*)
	(check-rdct)
	(setf cat:*tc* (cat:cmbn 2 1 (first (cat:basis ecc 2))))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn 3 1 (first (cat:basis ecc 3))))
	(check-rdct)
	(setf cat:*tc* (cat:cmbn 4 1 (first (cat:basis ecc 4)))
	      cat:*bc* cat:*tc*)
	(check-rdct)
	(setf s3-4-chml-clss (cat:chml-clss s3-4 4))
	(setf s3-4-fibration (cat:z2-whitehead s3-4 s3-4-chml-clss))
	(setf s3-5 (cat:fibration-total s3-4-fibration))
	(setf ecc (cat:echcm s3-5))
	(dotimes (i 7)
	  (format t "~%DIM = ~D ; LENGTH = ~D" i (length (cat:basis ecc i))))
	(setf rdct1 (cat:echcm-kill-epi ecc 2))
	(setf rdct2 (cat:echcm-kill-epi (cat:bcc rdct1) 3))
	(setf rdct3 (cat:echcm-kill-epi (cat:bcc rdct2) 4))
	(setf rdct12 (cat:cmps rdct2 rdct1))
	(setf rdct123 (cat:cmps rdct3 rdct12))
	(cat:pre-check-rdct rdct123)
	(setf cat:*tc* (cat:cmbn 0 1 (cat:bsgn ecc)) cat:*bc* cat:*tc*)
	(check-rdct)
	(setf cat:*tc* (cat:cmbn 2 1 (first (cat:basis ecc 2))))
	(check-rdct)
	(let ((b3 (cat:basis ecc 3)))
	  (setf cat:*tc* (cat:cmbn 3 1 (first b3) 10 (second b3))))
	(check-rdct)
	(let ((b4 (cat:basis ecc 4)))
	  (setf cat:*tc* (cat:cmbn 4 1 (first b4) 10 (second b4))))
	(check-rdct)
	(let ((b5 (cat:basis ecc 5)))
	  (setf cat:*tc* (cat:cmbn 5 1 (first b5)
			       10 (second b5)
			       100 (third b5)
			       1000 (fourth b5))))
	(check-rdct)
	(let ((b6 (cat:basis ecc 6)))
	  (setf cat:*tc* (cat:cmbn 6 1 (first b6)
				   10 (second b6)
				   100 (third b6)
				   1000 (fourth b6)
				   10000 (fifth b6)
				   100000 (sixth b6)
				   1000000 (seventh b6))))
	(check-rdct)))


(test kill-epis
      (cat:cat-init)
      (let* ((s3 (cat:sphere 3))
	     (s3-chml-clss (cat:chml-clss s3 3))
	     (s3-fibration (cat:z-whitehead s3 s3-chml-clss))
	     (s3-4 (cat:fibration-total s3-fibration))
	     (s3-4-chml-clss (cat:chml-clss s3-4 4))
	     (s3-4-fibration (cat:z2-whitehead s3-4 s3-4-chml-clss))
	     (s3-5 (cat:fibration-total s3-4-fibration)))
	(time (cat:homology s3-5 6))
	(cat:kill-epis s3-5 2 5)
	(cat:homology s3-5 0 7)))


(test kill-epi
      (cat:cat-init)
      (let* ((s3 (cat:sphere 3))
	     (s3-chml-clss (cat:chml-clss s3 3))
	     (s3-fibration (cat:z-whitehead s3 s3-chml-clss))
	     (s3-4 (cat:fibration-total s3-fibration))
	     s3-4-chml-clss s3-4-fibration s3-5)
	(cat:kill-epi s3-4 2)
	(setf s3-4-chml-clss (cat:chml-clss s3-4 4))
	(setf s3-4-fibration (cat:z2-whitehead s3-4 s3-4-chml-clss))
	(setf s3-5 (cat:fibration-total s3-4-fibration))
	(time (cat:homology s3-5 6))))


(test chml-clss-intr
      (cat:cat-init)
      (let* ((s3 (cat:sphere 3))
	     (c (cat:chml-clss-intr s3 3))
	     (s3-chml-clss (cat:chml-clss s3 3))
	     (s3-fibration (cat:z-whitehead s3 s3-chml-clss))
	     (s3-4 (cat:fibration-total s3-fibration))
	     s3-4-chml-clss s3-4-fibration s3-5)
	(funcall c (cat:cmbn 3 5 's3))
	(cat:kill-epi s3-4 2)
	(setf c (cat:chml-clss-intr s3-4 4))
	(funcall c (cat:cmbn 4 5 (first (cat:basis (cat:echcm s3-4) 4))))
	(setf s3-4-chml-clss (cat:chml-clss s3-4 4))
	(setf s3-4-fibration (cat:z2-whitehead s3-4 s3-4-chml-clss))
	(setf s3-5 (cat:fibration-total s3-4-fibration))
	(cat:kill-epis s3-5 3 5)
	(setf c (cat:chml-clss-intr s3-5 5))
	(let ((b5 (cat:basis (cat:echcm s3-5) 5)))
	  (funcall c (cat:cmbn 5 1 (first b5) 10 (second b5))))))


(test chml-clss
      (cat:cat-init)
      (let* ((s3 (cat:sphere 3))
	     (s3-chml-clss (cat:chml-clss s3 3))
	     (s3-fibration (cat:z-whitehead s3 s3-chml-clss))
	     (s3-4 (cat:fibration-total s3-fibration))
	     s3-4-chml-clss s3-4-fibration s3-5 s3-5-chml-clss s3-5-fibration)
	(cat:homology s3-4 0 6)
	(cat:kill-epi s3-4 2)
	(setf s3-4-chml-clss (cat:chml-clss s3-4 4))
	(setf s3-4-fibration (cat:z2-whitehead s3-4 s3-4-chml-clss))
	(setf s3-5 (cat:fibration-total s3-4-fibration))
	(cat:homology s3-5 0 6)
	(cat:kill-epis s3-5 3 5)
	(setf s3-5-chml-clss (cat:chml-clss s3-5 5))
	(setf s3-5-fibration (cat:z2-whitehead s3-5 s3-5-chml-clss))
	(setf s3-6 (cat:fibration-total s3-5-fibration))
	(cat:homology s3-6 0 7)))
