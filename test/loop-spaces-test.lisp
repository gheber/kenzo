
(in-package :kenzo-test)

(in-suite :kenzo)


(test normalize-loop
      (cat:normalize-loop 4 cat:+empty-list+)
      (cat:normalize-loop 4 '((1 a . 1)))
      (cat:normalize-loop 4 '((1 a . 1) (3 b . 1) (5 b . 1))))


(test unnormalize-loop
      (cat:unnormalize-loop (cat:absm 1 (cat:loop3 2 'a 3))))


(test loop3
      (cat:loop3 nil)
      (cat:loop3 '(1 a 2 3 b -2))
      (signals simple-error (cat:loop3 '(1 a 2 3 b b)))
      (signals simple-error (cat:loop3 '(1 a 2 3 b)))
      (cat:loop3 1 'a 2 3 'b 4)
      (signals simple-error (cat:loop3 1 'a 2 3 'b)))


(test loop-print
      (print (cat:loop3 nil))
      (print (cat:loop3 '(0 a 2)))
      (print (cat:loop3 '(1 a 2)))
      (print (cat:loop3 '(3 a 2 5 b -1 6 c 4)))
      (print (cat:loop3 '(7 a 1 11 b 1 13 c 1 14 d 1)))
      (print (cat:loop3 '(7 a 1 11 b 1 13 c 1 7 d 1)))
      (print (cat:loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))))


(test apowr-niloop
      (cat:apowr-niloop #'cat:s-cmpr '(0 a . 1) cat:+empty-list+)
      (cat:apowr-niloop #'cat:s-cmpr '(0 a . 1) '((1 a . 1)))
      (cat:apowr-niloop #'cat:s-cmpr '(0 a . 1) '((0 b . 1)))
      (cat:apowr-niloop #'cat:s-cmpr '(0 a . 1) '((0 a . 1) (0 b . 1)))
      (cat:apowr-niloop #'cat:s-cmpr '(0 a . 1) '((0 a . -1) (0 b . 1))))


(test loop-space-cmpr
      (let ((cmpr (cat:loop-space-cmpr #'cat:s-cmpr)))
	(is (equal :greater (funcall cmpr (cat:loop3 '(1 a 2))
				     (cat:loop3 '(1 a -1)))))
	(is (equal :greater (funcall cmpr (cat:loop3 '(3 b 4 1 a 2))
				     (cat:loop3 '(3 b 4 1 a -1)))))))


(test apowr-face4
      (cat:cat-init)
      (let ((face (cat:face (cat:delta-infinity))))
	(cat:apowr-face4 face 3 4 (cat:apowr 1 31 5))
	(setf face (cat:face (cat:sphere 5)))
	(cat:apowr-face4 face 2 5 (cat:apowr 1 's5 4))
	(cat:apowr-face4 face 2 5 (cat:apowr 2 's5 4))))


(test apowr-lastface4
      (cat:cat-init)
      (let ((cmpr #'cat:f-cmpr)
	    (face (cat:face (cat:delta-infinity))))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 63 1))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 63 -1))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 63 3))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 63 -3))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 8 31 1))
	(setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
		       (case indx
			 (5 (cat:absm 8 'a))
			 (4 (cat:absm 8 'a)))))
	(setf cmpr #'cat:s-cmpr)
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 'a 2))
	(setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
		       (case indx
			 (5 (cat:absm 1 'a))
			 (4 (cat:absm 8 'a)))))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 'a 2))
	(setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
		       (case indx
			 (5 (cat:absm 1 'a))
			 (4 (cat:absm 1 'a)))))
	(cat:apowr-lastface4 cmpr face 4 (cat:apowr 0 'a 2))))


(test loop-space-face
      (cat:cat-init)
      (let* ((cmpr #'cat:f-cmpr)
	     (face (cat:face (cat:delta-infinity)))
	     (rslt (cat:loop-space-face cmpr face)))
	(funcall rslt 2 4 (cat:loop3 '(0 63 3 1 31 -3)))
	(funcall rslt 2 4 (cat:loop3 '(0 63 3 0 63 -3)))
	(funcall rslt 4 4 (cat:loop3 '(0 63 3 0 63 -3)))
	(funcall rslt 4 4 (cat:loop3 '(0 63 3)))))


(test loop-space-grin-sintr
      (cat:loop-space-grin-sintr 5 (cat:loop3 0 'a 2 3 'b -3)))


(test loop-space-grml-sintr
      (let* ((cmpr #'cat:s-cmpr)
	     (ml (cat:loop-space-grml-sintr cmpr)))
	(funcall ml 2 (cat:crpr 3 (cat:loop3) 0 (cat:loop3 0 'a 2)))
	(funcall ml 2 (cat:crpr 0 (cat:loop3 0 'a 2) 3 (cat:loop3)))
	(funcall ml 2 (cat:crpr 0 (cat:loop3 0 'a 2) 0 (cat:loop3 0 'a -2)))
	(funcall ml 2 (cat:crpr 0 (cat:loop3 0 'a 2) 0 (cat:loop3 0 'a -3)))
	(funcall ml 2 (cat:crpr 0 (cat:loop3 0 'a 2) 0 (cat:loop3 0 'b -3)))
	(funcall ml 2 (cat:crpr 0 (cat:loop3 0 'a 2 0 'b -3)
				0 (cat:loop3 0 'b +3 0 'a -2)))))


(test loop-space
      (cat:cat-init)
      (let ((g (cat:gdeltab))
	    basis cmbn cmbn2 dd echcm efhm hat loop-list rslt x)
	(is (equal :equal (cat:cmpr g (cat:loop3 0 3 2) (cat:loop3 0 3 2))))
	(is (equal :less (cat:cmpr g (cat:loop3 0 3 2) (cat:loop3 0 3 3))))
	(is (equal :less (cat:cmpr g (cat:loop3 0 3 2) (cat:loop3 0 5 2))))
	(is (equal :greater (cat:cmpr g (cat:loop3 0 5 2) (cat:loop3 0 3 2))))
	(cat:face g 3 3 (cat:loop3 12 7 3))
	(cat:face g 3 3 (cat:loop3 5 7 3))
	(cat:face g 3 3 (cat:loop3 6 7 3 5 7 3 3 7 3))
	(cat:face g 4 4 (cat:absm 4 (cat:loop3 6 7 3 5 7 3 3 7 3)))
	(cat:check-faces (cat:cmpr g) (cat:face g) 3
			 (cat:loop3 6 7 -3 5 7 -3 3 7 -3))
	(setf dd (cat:cmps g g))
	(cat:? dd 3 (cat:loop3 6 7 3 5 7 3 3 7 3))
	(cat:grml g 2 (cat:crpr 0 (cat:loop3 0 15 3) 0 (cat:loop3 0 15 4)))
	(cat:grin g 2 (cat:loop3 0 15 -2))
	(setf rslt (cat:loop3 0 31 1))
	(setf hat (mapcar #'(lambda (i) (cat:face g i 3 rslt))
			  (cat:<a-b> 0 3)))
	(dotimes (i 4)
	  (print (cat:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
	  (cat:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

	(setf g (cat:loop-space (cat:sphere 2)))
	(setf rslt (cat:loop3 3 's2 2 5 's2 -2 6 's2 2))
	(setf hat (mapcar #'(lambda (i) (cat:face g i 3 rslt))
			  (cat:<a-b> 0 3)))
	(dotimes (i 4)
	  (print (cat:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
	  (cat:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

	(setf g (cat:loop-space (cat:sphere 3) 2))
	(setf x (first (cat:basis (cat:echcm g) 4)))
	(setf efhm (cat:efhm g))
	(setf x (cat:lf efhm (cat:rg efhm 4 x)))
	(setf rslt (cat:gnrt (first (cat:cmbn-list x))))
	(setf hat (mapcar #'(lambda (i) (cat:face g i 4 rslt))
			  (cat:<a-b> 0 4)))
	(dotimes (i 5)
	  (print (cat:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
	  (cat:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

	(setf rslt (cat:gnrt (third (cat:cmbn-list x))))
	(setf hat (mapcar #'(lambda (i) (cat:face g i 4 rslt))
			  (cat:<a-b> 0 4)))
	(dotimes (i 5)
	  (print (cat:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
	  (cat:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

	(setf g (cat:loop-space (cat:sphere 3) 2))
	(setf efhm (cat:efhm g))
	(setf echcm (cat:rbcc efhm))
	(setf basis (cat:basis echcm 4))
	(setf cmbn (cat:cmbn 4 1 (first basis) 10 (second basis)
			     100 (third basis)))
	(setf cmbn2 (cat:lf efhm (cat:rg efhm cmbn)))
	(setf loop-list (mapcar #'cdr (cat:cmbn-list cmbn2)))
	(dolist (loop loop-list)
	  (setf hat (mapcar #'(lambda (i) (cat:face g i 4 loop))
			    (cat:<a-b> 0 4)))
	  (dotimes (i 5)
	    (print (cat:kfll g i 4 (remove (nth i hat) hat)))
	    (cat:check-kan g i 4 (remove (nth i hat) hat))))))


(test loop-space2
      (cat:cat-init)
      (let* ((p (cat:r-proj-space 3))
	     (op (cat:loop-space p))
	     cmpr c)
	(dotimes (i 15) (print (random-apowr 8 3)))
	(dotimes (i 20)
	  (print (cat:normalize-loop 7 (random-niloop 7 3 3))))

	(setf cmpr (cat:cmpr op))
	(setf cat:+too-much-time+ -1)
	(setf c (random-loop-cmbn cmpr 8 10 4 4 100))
	(time (cat:? op (cat:? op c)))
	(time (cat:? op (cat:? op c)))))
