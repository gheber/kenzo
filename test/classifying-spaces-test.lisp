
(in-package :kenzo-test)

(in-suite :kenzo)


(test normalize-gbar
      (cat:normalize-gbar (list 0))
      (cat:normalize-gbar (list 1 (cat:absm 0 'i)))
      (cat:normalize-gbar (list 2 (cat:absm 1 'i) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 2 (cat:absm 0 'a) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 0 'a) (cat:absm 0 'b)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 7 'i) (cat:absm 3 'i)
				(cat:absm 1 'i) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 0 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 1 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 2 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 4 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 3 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 5 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 6 'a) (cat:absm 3 'i)
				(cat:absm 0 'c) (cat:absm 0 'i)))
      (cat:normalize-gbar (list 4 (cat:absm 1 'a) (cat:absm 3 'i)
				(cat:absm 1 'i) (cat:absm 0 'i))))


(test unnormalize-gbar
      (cat:unnormalize-gbar (cat:absm 1 cat:+null-gbar+) 'i)
      (cat:unnormalize-gbar (cat:absm 15 cat:+null-gbar+) 'i)
      (cat:normalize-gbar (cat:unnormalize-gbar
			   (cat:absm 15 cat:+null-gbar+) 'i))
      (cat:unnormalize-gbar
       (cat:absm 0 (cat:make-gbar :dmns 4
			  :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				      (cat:absm 0 'c) (cat:absm 0 'i)))) 'i)
      (cat:normalize-gbar
       (cat:unnormalize-gbar
	(cat:absm 0 (cat:make-gbar :dmns 4
			   :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				       (cat:absm 0 'c) (cat:absm 0 'i)))) 'i))
      (cat:unnormalize-gbar
       (cat:absm 5 (cat:make-gbar :dmns 4
			  :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				      (cat:absm 0 'c) (cat:absm 0 'i)))) 'i)
      (cat:normalize-gbar
       (cat:unnormalize-gbar
	(cat:absm 5 (cat:make-gbar :dmns 4
			   :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				       (cat:absm 0 'c) (cat:absm 0 'i)))) 'i))
      (cat:unnormalize-gbar
       (cat:absm 9 (cat:make-gbar :dmns 4
			  :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				      (cat:absm 0 'c) (cat:absm 0 'i)))) 'i)
      (cat:normalize-gbar
       (cat:unnormalize-gbar
	(cat:absm 9 (cat:make-gbar :dmns 4
			   :list (list (cat:absm 0 'a) (cat:absm 0 'b)
				       (cat:absm 0 'c) (cat:absm 0 'i)))) 'i)))


(test gbar
      (cat:gbar 0)
      (signals simple-error (cat:gbar 1))
      (cat:gbar 2 1 'a 2 'b))


(test classifying-space-cmpr
      (let ((cmpr (cat:classifying-space-cmpr #'cat:s-cmpr)))
	(is (equal :less (funcall cmpr
				  (cat:gbar 2 0 'a 0 'a)
				  (cat:gbar 2 1 'a 0 'a))))
	(is (equal :less (funcall cmpr
				  (cat:gbar 2 0 'a 0 'a)
				  (cat:gbar 2 0 'b 0 'a))))
	(is (equal :equal (funcall cmpr
				   (cat:gbar 2 0 'a 0 'a)
				   (cat:gbar 2 0 'a 0 'a))))))


(test classifying-space-basis
      (cat:cat-init)
      (let* ((k (cat:k-z2-1))
	     (b (cat:classifying-space-basis (cat:basis k))))
	(funcall b 0)
	(funcall b 1)
	(dotimes (i 5) (print (funcall b i)))))

(test classifying-space-face
      (let* ((om (cat:loop-space (cat:moore 2 2)))
	     (face (cat:classifying-space-face (cat:face om)
					       (cat:sintr (cat:grml om))))
	     (gbar (cat:gbar 4 0 (cat:loop3 3 'm2 1 4 'n3 1)
			     0 (cat:loop3 0 'n3 1)
			     0 (cat:loop3 0 'm2 1)
			     0 cat:+null-loop+)))
	(dotimes (i 5)
	  (print (funcall face i 4 gbar)))))


(test classifying-space
      (cat:cat-init)
      (let ((c (cat:classifying-space (cat:k-z2-1))))
	(cat:orgn c)
	(first (cat:basis c 4))
	(cat:? c 4 (first (cat:basis c 4)))
	(cat:? c (cat:? c 4 (first (cat:basis c 4))))
	(cat:cprd c 4 (first (cat:basis c 4)))
	(dotimes (i 5)
	  (print (cat:face c i 4 (first (cat:basis c 4)))))))


(test classifying-space-grml-sintr
      (let ((grml (cat:classifying-space-grml-sintr
		   '() (cat:sintr (cat:grml (cat:k-z-1))))))
	(funcall grml 3 (cat:crpr 0 (cat:gbar 3 0 '(1 2) 0 '(3) 0 '())
				  0 (cat:gbar 3 0 '(-1 -2) 0 '(-3) 0 '())))
	(funcall grml 3 (cat:crpr 0 (cat:gbar 3 0 '(1 2) 0 '(3) 0 '())
				  4 (cat:gbar 2 0 '(-3) 0 '())))
	(funcall grml 3 (cat:crpr 0 (cat:gbar 3 0 '(1 2) 0 '(3) 0 '())
				  1 (cat:gbar 2 0 '(-3) 0 '())))))


(test classifying-space-grin-sintr
      (let ((grin (cat:classifying-space-grin-sintr
		   (cat:sintr (cat:grin (cat:k-z-1))))))
	(funcall grin 3 (cat:gbar 3 0 '(1 2) 1 '() 0 '()))))


(test classifying-space1
      (cat:cat-init)
      (let* ((k-z-1 (cat:k-z-1))
	     (k-z-2 (cat:classifying-space k-z-1))
	     (k-z-3 (cat:classifying-space k-z-2))
	     (k-z2-1 (cat:k-z2-1))
	     (k-z2-2 (cat:classifying-space k-z2-1))
	     (k-z2-3 (cat:classifying-space k-z2-2))
	     (k-z2-4 (cat:classifying-space k-z2-3))
	     (k-z2-5 (cat:classifying-space k-z2-4)))
	(cat:homology k-z-3 0 10)
	(cat:homology k-z2-5 0 7)))
