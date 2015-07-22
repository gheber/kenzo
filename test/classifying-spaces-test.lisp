
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
      (signals #-ecl simple-error
	       #+ecl simple-type-error (cat:gbar 1))
      (cat:gbar 2 1 'a 2 'b))


(test classifying-space-cmpr
      (let ((cmpr (cat:classifying-space-cmpr 'cat:s-cmpr)))
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
#|
()
(cat-init)
(setf k (k-z2-1))
(setf b (classifying-space-basis (basis k)))
(funcall b 0)
(funcall b 1)
(dotimes (i 5) (print (funcall b i)))
|#
