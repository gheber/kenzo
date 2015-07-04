
(in-package :kenzo-test)

(in-suite :kenzo)

(defun circle ()
  (the cat:chain-complex
       (cat:build-chcm
	:cmpr #'(lambda (gnrt1 gnrt2)
		  (declare (ignore gnrt1 gnrt2))
		  (the cat:cmpr :equal))
	:basis #'(lambda (dmns)
		   (the list
			(case dmns (0 '(*)) (1 '(s1))
			      (otherwise cat:+empty-list+))))
	:bsgn '*
	:intr-dffr #'cat:zero-intr-dffr
	:strt :cmbn
	:orgn '(circle))))


(test circle
      (progn
	(cat:cat-init)
	(let ((circle (circle)))
	  (is (equal (cat:basis circle 0) '(*)))
	  (is (equal (cat:basis circle 1) '(s1)))
	  (is (equal (cat:basis circle 2) cat:+empty-list+)))))
