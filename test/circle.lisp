
(in-package :kenzo-test)

(in-suite :kenzo)


(test circle
      (cat:cat-init)
      (let ((circle (cat:circle)))
	(is (equal (cat:basis circle 0) '(*)))
	(is (equal (cat:basis circle 1) '(cat::s1)))
	(is (equal (cat:basis circle 2) cat:+empty-list+))))
