
(in-package :kenzo-test)

(in-suite :kenzo)

(test soft-delta-cmpr
      (is (equal :less (cat:soft-delta-cmpr '(d 12) '(d 23)))))

(defun faces (i lst)
  (cat:dlop-int-ext
   (cat:gmsm
    (cat:delta-face i 3 (cat:dlop-ext-int lst)))))

(test delta-face
      (is (equal '(1 2 3) (faces 0 '(0 1 2 3))))
      (is (equal '(0 2 3) (faces 1 '(0 1 2 3))))
      (is (equal '(0 1 3) (faces 2 '(0 1 2 3))))
      (is (equal '(0 1 2) (faces 3 '(0 1 2 3))))
      (is (equal '(2 4 6) (faces 0 '(0 2 4 6))))
      (is (equal '(0 4 6) (faces 1 '(0 2 4 6))))
      (is (equal '(0 2 6) (faces 2 '(0 2 4 6))))
      (is (equal '(0 2 4) (faces 3 '(0 2 4 6))))

      (dotimes (i 4)
	(print (cat:soft-delta-face i 3 `(:delt ,(cat:mask 4)))))
      
      (dotimes (i 4)
	(print (cat:soft-delta-face i 3 `(:delt
					  ,(cat:dlop-ext-int '(0 2 4 6)))))))

#|
(test homology
      (progn
	(cat:cat-init)
	(let ((d (cat:delta 3)))
	  (princ (cat:homology d 0)))))
|#
