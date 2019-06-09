;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test chcm-mat
      (progn
        (let ((d (cat-7:delta 3)))
          (princ (cat-7:chcm-mat d 2))
          (princ (cat-7:chcm-mat d 3))
          (princ (cat-7:chcm-mat d 4)))))

(test chcm-homology
      (progn
        (let ((d (cat-7:sphere 3)))
          (princ (cat-7:chcm-homology d 2))
          (princ (cat-7:chcm-homology d 3))
          (princ (cat-7:chcm-homology d 4)))))

(test chcm-homology-gen
      (progn
        (let ((d (cat-7:sphere 3)))
          (princ (cat-7:chcm-homology-gen d 2))
          (princ (cat-7:chcm-homology-gen d 3))
          (princ (cat-7:chcm-homology-gen d 4)))))


#|
()
(setf d (sphere 3))
(chcm-homology-gen-old d 2)
(chcm-homology-gen-old d 3)
(chcm-homology-gen-old d 4)
|#
