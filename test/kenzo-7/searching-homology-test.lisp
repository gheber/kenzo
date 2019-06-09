;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test homology
      (progn
        (cat-7:cat-init)
        (let ((d (cat-7:delta 3)))
          (princ (cat-7:homology d 0)))))
