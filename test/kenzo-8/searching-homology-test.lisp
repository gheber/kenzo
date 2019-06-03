;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test homology
      (progn
        (cat-8:cat-init)
        (let ((d (cat-8:delta 3)))
          (princ (cat-8:homology d 0)))))
