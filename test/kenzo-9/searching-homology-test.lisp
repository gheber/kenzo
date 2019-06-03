;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test homology
      (progn
        (cat-9:cat-9-init)
        (let ((d (cat-9:delta 3)))
          (princ (cat-9:homology d 0)))))
