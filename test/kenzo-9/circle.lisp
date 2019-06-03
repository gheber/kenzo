;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)


(test circle
      (cat-9:cat-9-init)
      (let ((circle (cat-9:circle)))
        (is (equal (cat-9:basis circle 0) '(*)))
        (is (equal (cat-9:basis circle 1) '(cat-9::s1)))
        (is (equal (cat-9:basis circle 2) cat-9:+empty-list+))))
