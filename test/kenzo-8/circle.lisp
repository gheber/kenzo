;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test circle
      (cat-8:cat-init)
      (let ((circle (cat-8:circle)))
        (is (equal (cat-8:basis circle 0) '(*)))
        (is (equal (cat-8:basis circle 1) '(cat-8::s1)))
        (is (equal (cat-8:basis circle 2) cat-8:+empty-list+))))
