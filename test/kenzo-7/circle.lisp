;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test circle
      (cat-7:cat-init)
      (let ((circle (cat-7:circle)))
        (is (equal (cat-7:basis circle 0) '(*)))
        (is (equal (cat-7:basis circle 1) '(cat-7::s1)))
        (is (equal (cat-7:basis circle 2) cat-7:+empty-list+))))
