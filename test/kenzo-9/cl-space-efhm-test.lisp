;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test classifying-space
      (cat-9:cat-init)
      (let* ((k (cat-9:k-z-1))
             (bk (cat-9:classifying-space k))
             (obk (cat-9:loop-space bk)))
        (cat-9:homology k 0 10)
        (cat-9:homology bk 0 10)
        (cat-9:homology obk 0 6)))
