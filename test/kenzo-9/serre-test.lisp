;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test fibration-total
      (cat-9:cat-init)
      (let* ((s2 (cat-9:sphere 2))
             (k (cat-9:k-z-1))
             (tw (cat-9:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-9:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (p3r (cat-9:fibration-total tw)))
        (cat-9:homology p3r 1)))


(test dummy
      (is (not (null t))))
