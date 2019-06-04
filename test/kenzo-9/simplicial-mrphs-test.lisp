;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)


(test build-smmr
      (let* ((d (cat-9:delta 3))
             (m (cat-9:build-smmr
                 :sorc d :trgt d :degr 0
                 :sintr #'(lambda (dmns gmsm) (cat-9:absm 0 gmsm))
                 :orgn '(identity delta-3)))
             (m2 (cat-9:build-smmr
                  :sorc d :trgt d :degr 0
                  :sintr #'(lambda (dmns gmsm) (cat-9:absm (cat-9:mask dmns) 1))
                  :orgn '(null delta-3))))
        (cat-9:? m2 2 7)
        ;;  (s? m2 2 7)
        ))
