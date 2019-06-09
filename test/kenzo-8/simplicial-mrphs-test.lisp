;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test build-smmr
      (let* ((d (cat-8:delta 3))
             (m (cat-8:build-smmr
                 :sorc d :trgt d :degr 0
                 :sintr #'(lambda (dmns gmsm) (cat-8:absm 0 gmsm))
                 :orgn '(identity delta-3)))
             (m2 (cat-8:build-smmr
                  :sorc d :trgt d :degr 0
                  :sintr #'(lambda (dmns gmsm) (cat-8:absm (cat-8:mask dmns) 1))
                  :orgn '(null delta-3))))
        (cat-8:? m2 2 7)
        ;;  (s? m2 2 7)
        ))
