;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test build-smmr
      (let* ((d (cat-7:delta 3))
             (m (cat-7:build-smmr
                 :sorc d :trgt d :degr 0
                 :sintr #'(lambda (dmns gmsm) (cat-7:absm 0 gmsm))
                 :orgn '(identity delta-3)))
             (m2 (cat-7:build-smmr
                  :sorc d :trgt d :degr 0
                  :sintr #'(lambda (dmns gmsm) (cat-7:absm (cat-7:mask dmns) 1))
                  :orgn '(null delta-3))))
        (cat-7:? m2 2 7)
        ;;  (s? m2 2 7)
        ))
