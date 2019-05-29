;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(defsystem :kenzo-test
    :description "A simple regression test suite for Kenzo"
    :serial t
    :version "0.1.0"
    :author "Francis Sergeraert <Francis.Sergeraert@ujf-grenoble.fr>"
    :license "GPLv3"
    :depends-on (:kenzo :fiveam)
    :pathname #P"test/"
    :components ((:file "package")
                 (:file "common")
                 ;; === test files ===
                 (:file "bar-test")
                 (:file "bicones-test")
                 (:file "cartesian-products-test")
                 (:file "ccn")
                 (:file "chain-complexes-test")
                 (:file "chcm-elementary-op-test")
                 (:file "circle")
                 (:file "cl-space-efhm-test")
                 (:file "classifying-spaces-test")
                 (:file "cobar-test")
                 (:file "combinations-test")
                 (:file "cones-test")
                 (:file "cs-twisted-products-test")
                 (:file "delta-test")
                 (:file "diabolo")
                 (:file "disk-pasting-test")
                 (:file "eilenberg-zilber-test")
                 (:file "effective-homology-test")
                 (:file "fibrations-test")
                 (:file "homology-groups-test")
                 (:file "k-pi-n-test")
                 (:file "kan-test")
                 (:file "loop-spaces-test")
                 (:file "lp-space-efhm-test")
                 (:file "ls-twisted-products-test")
                 (:file "macros-test")
                 (:file "searching-homology-test")
                 (:file "serre-test")
                 (:file "simplicial-groups-test")
                 (:file "simplicial-mrphs-test")
                 (:file "simplicial-sets-test")
                 ;;(:file "smith-test")
                 (:file "special-smsts-test")
                 (:file "suspensions-test")
                 (:file "tensor-products-test")
                 (:file "various-test")
                 ;;(:file "whitehead-test")
                 ))
