
(defsystem :kenzo-test
  :description "A simple test suite for Kenzo"
  :serial t
  :version "0.1.0"
  :author "Francis Sergeraert <Francis.Sergeraert@ujf-grenoble.fr>"
  :license "GPLv3"
  :depends-on (:kenzo :fiveam)
  :pathname #P"test/"
  :components ((:file "package")
	       (:file "common")
	       (:file "combinations-test")
	       (:file "various-test")
	       (:file "chain-complexes-test")
	       (:file "chcm-elementary-op-test")
	       (:file "diabolo")
	       (:file "circle")
               (:file "ccn")
	       (:file "effective-homology-test")
	       (:file "bicones-test")
	       (:file "homology-groups-test")
	       (:file "searching-homology-test")
	       (:file "delta-test")
	       (:file "bar-test")
	       (:file "cartesian-products-test")
	       (:file "cones-test")
	       ;;(:file "cl-space-efhm-test")
	       (:file "tensor-products-test")
	       (:file "k-pi-n-test")
	       (:file "simplicial-sets-test")
	       ))
