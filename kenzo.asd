;;;; kenzo.asd

(in-package :cl-user)

(defpackage :kenzo-asd
  (:use :cl :asdf))

(in-package :kenzo-asd)

(asdf:defsystem #:kenzo
  :description "A Symbolic Software for Effective Homology Computation by Francis Sergeraert"
  :serial t
  :components ((:file "package")
               (:file "kenzo")
	       (:module "src"
			:components ((:file "macros")
				     (:file "various")
				     (:file "classes")
				     (:file "combinations")
				     (:file "chain-complexes")
				     (:file "chcm-elementary-op")
				     (:file "effective-homology")
				     (:file "homology-groups")
				     (:file "searching-homology")
				     (:file "cones")
				     (:file "bicones")
				     (:file "tensor-products")
				     (:file "coalgebras")
				     (:file "cobar")
				     (:file "algebras")
				     (:file "bar")
				     (:file "simplicial-sets")
				     (:file "simplicial-mrphs")
				     (:file "delta")
				     (:file "special-smsts")
				     (:file "suspensions")
				     (:file "disk-pasting")
				     (:file "cartesian-products")
				     (:file "eilenberg-zilber")
				     (:file "kan")
				     (:file "simplicial-groups")
				     (:file "fibrations")
				     (:file "loop-spaces")
				     (:file "ls-twisted-products")
				     (:file "lp-space-efhm")
				     (:file "classifying-spaces")
				     (:file "k-pi-n")
				     (:file "serre")
				     (:file "cs-twisted-products")
				     (:file "cl-space-efhm")
				     (:file "whitehead")
				     (:file "smith")))))

