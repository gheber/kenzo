;;;; kenzo.asd

(in-package :cl-user)

(defpackage :kenzo-asd
  (:use :cl :asdf))

(in-package :kenzo-asd)

(asdf:defsystem #:kenzo
  :description "A Symbolic Software for Effective Homology Computation by Francis Sergeraert"
  :version "1.1.7"
  :author "Francis Sergeraert <Francis.Sergeraert@ujf-grenoble.fr>"
  :license "GPLv3"
  :serial t
  :components
  ((:file "package")
   (:module "src"
	    :components
	    ((:file "kenzo")
	     (:file "macros")
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
	     (:file "smith")
	     #|
	     (:module "anromero"
		      :components
		      ((:file "bicomplexes")
		       (:file "filtered-complexes")
		       (:file "spectral-sequences")))
	     |#
	     ))))

