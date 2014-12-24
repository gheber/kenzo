;;;; kenzo.asd

(in-package :cl-user)

(defpackage :kenzo-asd
  (:use :cl :asdf))

(in-package :kenzo-asd)

(asdf:defsystem #:kenzo
  :description "A repackaged version of the Kenzo program by Francis Sergeraert and Xavier Dousson"
  :serial t
  :components ((:file "package")
               (:file "kenzo")
	       (:module "src"
			:components ((:file "macros")
				     (:file "various")
				     (:file "classes")
				     (:file "combinations")
				     (:file "chain-complexes")
				     (:file "chcm-elementary-op")))))

