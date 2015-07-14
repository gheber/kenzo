
(in-package :kenzo-test)

(in-suite :kenzo)


(test build-smmr
      (let* ((d (cat:delta 3))
	     (m (cat:build-smmr
		 :sorc d :trgt d :degr 0
		 :sintr #'(lambda (dmns gmsm) (cat:absm 0 gmsm))
		 :orgn '(identity delta-3)))
	     (m2 (cat:build-smmr
		  :sorc d :trgt d :degr 0
		  :sintr #'(lambda (dmns gmsm) (cat:absm (cat:mask dmns) 1))
		  :orgn '(null delta-3))))
	(cat:? m2 2 7)
	;;  (s? m2 2 7)
	))
