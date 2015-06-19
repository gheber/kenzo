
(load "file-list.lisp")

(load-cfiles)

(load "resolutions.lisp")

(load "cylinders.lisp")


;;


(setf kz22 (k-zp 2 2))
(setf kz23 (k-zp 2 3))
(setf kz32 (k-zp 3 2))
(setf kz33 (k-zp 3 3))
(setf kz42 (k-zp 4 2))
(setf kz43 (k-zp 4 3))
(setf kz52 (k-zp 5 2))
(setf kz53 (k-zp 5 3))


(print "homology of kz22")
(time (homology kz22 0 7))

(print "homology of kz23")
(time (homology kz23 0 7))

(print "homology of kz32")
(time (homology kz32 0 7))

(print "homology of kz33")
(time (homology kz33 0 7))

(print "homology of kz42")
(time (homology kz42 0 7))

(print "homology of kz43")
(time (homology kz43 0 7))

(print "homology of kz52")
(time (homology kz52 0 7))

(print "homology of kz53")
(time (homology kz53 0 7))






