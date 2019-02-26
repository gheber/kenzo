;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST
;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST
;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST

(cat-init)
(homology (loop-space (sphere 3) 2) 7)
(homology (loop-space (moore 2 4) 2) 7)
(homology (k-z2 2) 3 8)
(homology (k-z2 3) 3 8)
(defparameter s3 (sphere 3))
(defparameter ch3 (chml-clss s3 3))
(defparameter f3 (z-whitehead s3 ch3))
(defparameter x4 (fibration-total f3))
(defparameter ch4 (chml-clss x4 4))
(defparameter f4 (z2-whitehead x4 ch4))
(defparameter x5 (fibration-total f4))
(defparameter ch5 (chml-clss x5 5))
(defparameter f5 (z-whitehead x5 ch5))
(defparameter x6 (fibration-total f5))
(homology x6 6)
