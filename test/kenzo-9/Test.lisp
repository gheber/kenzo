;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST
;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST
;;;  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST  TEST

(kenzo-version)

(time (progn

        (cat-init)
        (setf s3 (sphere 3))
        (homology s3 3)
        (setf os3 (loop-space s3))
        (homology os3 4)
        (setf o2s3 (loop-space os3))
        (homology o2s3 5)
        (homology (loop-space (moore 2 4) 2) 7)

        (homology (k-z2 2) 3 8)

        (homology (k-z2 3) 3 8)

        (setf s3 (sphere 3))
        (homology s3 3)
        (setf ch3 (chml-clss s3 3))
        (setf f3 (z-whitehead s3 ch3))
        (setf x4 (fibration-total f3))
        (homology x4 4)
        (setf ch4 (chml-clss x4 4))
        (setf f4 (z2-whitehead x4 ch4))
        (setf x5 (fibration-total f4))
        (homology x5 5)
        (setf ch5 (chml-clss x5 5))
        (setf f5 (z-whitehead x5 ch5))
        (setf x6 (fibration-total f5))

        ))

(time (homology o2s3 7))
(time (homology x6 6))
