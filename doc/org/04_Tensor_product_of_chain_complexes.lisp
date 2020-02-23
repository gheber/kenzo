(load "common.lisp")

(ql:quickload "kenzo")
;;(use-package :cat-7)
;;(use-package :cat-8)
;;(use-package :cat-9)
(use-package :cat)

(==> (tnpr 1 'a 2 'b))

(==> (inspect (tnpr 1 'a 2 'b)))

(==> (tnpr-p (tnpr 1 'a 2 'b)))

(==> (tnpr-p (cmbn 0 1 'a 2 'b)))

(==> (setf *tnpr-with-degrees* t))

(==> (2cmbn-tnpr (cmbn 2 3 'a 4 'b -5 'c) (cmbn 3 4 'x -3 'y 2 'z)))

(==> (setf *tnpr-with-degrees* nil))

(==> (2cmbn-tnpr (cmbn 2 3 'a 4 'b -5 'c) (cmbn 3 4 'x -3 'y 2 'z)))

(==> (cat-init))

(==> (defun cdelta (dmns)
       (build-chcm
        :cmpr #'l-cmpr
        :basis #'(lambda (n)
                   (mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
        :bsgn '(0)
        :intr-dffr
        #'(lambda (degr gmsm)
            (make-cmbn
             :degr (1- degr)
             :list (do ((rslt +empty-list+
                              (cons (cons sign
                                          (append
                                           (subseq gmsm 0 nark)
                                           (subseq gmsm (1+ nark))))
                                    rslt))
                        (sign 1 (- sign))
                        (nark 0 (1+ nark)))
                       ((> nark degr) rslt))))
        :strt :gnrt
        :orgn `(Effective version of C_* delta ,dmns))))

(==> (def triangle (cdelta 2)))

(==> (basis triangle 1))

(==> (def tpr-triangles (tnsr-prdc triangle triangle)))

(==> (basis tpr-triangles 0))

(==> (basis tpr-triangles 1))

(==> (basis tpr-triangles 2))

(==> (basis tpr-triangles 3))

(==> (basis tpr-triangles 4))

(def ccn-boundary #'(lambda (dgr gnr)
                      (if (evenp (+ dgr gnr))
                          (cmbn (1- dgr) 1 (- gnr 10))
                          (cmbn (1- dgr)))))

(==> (def ccn (build-chcm :cmpr #'f-cmpr
                          :basis #'(lambda (n) (<a-b< (* 10 n) (* 10 (1+ n))))
                          :intr-dffr  ccn-boundary
                          :strt :gnrt
                          :orgn '(ccn) )))

(==> (basis ccn 3))

(==> (def tpr-ccn-ccn (tnsr-prdc ccn ccn)))

(==> (def comb2 (cmbn 2 1 21 5 25 9 29)))

(==> (def comb3 (cmbn 3 2 32 3 33 -4 34 -6 36)))

(==> (def tcmb (2cmbn-tnpr comb2 comb3)))

(==> (? tpr-ccn-ccn tcmb))

(==> (? tpr-ccn-ccn (? tpr-ccn-ccn tcmb)))

(sb-ext:exit)
