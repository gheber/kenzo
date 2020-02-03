(load "common.lisp")

(ql:quickload "kenzo")
;;(use-package :cat-7)
;;(use-package :cat-8)
;;(use-package :cat-9)
(use-package :cat)

(==> (defun cdelta (dmns)
       (build-chcm
        :cmpr #'l-cmpr
        :basis :locally-effective
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
        :orgn `(locally effective version of C_* delta ,dmns))))

(==> (defun make-f (tdmns bdmns)
       (build-mrph
        :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
        :intr #'(lambda (degr gmsm)
                  (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns))
                                          gmsm)))
                    (if pos
                        (if (< pos degr)
                            (zero-cmbn degr)
                            (cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                        (cmbn degr 1 gmsm))))
        :strt :gnrt
        :orgn `(projection delta ,tdmns => delta ,bdmns))))

(==> (defun make-g (tdmns bdmns)
       (build-mrph
        :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
        :intr #'identity
        :strt :cmbn
        :orgn `(injection delta ,bdmns => delta ,tdmns))))

(==> (defun make-h (tdmns bdmns)
       (build-mrph
        :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
        :intr #'(lambda (degr gmsm)
                  (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns))
                                          gmsm)))
                    (if pos
                        (if (member bdmns gmsm)
                            (zero-cmbn (1+ degr))
                            (cmbn (1+ degr) (-1-expt-n pos)
                                  (append (subseq gmsm 0 pos) (list bdmns)
                                          (subseq gmsm pos))))
                        (zero-cmbn (1+ degr)))))
        :strt :gnrt
        :orgn `(homotopy for delta ,tdmns => ,bdmns))))

(==> (defun make-rdct (tdmns bdmns)
       (let ((rdct (build-rdct
                    :f (make-f tdmns bdmns)
                    :g (make-g tdmns bdmns)
                    :h (make-h tdmns bdmns)
                    :orgn `(reduction delta ,tdmns ,bdmns))))
         rdct)))

(==> (def rdct (make-rdct 6 3)))

(==> (inspect rdct))

(==> (orgn rdct))

(==> (pre-check-rdct rdct))

(==> (setf *tc* (cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4) 1000 '(2 3 4))))

(==> (setf *bc* (cmbn 3 4 '(0 1 2 3))))

(==> (check-rdct-no-wait))

(==> (def trdct (make-rdct 6 4)))

(==> (def brdct (make-rdct 4 3)))

(==> (setf rdct (cmps brdct trdct)))

(==> (pre-check-rdct rdct))

(==> (check-rdct-no-wait))

(==> (def hmeq (trivial-hmeq (cdelta 4))))

(==> (setf hmeq (add hmeq (opps (dffr (cdelta 4))))))

(==> (gnrt-? (dffr (rbcc hmeq)) 3 '(0 1 2 3)))

(==> (def comb-bic (cmbn 3 2 (bcnb 'b1) 4 (bcnb 'b1) 6 (bcnb 'b3)
                         3 (bcnb 'c1) 5 (bcnb 'c1) 7 (bcnb 'd1))))

(==> (cmbn-list comb-bic))

(==> (def comb-b (cmbn 3 2 'b1 4 'b2 6 'b3)))

(==> (def comb-c (cmbn 4 3 'c1 5 'c2)))

(==> (def comb-d (cmbn 3 7 'd1)))

(==> (def comb-bic (make-bicn-cmbn comb-b comb-c comb-d)))

(==> (bicn-cmbn-cmbnb comb-bic))

(==> (bicn-cmbn-cmbnc comb-bic))

(==> (bicn-cmbn-cmbnd comb-bic))

(==> (multiple-value-bind (b c d)
         (dispatch-bicn-cmbn comb-bic)
       (list b c d)))

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

(==> (def delta3 (cdelta 3)))

(==> (basis delta3 0))

(==> (basis delta3 1))

(==> (basis delta3 2))

(==> (basis delta3 3))

(==> (basis delta3 4))

(==> (def bic (bicone (make-rdct 3 2) (make-rdct 4 2))))

(==> (basis bic 0))

(==> (basis bic 1))

(==> (basis bic 4))

(==> (? bic (cmbn 2 3 (bcnb '(0 1 3)) 4 (bcnc '(0 1 2 3)) 5 (bcnd '(0 1 4)))))

(==> (? bic (? bic (cmbn 2 3 (bcnb '(0 1 3))
                         4 (bcnc '(0 1 2 3))
                         5 (bcnd '(0 1 4))))))

(==> (cat-init))

(==> (def c (build-chcm
             :cmpr #'s-cmpr
             :basis #'(lambda (dmns) (declare (ignore dmns)) '(a))
             :bsgn 'a
             :intr-dffr #'zero-intr-dffr
             :strt :cmbn
             :orgn '(c))))

(==> (def h1 (trivial-hmeq c)))

(==> (def h2 (cmps h1 h1)))

(==> (pre-check-rdct (lrdct h2)))

(==> (setf *tc* (cmbn 3 1 (bcnb 'a) 10 (bcnc 'a) 100 (bcnd 'a))))

(==> (setf *bc* (cmbn 3 1 'a)))

(==> (check-rdct-no-wait))

(==> (pre-check-rdct (rrdct h2)))

(==> (check-rdct-no-wait))

(==> (def h3 (cmps h2 h2)))

(==> (setf *tc* (cmbn 3 1 (bcnb (bcnb 'a)) 10 (bcnb (bcnc 'a))
                      100 (bcnb (bcnd 'a)) 1000 (bcnc 'a)
                      10000 (bcnd (bcnb 'a)) 5234 (bcnd (bcnc 'a))
                      223 (bcnd (bcnd 'a)))))

(==> (pre-check-rdct (lrdct h3)))

(==> (check-rdct-no-wait))

(==> (pre-check-rdct (rrdct h3)))

(==> (check-rdct-no-wait))

(sb-ext:exit)
