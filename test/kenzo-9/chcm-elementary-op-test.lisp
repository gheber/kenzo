;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test zero-mrph
      (progn
        (cat-9:cat-init)
        (let* ((z (cat-9:zero-mrph (cat-9:Z-chcm) (cat-9:Z-chcm) 2))
               (z2 (cat-9:zero-mrph (cat-9:Z-chcm) (cat-9:Z-chcm) 2))
               (z3 (cat-9:zero-mrph (cat-9:Z-chcm) (cat-9:Z-chcm) 3))
               (comb (cat-9:gnrt-? z 0 :z-gnrt))
               (comb1 (cat-9:cmbn-? z (cat-9:cmbn 3)))
               (comb2 (cat-9:gnrt-? z3 0 :z-gnrt)))
          (is (equal (cat-9:cmbn-degr comb) 2))
          (is (equal (cat-9:cmbn-list comb) nil))
          (is (equal (cat-9:cmbn-degr comb1) 5))
          (is (equal (cat-9:cmbn-list comb1) nil))
          (is (eq z z2))
          (is (equal (cat-9:cmbn-degr comb2) 3))
          (is (equal (cat-9:cmbn-list comb2) nil))
          (is (not (eq z z3))))))


(test idnt-mrph
      (progn
        (cat-9:cat-init)
        (let* ((zi (cat-9:idnt-mrph (cat-9:Z-chcm)))
               (comb (cat-9:gnrt-? zi 0 :z-gnrt))
               (zi2 (cat-9:idnt-mrph (cat-9:Z-chcm))))
          (is (equal (cat-9:cmbn-degr comb) 0))
          (is (equal (cat-9:cmbn-list comb) (cons (cons 1 :z-gnrt) nil)))
          (is (eq zi zi2)))))


(test opps
      (progn
        (cat-9:cat-init)
        (let* ((-zi (cat-9:opps (cat-9:idnt-mrph (cat-9:Z-chcm))))
               (comb (cat-9:gnrt-? -zi 0 :z-gnrt))
               (-zi2 (cat-9:opps (cat-9:idnt-mrph (cat-9:Z-chcm)))))
          (is (equal (cat-9:cmbn-degr comb) 0))
          (is (equal (cat-9:cmbn-list comb) (cons (cons -1 :z-gnrt) nil)))
          (is (eq -zi -zi2)))))


(setf *n* 5)

(test cmps
      (progn
        (cat-9:cat-init)
        (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-9:cmbn
                                                   (1- (cat-9:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                     :intr #'ff :strt :gnrt :orgn '(test)))
               (mrph2 (cat-9:cmps mrph mrph :gnrt))
               (comb (cat-9:gnrt-? mrph2 0 0))
               (mrph3 (cat-9:cmps mrph mrph :cmbn))
               (comb1 (cat-9:gnrt-? mrph3 0 0))
               (mrph33 (cat-9:cmps mrph mrph :cmbn)))
          (dotimes (i 5)
            (setq comb (cat-9:gnrt-? mrph2 0 i))
            (is (equal (cat-9:cmbn-degr comb) 0)))
          (dotimes (i 5)
            (setq comb1 (cat-9:gnrt-? mrph3 0 i))
            (is (equal (cat-9:cmbn-degr comb1) 0)))
          (is (eq mrph3 mrph33)))))

#|
(setf s3 (sphere 3))
(setf ch3 (chml-clss s3 3))
(setf 2ch3 (n-mrph 2 ch3))
(setf f3 (z-whitehead s3 2ch3))
(setf x (fibration-total f3))
(homology x 0 10)
(setf k (k-z 3))
(setf ch3 (chml-clss k 3))
(setf 2ch3 (n-mrph 2 ch3))
(setf f3 (z-whitehead k 2ch3))
(setf x (fibration-total f3))
(homology x 0 10)
|#

(test n-mrph
      (progn
        (cat-9:cat-init)
        (let* ((s3 (cat-9:sphere 3))
               (ch3 (cat-9:chml-clss s3 3))
               (2ch3 (cat-9:n-mrph 2 ch3))
               ;;          (f3 (cat-9:z-whitehead s3 2ch3))
               ;;          (x (cat-9:fibration-total f3))
               (k (cat-9:k-z 3)))
          ;;      (cat-9:homology x 0 10)
          ;;      (setf ch3 (cat-9:chml-clss k 3))
          ;;      (setf 2ch3 (cat-9:n-mrph 2 ch3))
          ;;      (setf f3 (cat-9:z-whitehead k 2ch3))
          ;;      (setf x (cat-9:fibration-total f3))
          ;;      (cat-9:homology x 0 10)
          )))


(setf *n* 10)
(setf cat-9:+too-much-time+ -1)
(test add
      (progn
        (cat-9:cat-init)
        (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-9:cmbn
                                                   (1- (cat-9:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph1 (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                      :intr #'ff :strt :gnrt :orgn '(test)))
               (mrph2 (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                      :intr #'ff :strt :gnrt :orgn '(test2)))
               (mrph3 (cat-9:add mrph1 mrph2 :gnrt))

               (comb1 (cat-9:gnrt-? mrph1 0 0))
               (comb2 (cat-9:gnrt-? mrph2 0 0))
               (comb3 (cat-9:gnrt-? mrph3 0 0))
               (mrph4 (cat-9:add mrph1 mrph2 :cmbn))
               (comb4 (cat-9:gnrt-? mrph4 0 0))
               (mrph44 (cat-9:add mrph1 mrph2 :cmbn)))
          (is (equal (cat-9:cmbn-degr comb1) 0))
          (is (equal (length (cat-9:cmbn-list comb1)) 10))
          (is (equal (cat-9:cmbn-degr comb2) 0))
          (is (equal (length (cat-9:cmbn-list comb2)) 10))
          (is (equal (cat-9:cmbn-degr comb3) 0))
          (is (<= (length (cat-9:cmbn-list comb3)) 20))
          (is (equal (cat-9:cmbn-degr comb4) 0))
          (is (<= (length (cat-9:cmbn-list comb4)) 20))
          (is (eq mrph4 mrph44)))))


(test sbtr
      (progn
        (cat-9:cat-init)
        (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-9:cmbn
                                                   (1- (cat-9:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph1 (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                      :intr #'ff :strt :gnrt :orgn '(test)))
               (mrph2 (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                      :intr #'ff :strt :gnrt :orgn '(test2)))
               (mrph3 (cat-9:sbtr mrph1 mrph2 :gnrt))
               (comb1 (cat-9:gnrt-? mrph1 0 0))
               (comb2 (cat-9:gnrt-? mrph2 0 0))
               (comb3 (cat-9:gnrt-? mrph3 0 0))
               (mrph4 (cat-9:sbtr mrph1 mrph2 :cmbn))
               (comb4 (cat-9:gnrt-? mrph4 0 0))
               (mrph44 (cat-9:sbtr mrph1 mrph2 :cmbn)))
          (is (equal (cat-9:cmbn-degr comb1) 0))
          (is (equal (length (cat-9:cmbn-list comb1)) 10))
          (is (equal (cat-9:cmbn-degr comb2) 0))
          (is (equal (length (cat-9:cmbn-list comb2)) 10))
          (is (equal (cat-9:cmbn-degr comb3) 0))
          (is (<= (length (cat-9:cmbn-list comb3)) 20))
          (is (equal (cat-9:cmbn-degr comb4) 0))
          (is (<= (length (cat-9:cmbn-list comb4)) 20))
          (is (eq mrph4 mrph44)))))
