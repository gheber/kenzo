;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test cdelta
      (progn
        (cat-8:cat-init)
        (let* ((cc (cdelta 5))
               (hmeq (cat-8:trivial-hmeq cc)))
          (declare (ignore hmeq))
          (signals simple-error (equal (cat-8:basis cc 0) cat-8:+empty-list+))
          (signals simple-error (equal (cat-8:basis cc 1) cat-8:+empty-list+))
          (signals simple-error (equal (cat-8:basis cc 2) cat-8:+empty-list+))
          (signals simple-error (equal (cat-8:basis cc 3) cat-8:+empty-list+))
          (signals simple-error (equal (cat-8:basis cc 4) cat-8:+empty-list+))
          (signals simple-error (equal (cat-8:basis cc 5) cat-8:+empty-list+)))))


(test make-rdct
      (progn
        (cat-8:cat-init)
        (let ((rdct (make-rdct 6 3)))
          (is (equal (cat-8:orgn rdct) '(REDUCTION DELTA 6 3)))
          (cat-8:pre-check-rdct rdct)
          (setf cat-8:*tc* (cat-8:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   1000 '(2 3 4)))
          (setf cat-8:*bc* (cat-8:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test cmps
      (progn
        (cat-8:cat-init)
        (let* ((trdct (make-rdct 6 4))
               (brdct (make-rdct 4 3))
               (rdct (cat-8:cmps brdct trdct)))
          (cat-8:pre-check-rdct rdct)
          (setf cat-8:*tc* (cat-8:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   100 '(1 3 5) 10 '(1 4 5) 1 '(3 4 5)))
          (setf cat-8:*bc* (cat-8:cmbn 2 1 '(0 1 3)))
          (check-rdct))))


(test zero-mrph
      (progn
        (cat-8:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-8:zero-mrph (cdelta 6) (cdelta 6) -1))
               (new-rdct (cat-8:add rdct perturb)))
          (cat-8:pre-check-rdct new-rdct)
          (setf cat-8:*tc* (cat-8:cmbn 2 3 '(0 1 2)))
          (setf cat-8:*bc* (cat-8:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test opps
      (progn
        (cat-8:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-8:opps (cat-8:dffr (cat-8:tcc rdct))))
               (new-rdct (cat-8:add rdct perturb)))
          (cat-8:pre-check-rdct new-rdct)
          (setf cat-8:*tc* (cat-8:cmbn 2 3 '(0 1 2)))
          (setf cat-8:*bc* (cat-8:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


;; an absurd reduction ; just to test bpl-*-sigma
(test bpl-*-sigma
      (progn
        (cat-8:cat-init)
        (let* ((tcc (cat-8:build-chcm
                     :cmpr #'cat-8:l-cmpr
                     :basis :locally-effective
                     :bsgn '(0)
                     :intr-dffr #'(lambda (degr gnrt)
                                    (cat-8:cmbn (1- degr) 1 gnrt))
                     :strt :gnrt
                     :orgn '(test1)))
               (rdct (cat-8:trivial-rdct tcc))
               (perturb (cat-8:build-mrph
                         :sorc tcc :trgt tcc :degr -1
                         :intr #'(lambda (degr gnrt)
                                   (if (zerop (first gnrt))
                                       (cat-8:zero-cmbn (1- degr))
                                       (cat-8:cmbn (1- degr) 1
                                                 (list (1- (first gnrt))
                                                       (1+ (second gnrt))))))
                         :strt :gnrt
                         :orgn '(test3)))
               (new-rdct))
          (setf (slot-value rdct 'cat-8:h)
                (cat-8:build-mrph
                 :sorc tcc :trgt tcc :degr +1
                 :intr #'(lambda (degr gnrt)
                           (cat-8:cmbn (1+ degr) 1 gnrt))
                 :strt :gnrt
                 :orgn '(test2)))
          (setf new-rdct (cat-8:add rdct perturb))
          (cat-8:gnrt-? (cat-8:dffr (cat-8:tcc new-rdct)) 3 '(3 5))
          (cat-8:gnrt-? (cat-8:dffr (cat-8:bcc new-rdct)) 3 '(3 5)))))


(test trivial-hmeq
      (progn
        (cat-8:cat-init)
        (let ((hmeq (cat-8:trivial-hmeq (cdelta 4))))
          (cat-8:add (cat-8:lrdct hmeq) (cat-8:opps (cat-8:dffr (cdelta 4))))
          (setf hmeq (cat-8:add hmeq (cat-8:opps (cat-8:dffr (cdelta 4)))))
          (cat-8:gnrt-? (cat-8:dffr (cat-8:rbcc hmeq)) 3 '(0 1 2 3)))))
