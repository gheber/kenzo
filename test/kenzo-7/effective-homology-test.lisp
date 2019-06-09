;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test cdelta
      (progn
        (cat-7:cat-init)
        (let* ((cc (cdelta 5))
               (hmeq (cat-7:trivial-hmeq cc)))
          (declare (ignore hmeq))
          (signals simple-error (equal (cat-7:basis cc 0) cat-7:+empty-list+))
          (signals simple-error (equal (cat-7:basis cc 1) cat-7:+empty-list+))
          (signals simple-error (equal (cat-7:basis cc 2) cat-7:+empty-list+))
          (signals simple-error (equal (cat-7:basis cc 3) cat-7:+empty-list+))
          (signals simple-error (equal (cat-7:basis cc 4) cat-7:+empty-list+))
          (signals simple-error (equal (cat-7:basis cc 5) cat-7:+empty-list+)))))


(test make-rdct
      (progn
        (cat-7:cat-init)
        (let ((rdct (make-rdct 6 3)))
          (is (equal (cat-7:orgn rdct) '(REDUCTION DELTA 6 3)))
          (cat-7:pre-check-rdct rdct)
          (setf cat-7:*tc* (cat-7:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   1000 '(2 3 4)))
          (setf cat-7:*bc* (cat-7:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test cmps
      (progn
        (cat-7:cat-init)
        (let* ((trdct (make-rdct 6 4))
               (brdct (make-rdct 4 3))
               (rdct (cat-7:cmps brdct trdct)))
          (cat-7:pre-check-rdct rdct)
          (setf cat-7:*tc* (cat-7:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   100 '(1 3 5) 10 '(1 4 5) 1 '(3 4 5)))
          (setf cat-7:*bc* (cat-7:cmbn 2 1 '(0 1 3)))
          (check-rdct))))


(test zero-mrph
      (progn
        (cat-7:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-7:zero-mrph (cdelta 6) (cdelta 6) -1))
               (new-rdct (cat-7:add rdct perturb)))
          (cat-7:pre-check-rdct new-rdct)
          (setf cat-7:*tc* (cat-7:cmbn 2 3 '(0 1 2)))
          (setf cat-7:*bc* (cat-7:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test opps
      (progn
        (cat-7:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-7:opps (cat-7:dffr (cat-7:tcc rdct))))
               (new-rdct (cat-7:add rdct perturb)))
          (cat-7:pre-check-rdct new-rdct)
          (setf cat-7:*tc* (cat-7:cmbn 2 3 '(0 1 2)))
          (setf cat-7:*bc* (cat-7:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


;; an absurd reduction ; just to test bpl-*-sigma
(test bpl-*-sigma
      (progn
        (cat-7:cat-init)
        (let* ((tcc (cat-7:build-chcm
                     :cmpr #'cat-7:l-cmpr
                     :basis :locally-effective
                     :bsgn '(0)
                     :intr-dffr #'(lambda (degr gnrt)
                                    (cat-7:cmbn (1- degr) 1 gnrt))
                     :strt :gnrt
                     :orgn '(test1)))
               (rdct (cat-7:trivial-rdct tcc))
               (perturb (cat-7:build-mrph
                         :sorc tcc :trgt tcc :degr -1
                         :intr #'(lambda (degr gnrt)
                                   (if (zerop (first gnrt))
                                       (cat-7:zero-cmbn (1- degr))
                                       (cat-7:cmbn (1- degr) 1
                                                 (list (1- (first gnrt))
                                                       (1+ (second gnrt))))))
                         :strt :gnrt
                         :orgn '(test3)))
               (new-rdct))
          (setf (slot-value rdct 'cat-7:h)
                (cat-7:build-mrph
                 :sorc tcc :trgt tcc :degr +1
                 :intr #'(lambda (degr gnrt)
                           (cat-7:cmbn (1+ degr) 1 gnrt))
                 :strt :gnrt
                 :orgn '(test2)))
          (setf new-rdct (cat-7:add rdct perturb))
          (cat-7:gnrt-? (cat-7:dffr (cat-7:tcc new-rdct)) 3 '(3 5))
          (cat-7:gnrt-? (cat-7:dffr (cat-7:bcc new-rdct)) 3 '(3 5)))))


(test trivial-hmeq
      (progn
        (cat-7:cat-init)
        (let ((hmeq (cat-7:trivial-hmeq (cdelta 4))))
          (cat-7:add (cat-7:lrdct hmeq) (cat-7:opps (cat-7:dffr (cdelta 4))))
          (setf hmeq (cat-7:add hmeq (cat-7:opps (cat-7:dffr (cdelta 4)))))
          (cat-7:gnrt-? (cat-7:dffr (cat-7:rbcc hmeq)) 3 '(0 1 2 3)))))
