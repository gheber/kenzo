;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test cdelta
      (progn
        (cat-9:cat-init)
        (let* ((cc (cdelta 5))
               (hmeq (cat-9:trivial-hmeq cc)))
          (declare (ignore hmeq))
          (signals simple-error (equal (cat-9:basis cc 0) cat-9:+empty-list+))
          (signals simple-error (equal (cat-9:basis cc 1) cat-9:+empty-list+))
          (signals simple-error (equal (cat-9:basis cc 2) cat-9:+empty-list+))
          (signals simple-error (equal (cat-9:basis cc 3) cat-9:+empty-list+))
          (signals simple-error (equal (cat-9:basis cc 4) cat-9:+empty-list+))
          (signals simple-error (equal (cat-9:basis cc 5) cat-9:+empty-list+)))))


(test make-rdct
      (progn
        (cat-9:cat-init)
        (let ((rdct (make-rdct 6 3)))
          (is (equal (cat-9:orgn rdct) '(REDUCTION DELTA 6 3)))
          (cat-9:pre-check-rdct rdct)
          (setf cat-9:*tc* (cat-9:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   1000 '(2 3 4)))
          (setf cat-9:*bc* (cat-9:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test cmps
      (progn
        (cat-9:cat-init)
        (let* ((trdct (make-rdct 6 4))
               (brdct (make-rdct 4 3))
               (rdct (cat-9:cmps brdct trdct)))
          (cat-9:pre-check-rdct rdct)
          (setf cat-9:*tc* (cat-9:cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                                   100 '(1 3 5) 10 '(1 4 5) 1 '(3 4 5)))
          (setf cat-9:*bc* (cat-9:cmbn 2 1 '(0 1 3)))
          (check-rdct))))


(test zero-mrph
      (progn
        (cat-9:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-9:zero-mrph (cdelta 6) (cdelta 6) -1))
               (new-rdct (cat-9:add rdct perturb)))
          (cat-9:pre-check-rdct new-rdct)
          (setf cat-9:*tc* (cat-9:cmbn 2 3 '(0 1 2)))
          (setf cat-9:*bc* (cat-9:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


(test opps
      (progn
        (cat-9:cat-init)
        (let* ((rdct (make-rdct 6 3))
               (perturb (cat-9:opps (cat-9:dffr (cat-9:tcc rdct))))
               (new-rdct (cat-9:add rdct perturb)))
          (cat-9:pre-check-rdct new-rdct)
          (setf cat-9:*tc* (cat-9:cmbn 2 3 '(0 1 2)))
          (setf cat-9:*bc* (cat-9:cmbn 3 4 '(0 1 2 3)))
          (check-rdct))))


;; an absurd reduction ; just to test bpl-*-sigma
(test bpl-*-sigma
      (progn
        (cat-9:cat-init)
        (let* ((tcc (cat-9:build-chcm
                     :cmpr #'cat-9:l-cmpr
                     :basis :locally-effective
                     :bsgn '(0)
                     :intr-dffr #'(lambda (degr gnrt)
                                    (cat-9:cmbn (1- degr) 1 gnrt))
                     :strt :gnrt
                     :orgn '(test1)))
               (rdct (cat-9:trivial-rdct tcc))
               (perturb (cat-9:build-mrph
                         :sorc tcc :trgt tcc :degr -1
                         :intr #'(lambda (degr gnrt)
                                   (if (zerop (first gnrt))
                                       (cat-9:zero-cmbn (1- degr))
                                       (cat-9:cmbn (1- degr) 1
                                                 (list (1- (first gnrt))
                                                       (1+ (second gnrt))))))
                         :strt :gnrt
                         :orgn '(test3)))
               (new-rdct))
          (setf (slot-value rdct 'cat-9:h)
                (cat-9:build-mrph
                 :sorc tcc :trgt tcc :degr +1
                 :intr #'(lambda (degr gnrt)
                           (cat-9:cmbn (1+ degr) 1 gnrt))
                 :strt :gnrt
                 :orgn '(test2)))
          (setf new-rdct (cat-9:add rdct perturb))
          (cat-9:gnrt-? (cat-9:dffr (cat-9:tcc new-rdct)) 3 '(3 5))
          (cat-9:gnrt-? (cat-9:dffr (cat-9:bcc new-rdct)) 3 '(3 5)))))


(test trivial-hmeq
      (progn
        (cat-9:cat-init)
        (let ((hmeq (cat-9:trivial-hmeq (cdelta 4))))
          (cat-9:add (cat-9:lrdct hmeq) (cat-9:opps (cat-9:dffr (cdelta 4))))
          (setf hmeq (cat-9:add hmeq (cat-9:opps (cat-9:dffr (cdelta 4)))))
          (cat-9:gnrt-? (cat-9:dffr (cat-9:rbcc hmeq)) 3 '(0 1 2 3)))))
