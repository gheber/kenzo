;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test shuffle-sign
      (dotimes (i 30)
        (format t "~%~A => ~@D" (cat-8:hyphenize-list (cat-8:dlop-int-ext i))
                (cat-8:shuffle-sign i))))


(test intr-eml
      (cat-8:intr-eml (cat-8:cmbn 3 1 (cat-8:tnpr 0 'a 3 'b)
                              2 (cat-8:tnpr 2 'a 1 'b)
                              3 (cat-8:tnpr 2 'aa 1 'bb)
                              4 (cat-8:tnpr 3 'a 0 'b)))
      (cat-8:intr-eml (cat-8:cmbn 3 1 (cat-8:tnpr 1 'c 2 'd)
                              10 (cat-8:tnpr 2 'a 1 'b)))
      (cat-8:intr-eml (cat-8:cmbn 3 10 (cat-8:tnpr 1 'c 2 'd)
                              1 (cat-8:tnpr 2 'a 1 'b)
                              100 (cat-8:tnpr 2 'aa 1 'bb)))
      (cat-8:intr-eml (cat-8:cmbn 3 10 (cat-8:tnpr 1 'c 2 'd)
                              100 (cat-8:tnpr 1 'cc 2 'dd)
                              1 (cat-8:tnpr 2 'a 1 'b))))


(test eml
      (cat-8:cat-init)
      (let* ((d (cat-8:delta-infinity))
             (eml (cat-8:eml d d)))
        (cat-8:? eml (cat-8:cmbn 3 1 (cat-8:tnpr 0 1 3 30)
                             10 (cat-8:tnpr 1 3 2 28)
                             100 (cat-8:tnpr 2 7 1 24)
                             1000 (cat-8:tnpr 3 15 0 16)))))


(test intr-phi
      (cat-8:cat-init)
      (let* ((d (cat-8:delta-infinity))
             (rslt (cat-8:intr-phi d d)))
        (funcall rslt (cat-8:cmbn 3 1 (cat-8:crpr 0 15 0 15)))
        (funcall rslt (cat-8:cmbn 2 1 (cat-8:crpr 0 7 0 7)))
        (funcall rslt (cat-8:cmbn 1 1 (cat-8:crpr 0 3 0 3)))
        (funcall rslt (cat-8:cmbn 0 1 (cat-8:crpr 0 1 0 1)))))


(test phi
      (cat-8:cat-init)
      (let ((phi (cat-8:phi (cat-8:delta-infinity) (cat-8:delta-infinity))))
        (cat-8:? phi 3 (cat-8:crpr 0 15 0 15))))


(test eat
      (cat-8:cat-init)
      (let* ((s (cat-8:sphere 5))
             (p (cat-8:crts-prdc s s))
             (b (cat-8:basis p 8))
             (c (cat-8:make-cmbn :degr 8
                               :list (mapcar #'(lambda (item)
                                                 (cat-8:term (1+ (random 5))
                                                           item))
                                             b)))
             (phi (cat-8:phi s s)))
        (time (cat-8:? phi c))))


(test intr-aw
      (cat-8:cat-init)
      (let* ((r (cat-8:intr-aw #'cat-8:delta-face #'cat-8:delta-face))
             (s (cat-8:sphere 3))
             (f (cat-8:face s)))
        (funcall r 3 (cat-8:crpr 0 15 0 15))
        (setf r (cat-8:intr-aw f f))
        (funcall r 3 (cat-8:crpr 0 's3 0 's3))))


(test aw
      (cat-8:cat-init)
      (let ((aw (cat-8:aw (cat-8:delta-infinity) (cat-8:delta-infinity))))
        (cat-8:? aw (cat-8:cmbn 3 1 (cat-8:crpr 0 15 0 30)
                            -1 (cat-8:crpr 0 23 0 29)))))


(test ez
      (cat-8:cat-init)
      (let ((ez (cat-8:ez (cat-8:delta-infinity) (cat-8:delta-infinity))))
        (setf cat-8:*bc* (cat-8:cmbn 3 1 (cat-8:tnpr 0 1 3 30)
                                 10 (cat-8:tnpr 1 96 2 896)
                                 100 (cat-8:tnpr 2 7168 1 3)
                                 1000 (cat-8:tnpr 3 15 0 4096)))
        (setf cat-8:*tc* (cat-8:cmbn 3 1 (cat-8:crpr 0 15 0 480)
                                 100 (cat-8:crpr 0 15 5 96)
                                 10 (cat-8:crpr 6 3 0 480)
                                 1000 (cat-8:crpr 6 3 1 224)))
        (cat-8:pre-check-rdct ez)
        (check-rdct)))


(test eat1
      (cat-8:cat-init)
      (let* ((s (cat-8:sphere 5))
             (ez (cat-8:ez s s))
             (b (cat-8:basis (cat-8:tcc ez) 8))
             (c (cat-8:make-cmbn :degr 8
                               :list (mapcar #'(lambda (item)
                                                 (cat-8:term (1+ (random 5))
                                                           item))
                                             b))))
        (cat-8:pre-check-rdct ez)
        (setf cat-8:+too-much-time+ -1)
        (time (cat-8:? cat-8:*id-gf-dh-hd* c))
        (time (cat-8:? cat-8:*id-gf-dh-hd* c))))


(test crts-prdc
      (cat-8:cat-init)
      (let* ((k (cat-8:k-z 2))
             (k2 (cat-8:crts-prdc k k)))
        (cat-8:homology k2 0 10)))
