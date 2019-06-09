;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)


(test shuffle-sign
      (dotimes (i 30)
        (format t "~%~A => ~@D" (cat-9:hyphenize-list (cat-9:dlop-int-ext i))
                (cat-9:shuffle-sign i))))


(test intr-eml
      (cat-9:intr-eml (cat-9:cmbn 3 1 (cat-9:tnpr 0 'a 3 'b)
                              2 (cat-9:tnpr 2 'a 1 'b)
                              3 (cat-9:tnpr 2 'aa 1 'bb)
                              4 (cat-9:tnpr 3 'a 0 'b)))
      (cat-9:intr-eml (cat-9:cmbn 3 1 (cat-9:tnpr 1 'c 2 'd)
                              10 (cat-9:tnpr 2 'a 1 'b)))
      (cat-9:intr-eml (cat-9:cmbn 3 10 (cat-9:tnpr 1 'c 2 'd)
                              1 (cat-9:tnpr 2 'a 1 'b)
                              100 (cat-9:tnpr 2 'aa 1 'bb)))
      (cat-9:intr-eml (cat-9:cmbn 3 10 (cat-9:tnpr 1 'c 2 'd)
                              100 (cat-9:tnpr 1 'cc 2 'dd)
                              1 (cat-9:tnpr 2 'a 1 'b))))


(test eml
      (cat-9:cat-init)
      (let* ((d (cat-9:delta-infinity))
             (eml (cat-9:eml d d)))
        (cat-9:? eml (cat-9:cmbn 3 1 (cat-9:tnpr 0 1 3 30)
                             10 (cat-9:tnpr 1 3 2 28)
                             100 (cat-9:tnpr 2 7 1 24)
                             1000 (cat-9:tnpr 3 15 0 16)))))


(test phi
      (cat-9:cat-init)
      (let ((phi (cat-9:phi (cat-9:delta-infinity) (cat-9:delta-infinity))))
        (cat-9:? phi 3 (cat-9:crpr 0 15 0 15))))


(test eat
      (cat-9:cat-init)
      (let* ((s (cat-9:sphere 5))
             (p (cat-9:crts-prdc s s))
             (b (cat-9:basis p 8))
             (c (cat-9:make-cmbn :degr 8
                               :list (mapcar #'(lambda (item)
                                                 (cat-9:term (1+ (random 5))
                                                           item))
                                             b)))
             (phi (cat-9:phi s s)))
        (time (cat-9:? phi c))))


(test intr-aw
      (cat-9:cat-init)
      (let* ((r (cat-9:intr-aw #'cat-9:delta-face #'cat-9:delta-face))
             (s (cat-9:sphere 3))
             (f (cat-9:face s)))
        (funcall r 3 (cat-9:crpr 0 15 0 15))
        (setf r (cat-9:intr-aw f f))
        (funcall r 3 (cat-9:crpr 0 's3 0 's3))))


(test aw
      (cat-9:cat-init)
      (let ((aw (cat-9:aw (cat-9:delta-infinity) (cat-9:delta-infinity))))
        (cat-9:? aw (cat-9:cmbn 3 1 (cat-9:crpr 0 15 0 30)
                            -1 (cat-9:crpr 0 23 0 29)))))


(test ez
      (cat-9:cat-init)
      (let ((ez (cat-9:ez (cat-9:delta-infinity) (cat-9:delta-infinity))))
        (setf cat-9:*bc* (cat-9:cmbn 3 1 (cat-9:tnpr 0 1 3 30)
                                 10 (cat-9:tnpr 1 96 2 896)
                                 100 (cat-9:tnpr 2 7168 1 3)
                                 1000 (cat-9:tnpr 3 15 0 4096)))
        (setf cat-9:*tc* (cat-9:cmbn 3 1 (cat-9:crpr 0 15 0 480)
                                 100 (cat-9:crpr 0 15 5 96)
                                 10 (cat-9:crpr 6 3 0 480)
                                 1000 (cat-9:crpr 6 3 1 224)))
        (cat-9:pre-check-rdct ez)
        (check-rdct)))


(test eat1
      (cat-9:cat-init)
      (let* ((s (cat-9:sphere 5))
             (ez (cat-9:ez s s))
             (b (cat-9:basis (cat-9:tcc ez) 8))
             (c (cat-9:make-cmbn :degr 8
                               :list (mapcar #'(lambda (item)
                                                 (cat-9:term (1+ (random 5))
                                                           item))
                                             b))))
        (cat-9:pre-check-rdct ez)
        (setf cat-9:+too-much-time+ -1)
        (time (cat-9:? cat-9:*id-gf-dh-hd* c))
        (time (cat-9:? cat-9:*id-gf-dh-hd* c))))


(test crts-prdc
      (cat-9:cat-init)
      (let* ((k (cat-9:k-z 2))
             (k2 (cat-9:crts-prdc k k)))
        (cat-9:homology k2 0 10)))
