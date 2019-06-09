;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test suspension-cmpr
      (cat-7:cat-init)
      (let* ((cc (cat-7:deltab))
             (cmpr (cat-7:suspension-cmpr (cat-7:cmpr cc))))
        (is (equal :equal (funcall cmpr :s-bsgn :s-bsgn)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test suspension-basis
      (cat-7:cat-init)
      (let* ((cc (cat-7:sphere 2))
             (basis (cat-7:suspension-basis (cat-7:basis cc))))
        (dotimes (i 6)
          (print (funcall basis i)))
        (cat-7:suspension-basis :locally-effective)))


(test suspension-intr-dffr
      (cat-7:cat-init)
      (let* ((cc (cat-7:deltab))
             (intr (cat-7:suspension-intr-dffr (cat-7:dffr cc))))
        (funcall intr (cat-7:cmbn 0 3 :s-bsgn))
        (funcall intr (cat-7:cmbn 3 66 7))))


(test suspension
      (cat-7:cat-init)
      (let* ((cc (cat-7:deltab))
             (scc (cat-7:suspension cc)))
        (cat-7:cmpr scc 7 11)
        (signals simple-error (cat-7:basis scc 3))
        (cat-7:bsgn scc)
        (cat-7:? scc 3 11)
        (setf scc (cat-7:suspension cc 2))
        (cat-7:? scc 4 11)))


(test suspension-intr-cprd
      (cat-7:suspension-intr-cprd (cat-7:cmbn 0))
      (cat-7:suspension-intr-cprd (cat-7:cmbn 0 5 :s-bsgn))
      (cat-7:suspension-intr-cprd (cat-7:cmbn 3 4 7 5 11)))


(test coal
      (cat-7:cat-init)
      (let* ((coal (cat-7:deltab))
             (scoal (cat-7:suspension coal)))
        (setf scoal (cat-7:suspension coal))
        (cat-7:? scoal 3 7)
        (cat-7:cprd scoal 3 7)))


(test suspension-face
      (cat-7:cat-init)
      (let* ((ss (cat-7:deltab))
             (face (cat-7:suspension-face (cat-7:face ss)))
             (m (cat-7:moore 2 3)))
        (funcall face 4 4 15)
        (funcall face 0 4 15)
        #+(or ecl sbcl)
        (signals type-error (cat-7:check-faces #'cat-7:f-cmpr face 4 15))
        #+(or ecl sbcl)
        (signals type-error (cat-7:check-faces #'cat-7:f-cmpr face 3 7))
        #+(or ecl sbcl)
        (signals type-error (cat-7:check-faces #'cat-7:f-cmpr face 2 3))
        #-(or ecl sbcl) (cat-7:check-faces #'cat-7:f-cmpr face 4 15)
        #-(or ecl sbcl) (cat-7:check-faces #'cat-7:f-cmpr face 3 7)
        #-(or ecl sbcl) (cat-7:check-faces #'cat-7:f-cmpr face 2 3)
        (setf face (cat-7:suspension-face (cat-7:face m)))
        (dotimes (i 5) (print (funcall face i 4 'm3)))
        (dotimes (i 6) (print (funcall face i 5 'mm4)))
        (cat-7:check-faces #'cat-7:s-cmpr face 4 'm3)
        (cat-7:check-faces #'cat-7:s-cmpr face 5 'mm4)
        ))


(test suspension1
      (cat-7:cat-init)
      (let* ((d (cat-7:deltab))
             (sd (cat-7:suspension d))
             (m (cat-7:moore 2 1))
             (sm (cat-7:suspension m)))
        (setf sd (cat-7:suspension d))
        (cat-7:? sd 3 7)
        (cat-7:cprd sd 3 7)
        (cat-7:face sd 3 3 7)
        (cat-7:face sd 2 3 7)
        (cat-7:homology sm 0 5)
        (setf ssm (cat-7:suspension sm))
        (cat-7:homology ssm 0 6)))


(test suspension-intr
      (cat-7:cat-init)
      (let* ((f (cat-7:idnt-mrph (cat-7:deltab)))
             (sf (cat-7:suspension-intr f))
             (d (cat-7:dffr (cat-7:deltab)))
             (sd (cat-7:suspension-intr d)))
        (funcall sf (cat-7:cmbn 0 3 :s-bsgn))
        (funcall sf (cat-7:cmbn 2 4 3))
        (funcall sd (cat-7:cmbn 0 3 :s-bsgn))
        (funcall sd (cat-7:cmbn 2 4 3))
        (funcall sd (cat-7:cmbn 3 4 7))))


(test suspension2
      (cat-7:cat-init)
      (let* ((f (cat-7:idnt-mrph (cat-7:deltab)))
             (sf (cat-7:suspension f))
             (d (cat-7:dffr (cat-7:deltab)))
             (sd (cat-7:suspension d)))
        (cat-7:? sf (cat-7:cmbn 0 3 :s-bsgn))
        (cat-7:? sf (cat-7:cmbn 2 4 3))
        (cat-7:? sd (cat-7:cmbn 0 3 :s-bsgn))
        (cat-7:? sd (cat-7:cmbn 2 4 3))
        (cat-7:? sd (cat-7:cmbn 3 4 7))))


(test suspension3
      (cat-7:cat-init)
      (let* ((rdct (cat-7:ez (cat-7:deltab) (cat-7:deltab)))
             (srdct (cat-7:suspension rdct)))
        (cat-7:pre-check-rdct srdct)
        (setf cat-7:*tc* (cat-7:cmbn 4 1 (cat-7:crpr 0 15 0 15)))
        (setf cat-7:*bc* (cat-7:cmbn 4 1 (cat-7:tnpr 0 1 3 15)
                                 5 (cat-7:tnpr 1 3 2 7)
                                 10 (cat-7:tnpr 2 7 1 3)
                                 100 (cat-7:tnpr 3 15 0 1)))
        (check-rdct)))


(test suspension4
      (cat-7:cat-init)
      (let ((sk (cat-7:suspension (cat-7:k-z 2))))
        (cat-7:homology sk 0 10)))
