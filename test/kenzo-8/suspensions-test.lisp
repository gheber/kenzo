;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test suspension-cmpr
      (cat-8:cat-init)
      (let* ((cc (cat-8:deltab))
             (cmpr (cat-8:suspension-cmpr (cat-8:cmpr cc))))
        (is (equal :equal (funcall cmpr :s-bsgn :s-bsgn)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test suspension-basis
      (cat-8:cat-init)
      (let* ((cc (cat-8:sphere 2))
             (basis (cat-8:suspension-basis (cat-8:basis cc))))
        (dotimes (i 6)
          (print (funcall basis i)))
        (cat-8:suspension-basis :locally-effective)))


(test suspension-intr-dffr
      (cat-8:cat-init)
      (let* ((cc (cat-8:deltab))
             (intr (cat-8:suspension-intr-dffr (cat-8:dffr cc))))
        (funcall intr (cat-8:cmbn 0 3 :s-bsgn))
        (funcall intr (cat-8:cmbn 3 66 7))))


(test suspension
      (cat-8:cat-init)
      (let* ((cc (cat-8:deltab))
             (scc (cat-8:suspension cc)))
        (cat-8:cmpr scc 7 11)
        (signals simple-error (cat-8:basis scc 3))
        (cat-8:bsgn scc)
        (cat-8:? scc 3 11)
        (setf scc (cat-8:suspension cc 2))
        (cat-8:? scc 4 11)))


(test suspension-intr-cprd
      (cat-8:suspension-intr-cprd (cat-8:cmbn 0))
      (cat-8:suspension-intr-cprd (cat-8:cmbn 0 5 :s-bsgn))
      (cat-8:suspension-intr-cprd (cat-8:cmbn 3 4 7 5 11)))


(test coal
      (cat-8:cat-init)
      (let* ((coal (cat-8:deltab))
             (scoal (cat-8:suspension coal)))
        (setf scoal (cat-8:suspension coal))
        (cat-8:? scoal 3 7)
        (cat-8:cprd scoal 3 7)))


(test suspension-face
      (cat-8:cat-init)
      (let* ((ss (cat-8:deltab))
             (face (cat-8:suspension-face (cat-8:face ss)))
             (m (cat-8:moore 2 3)))
        (funcall face 4 4 15)
        (funcall face 0 4 15)
        #+(or ecl sbcl)
        (signals type-error (cat-8:check-faces #'cat-8:f-cmpr face 4 15))
        #+(or ecl sbcl)
        (signals type-error (cat-8:check-faces #'cat-8:f-cmpr face 3 7))
        #+(or ecl sbcl)
        (signals type-error (cat-8:check-faces #'cat-8:f-cmpr face 2 3))
        #-(or ecl sbcl) (cat-8:check-faces #'cat-8:f-cmpr face 4 15)
        #-(or ecl sbcl) (cat-8:check-faces #'cat-8:f-cmpr face 3 7)
        #-(or ecl sbcl) (cat-8:check-faces #'cat-8:f-cmpr face 2 3)
        (setf face (cat-8:suspension-face (cat-8:face m)))
        (dotimes (i 5) (print (funcall face i 4 'm3)))
        (dotimes (i 6) (print (funcall face i 5 'mm4)))
        (cat-8:check-faces #'cat-8:s-cmpr face 4 'm3)
        (cat-8:check-faces #'cat-8:s-cmpr face 5 'mm4)
        ))


(test suspension1
      (cat-8:cat-init)
      (let* ((d (cat-8:deltab))
             (sd (cat-8:suspension d))
             (m (cat-8:moore 2 1))
             (sm (cat-8:suspension m)))
        (setf sd (cat-8:suspension d))
        (cat-8:? sd 3 7)
        (cat-8:cprd sd 3 7)
        (cat-8:face sd 3 3 7)
        (cat-8:face sd 2 3 7)
        (cat-8:homology sm 0 5)
        (setf ssm (cat-8:suspension sm))
        (cat-8:homology ssm 0 6)))


(test suspension-intr
      (cat-8:cat-init)
      (let* ((f (cat-8:idnt-mrph (cat-8:deltab)))
             (sf (cat-8:suspension-intr f))
             (d (cat-8:dffr (cat-8:deltab)))
             (sd (cat-8:suspension-intr d)))
        (funcall sf (cat-8:cmbn 0 3 :s-bsgn))
        (funcall sf (cat-8:cmbn 2 4 3))
        (funcall sd (cat-8:cmbn 0 3 :s-bsgn))
        (funcall sd (cat-8:cmbn 2 4 3))
        (funcall sd (cat-8:cmbn 3 4 7))))


(test suspension2
      (cat-8:cat-init)
      (let* ((f (cat-8:idnt-mrph (cat-8:deltab)))
             (sf (cat-8:suspension f))
             (d (cat-8:dffr (cat-8:deltab)))
             (sd (cat-8:suspension d)))
        (cat-8:? sf (cat-8:cmbn 0 3 :s-bsgn))
        (cat-8:? sf (cat-8:cmbn 2 4 3))
        (cat-8:? sd (cat-8:cmbn 0 3 :s-bsgn))
        (cat-8:? sd (cat-8:cmbn 2 4 3))
        (cat-8:? sd (cat-8:cmbn 3 4 7))))


(test suspension3
      (cat-8:cat-init)
      (let* ((rdct (cat-8:ez (cat-8:deltab) (cat-8:deltab)))
             (srdct (cat-8:suspension rdct)))
        (cat-8:pre-check-rdct srdct)
        (setf cat-8:*tc* (cat-8:cmbn 4 1 (cat-8:crpr 0 15 0 15)))
        (setf cat-8:*bc* (cat-8:cmbn 4 1 (cat-8:tnpr 0 1 3 15)
                                 5 (cat-8:tnpr 1 3 2 7)
                                 10 (cat-8:tnpr 2 7 1 3)
                                 100 (cat-8:tnpr 3 15 0 1)))
        (check-rdct)))


(test suspension4
      (cat-8:cat-init)
      (let ((sk (cat-8:suspension (cat-8:k-z 2))))
        (cat-8:homology sk 0 10)))
