;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test suspension-cmpr
      (cat-9:cat-9-init)
      (let* ((cc (cat-9:deltab))
             (cmpr (cat-9:suspension-cmpr (cat-9:cmpr cc))))
        (is (equal :equal (funcall cmpr :s-bsgn :s-bsgn)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test suspension-basis
      (cat-9:cat-9-init)
      (let* ((cc (cat-9:sphere 2))
             (basis (cat-9:suspension-basis (cat-9:basis cc))))
        (dotimes (i 6)
          (print (funcall basis i)))
        (cat-9:suspension-basis :locally-effective)))


(test suspension-intr-dffr
      (cat-9:cat-9-init)
      (let* ((cc (cat-9:deltab))
             (intr (cat-9:suspension-intr-dffr (cat-9:dffr cc))))
        (funcall intr (cat-9:cmbn 0 3 :s-bsgn))
        (funcall intr (cat-9:cmbn 3 66 7))))


(test suspension
      (cat-9:cat-9-init)
      (let* ((cc (cat-9:deltab))
             (scc (cat-9:suspension cc)))
        (cat-9:cmpr scc 7 11)
        (signals simple-error (cat-9:basis scc 3))
        (cat-9:bsgn scc)
        (cat-9:? scc 3 11)
        (setf scc (cat-9:suspension cc 2))
        (cat-9:? scc 4 11)))


(test suspension-intr-cprd
      (cat-9:suspension-intr-cprd (cat-9:cmbn 0))
      (cat-9:suspension-intr-cprd (cat-9:cmbn 0 5 :s-bsgn))
      (cat-9:suspension-intr-cprd (cat-9:cmbn 3 4 7 5 11)))


(test coal
      (cat-9:cat-9-init)
      (let* ((coal (cat-9:deltab))
             (scoal (cat-9:suspension coal)))
        (setf scoal (cat-9:suspension coal))
        (cat-9:? scoal 3 7)
        (cat-9:cprd scoal 3 7)))


(test suspension-face
      (cat-9:cat-9-init)
      (let* ((ss (cat-9:deltab))
             (face (cat-9:suspension-face (cat-9:face ss)))
             (m (cat-9:moore 2 3)))
        (funcall face 4 4 15)
        (funcall face 0 4 15)
        #+(or ecl sbcl)
        (signals type-error (cat-9:check-faces #'cat-9:f-cmpr face 4 15))
        #+(or ecl sbcl)
        (signals type-error (cat-9:check-faces #'cat-9:f-cmpr face 3 7))
        #+(or ecl sbcl)
        (signals type-error (cat-9:check-faces #'cat-9:f-cmpr face 2 3))
        #-(or ecl sbcl) (cat-9:check-faces #'cat-9:f-cmpr face 4 15)
        #-(or ecl sbcl) (cat-9:check-faces #'cat-9:f-cmpr face 3 7)
        #-(or ecl sbcl) (cat-9:check-faces #'cat-9:f-cmpr face 2 3)
        (setf face (cat-9:suspension-face (cat-9:face m)))
        (dotimes (i 5) (print (funcall face i 4 'm3)))
        (dotimes (i 6) (print (funcall face i 5 'mm4)))
        (cat-9:check-faces #'cat-9:s-cmpr face 4 'm3)
        (cat-9:check-faces #'cat-9:s-cmpr face 5 'mm4)
        ))


(test suspension1
      (cat-9:cat-9-init)
      (let* ((d (cat-9:deltab))
             (sd (cat-9:suspension d))
             (m (cat-9:moore 2 1))
             (sm (cat-9:suspension m)))
        (setf sd (cat-9:suspension d))
        (cat-9:? sd 3 7)
        (cat-9:cprd sd 3 7)
        (cat-9:face sd 3 3 7)
        (cat-9:face sd 2 3 7)
        (cat-9:homology sm 0 5)
        (setf ssm (cat-9:suspension sm))
        (cat-9:homology ssm 0 6)))


(test suspension-intr
      (cat-9:cat-9-init)
      (let* ((f (cat-9:idnt-mrph (cat-9:deltab)))
             (sf (cat-9:suspension-intr f))
             (d (cat-9:dffr (cat-9:deltab)))
             (sd (cat-9:suspension-intr d)))
        (funcall sf (cat-9:cmbn 0 3 :s-bsgn))
        (funcall sf (cat-9:cmbn 2 4 3))
        (funcall sd (cat-9:cmbn 0 3 :s-bsgn))
        (funcall sd (cat-9:cmbn 2 4 3))
        (funcall sd (cat-9:cmbn 3 4 7))))


(test suspension2
      (cat-9:cat-9-init)
      (let* ((f (cat-9:idnt-mrph (cat-9:deltab)))
             (sf (cat-9:suspension f))
             (d (cat-9:dffr (cat-9:deltab)))
             (sd (cat-9:suspension d)))
        (cat-9:? sf (cat-9:cmbn 0 3 :s-bsgn))
        (cat-9:? sf (cat-9:cmbn 2 4 3))
        (cat-9:? sd (cat-9:cmbn 0 3 :s-bsgn))
        (cat-9:? sd (cat-9:cmbn 2 4 3))
        (cat-9:? sd (cat-9:cmbn 3 4 7))))


(test suspension3
      (cat-9:cat-9-init)
      (let* ((rdct (cat-9:ez (cat-9:deltab) (cat-9:deltab)))
             (srdct (cat-9:suspension rdct)))
        (cat-9:pre-check-rdct srdct)
        (setf cat-9:*tc* (cat-9:cmbn 4 1 (cat-9:crpr 0 15 0 15)))
        (setf cat-9:*bc* (cat-9:cmbn 4 1 (cat-9:tnpr 0 1 3 15)
                                 5 (cat-9:tnpr 1 3 2 7)
                                 10 (cat-9:tnpr 2 7 1 3)
                                 100 (cat-9:tnpr 3 15 0 1)))
        (check-rdct)))


(test suspension4
      (cat-9:cat-9-init)
      (let ((sk (cat-9:suspension (cat-9:k-z 2))))
        (cat-9:homology sk 0 10)))
