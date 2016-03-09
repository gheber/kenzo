;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test suspension-cmpr
      (cat:cat-init)
      (let* ((cc (cat:deltab))
             (cmpr (cat:suspension-cmpr (cat:cmpr cc))))
        (is (equal :equal (funcall cmpr :s-bsgn :s-bsgn)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test suspension-basis
      (cat:cat-init)
      (let* ((cc (cat:sphere 2))
             (basis (cat:suspension-basis (cat:basis cc))))
        (dotimes (i 6)
          (print (funcall basis i)))
        (cat:suspension-basis :locally-effective)))


(test suspension-intr-dffr
      (cat:cat-init)
      (let* ((cc (cat:deltab))
             (intr (cat:suspension-intr-dffr (cat:dffr cc))))
        (funcall intr (cat:cmbn 0 3 :s-bsgn))
        (funcall intr (cat:cmbn 3 66 7))))


(test suspension
      (cat:cat-init)
      (let* ((cc (cat:deltab))
             (scc (cat:suspension cc)))
        (cat:cmpr scc 7 11)
        (signals simple-error (cat:basis scc 3))
        (cat:bsgn scc)
        (cat:? scc 3 11)
        (setf scc (cat:suspension cc 2))
        (cat:? scc 4 11)))


(test suspension-intr-cprd
      (cat:suspension-intr-cprd (cat:cmbn 0))
      (cat:suspension-intr-cprd (cat:cmbn 0 5 :s-bsgn))
      (cat:suspension-intr-cprd (cat:cmbn 3 4 7 5 11)))


(test coal
      (cat:cat-init)
      (let* ((coal (cat:deltab))
             (scoal (cat:suspension coal)))
        (setf scoal (cat:suspension coal))
        (cat:? scoal 3 7)
        (cat:cprd scoal 3 7)))


(test suspension-face
      (cat:cat-init)
      (let* ((ss (cat:deltab))
             (face (cat:suspension-face (cat:face ss)))
             (m (cat:moore 2 3)))
        (funcall face 4 4 15)
        (funcall face 0 4 15)
        #+(or ecl sbcl)
        (signals type-error (cat:check-faces #'cat:f-cmpr face 4 15))
        #+(or ecl sbcl)
        (signals type-error (cat:check-faces #'cat:f-cmpr face 3 7))
        #+(or ecl sbcl)
        (signals type-error (cat:check-faces #'cat:f-cmpr face 2 3))
        #-(or ecl sbcl) (cat:check-faces #'cat:f-cmpr face 4 15)
        #-(or ecl sbcl) (cat:check-faces #'cat:f-cmpr face 3 7)
        #-(or ecl sbcl) (cat:check-faces #'cat:f-cmpr face 2 3)
        (setf face (cat:suspension-face (cat:face m)))
        (dotimes (i 5) (print (funcall face i 4 'm3)))
        (dotimes (i 6) (print (funcall face i 5 'mm4)))
        (cat:check-faces #'cat:s-cmpr face 4 'm3)
        (cat:check-faces #'cat:s-cmpr face 5 'mm4)
        ))


(test suspension1
      (cat:cat-init)
      (let* ((d (cat:deltab))
             (sd (cat:suspension d))
             (m (cat:moore 2 1))
             (sm (cat:suspension m)))
        (setf sd (cat:suspension d))
        (cat:? sd 3 7)
        (cat:cprd sd 3 7)
        (cat:face sd 3 3 7)
        (cat:face sd 2 3 7)
        (cat:homology sm 0 5)
        (setf ssm (cat:suspension sm))
        (cat:homology ssm 0 6)))


(test suspension-intr
      (cat:cat-init)
      (let* ((f (cat:idnt-mrph (cat:deltab)))
             (sf (cat:suspension-intr f))
             (d (cat:dffr (cat:deltab)))
             (sd (cat:suspension-intr d)))
        (funcall sf (cat:cmbn 0 3 :s-bsgn))
        (funcall sf (cat:cmbn 2 4 3))
        (funcall sd (cat:cmbn 0 3 :s-bsgn))
        (funcall sd (cat:cmbn 2 4 3))
        (funcall sd (cat:cmbn 3 4 7))))


(test suspension2
      (cat:cat-init)
      (let* ((f (cat:idnt-mrph (cat:deltab)))
             (sf (cat:suspension f))
             (d (cat:dffr (cat:deltab)))
             (sd (cat:suspension d)))
        (cat:? sf (cat:cmbn 0 3 :s-bsgn))
        (cat:? sf (cat:cmbn 2 4 3))
        (cat:? sd (cat:cmbn 0 3 :s-bsgn))
        (cat:? sd (cat:cmbn 2 4 3))
        (cat:? sd (cat:cmbn 3 4 7))))


(test suspension3
      (cat:cat-init)
      (let* ((rdct (cat:ez (cat:deltab) (cat:deltab)))
             (srdct (cat:suspension rdct)))
        (cat:pre-check-rdct srdct)
        (setf cat:*tc* (cat:cmbn 4 1 (cat:crpr 0 15 0 15)))
        (setf cat:*bc* (cat:cmbn 4 1 (cat:tnpr 0 1 3 15)
                                 5 (cat:tnpr 1 3 2 7)
                                 10 (cat:tnpr 2 7 1 3)
                                 100 (cat:tnpr 3 15 0 1)))
        (check-rdct)))


(test suspension4
      (cat:cat-init)
      (let ((sk (cat:suspension (cat:k-z 2))))
        (cat:homology sk 0 10)))
