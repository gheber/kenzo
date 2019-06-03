;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test fibration-dtau-d-intr
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z2-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 1))
                  :orgn '(s2-tw-kz2)))
             (dt-d (cat-7:fibration-dtau-d-intr tw)))
        (funcall dt-d 0 (cat-7:crpr 0 '* 0 0))
        (funcall dt-d 3 (cat-7:crpr 4 's2 0 3))
        (funcall dt-d 3 (cat-7:crpr 2 's2 0 3))
        (funcall dt-d 3 (cat-7:crpr 2 's2 5 1))
        (funcall dt-d 2 (cat-7:crpr 0 's2 0 2))))

(test fibration-dtau-d-intr1
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 (list 1)))
                  :orgn '(s2-tw-kz)))
             (dt-d (cat-7:fibration-dtau-d-intr tw)))
        (funcall dt-d 0 (cat-7:crpr 0 '* 0 nil))
        (funcall dt-d 3 (cat-7:crpr 4 's2 0 '(2 3 4)))
        (funcall dt-d 3 (cat-7:crpr 2 's2 0 '(2 3 4)))
        (funcall dt-d 3 (cat-7:crpr 2 's2 5 '(5)))
        (funcall dt-d 2 (cat-7:crpr 0 's2 0 '(3 -2)))))


(test fibration-dtau-d
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 (list 1)))
                  :orgn '(s2-tw-kz)))
             (dt-d (cat-7:fibration-dtau-d tw)))
        (cat-7:? dt-d 0 (cat-7:crpr 0 '* 0 nil))
        (cat-7:? dt-d 3 (cat-7:crpr 4 's2 0 '(2 3 4)))
        (cat-7:? dt-d 3 (cat-7:crpr 2 's2 0 '(2 3 4)))
        (cat-7:? dt-d 3 (cat-7:crpr 2 's2 5 '(5)))
        (cat-7:? dt-d 2 (cat-7:crpr 0 's2 0 '(3 -2)))))


(test brown-reduction
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z2-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 1))
                  :orgn '(s2-tw-kz2)))
             (brown (cat-7:brown-reduction tw))
             (tcc (cat-7:tcc brown))
             (bcc (cat-7:bcc brown)))
        (cat-7:homology bcc 3)
        (cat-7:homology tcc 1 8)
        (cat-7:homology bcc 1 8)))

#|
(test brown-reduction1
(cat-7:cat-init)
(let* ((s2 (cat-7:sphere 2))
(k (cat-7:k-z-1))
(tw (cat-7:build-smmr
:sorc s2
:trgt k
:degr -1
:sintr #'(lambda (dmns gmsm)
(cat-7:absm 0 (list 1)))
:orgn '(s2-tw-kz)))
(brown (cat-7:brown-reduction tw)))
(cat-7:homology (cat-7:tcc brown) 1 5)
(cat-7:homology (cat-7:bcc brown) 1 5)))
|#

(test right-serre-efhm
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (rh (cat-7:right-serre-efhm tw))
             (rbcc (cat-7:rbcc rh)))
        (cat-7:homology rbcc 0 5)))



(test fibration-total
      (cat-7:cat-init)
      (let* ((s2 (cat-7:sphere 2))
             (k (cat-7:k-z-1))
             (tw (cat-7:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-7:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (p3r (cat-7:fibration-total tw)))
        (cat-7:homology p3r 1)))


(test dummy
      (is (not (null t))))
