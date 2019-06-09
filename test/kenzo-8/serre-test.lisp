;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test fibration-dtau-d-intr
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z2-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 1))
                  :orgn '(s2-tw-kz2)))
             (dt-d (cat-8:fibration-dtau-d-intr tw)))
        (funcall dt-d 0 (cat-8:crpr 0 '* 0 0))
        (funcall dt-d 3 (cat-8:crpr 4 's2 0 3))
        (funcall dt-d 3 (cat-8:crpr 2 's2 0 3))
        (funcall dt-d 3 (cat-8:crpr 2 's2 5 1))
        (funcall dt-d 2 (cat-8:crpr 0 's2 0 2))))

(test fibration-dtau-d-intr1
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 (list 1)))
                  :orgn '(s2-tw-kz)))
             (dt-d (cat-8:fibration-dtau-d-intr tw)))
        (funcall dt-d 0 (cat-8:crpr 0 '* 0 nil))
        (funcall dt-d 3 (cat-8:crpr 4 's2 0 '(2 3 4)))
        (funcall dt-d 3 (cat-8:crpr 2 's2 0 '(2 3 4)))
        (funcall dt-d 3 (cat-8:crpr 2 's2 5 '(5)))
        (funcall dt-d 2 (cat-8:crpr 0 's2 0 '(3 -2)))))


(test fibration-dtau-d
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 (list 1)))
                  :orgn '(s2-tw-kz)))
             (dt-d (cat-8:fibration-dtau-d tw)))
        (cat-8:? dt-d 0 (cat-8:crpr 0 '* 0 nil))
        (cat-8:? dt-d 3 (cat-8:crpr 4 's2 0 '(2 3 4)))
        (cat-8:? dt-d 3 (cat-8:crpr 2 's2 0 '(2 3 4)))
        (cat-8:? dt-d 3 (cat-8:crpr 2 's2 5 '(5)))
        (cat-8:? dt-d 2 (cat-8:crpr 0 's2 0 '(3 -2)))))


(test brown-reduction
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z2-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 1))
                  :orgn '(s2-tw-kz2)))
             (brown (cat-8:brown-reduction tw))
             (tcc (cat-8:tcc brown))
             (bcc (cat-8:bcc brown)))
        (cat-8:homology bcc 3)
        (cat-8:homology tcc 1 8)
        (cat-8:homology bcc 1 8)))

#|
(test brown-reduction1
(cat-8:cat-init)
(let* ((s2 (cat-8:sphere 2))
(k (cat-8:k-z-1))
(tw (cat-8:build-smmr
:sorc s2
:trgt k
:degr -1
:sintr #'(lambda (dmns gmsm)
(cat-8:absm 0 (list 1)))
:orgn '(s2-tw-kz)))
(brown (cat-8:brown-reduction tw)))
(cat-8:homology (cat-8:tcc brown) 1 5)
(cat-8:homology (cat-8:bcc brown) 1 5)))
|#

(test right-serre-efhm
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (rh (cat-8:right-serre-efhm tw))
             (rbcc (cat-8:rbcc rh)))
        (cat-8:homology rbcc 0 5)))



(test fibration-total
      (cat-8:cat-init)
      (let* ((s2 (cat-8:sphere 2))
             (k (cat-8:k-z-1))
             (tw (cat-8:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-8:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (p3r (cat-8:fibration-total tw)))
        (cat-8:homology p3r 1)))


(test dummy
      (is (not (null t))))
