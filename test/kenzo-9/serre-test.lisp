;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(when (or (string= (package-name (find-package 'cat)) "CAT-7")
          (string= (package-name (find-package 'cat)) "CAT-8"))


  (test fibration-dtau-d-intr
        (cat-9:cat-9-init)
        (let* ((s2 (cat-9:sphere 2))
               (k (cat-9:k-z2-1))
               (tw (cat-9:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat-9:absm 0 1))
                    :orgn '(s2-tw-kz2)))
               (dt-d (cat-9:fibration-dtau-d-intr tw)))
          (funcall dt-d 0 (cat-9:crpr 0 '* 0 0))
          (funcall dt-d 3 (cat-9:crpr 4 's2 0 3))
          (funcall dt-d 3 (cat-9:crpr 2 's2 0 3))
          (funcall dt-d 3 (cat-9:crpr 2 's2 5 1))
          (funcall dt-d 2 (cat-9:crpr 0 's2 0 2))))

  (test fibration-dtau-d-intr1
        (cat-9:cat-9-init)
        (let* ((s2 (cat-9:sphere 2))
               (k (cat-9:k-z-1))
               (tw (cat-9:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat-9:absm 0 (list 1)))
                    :orgn '(s2-tw-kz)))
               (dt-d (cat-9:fibration-dtau-d-intr tw)))
          (funcall dt-d 0 (cat-9:crpr 0 '* 0 nil))
          (funcall dt-d 3 (cat-9:crpr 4 's2 0 '(2 3 4)))
          (funcall dt-d 3 (cat-9:crpr 2 's2 0 '(2 3 4)))
          (funcall dt-d 3 (cat-9:crpr 2 's2 5 '(5)))
          (funcall dt-d 2 (cat-9:crpr 0 's2 0 '(3 -2)))))


  (test fibration-dtau-d
        (cat-9:cat-9-init)
        (let* ((s2 (cat-9:sphere 2))
               (k (cat-9:k-z-1))
               (tw (cat-9:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat-9:absm 0 (list 1)))
                    :orgn '(s2-tw-kz)))
               (dt-d (cat-9:fibration-dtau-d tw)))
          (cat-9:? dt-d 0 (cat-9:crpr 0 '* 0 nil))
          (cat-9:? dt-d 3 (cat-9:crpr 4 's2 0 '(2 3 4)))
          (cat-9:? dt-d 3 (cat-9:crpr 2 's2 0 '(2 3 4)))
          (cat-9:? dt-d 3 (cat-9:crpr 2 's2 5 '(5)))
          (cat-9:? dt-d 2 (cat-9:crpr 0 's2 0 '(3 -2)))))


  (test brown-reduction
        (cat-9:cat-9-init)
        (let* ((s2 (cat-9:sphere 2))
               (k (cat-9:k-z2-1))
               (tw (cat-9:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat-9:absm 0 1))
                    :orgn '(s2-tw-kz2)))
               (brown (cat-9:brown-reduction tw))
               (tcc (cat-9:tcc brown))
               (bcc (cat-9:bcc brown)))
          (cat-9:homology bcc 3)
          (cat-9:homology tcc 1 8)
          (cat-9:homology bcc 1 8)))

  #|
  (test brown-reduction1
  (cat-9:cat-9-init)
  (let* ((s2 (cat-9:sphere 2))
  (k (cat-9:k-z-1))
  (tw (cat-9:build-smmr
  :sorc s2
  :trgt k
  :degr -1
  :sintr #'(lambda (dmns gmsm)
  (cat-9:absm 0 (list 1)))
  :orgn '(s2-tw-kz)))
  (brown (cat-9:brown-reduction tw)))
  (cat-9:homology (cat-9:tcc brown) 1 5)
  (cat-9:homology (cat-9:bcc brown) 1 5)))
  |#

  (test right-serre-efhm
        (cat-9:cat-9-init)
        (let* ((s2 (cat-9:sphere 2))
               (k (cat-9:k-z-1))
               (tw (cat-9:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat-9:absm 0 (list 1)))
                    :orgn '(s2-tw-kz1)))
               (rh (cat-9:right-serre-efhm tw))
               (rbcc (cat-9:rbcc rh)))
          (cat-9:homology rbcc 0 5))))


(test fibration-total
      (cat-9:cat-9-init)
      (let* ((s2 (cat-9:sphere 2))
             (k (cat-9:k-z-1))
             (tw (cat-9:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat-9:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (p3r (cat-9:fibration-total tw)))
        (cat-9:homology p3r 1)))


(test dummy
      (is (not (null t))))
