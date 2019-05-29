;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(when (or (string= (package-name (find-package 'cat)) "CAT-7")
          (string= (package-name (find-package 'cat)) "CAT-8"))


  (test fibration-dtau-d-intr
        (cat:cat-init)
        (let* ((s2 (cat:sphere 2))
               (k (cat:k-z2-1))
               (tw (cat:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat:absm 0 1))
                    :orgn '(s2-tw-kz2)))
               (dt-d (cat:fibration-dtau-d-intr tw)))
          (funcall dt-d 0 (cat:crpr 0 '* 0 0))
          (funcall dt-d 3 (cat:crpr 4 's2 0 3))
          (funcall dt-d 3 (cat:crpr 2 's2 0 3))
          (funcall dt-d 3 (cat:crpr 2 's2 5 1))
          (funcall dt-d 2 (cat:crpr 0 's2 0 2))))

  (test fibration-dtau-d-intr1
        (cat:cat-init)
        (let* ((s2 (cat:sphere 2))
               (k (cat:k-z-1))
               (tw (cat:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat:absm 0 (list 1)))
                    :orgn '(s2-tw-kz)))
               (dt-d (cat:fibration-dtau-d-intr tw)))
          (funcall dt-d 0 (cat:crpr 0 '* 0 nil))
          (funcall dt-d 3 (cat:crpr 4 's2 0 '(2 3 4)))
          (funcall dt-d 3 (cat:crpr 2 's2 0 '(2 3 4)))
          (funcall dt-d 3 (cat:crpr 2 's2 5 '(5)))
          (funcall dt-d 2 (cat:crpr 0 's2 0 '(3 -2)))))


  (test fibration-dtau-d
        (cat:cat-init)
        (let* ((s2 (cat:sphere 2))
               (k (cat:k-z-1))
               (tw (cat:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat:absm 0 (list 1)))
                    :orgn '(s2-tw-kz)))
               (dt-d (cat:fibration-dtau-d tw)))
          (cat:? dt-d 0 (cat:crpr 0 '* 0 nil))
          (cat:? dt-d 3 (cat:crpr 4 's2 0 '(2 3 4)))
          (cat:? dt-d 3 (cat:crpr 2 's2 0 '(2 3 4)))
          (cat:? dt-d 3 (cat:crpr 2 's2 5 '(5)))
          (cat:? dt-d 2 (cat:crpr 0 's2 0 '(3 -2)))))


  (test brown-reduction
        (cat:cat-init)
        (let* ((s2 (cat:sphere 2))
               (k (cat:k-z2-1))
               (tw (cat:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat:absm 0 1))
                    :orgn '(s2-tw-kz2)))
               (brown (cat:brown-reduction tw))
               (tcc (cat:tcc brown))
               (bcc (cat:bcc brown)))
          (cat:homology bcc 3)
          (cat:homology tcc 1 8)
          (cat:homology bcc 1 8)))

  #|
  (test brown-reduction1
  (cat:cat-init)
  (let* ((s2 (cat:sphere 2))
  (k (cat:k-z-1))
  (tw (cat:build-smmr
  :sorc s2
  :trgt k
  :degr -1
  :sintr #'(lambda (dmns gmsm)
  (cat:absm 0 (list 1)))
  :orgn '(s2-tw-kz)))
  (brown (cat:brown-reduction tw)))
  (cat:homology (cat:tcc brown) 1 5)
  (cat:homology (cat:bcc brown) 1 5)))
  |#

  (test right-serre-efhm
        (cat:cat-init)
        (let* ((s2 (cat:sphere 2))
               (k (cat:k-z-1))
               (tw (cat:build-smmr
                    :sorc s2
                    :trgt k
                    :degr -1
                    :sintr #'(lambda (dmns gmsm)
                               (cat:absm 0 (list 1)))
                    :orgn '(s2-tw-kz1)))
               (rh (cat:right-serre-efhm tw))
               (rbcc (cat:rbcc rh)))
          (cat:homology rbcc 0 5))))


(test fibration-total
      (cat:cat-init)
      (let* ((s2 (cat:sphere 2))
             (k (cat:k-z-1))
             (tw (cat:build-smmr
                  :sorc s2
                  :trgt k
                  :degr -1
                  :sintr #'(lambda (dmns gmsm)
                             (cat:absm 0 (list 1)))
                  :orgn '(s2-tw-kz1)))
             (p3r (cat:fibration-total tw)))
        (cat:homology p3r 1)))


(test dummy
      (is (not (null t))))
