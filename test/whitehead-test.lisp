;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)


(test z-whitehead-sintr
      (cat:cat-init)
      (let* ((k (cat:k-z 2))
             (ecc (cat:echcm k))
             (chml-clss
              (if (or (string= (package-name (find-package 'cat)) "CAT-7")
                      (string= (package-name (find-package 'cat)) "CAT-8"))
                  (cat:build-mrph
                   :sorc ecc :trgt (cat:z-chcm) :degr 2
                   :intr #'(lambda (degr gnrt)
                             (if (= degr 2)
                                 (cat:term-cmbn 0 1 :z-gnrt)
                                 (cat:zero-cmbn (- degr 2))))
                   :strt :gnrt :orgn '(fundamental-class k-z-2))
                  (cat:chml-clss k 2)))
             (tw (cat:z-whitehead-sintr k 2 chml-clss)))
        (cat:tw-a-sintr3 tw 1 (cat:absm 1 cat:+null-gbar+) '())
        (cat:tw-a-sintr3 tw 2 (cat:absm 3 cat:+null-gbar+) '())
        (funcall tw 2 (cat:gbar 2 0 '(55) 0 '()))
        (funcall tw 3 (cat:gbar 3 0 '(12 23) 0 '(34) 0 '()))
        (funcall tw 4 (cat:gbar 4 0 '(12 23 34) 2 '(45) 0 '(56) 0 '()))
        (setf k (cat:k-z 3))
        (setf ecc (cat:echcm k))
        (setf chml-clss
              (if (or (string= (package-name (find-package 'cat)) "CAT-7")
                      (string= (package-name (find-package 'cat)) "CAT-8"))
                  (cat:build-mrph
                   :sorc ecc :trgt (cat:z-chcm) :degr 3
                   :intr #'(lambda (degr gnrt)
                             (if (= degr 3)
                                 (cat:term-cmbn 0 -1 :z-gnrt)
                                 (cat:zero-cmbn (- degr 3))))
                   :strt :gnrt :orgn '(fundamental-class k-z-3))
                  (cat:chml-clss k 3)))
        (setf tw (cat:z-whitehead-sintr k 3 chml-clss))
        (cat:tw-a-sintr3 tw 1 (cat:absm 1 cat:+null-gbar+) cat:+null-gbar+)
        (cat:tw-a-sintr3 tw 2 (cat:absm 3 cat:+null-gbar+) cat:+null-gbar+)
        (cat:tw-a-sintr3 tw 3 (cat:absm 7 cat:+null-gbar+) cat:+null-gbar+)
        (funcall tw 3 (cat:z-fundamental-gmsm 3 55))
        (is (equalp (funcall tw 3 (cat:z-fundamental-gmsm 3 55))
                    (cat:absm 0 (cat:z-fundamental-gmsm 2 55))))
        (funcall tw 4 (cat:gbar 4 0 (cat:gbar 3 0 '(12 23) 0 '(34) 0 '())
                                0 (cat:gbar 2 0 '(45) 0 '())
                                1 (cat:gbar 0)
                                0 (cat:gbar 0)))))


(test Z2-whitehead-sintr
      (cat:cat-init)
      (let* ((k (cat:k-z2 2))
             (ecc (cat:echcm k))
             (chml-clss
              (if (or (string= (package-name (find-package 'cat)) "CAT-7")
                      (string= (package-name (find-package 'cat)) "CAT-8"))
                  (cat:build-mrph
                   :sorc ecc :trgt (cat:z-chcm) :degr 2
                   :intr #'(lambda (degr gnrt)
                             (if (= degr 2)
                                 (cat:term-cmbn 0 1 :z-gnrt)
                                 (cat:zero-cmbn (- degr 2))))
                   :strt :gnrt :orgn '(fundamental-class k-z-2))
                  (cat:chml-clss k 2)))
             (tw (cat:Z2-whitehead-sintr k 2 chml-clss)))
        (cat:tw-a-sintr3 tw 1 (cat:absm 1 cat:+null-gbar+) 0)
        (cat:tw-a-sintr3 tw 2 (cat:absm 3 cat:+null-gbar+) 0)
        (funcall tw 2 (cat:gbar 2 0 1 0 '()))
        (funcall tw 3 (cat:gbar 3 0 2
                                0 1
                                0 0))
        (funcall tw 4 (cat:gbar 4 0 3
                                2 1
                                0 1
                                0 0))
        (setf k (cat:k-z2 3))
        (setf ecc (cat:echcm k))
        (setf chml-clss
              (if (or (string= (package-name (find-package 'cat)) "CAT-7")
                      (string= (package-name (find-package 'cat)) "CAT-8"))
                  (cat:build-mrph
                   :sorc ecc :trgt (cat:z-chcm) :degr 3
                   :intr #'(lambda (degr gnrt)
                             (if (= degr 3)
                                 (cat:term-cmbn 0 -1 :z-gnrt)
                                 (cat:zero-cmbn (- degr 3))))
                   :strt :gnrt :orgn '(fundamental-class k-z-3))
                  (cat:chml-clss k 3)))
        (setf tw (cat:Z2-whitehead-sintr k 3 chml-clss))
        (cat:tw-a-sintr3 tw 1 (cat:absm 1 cat:+null-gbar+) 0)
        (cat:tw-a-sintr3 tw 2 (cat:absm 3 cat:+null-gbar+) 0)
        (cat:tw-a-sintr3 tw 3 (cat:absm 7 cat:+null-gbar+) 0)
        (funcall tw 3 (cat:Z2-fundamental-gmsm 3 1))
        (is (equalp (funcall tw 3 (cat:Z2-fundamental-gmsm 3 1))
                    (cat:absm 0 (cat:Z2-fundamental-gmsm 2 1))))
        (funcall tw 4 (cat:gbar 4 0 (cat:gbar 3 0 2 0 1 0 0)
                                0 (cat:gbar 2 0 1 0 0)
                                1 (cat:gbar 0)
                                0 (cat:gbar 0)))))


#|

;; long running... -> should go into a performance test suite

(test Z2-whitehead
      (cat:cat-init)
      (let* ((m (cat:moore 2 4))
             (chml-clss
              (cat:build-mrph
               :sorc m :trgt (cat:z-chcm) :degr -4
               :intr #'(lambda (dmns gmsm)
                         (if (= 4 dmns)
                             (cat:cmbn 0 1 :z-gnrt)
                             (cat:zero-cmbn (- dmns 4))))
               :strt :gnrt
               :orgn '(chml-clss moore 2 4)))
             (mf (cat:z2-whitehead m chml-clss))
             (mt (cat:fibration-total mf)))
        (cat:homology mt 0 10)))
|#
