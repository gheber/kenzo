;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test z-whitehead-sintr
      (cat-7:cat-init)
      (let* ((k (cat-7:k-z 2))
             (ecc (cat-7:echcm k))
             (chml-clss
              (cat-7:build-mrph
               :sorc ecc :trgt (cat-7:z-chcm) :degr 2
               :intr #'(lambda (degr gnrt)
                         (if (= degr 2)
                             (cat-7:term-cmbn 0 1 :z-gnrt)
                             (cat-7:zero-cmbn (- degr 2))))
               :strt :gnrt :orgn '(fundamental-class k-z-2)))
             (tw (cat-7:z-whitehead-sintr k 2 chml-clss)))
        (cat-7:tw-a-sintr3 tw 1 (cat-7:absm 1 cat-7:+null-gbar+) '())
        (cat-7:tw-a-sintr3 tw 2 (cat-7:absm 3 cat-7:+null-gbar+) '())
        (funcall tw 2 (cat-7:gbar 2 0 '(55) 0 '()))
        (funcall tw 3 (cat-7:gbar 3 0 '(12 23) 0 '(34) 0 '()))
        (funcall tw 4 (cat-7:gbar 4 0 '(12 23 34) 2 '(45) 0 '(56) 0 '()))
        (setf k (cat-7:k-z 3))
        (setf ecc (cat-7:echcm k))
        (setf chml-clss
              (cat-7:build-mrph
               :sorc ecc :trgt (cat-7:z-chcm) :degr 3
               :intr #'(lambda (degr gnrt)
                         (if (= degr 3)
                             (cat-7:term-cmbn 0 -1 :z-gnrt)
                             (cat-7:zero-cmbn (- degr 3))))
               :strt :gnrt :orgn '(fundamental-class k-z-3)))
        (setf tw (cat-7:z-whitehead-sintr k 3 chml-clss))
        (cat-7:tw-a-sintr3 tw 1 (cat-7:absm 1 cat-7:+null-gbar+) cat-7:+null-gbar+)
        (cat-7:tw-a-sintr3 tw 2 (cat-7:absm 3 cat-7:+null-gbar+) cat-7:+null-gbar+)
        (cat-7:tw-a-sintr3 tw 3 (cat-7:absm 7 cat-7:+null-gbar+) cat-7:+null-gbar+)
        (funcall tw 3 (cat-7:z-fundamental-gmsm 3 55))
        (is (equalp (funcall tw 3 (cat-7:z-fundamental-gmsm 3 55))
                    (cat-7:absm 0 (cat-7:z-fundamental-gmsm 2 55))))
        (funcall tw 4 (cat-7:gbar 4 0 (cat-7:gbar 3 0 '(12 23) 0 '(34) 0 '())
                                0 (cat-7:gbar 2 0 '(45) 0 '())
                                1 (cat-7:gbar 0)
                                0 (cat-7:gbar 0)))))


(test Z2-whitehead-sintr
      (cat-7:cat-init)
      (let* ((k (cat-7:k-z2 2))
             (ecc (cat-7:echcm k))
             (chml-clss
              (cat-7:build-mrph
               :sorc ecc :trgt (cat-7:z-chcm) :degr 2
               :intr #'(lambda (degr gnrt)
                         (if (= degr 2)
                             (cat-7:term-cmbn 0 1 :z-gnrt)
                             (cat-7:zero-cmbn (- degr 2))))
               :strt :gnrt :orgn '(fundamental-class k-z-2)))
             (tw (cat-7:Z2-whitehead-sintr k 2 chml-clss)))
        (cat-7:tw-a-sintr3 tw 1 (cat-7:absm 1 cat-7:+null-gbar+) 0)
        (cat-7:tw-a-sintr3 tw 2 (cat-7:absm 3 cat-7:+null-gbar+) 0)
        (funcall tw 2 (cat-7:gbar 2 0 1 0 '()))
        (funcall tw 3 (cat-7:gbar 3 0 2
                                0 1
                                0 0))
        (funcall tw 4 (cat-7:gbar 4 0 3
                                2 1
                                0 1
                                0 0))
        (setf k (cat-7:k-z2 3))
        (setf ecc (cat-7:echcm k))
        (setf chml-clss
              (cat-7:build-mrph
               :sorc ecc :trgt (cat-7:z-chcm) :degr 3
               :intr #'(lambda (degr gnrt)
                         (if (= degr 3)
                             (cat-7:term-cmbn 0 -1 :z-gnrt)
                             (cat-7:zero-cmbn (- degr 3))))
               :strt :gnrt :orgn '(fundamental-class k-z-3)))
        (setf tw (cat-7:Z2-whitehead-sintr k 3 chml-clss))
        (cat-7:tw-a-sintr3 tw 1 (cat-7:absm 1 cat-7:+null-gbar+) 0)
        (cat-7:tw-a-sintr3 tw 2 (cat-7:absm 3 cat-7:+null-gbar+) 0)
        (cat-7:tw-a-sintr3 tw 3 (cat-7:absm 7 cat-7:+null-gbar+) 0)
        (funcall tw 3 (cat-7:Z2-fundamental-gmsm 3 1))
        (is (equalp (funcall tw 3 (cat-7:Z2-fundamental-gmsm 3 1))
                    (cat-7:absm 0 (cat-7:Z2-fundamental-gmsm 2 1))))
        (funcall tw 4 (cat-7:gbar 4 0 (cat-7:gbar 3 0 2 0 1 0 0)
                                0 (cat-7:gbar 2 0 1 0 0)
                                1 (cat-7:gbar 0)
                                0 (cat-7:gbar 0)))))


#|

;; long running... -> should go into a performance test suite

(test Z2-whitehead
      (cat-7:cat-init)
      (let* ((m (cat-7:moore 2 4))
             (chml-clss
              (cat-7:build-mrph
               :sorc m :trgt (cat-7:z-chcm) :degr -4
               :intr #'(lambda (dmns gmsm)
                         (if (= 4 dmns)
                             (cat-7:cmbn 0 1 :z-gnrt)
                             (cat-7:zero-cmbn (- dmns 4))))
               :strt :gnrt
               :orgn '(chml-clss moore 2 4)))
             (mf (cat-7:z2-whitehead m chml-clss))
             (mt (cat-7:fibration-total mf)))
        (cat-7:homology mt 0 10)))
|#
