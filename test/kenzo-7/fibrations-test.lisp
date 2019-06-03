;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


#|
(test fibration-total-face
      (cat-7:cat-init)
      (let* ((h (cat-7:hopf 1))
             (sf (cat-7:serre-face h)))
        (funcall sf 0 2 (cat-7:crpr 0 's2 0 '(2 4)))
        (funcall sf 1 2 (cat-7:crpr 0 's2 0 '(2 4)))
        (funcall sf 2 2 (cat-7:crpr 0 's2 0 '(2 4)))
        (funcall sf 3 3 (cat-7:crpr 1 's2 2 '(2 4)))))
|#


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
             (e (cat-7:fibration-total tw)))
        (cat-7:homology e 2)
        (cat-7:homology e 0 8)))


(test fibration-total1
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
             (e (cat-7:fibration-total tw)))
        (cat-7:homology e 2)
        (cat-7:homology e 0 8)))

#|
(test fibration-total2
      (cat-7:cat-init)
      (let* ((tw (cat-7:kdivide-z2-twist opr opr-chml-clss))
             (pr-4 (cat-7:fibration-total tw))
             (absm (cat-7:absm 0 (cat-7:crpr 0 (cat-7:loop3 0 5 2) 0 4)))
             (hat (mapcar #'(lambda (i) (cat-7:face pr-4 i 4 absm))
                          (cat-7:<a-b> 0 4))))
        (dotimes (i 5)
          (print (cat-7:kfll pr-4 i 4 (remove (nth i hat) hat)))
          (cat-7:check-kan pr-4 i 4 (remove (nth i hat) hat)))))


(test fibration-total3
      (cat-7:cat-init)
      (let* ((os3-fibration os3-fibration)
             (total (cat-7:fibration-total os3-fibration))
             (absm (cat-7:absm 1 (cat-7:crpr 0 (cat-7:loop3 1 's3 1 2 's3 -2)
                                         0 '(-2 1 1))))
             (hat (mapcar #'(lambda (i) (cat-7:face total i 4 absm))
                          '(0 1 2 3 4))))
        (dotimes (i 5)
          (print (kfll total i 4 (remove (nth i hat) hat)))
          (check-kan total i 4 (remove (nth i hat) hat)))))
|#
