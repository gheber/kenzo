;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)


#|
(test fibration-total-face
      (cat:cat-init)
      (let* ((h (cat:hopf 1))
             (sf (cat:serre-face h)))
        (funcall sf 0 2 (cat:crpr 0 's2 0 '(2 4)))
        (funcall sf 1 2 (cat:crpr 0 's2 0 '(2 4)))
        (funcall sf 2 2 (cat:crpr 0 's2 0 '(2 4)))
        (funcall sf 3 3 (cat:crpr 1 's2 2 '(2 4)))))
|#


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
             (e (cat:fibration-total tw)))
        (cat:homology e 2)
        (cat:homology e 0 8)))


(test fibration-total1
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
             (e (cat:fibration-total tw)))
        (cat:homology e 2)
        (cat:homology e 0 8)))

#|
(test fibration-total2
      (cat:cat-init)
      (let* ((tw (cat:kdivide-z2-twist opr opr-chml-clss))
             (pr-4 (cat:fibration-total tw))
             (absm (cat:absm 0 (cat:crpr 0 (cat:loop3 0 5 2) 0 4)))
             (hat (mapcar #'(lambda (i) (cat:face pr-4 i 4 absm))
                          (cat:<a-b> 0 4))))
        (dotimes (i 5)
          (print (cat:kfll pr-4 i 4 (remove (nth i hat) hat)))
          (cat:check-kan pr-4 i 4 (remove (nth i hat) hat)))))


(test fibration-total3
      (cat:cat-init)
      (let* ((os3-fibration os3-fibration)
             (total (cat:fibration-total os3-fibration))
             (absm (cat:absm 1 (cat:crpr 0 (cat:loop3 1 's3 1 2 's3 -2)
                                         0 '(-2 1 1))))
             (hat (mapcar #'(lambda (i) (cat:face total i 4 absm))
                          '(0 1 2 3 4))))
        (dotimes (i 5)
          (print (kfll total i 4 (remove (nth i hat) hat)))
          (check-kan total i 4 (remove (nth i hat) hat)))))
|#
