;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test random-matrix
      (cat-8:random-matrix 2 3 10))


(test idnt-mtrx
      (cat-8:idnt-mtrx 3))


(test copy-mtrx
      (let ((m (cat-8:random-matrix 3 4 10)))
        (is (equalp m (cat-8:copy-mtrx m)))))


(test left-submatrix
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:left-submatrix m 2)))


(test mtrx-prdc
      (let ((m1 (cat-8:random-matrix 2 3 10))
            (m2 (cat-8:random-matrix 3 2 10)))
        (cat-8:mtrx-prdc m1 m2)
        (cat-8:mtrx-prdc m2 m1)))


(test chcm-mtrx
      (cat-8:cat-init)
      (let ((d (cat-8:delta 5))
            (m (cat-8:moore 2 2)))
        (cat-8:chcm-mtrx d 3)
        (dotimes (i 5)
          (print (cat-8:chcm-mtrx m i)))
        (dotimes (i 6)
          (print (array-dimensions (cat-8:chcm-mtrx m i))))))


(test line-op
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:line-op m 1 3 2 0)))

;; mtrx-list = (P P^-1 M Q Q^-1)

(test line-op-5
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5) )
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:line-op-5 list 0 3 1 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)))


(test column-op
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:column-op m 1 3 2 0)))


(test column-op-5
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:column-op-5 list 0 3 1 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test line-swap
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:line-swap m 1 0 2)))


(test line-swap-5
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:line-swap-5 list 0 1 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test column-swap
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:column-swap m 1 0 2)))


(test column-swap-5
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:column-swap-5 list 0 1 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test column-minus
      (let ((m (cat-8:random-matrix 3 4 10)))
        (cat-8:line-minus m 1 2)
        (cat-8:column-minus m 1 2)))


(test column-minus-5
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:line-minus-5 list 0 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)
        (cat-8:column-minus-5 list 0 2)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test minimal-term
      (let ((m (cat-8:random-matrix 4 5 10)))
        (cat-8:minimal-term m 1)))


#|
;; potential divide by 0
(test minimal-rest-1
      (let ((m (cat-8:random-matrix 4 5 10)))
        (cat-8:minimal-rest-1 m 1)))

(test minimal-rest-2
      (let ((m (cat-8:random-matrix 4 5 10)))
        (cat-8:minimal-rest-2 m 1)))
|#


(test minimal-term-top-left
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:minimal-term-top-left list 0 1 3)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test minimal-term-top-left1
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:pivott list 0)
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test list-smith
      (let* ((p (cat-8:idnt-mtrx 4))
             (p-1 (cat-8:idnt-mtrx 4))
             (m (cat-8:random-matrix 4 5 10))
             (q (cat-8:idnt-mtrx 5))
             (q-1 (cat-8:idnt-mtrx 5))
             (list (list p p-1 m q q-1))
             (t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (third (cat-8:list-smith list))
        (is (equalp t1 (cat-8:mtrx-prdc p (cat-8:mtrx-prdc m q-1))))
        (cat-8:mtrx-prdc p p-1)
        (cat-8:mtrx-prdc q q-1)))


(test gnrt-name-basis
      (cat-8:gnrt-name 4)
      (cat-8:gnrt-name-basis 4))


(test echcm-kill-epi-f-intr
      (let* ((q-1 (cat-8:random-matrix 5 5 10))
             (f (cat-8:echcm-kill-epi-f-intr #'cat-8:s-cmpr 2 2 5 '(a b c d e)
                                           (list 0 0 0 0 q-1))))
        (funcall f (cat-8:cmbn 2 1 'a))
        (funcall f (cat-8:cmbn 4 1 'a))
        (funcall f (cat-8:cmbn 3 1 'a 10 'b 100 'c 1000 'd 10000 'e))))

#|
(test echcm-kill-epi-g-intr
      (let* ((q (cat-8:random-matrix 5 5 10))
             (g (cat-8:echcm-kill-epi-g-intr 2 2 5 '(a b c d e)
                                           (list 0 0 0 q 0))))
        (funcall g (cat-8:cmbn 2))
        (funcall g (cat-8:cmbn 4 1 'a))
        (funcall g (cat-8:cmbn 3 1 :gn-0 10 :gn-1 100 :gn-2))))
|#

(test echcm-kill-epi-h-intr
      (let* ((p-1 (cat-8:random-matrix 2 2 10))
             (q (cat-8:random-matrix 5 5 10))
             (h (cat-8:echcm-kill-epi-h-intr #'cat-8:s-cmpr 2 2 5 '(a b)
                                           '(a b c d e)
                                           (list 0 p-1 0 q 0))))
        (funcall h (cat-8:cmbn 2 1 'a 1000 'b))
        (funcall h (cat-8:cmbn 4 1 'a))))


(test echcm-kill-epi
      (cat-8:cat-init)
      (let* ((s3 (cat-8:sphere 3))
             (s3-chml-clss (cat-8:chml-clss s3 3))
             (s3-fibration (cat-8:z-whitehead s3 s3-chml-clss))
             (s3-4 (cat-8:fibration-total s3-fibration))
             (ecc (cat-8:echcm s3-4))
             (rdct (cat-8:echcm-kill-epi ecc 2))
             s3-4-chml-clss s3-4-fibration s3-5
             rdct1 rdct2 rdct3 rdct12 rdct123)
        (cat-8:pre-check-rdct rdct)
        (setf cat-8:*tc* (cat-8:cmbn 0 1 (cat-8:bsgn ecc))
              cat-8:*bc* cat-8:*tc*)
        (check-rdct)
        (setf cat-8:*tc* (cat-8:cmbn 2 1 (first (cat-8:basis ecc 2))))
        (check-rdct)
        (setf cat-8:*tc* (cat-8:cmbn 3 1 (first (cat-8:basis ecc 3))))
        (check-rdct)
        (setf cat-8:*tc* (cat-8:cmbn 4 1 (first (cat-8:basis ecc 4)))
              cat-8:*bc* cat-8:*tc*)
        (check-rdct)
        (setf s3-4-chml-clss (cat-8:chml-clss s3-4 4))
        (setf s3-4-fibration (cat-8:z2-whitehead s3-4 s3-4-chml-clss))
        (setf s3-5 (cat-8:fibration-total s3-4-fibration))
        (setf ecc (cat-8:echcm s3-5))
        (dotimes (i 7)
          (format t "~%DIM = ~D ; LENGTH = ~D" i (length (cat-8:basis ecc i))))
        (setf rdct1 (cat-8:echcm-kill-epi ecc 2))
        (setf rdct2 (cat-8:echcm-kill-epi (cat-8:bcc rdct1) 3))
        (setf rdct3 (cat-8:echcm-kill-epi (cat-8:bcc rdct2) 4))
        (setf rdct12 (cat-8:cmps rdct2 rdct1))
        (setf rdct123 (cat-8:cmps rdct3 rdct12))
        (cat-8:pre-check-rdct rdct123)
        (setf cat-8:*tc* (cat-8:cmbn 0 1 (cat-8:bsgn ecc)) cat-8:*bc* cat-8:*tc*)
        (check-rdct)
        (setf cat-8:*tc* (cat-8:cmbn 2 1 (first (cat-8:basis ecc 2))))
        (check-rdct)
        (let ((b3 (cat-8:basis ecc 3)))
          (setf cat-8:*tc* (cat-8:cmbn 3 1 (first b3) 10 (second b3))))
        (check-rdct)
        (let ((b4 (cat-8:basis ecc 4)))
          (setf cat-8:*tc* (cat-8:cmbn 4 1 (first b4) 10 (second b4))))
        (check-rdct)
        (let ((b5 (cat-8:basis ecc 5)))
          (setf cat-8:*tc* (cat-8:cmbn 5 1 (first b5)
                                   10 (second b5)
                                   100 (third b5)
                                   1000 (fourth b5))))
        (check-rdct)
        (let ((b6 (cat-8:basis ecc 6)))
          (setf cat-8:*tc* (cat-8:cmbn 6 1 (first b6)
                                   10 (second b6)
                                   100 (third b6)
                                   1000 (fourth b6)
                                   10000 (fifth b6)
                                   100000 (sixth b6)
                                   1000000 (seventh b6))))
        (check-rdct)))


(test kill-epis
      (cat-8:cat-init)
      (let* ((s3 (cat-8:sphere 3))
             (s3-chml-clss (cat-8:chml-clss s3 3))
             (s3-fibration (cat-8:z-whitehead s3 s3-chml-clss))
             (s3-4 (cat-8:fibration-total s3-fibration))
             (s3-4-chml-clss (cat-8:chml-clss s3-4 4))
             (s3-4-fibration (cat-8:z2-whitehead s3-4 s3-4-chml-clss))
             (s3-5 (cat-8:fibration-total s3-4-fibration)))
        (time (cat-8:homology s3-5 6))
        (cat-8:kill-epis s3-5 2 5)
        (cat-8:homology s3-5 0 7)))


(test kill-epi
      (cat-8:cat-init)
      (let* ((s3 (cat-8:sphere 3))
             (s3-chml-clss (cat-8:chml-clss s3 3))
             (s3-fibration (cat-8:z-whitehead s3 s3-chml-clss))
             (s3-4 (cat-8:fibration-total s3-fibration))
             s3-4-chml-clss s3-4-fibration s3-5)
        (cat-8:kill-epi s3-4 2)
        (setf s3-4-chml-clss (cat-8:chml-clss s3-4 4))
        (setf s3-4-fibration (cat-8:z2-whitehead s3-4 s3-4-chml-clss))
        (setf s3-5 (cat-8:fibration-total s3-4-fibration))
        (time (cat-8:homology s3-5 6))))


(test chml-clss-intr
      (cat-8:cat-init)
      (let* ((s3 (cat-8:sphere 3))
             (c (cat-8:chml-clss-intr s3 3))
             (s3-chml-clss (cat-8:chml-clss s3 3))
             (s3-fibration (cat-8:z-whitehead s3 s3-chml-clss))
             (s3-4 (cat-8:fibration-total s3-fibration))
             s3-4-chml-clss s3-4-fibration s3-5)
        (funcall c (cat-8:cmbn 3 5 's3))
        (cat-8:kill-epi s3-4 2)
        (setf c (cat-8:chml-clss-intr s3-4 4))
        (funcall c (cat-8:cmbn 4 5 (first (cat-8:basis (cat-8:echcm s3-4) 4))))
        (setf s3-4-chml-clss (cat-8:chml-clss s3-4 4))
        (setf s3-4-fibration (cat-8:z2-whitehead s3-4 s3-4-chml-clss))
        (setf s3-5 (cat-8:fibration-total s3-4-fibration))
        (cat-8:kill-epis s3-5 3 5)
        (setf c (cat-8:chml-clss-intr s3-5 5))
        (let ((b5 (cat-8:basis (cat-8:echcm s3-5) 5)))
          (funcall c (cat-8:cmbn 5 1 (first b5) 10 (second b5))))))


(test chml-clss
      (cat-8:cat-init)
      (let* ((s3 (cat-8:sphere 3))
             (s3-chml-clss (cat-8:chml-clss s3 3))
             (s3-fibration (cat-8:z-whitehead s3 s3-chml-clss))
             (s3-4 (cat-8:fibration-total s3-fibration))
             s3-4-chml-clss s3-4-fibration s3-5 s3-5-chml-clss s3-5-fibration)
        (cat-8:homology s3-4 0 6)
        (cat-8:kill-epi s3-4 2)
        (setf s3-4-chml-clss (cat-8:chml-clss s3-4 4))
        (setf s3-4-fibration (cat-8:z2-whitehead s3-4 s3-4-chml-clss))
        (setf s3-5 (cat-8:fibration-total s3-4-fibration))
        (cat-8:homology s3-5 0 6)
        (cat-8:kill-epis s3-5 3 5)
        (setf s3-5-chml-clss (cat-8:chml-clss s3-5 5))
        (setf s3-5-fibration (cat-8:z2-whitehead s3-5 s3-5-chml-clss))
        (setf s3-6 (cat-8:fibration-total s3-5-fibration))
        (cat-8:homology s3-6 0 7)))
