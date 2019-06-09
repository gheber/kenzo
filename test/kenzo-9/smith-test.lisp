;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test idnt-mtrx
      (cat-9:idnt-mtrx 3))

(test gnrt-name-basis
      (cat-9:gnrt-name 4)
      (cat-9:gnrt-name-basis 4))




(test random-matrix
      (cat-9:random-matrix 5 10 .2 100))

(test copy-mtrx
      (let ((m (cat-9:random-matrix 3 4 .4 10)))
        (cat-9:copy-mtrx m)))

(test left-submatrix
      (let ((m (cat-9:random-matrix 5 10 .6 10)))
        (cat-9:left-submatrix m 5)
        (cat-9:left-submatrix m 10)
        (cat-9:left-submatrix m 0)
        (setf m (cat-9::creer-matrice 5 10))
        (cat-9:left-submatrix m 5)))

(test mtrx-prdc
      (let ((m (cat-9:random-matrix 1 1 1 10))
            (m1 (cat-9:random-matrix 1 1 1 10))
            (m2 (cat-9:random-matrix 1 1 1 10)))
        (cat-9:mtrx-prdc m m)
        (setf m1 (cat-9:random-matrix 2 3 .8 5))
        (setf m2 (cat-9:random-matrix 3 2 .8 5))
        (cat-9:mtrx-prdc m1 m2)
        (setf m1 (cat-9:random-matrix 3 5 .8 2))
        (setf m2 (cat-9:random-matrix 5 1 .8 2))
        (cat-9:mtrx-prdc m1 m2)
        (setf m1 (cat-9::creer-matrice 3 5))
        (setf m2 (cat-9:random-matrix 5 4 .8 10))
        (cat-9:mtrx-prdc m1 m2)
        (setf m1 (cat-9::creer-matrice 3 5))
        (setf m2 (cat-9:random-matrix 6 3 .8 10))
        (cat-9:mtrx-prdc m2 m1)
        (setf m1 (cat-9::creer-matrice 3 5))
        (setf m2 (cat-9::creer-matrice 5 4))
        (cat-9:mtrx-prdc m1 m2)
        (setf m1 (cat-9:random-matrix 1 2 2 10))
        (setf m2 (cat-9::creer-matrice 2 1))
        (cat-9::inserer-terme (cat-9::baselig m2 1)
                              (cat-9::basecol m2 1)
                              (cat-9::val (cat-9::up (cat-9::basecol m1 2))))
        ;;(cat-9::inserer-terme (cat-9::baselig m2 2) (cat-9::basecol m2 1) (- (cat-9::val (cat-9::up (cat-9::basecol m1 1)))))
        m2
        (cat-9:mtrx-prdc m1 m2)))


(test chcm-mtrx
      (cat-9:cat-init)
      (let ((d (cat-9:delta 5))
            (m (cat-9:moore 2 2)))
        (cat-9:chcm-mtrx d 3)
        (dotimes (i 5)
          (print (cat-9:chcm-mtrx m i)))
        (dotimes (i 6)
          (print (list (length (cat-9::leftcol (cat-9:chcm-mtrx m i)))
                       (length (cat-9::uplig (cat-9:chcm-mtrx m i))))))))

(test extract-line
      (let ((m (cat-9:random-matrix 4 10 1 10)))
        (cat-9:extract-line m 3)))

(test extract-column
      (let ((m (cat-9:random-matrix 4 10 1 10)))
        (cat-9:extract-column m 3)))

(test extract-term
      (let ((m (cat-9:random-matrix 2 3 1 10)))
        (dotimes (il 2)
          (dotimes (ic 3)
            (print (list il ic (cat-9:extract-term m (1+ il) (1+ ic))))))))

(test new-line
      (let ((m (cat-9:random-matrix 5 10 1 10)))
        (cat-9:new-line m 3 nil)
        (setf m (cat-9:random-matrix 5 10 1 10))
        (cat-9:new-line m 3 '((2 5) (10 10)))))

(test new-column
      (let ((m (cat-9:random-matrix 5 10 1 10)))
        (cat-9:new-column m 3 nil)
        (setf m (cat-9:random-matrix 5 10 1 10))
        (cat-9:new-column m 3 '((2 5) (5 10)))))

(test safe-*
      (is (= 536848900 (cat-9:safe-* 23170 23170)))
      (is (= 536872070 (cat-9:safe-* 23170 23171))))

(test safe-+
      (is (= 536870911 (cat-9:safe-+ 268435456 268435455)))
      (is (= 536870912 (cat-9:safe-+ 268435456 268435456))))

(test line-op
      (let ((m (cat-9:random-matrix 5 10 1 10)))
        (cat-9:line-op m 2 3 4)))

(test column-op
      (let ((m (cat-9:random-matrix 5 10 1 10)))
        (cat-9:column-op m 2 3 4)))


(test equal-matrix
      (let ((m1 (cat-9::creer-matrice 2 3))
            (m2 (cat-9::creer-matrice 3 3)))
        (cat-9:equal-matrix m1 m2)
        (setf m1 (cat-9::creer-matrice 2 3))
        (setf m2 (cat-9::creer-matrice 2 4))
        (cat-9:equal-matrix m1 m2)
        (setf m1 (cat-9::creer-matrice 1 1))
        (setf m2 (cat-9::creer-matrice 1 1))
        (cat-9:new-line m2 1 '((1 1)))
        (cat-9:equal-matrix m1 m2)
        (cat-9:new-line m1 1 '((1 1)))
        (cat-9:new-line m2 1 nil)
        (cat-9:equal-matrix m1 m2)
        (setf m1 (cat-9::creer-matrice 1 2))
        (setf m2 (cat-9::creer-matrice 1 2))
        (cat-9:new-line m1 1 '((2 1)))
        (cat-9:new-line m2 1 '((1 1)))
        (cat-9:equal-matrix m1 m2)
        (cat-9:new-line m2 1 '((2 2)))
        (cat-9:equal-matrix m1 m2)
        (cat-9:new-line m2 1 '((2 1)))
        (cat-9:equal-matrix m1 m2)
        (setf m1 (cat-9:random-matrix 5 10 1 10))
        (setf m2 (cat-9::copier-matrice m1))
        (cat-9:equal-matrix m1 m2)))

(test line-swap
      (let ((m (cat-9:random-matrix 3 4 1 10)))
        (cat-9:line-swap m 1 2)))

(test column-swap
      (let ((m (cat-9:random-matrix 3 4 1 10)))
        (cat-9:column-swap m 3 2)))

(test safe--
      (is (= 536870911 (cat-9:safe-- -536870911)))
      (is (= 536870912 (cat-9:safe-- -536870912))))

(test column-minus
      (let ((m (cat-9:random-matrix 3 4 1 10)))
        (cat-9:line-minus m 2)
        (cat-9:column-minus m 1)))

(test minimal-term
      (let ((m (cat-9:random-matrix 4 5 1 10)))
        (cat-9:minimal-term m 1)
        (setf m (cat-9::creer-matrice 3 5))
        (cat-9:minimal-term m 1)
        (setf m (cat-9:random-matrix 4 5 1 10))
        (setf m2 (cat-9::copier-matrice m))
        (cat-9:minimal-term m 1)
        (cat-9:minimal-term m 2)
        (cat-9:minimal-term m 3)
        (cat-9:minimal-term m 4)
        (cat-9:minimal-term m 5)))



#|


(test minimal-term-top-left
(let* ((p (cat-9:idnt-mtrx 4))
(p-1 (cat-9:idnt-mtrx 4))
(m (cat-9:random-matrix 4 5 10))
(q (cat-9:idnt-mtrx 5))
(q-1 (cat-9:idnt-mtrx 5))
(list (list p p-1 m q q-1))
(t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(cat-9:minimal-term-top-left list 0 1 3)
(is (equalp t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(cat-9:mtrx-prdc p p-1)
(cat-9:mtrx-prdc q q-1)))


(test minimal-term-top-left1
(let* ((p (cat-9:idnt-mtrx 4))
(p-1 (cat-9:idnt-mtrx 4))
(m (cat-9:random-matrix 4 5 10))
(q (cat-9:idnt-mtrx 5))
(q-1 (cat-9:idnt-mtrx 5))
(list (list p p-1 m q q-1))
(t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(cat-9:pivott list 0)
(is (equalp t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(cat-9:mtrx-prdc p p-1)
(cat-9:mtrx-prdc q q-1)))


(test list-smith
(let* ((p (cat-9:idnt-mtrx 4))
(p-1 (cat-9:idnt-mtrx 4))
(m (cat-9:random-matrix 4 5 10))
(q (cat-9:idnt-mtrx 5))
(q-1 (cat-9:idnt-mtrx 5))
(list (list p p-1 m q q-1))
(t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(third (cat-9:list-smith list))
(is (equalp t1 (cat-9:mtrx-prdc p (cat-9:mtrx-prdc m q-1))))
(cat-9:mtrx-prdc p p-1)
(cat-9:mtrx-prdc q q-1)))



(test echcm-kill-epi-f-intr
(let* ((q-1 (cat-9:random-matrix 5 5 10))
(f (cat-9:echcm-kill-epi-f-intr #'cat-9:s-cmpr 2 2 5 '(a b c d e)
(list 0 0 0 0 q-1))))
(funcall f (cat-9:cmbn 2 1 'a))
(funcall f (cat-9:cmbn 4 1 'a))
(funcall f (cat-9:cmbn 3 1 'a 10 'b 100 'c 1000 'd 10000 'e))))

#|
(test echcm-kill-epi-g-intr
(let* ((q (cat-9:random-matrix 5 5 10))
(g (cat-9:echcm-kill-epi-g-intr 2 2 5 '(a b c d e)
(list 0 0 0 q 0))))
(funcall g (cat-9:cmbn 2))
(funcall g (cat-9:cmbn 4 1 'a))
(funcall g (cat-9:cmbn 3 1 :gn-0 10 :gn-1 100 :gn-2))))
|#

(test echcm-kill-epi-h-intr
(let* ((p-1 (cat-9:random-matrix 2 2 10))
(q (cat-9:random-matrix 5 5 10))
(h (cat-9:echcm-kill-epi-h-intr #'cat-9:s-cmpr 2 2 5 '(a b)
'(a b c d e)
(list 0 p-1 0 q 0))))
(funcall h (cat-9:cmbn 2 1 'a 1000 'b))
(funcall h (cat-9:cmbn 4 1 'a))))


(test echcm-kill-epi
(cat-9:cat-init)
(let* ((s3 (cat-9:sphere 3))
(s3-chml-clss (cat-9:chml-clss s3 3))
(s3-fibration (cat-9:z-whitehead s3 s3-chml-clss))
(s3-4 (cat-9:fibration-total s3-fibration))
(ecc (cat-9:echcm s3-4))
(rdct (cat-9:echcm-kill-epi ecc 2))
s3-4-chml-clss s3-4-fibration s3-5
rdct1 rdct2 rdct3 rdct12 rdct123)
(cat-9:pre-check-rdct rdct)
(setf cat-9:*tc* (cat-9:cmbn 0 1 (cat-9:bsgn ecc))
cat-9:*bc* cat-9:*tc*)
(check-rdct)
(setf cat-9:*tc* (cat-9:cmbn 2 1 (first (cat-9:basis ecc 2))))
(check-rdct)
(setf cat-9:*tc* (cat-9:cmbn 3 1 (first (cat-9:basis ecc 3))))
(check-rdct)
(setf cat-9:*tc* (cat-9:cmbn 4 1 (first (cat-9:basis ecc 4)))
cat-9:*bc* cat-9:*tc*)
(check-rdct)
(setf s3-4-chml-clss (cat-9:chml-clss s3-4 4))
(setf s3-4-fibration (cat-9:z2-whitehead s3-4 s3-4-chml-clss))
(setf s3-5 (cat-9:fibration-total s3-4-fibration))
(setf ecc (cat-9:echcm s3-5))
(dotimes (i 7)
(format t "~%DIM = ~D ; LENGTH = ~D" i (length (cat-9:basis ecc i))))
(setf rdct1 (cat-9:echcm-kill-epi ecc 2))
(setf rdct2 (cat-9:echcm-kill-epi (cat-9:bcc rdct1) 3))
(setf rdct3 (cat-9:echcm-kill-epi (cat-9:bcc rdct2) 4))
(setf rdct12 (cat-9:cmps rdct2 rdct1))
(setf rdct123 (cat-9:cmps rdct3 rdct12))
(cat-9:pre-check-rdct rdct123)
(setf cat-9:*tc* (cat-9:cmbn 0 1 (cat-9:bsgn ecc)) cat-9:*bc* cat-9:*tc*)
(check-rdct)
(setf cat-9:*tc* (cat-9:cmbn 2 1 (first (cat-9:basis ecc 2))))
(check-rdct)
(let ((b3 (cat-9:basis ecc 3)))
(setf cat-9:*tc* (cat-9:cmbn 3 1 (first b3) 10 (second b3))))
(check-rdct)
(let ((b4 (cat-9:basis ecc 4)))
(setf cat-9:*tc* (cat-9:cmbn 4 1 (first b4) 10 (second b4))))
(check-rdct)
(let ((b5 (cat-9:basis ecc 5)))
(setf cat-9:*tc* (cat-9:cmbn 5 1 (first b5)
10 (second b5)
100 (third b5)
1000 (fourth b5))))
(check-rdct)
(let ((b6 (cat-9:basis ecc 6)))
(setf cat-9:*tc* (cat-9:cmbn 6 1 (first b6)
10 (second b6)
100 (third b6)
1000 (fourth b6)
10000 (fifth b6)
100000 (sixth b6)
1000000 (seventh b6))))
(check-rdct)))


(test kill-epis
(cat-9:cat-init)
(let* ((s3 (cat-9:sphere 3))
(s3-chml-clss (cat-9:chml-clss s3 3))
(s3-fibration (cat-9:z-whitehead s3 s3-chml-clss))
(s3-4 (cat-9:fibration-total s3-fibration))
(s3-4-chml-clss (cat-9:chml-clss s3-4 4))
(s3-4-fibration (cat-9:z2-whitehead s3-4 s3-4-chml-clss))
(s3-5 (cat-9:fibration-total s3-4-fibration)))
(time (cat-9:homology s3-5 6))
(cat-9:kill-epis s3-5 2 5)
(cat-9:homology s3-5 0 7)))


(test kill-epi
(cat-9:cat-init)
(let* ((s3 (cat-9:sphere 3))
(s3-chml-clss (cat-9:chml-clss s3 3))
(s3-fibration (cat-9:z-whitehead s3 s3-chml-clss))
(s3-4 (cat-9:fibration-total s3-fibration))
s3-4-chml-clss s3-4-fibration s3-5)
(cat-9:kill-epi s3-4 2)
(setf s3-4-chml-clss (cat-9:chml-clss s3-4 4))
(setf s3-4-fibration (cat-9:z2-whitehead s3-4 s3-4-chml-clss))
(setf s3-5 (cat-9:fibration-total s3-4-fibration))
(time (cat-9:homology s3-5 6))))


(test chml-clss-intr
(cat-9:cat-init)
(let* ((s3 (cat-9:sphere 3))
(c (cat-9:chml-clss-intr s3 3))
(s3-chml-clss (cat-9:chml-clss s3 3))
(s3-fibration (cat-9:z-whitehead s3 s3-chml-clss))
(s3-4 (cat-9:fibration-total s3-fibration))
s3-4-chml-clss s3-4-fibration s3-5)
(funcall c (cat-9:cmbn 3 5 's3))
(cat-9:kill-epi s3-4 2)
(setf c (cat-9:chml-clss-intr s3-4 4))
(funcall c (cat-9:cmbn 4 5 (first (cat-9:basis (cat-9:echcm s3-4) 4))))
(setf s3-4-chml-clss (cat-9:chml-clss s3-4 4))
(setf s3-4-fibration (cat-9:z2-whitehead s3-4 s3-4-chml-clss))
(setf s3-5 (cat-9:fibration-total s3-4-fibration))
(cat-9:kill-epis s3-5 3 5)
(setf c (cat-9:chml-clss-intr s3-5 5))
(let ((b5 (cat-9:basis (cat-9:echcm s3-5) 5)))
(funcall c (cat-9:cmbn 5 1 (first b5) 10 (second b5))))))


(test chml-clss
(cat-9:cat-init)
(let* ((s3 (cat-9:sphere 3))
(s3-chml-clss (cat-9:chml-clss s3 3))
(s3-fibration (cat-9:z-whitehead s3 s3-chml-clss))
(s3-4 (cat-9:fibration-total s3-fibration))
s3-4-chml-clss s3-4-fibration s3-5 s3-5-chml-clss s3-5-fibration)
(cat-9:homology s3-4 0 6)
(cat-9:kill-epi s3-4 2)
(setf s3-4-chml-clss (cat-9:chml-clss s3-4 4))
(setf s3-4-fibration (cat-9:z2-whitehead s3-4 s3-4-chml-clss))
(setf s3-5 (cat-9:fibration-total s3-4-fibration))
(cat-9:homology s3-5 0 6)
(cat-9:kill-epis s3-5 3 5)
(setf s3-5-chml-clss (cat-9:chml-clss s3-5 5))
(setf s3-5-fibration (cat-9:z2-whitehead s3-5 s3-5-chml-clss))
(setf s3-6 (cat-9:fibration-total s3-5-fibration))
(cat-9:homology s3-6 0 7)))
|#
