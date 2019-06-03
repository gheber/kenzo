;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test con-test
      (cat-8:con0 'a)
      (cat-8:con1 'a)
      (cat-8:con0 (cat-8:con0 'a))
      (cat-8:con0 (cat-8:con1 'a))
      (cat-8:con1 (cat-8:con0 'a))
      (cat-8:con1 (cat-8:con1 'a))
      (cat-8:con0 (cat-8:con1 (cat-8:con0 (cat-8:con1 'a)))))


(test cone-cmpr
      (let ((cmpr (cat-8:cone-cmpr #'cat-8:s-cmpr #'cat-8:f-cmpr)))
        (is (equal :less (funcall cmpr (cat-8:con0 'a) (cat-8:con0 'b))))
        (is (equal :equal (funcall cmpr (cat-8:con0 'b) (cat-8:con0 'b))))
        (is (equal :greater (funcall cmpr (cat-8:con0 'b) (cat-8:con0 'a))))
        (is (equal :less (funcall cmpr (cat-8:con0 'b) (cat-8:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-8:con1 '2) (cat-8:con0 'b))))
        (is (equal :less (funcall cmpr (cat-8:con1 '1) (cat-8:con1 '2))))
        (is (equal :equal (funcall cmpr (cat-8:con1 '2) (cat-8:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-8:con1 '2) (cat-8:con1 '1))))))


(test cone-basis
      (let ((basis (cat-8:cone-basis #'(lambda (n) (list n))
                                   #'(lambda (n) (list n)))))
        (funcall basis 4)))


(test term-con
      (cat-8:term-con0 (cat-8:term 2 'a))
      (cat-8:term-con1 (cat-8:term 2 'a))
      (cat-8:term-uncon (cat-8:term 2 (cat-8:con0 'a))))


(test cmbn-con
      (cat-8:cmbn-con0 (cat-8:cmbn 3 1 'a 2 'b))
      (cat-8:cmbn-con1 (cat-8:cmbn 3 1 'a 2 'b)))


(test cone-cmbn-split
      (cat-8:cone-cmbn-split (cat-8:cmbn 3 4 (cat-8:con0 'a) 5 (cat-8:con1 'b)))
      (cat-8:cone-cmbn-split (cat-8:cmbn 3 4 (cat-8:con0 'a)))
      (cat-8:cone-cmbn-split (cat-8:cmbn 3 4 (cat-8:con1 'a))))


(test cone-2cmbn-append
      (cat-8:cone-2cmbn-append (cat-8:cmbn 3 4 'a) (cat-8:cmbn 2 5 'b)))


(test cone-2mrph-diag-impl
      (let* ((mrph (cat-8:dffr (cat-8:delta 4)))
             (rslt (cat-8:cone-2mrph-diag-impl mrph mrph)))
        (funcall rslt (cat-8:cmbn 3 4 (cat-8:con0 15) 5 (cat-8:con1 7)))))


(test cone-3mrph-triangle-impl
      (let ((rslt (cat-8:cone-3mrph-triangle-impl #'cat-8:f-cmpr
                                                (cat-8:dffr (cat-8:delta 4))
                                                (cat-8:dffr (cat-8:delta 4))
                                                (cat-8:idnt-mrph (cat-8:delta 4)))))
        (funcall rslt 4 (cat-8:con0 '31))
        (funcall rslt 5 (cat-8:con1 '31))
        (funcall rslt 4 (cat-8:con1 '15))))


(test cone
      (progn
        (cat-8:cat-init)
        (let* ((k (cat-8:k-z 1))
               (u (cat-8:idnt-mrph k))
               (c (cat-8:cone u))
               (cone))
          (setf cat-8:*tc* (cat-8:cmbn 4 1 (cat-8:con0 '(1 2 3 4))))
          (cat-8:? c 4 (cat-8:con0 '(1 2 3 4)))
          (cat-8:? c cat-8:*tc*)

          (setf cone (cat-8:cone (cat-8:idnt-mrph (cat-8:delta 4))))
          (cat-8:? cone (cat-8:cmbn 4 1 (cat-8:con0 '31) -1 (cat-8:con1 '15)))
          (cat-8:? cone
                 (cat-8:? cone (cat-8:cmbn 4
                                       1 (cat-8:con0 '31) -1 (cat-8:con1 '15)))))))


(test cone-2mrph-diag
      (let* ((idnt (cat-8:idnt-mrph (cat-8:delta 4)))
             (cone (cat-8:cone idnt))
             (ff (cat-8:cone-2mrph-diag cone cone idnt idnt)))
        (cat-8:? ff (cat-8:cmbn 4 1 (cat-8:con0 '31) 10 (cat-8:con1 '15)))))


(test cone-3mrph-triangle
      (progn
        (cat-8:cat-init)
        (let* ((d (cat-8:delta 4))
               (df (cat-8:dffr (cat-8:delta 4)))
               (n (cat-8:idnt-mrph d))
               (c (cat-8:cone n))
               (z (cat-8:sbtr (cat-8:dffr c)
                            (cat-8:cone-3mrph-triangle c c df
                                                     (cat-8:n-mrph -1 df) n))))
          (cat-8:? z 0 (cat-8:con0 '1))
          (cat-8:? z 1 (cat-8:con1 '1))
          (cat-8:? z 1 (cat-8:con0 '3))
          (cat-8:? z 2 (cat-8:con1 '3))
          (cat-8:? z 4 (cat-8:con0 '31))
          (cat-8:? z 5 (cat-8:con1 '31)))))
