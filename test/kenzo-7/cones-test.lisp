;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test con-test
      (cat-7:con0 'a)
      (cat-7:con1 'a)
      (cat-7:con0 (cat-7:con0 'a))
      (cat-7:con0 (cat-7:con1 'a))
      (cat-7:con1 (cat-7:con0 'a))
      (cat-7:con1 (cat-7:con1 'a))
      (cat-7:con0 (cat-7:con1 (cat-7:con0 (cat-7:con1 'a)))))


(test cone-cmpr
      (let ((cmpr (cat-7:cone-cmpr #'cat-7:s-cmpr #'cat-7:f-cmpr)))
        (is (equal :less (funcall cmpr (cat-7:con0 'a) (cat-7:con0 'b))))
        (is (equal :equal (funcall cmpr (cat-7:con0 'b) (cat-7:con0 'b))))
        (is (equal :greater (funcall cmpr (cat-7:con0 'b) (cat-7:con0 'a))))
        (is (equal :less (funcall cmpr (cat-7:con0 'b) (cat-7:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-7:con1 '2) (cat-7:con0 'b))))
        (is (equal :less (funcall cmpr (cat-7:con1 '1) (cat-7:con1 '2))))
        (is (equal :equal (funcall cmpr (cat-7:con1 '2) (cat-7:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-7:con1 '2) (cat-7:con1 '1))))))


(test cone-basis
      (let ((basis (cat-7:cone-basis #'(lambda (n) (list n))
                                   #'(lambda (n) (list n)))))
        (funcall basis 4)))


(test term-con
      (cat-7:term-con0 (cat-7:term 2 'a))
      (cat-7:term-con1 (cat-7:term 2 'a))
      (cat-7:term-uncon (cat-7:term 2 (cat-7:con0 'a))))


(test cmbn-con
      (cat-7:cmbn-con0 (cat-7:cmbn 3 1 'a 2 'b))
      (cat-7:cmbn-con1 (cat-7:cmbn 3 1 'a 2 'b)))


(test cone-cmbn-split
      (cat-7:cone-cmbn-split (cat-7:cmbn 3 4 (cat-7:con0 'a) 5 (cat-7:con1 'b)))
      (cat-7:cone-cmbn-split (cat-7:cmbn 3 4 (cat-7:con0 'a)))
      (cat-7:cone-cmbn-split (cat-7:cmbn 3 4 (cat-7:con1 'a))))


(test cone-2cmbn-append
      (cat-7:cone-2cmbn-append (cat-7:cmbn 3 4 'a) (cat-7:cmbn 2 5 'b)))


(test cone-2mrph-diag-impl
      (let* ((mrph (cat-7:dffr (cat-7:delta 4)))
             (rslt (cat-7:cone-2mrph-diag-impl mrph mrph)))
        (funcall rslt (cat-7:cmbn 3 4 (cat-7:con0 15) 5 (cat-7:con1 7)))))


(test cone-3mrph-triangle-impl
      (let ((rslt (cat-7:cone-3mrph-triangle-impl #'cat-7:f-cmpr
                                                (cat-7:dffr (cat-7:delta 4))
                                                (cat-7:dffr (cat-7:delta 4))
                                                (cat-7:idnt-mrph (cat-7:delta 4)))))
        (funcall rslt 4 (cat-7:con0 '31))
        (funcall rslt 5 (cat-7:con1 '31))
        (funcall rslt 4 (cat-7:con1 '15))))


(test cone
      (progn
        (cat-7:cat-init)
        (let* ((k (cat-7:k-z 1))
               (u (cat-7:idnt-mrph k))
               (c (cat-7:cone u))
               (cone))
          (setf cat-7:*tc* (cat-7:cmbn 4 1 (cat-7:con0 '(1 2 3 4))))
          (cat-7:? c 4 (cat-7:con0 '(1 2 3 4)))
          (cat-7:? c cat-7:*tc*)

          (setf cone (cat-7:cone (cat-7:idnt-mrph (cat-7:delta 4))))
          (cat-7:? cone (cat-7:cmbn 4 1 (cat-7:con0 '31) -1 (cat-7:con1 '15)))
          (cat-7:? cone
                 (cat-7:? cone (cat-7:cmbn 4
                                       1 (cat-7:con0 '31) -1 (cat-7:con1 '15)))))))


(test cone-2mrph-diag
      (let* ((idnt (cat-7:idnt-mrph (cat-7:delta 4)))
             (cone (cat-7:cone idnt))
             (ff (cat-7:cone-2mrph-diag cone cone idnt idnt)))
        (cat-7:? ff (cat-7:cmbn 4 1 (cat-7:con0 '31) 10 (cat-7:con1 '15)))))


(test cone-3mrph-triangle
      (progn
        (cat-7:cat-init)
        (let* ((d (cat-7:delta 4))
               (df (cat-7:dffr (cat-7:delta 4)))
               (n (cat-7:idnt-mrph d))
               (c (cat-7:cone n))
               (z (cat-7:sbtr (cat-7:dffr c)
                            (cat-7:cone-3mrph-triangle c c df
                                                     (cat-7:n-mrph -1 df) n))))
          (cat-7:? z 0 (cat-7:con0 '1))
          (cat-7:? z 1 (cat-7:con1 '1))
          (cat-7:? z 1 (cat-7:con0 '3))
          (cat-7:? z 2 (cat-7:con1 '3))
          (cat-7:? z 4 (cat-7:con0 '31))
          (cat-7:? z 5 (cat-7:con1 '31)))))
