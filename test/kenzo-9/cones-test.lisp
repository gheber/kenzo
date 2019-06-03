;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test con-test
      (cat-9:con0 'a)
      (cat-9:con1 'a)
      (cat-9:con0 (cat-9:con0 'a))
      (cat-9:con0 (cat-9:con1 'a))
      (cat-9:con1 (cat-9:con0 'a))
      (cat-9:con1 (cat-9:con1 'a))
      (cat-9:con0 (cat-9:con1 (cat-9:con0 (cat-9:con1 'a)))))


(test cone-cmpr
      (let ((cmpr (cat-9:cone-cmpr #'cat-9:s-cmpr #'cat-9:f-cmpr)))
        (is (equal :less (funcall cmpr (cat-9:con0 'a) (cat-9:con0 'b))))
        (is (equal :equal (funcall cmpr (cat-9:con0 'b) (cat-9:con0 'b))))
        (is (equal :greater (funcall cmpr (cat-9:con0 'b) (cat-9:con0 'a))))
        (is (equal :less (funcall cmpr (cat-9:con0 'b) (cat-9:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-9:con1 '2) (cat-9:con0 'b))))
        (is (equal :less (funcall cmpr (cat-9:con1 '1) (cat-9:con1 '2))))
        (is (equal :equal (funcall cmpr (cat-9:con1 '2) (cat-9:con1 '2))))
        (is (equal :greater (funcall cmpr (cat-9:con1 '2) (cat-9:con1 '1))))))


(test cone-basis
      (let ((basis (cat-9:cone-basis #'(lambda (n) (list n))
                                   #'(lambda (n) (list n)))))
        (funcall basis 4)))


(test term-con
      (cat-9:term-con0 (cat-9:term 2 'a))
      (cat-9:term-con1 (cat-9:term 2 'a))
      (cat-9:term-uncon (cat-9:term 2 (cat-9:con0 'a))))


(test cmbn-con
      (cat-9:cmbn-con0 (cat-9:cmbn 3 1 'a 2 'b))
      (cat-9:cmbn-con1 (cat-9:cmbn 3 1 'a 2 'b)))


(test cone-cmbn-split
      (cat-9:cone-cmbn-split (cat-9:cmbn 3 4 (cat-9:con0 'a) 5 (cat-9:con1 'b)))
      (cat-9:cone-cmbn-split (cat-9:cmbn 3 4 (cat-9:con0 'a)))
      (cat-9:cone-cmbn-split (cat-9:cmbn 3 4 (cat-9:con1 'a))))


(test cone-2cmbn-append
      (cat-9:cone-2cmbn-append (cat-9:cmbn 3 4 'a) (cat-9:cmbn 2 5 'b)))


(test cone-2mrph-diag-impl
      (let* ((mrph (cat-9:dffr (cat-9:delta 4)))
             (rslt (cat-9:cone-2mrph-diag-impl mrph mrph)))
        (funcall rslt (cat-9:cmbn 3 4 (cat-9:con0 15) 5 (cat-9:con1 7)))))


(test cone-3mrph-triangle-impl
      (let ((rslt (cat-9:cone-3mrph-triangle-impl #'cat-9:f-cmpr
                                                (cat-9:dffr (cat-9:delta 4))
                                                (cat-9:dffr (cat-9:delta 4))
                                                (cat-9:idnt-mrph (cat-9:delta 4)))))
        (funcall rslt 4 (cat-9:con0 '31))
        (funcall rslt 5 (cat-9:con1 '31))
        (funcall rslt 4 (cat-9:con1 '15))))


(test cone
      (progn
        (cat-9:cat-9-init)
        (let* ((k (cat-9:k-z 1))
               (u (cat-9:idnt-mrph k))
               (c (cat-9:cone u))
               (cone))
          (setf cat-9:*tc* (cat-9:cmbn 4 1 (cat-9:con0 '(1 2 3 4))))
          (cat-9:? c 4 (cat-9:con0 '(1 2 3 4)))
          (cat-9:? c cat-9:*tc*)

          (setf cone (cat-9:cone (cat-9:idnt-mrph (cat-9:delta 4))))
          (cat-9:? cone (cat-9:cmbn 4 1 (cat-9:con0 '31) -1 (cat-9:con1 '15)))
          (cat-9:? cone
                 (cat-9:? cone (cat-9:cmbn 4
                                       1 (cat-9:con0 '31) -1 (cat-9:con1 '15)))))))


(test cone-2mrph-diag
      (let* ((idnt (cat-9:idnt-mrph (cat-9:delta 4)))
             (cone (cat-9:cone idnt))
             (ff (cat-9:cone-2mrph-diag cone cone idnt idnt)))
        (cat-9:? ff (cat-9:cmbn 4 1 (cat-9:con0 '31) 10 (cat-9:con1 '15)))))


(test cone-3mrph-triangle
      (progn
        (cat-9:cat-9-init)
        (let* ((d (cat-9:delta 4))
               (df (cat-9:dffr (cat-9:delta 4)))
               (n (cat-9:idnt-mrph d))
               (c (cat-9:cone n))
               (z (cat-9:sbtr (cat-9:dffr c)
                            (cat-9:cone-3mrph-triangle c c df
                                                     (cat-9:n-mrph -1 df) n))))
          (cat-9:? z 0 (cat-9:con0 '1))
          (cat-9:? z 1 (cat-9:con1 '1))
          (cat-9:? z 1 (cat-9:con0 '3))
          (cat-9:? z 2 (cat-9:con1 '3))
          (cat-9:? z 4 (cat-9:con0 '31))
          (cat-9:? z 5 (cat-9:con1 '31)))))
