;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test con-test
      (cat:con0 'a)
      (cat:con1 'a)
      (cat:con0 (cat:con0 'a))
      (cat:con0 (cat:con1 'a))
      (cat:con1 (cat:con0 'a))
      (cat:con1 (cat:con1 'a))
      (cat:con0 (cat:con1 (cat:con0 (cat:con1 'a)))))


(test cone-cmpr
      (let ((cmpr (cat:cone-cmpr #'cat:s-cmpr #'cat:f-cmpr)))
        (is (equal :less (funcall cmpr (cat:con0 'a) (cat:con0 'b))))
        (is (equal :equal (funcall cmpr (cat:con0 'b) (cat:con0 'b))))
        (is (equal :greater (funcall cmpr (cat:con0 'b) (cat:con0 'a))))
        (is (equal :less (funcall cmpr (cat:con0 'b) (cat:con1 '2))))
        (is (equal :greater (funcall cmpr (cat:con1 '2) (cat:con0 'b))))
        (is (equal :less (funcall cmpr (cat:con1 '1) (cat:con1 '2))))
        (is (equal :equal (funcall cmpr (cat:con1 '2) (cat:con1 '2))))
        (is (equal :greater (funcall cmpr (cat:con1 '2) (cat:con1 '1))))))


(test cone-basis
      (let ((basis (cat:cone-basis #'(lambda (n) (list n))
                                   #'(lambda (n) (list n)))))
        (funcall basis 4)))


(test term-con
      (cat:term-con0 (cat:term 2 'a))
      (cat:term-con1 (cat:term 2 'a))
      (cat:term-uncon (cat:term 2 (cat:con0 'a))))


(test cmbn-con
      (cat:cmbn-con0 (cat:cmbn 3 1 'a 2 'b))
      (cat:cmbn-con1 (cat:cmbn 3 1 'a 2 'b)))


(test cone-cmbn-split
      (cat:cone-cmbn-split (cat:cmbn 3 4 (cat:con0 'a) 5 (cat:con1 'b)))
      (cat:cone-cmbn-split (cat:cmbn 3 4 (cat:con0 'a)))
      (cat:cone-cmbn-split (cat:cmbn 3 4 (cat:con1 'a))))


(test cone-2cmbn-append
      (cat:cone-2cmbn-append (cat:cmbn 3 4 'a) (cat:cmbn 2 5 'b)))


(test cone-2mrph-diag-impl
      (let* ((mrph (cat:dffr (cat:delta 4)))
             (rslt (cat:cone-2mrph-diag-impl mrph mrph)))
        (funcall rslt (cat:cmbn 3 4 (cat:con0 15) 5 (cat:con1 7)))))


(test cone-3mrph-triangle-impl
      (let ((rslt (cat:cone-3mrph-triangle-impl #'cat:f-cmpr
                                                (cat:dffr (cat:delta 4))
                                                (cat:dffr (cat:delta 4))
                                                (cat:idnt-mrph (cat:delta 4)))))
        (funcall rslt 4 (cat:con0 '31))
        (funcall rslt 5 (cat:con1 '31))
        (funcall rslt 4 (cat:con1 '15))))


(test cone
      (progn
        (cat:cat-init)
        (let* ((k (cat:k-z 1))
               (u (cat:idnt-mrph k))
               (c (cat:cone u))
               (cone))
          (setf cat:*tc* (cat:cmbn 4 1 (cat:con0 '(1 2 3 4))))
          (cat:? c 4 (cat:con0 '(1 2 3 4)))
          (cat:? c cat:*tc*)

          (setf cone (cat:cone (cat:idnt-mrph (cat:delta 4))))
          (cat:? cone (cat:cmbn 4 1 (cat:con0 '31) -1 (cat:con1 '15)))
          (cat:? cone
                 (cat:? cone (cat:cmbn 4
                                       1 (cat:con0 '31) -1 (cat:con1 '15)))))))


(test cone-2mrph-diag
      (let* ((idnt (cat:idnt-mrph (cat:delta 4)))
             (cone (cat:cone idnt))
             (ff (cat:cone-2mrph-diag cone cone idnt idnt)))
        (cat:? ff (cat:cmbn 4 1 (cat:con0 '31) 10 (cat:con1 '15)))))


(test cone-3mrph-triangle
      (progn
        (cat:cat-init)
        (let* ((d (cat:delta 4))
               (df (cat:dffr (cat:delta 4)))
               (n (cat:idnt-mrph d))
               (c (cat:cone n))
               (z (cat:sbtr (cat:dffr c)
                            (cat:cone-3mrph-triangle c c df
                                                     (cat:n-mrph -1 df) n))))
          (cat:? z 0 (cat:con0 '1))
          (cat:? z 1 (cat:con1 '1))
          (cat:? z 1 (cat:con0 '3))
          (cat:? z 2 (cat:con1 '3))
          (cat:? z 4 (cat:con0 '31))
          (cat:? z 5 (cat:con1 '31)))))
