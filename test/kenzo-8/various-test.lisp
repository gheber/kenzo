;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test binomial-n-p
      (let ((rslt '(1 5 10 10 5 1)))
        (dotimes (i 6)
          (is (equal (cat-8:binomial-n-p 5 i)
                     (nth i rslt))))))

(test binomial-p-q
      (let ((rslt '(1 5 10 10 5 1)))
        (dotimes (i 6)
          (is (equal (cat-8:binomial-p-q (- 5 i) i)
                     (nth i rslt))))))

(test <a-b<
      (is (equal (cat-8:<a-b< 0 5) '(0 1 2 3 4))))

(test <a-b>
      (is (equal (cat-8:<a-b> 0 5) '(0 1 2 3 4 5))))

(test >a-b<
      (is (equal (cat-8:>a-b< 0 5) '(1 2 3 4))))

(test >a-b>
      (is (equal (cat-8:>a-b> 0 5) '(1 2 3 4 5))))

(test v<a-b>
      (is (equalp (cat-8:v<a-b> -5 5)
                  (make-array 11 :element-type 'fixnum
                              :initial-contents '(-5 -4 -3 -2 -1
                                                  0 1 2 3 4 5)))))

(test srandom
      (dotimes (i 20)
        (let ((rslt (cat-8:srandom 3)))
          (is (and (typep rslt 'fixnum)
                   (<= -3 rslt 3))))))
