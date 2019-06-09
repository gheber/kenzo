;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test check-kan
      (cat-7:cat-init)
      (let* ((k (cat-7:k-z-1))
             (rslt '(1 10 100))
             (hat (mapcar #'(lambda (i) (cat-7:face k i 3 rslt))
                          (cat-7:<a-b> 0 3))))
        (dotimes (i 4)
          (cat-7:check-kan k i 3 (remove (nth i hat) hat :test #'equal)))))
