;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test check-kan
      (cat-8:cat-init)
      (let* ((k (cat-8:k-z-1))
             (rslt '(1 10 100))
             (hat (mapcar #'(lambda (i) (cat-8:face k i 3 rslt))
                          (cat-8:<a-b> 0 3))))
        (dotimes (i 4)
          (cat-8:check-kan k i 3 (remove (nth i hat) hat :test #'equal)))))
