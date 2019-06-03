;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test check-kan
      (cat-9:cat-9-init)
      (let* ((k (cat-9:k-z-1))
             (rslt '(1 10 100))
             (hat (mapcar #'(lambda (i) (cat-9:face k i 3 rslt))
                          (cat-9:<a-b> 0 3))))
        (dotimes (i 4)
          (cat-9:check-kan k i 3 (remove (nth i hat) hat :test #'equal)))))
