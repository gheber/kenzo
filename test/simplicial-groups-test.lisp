;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test check-kan
      (cat:cat-init)
      (let* ((k (cat:k-z-1))
             (rslt '(1 10 100))
             (hat (mapcar #'(lambda (i) (cat:face k i 3 rslt))
                          (cat:<a-b> 0 3))))
        (dotimes (i 4)
          (cat:check-kan k i 3 (remove (nth i hat) hat :test #'equal)))))
