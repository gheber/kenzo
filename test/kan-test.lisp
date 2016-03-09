;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test vertex-i
      (let ((d (cat:delta-infinity)))
        (cat:vertex-i (cat:absm 0 1) 0)
        (cat:vertex-i (cat:absm 1 1) 0)
        (cat:vertex-i (cat:absm 1 1) 1)
        (cat:vertex-i (cat:absm 0 3) 0)
        (cat:vertex-i (cat:absm 0 3) 1)
        (cat:vertex-i (cat:absm 3 1) 0)
        (cat:vertex-i (cat:absm 3 1) 1)
        (cat:vertex-i (cat:absm 3 1) 2)
        (cat:vertex-i (cat:absm 1 3) 0)
        (cat:vertex-i (cat:absm 1 3) 1)
        (cat:vertex-i (cat:absm 1 3) 2)
        (cat:vertex-i (cat:absm 2 3) 0)
        (cat:vertex-i (cat:absm 2 3) 1)
        (cat:vertex-i (cat:absm 2 3) 2)
        (cat:vertex-i (cat:absm 0 7) 0)
        (cat:vertex-i (cat:absm 0 7) 1)
        (cat:vertex-i (cat:absm 0 7) 2)))

(test absm-int-ext
      (cat:absm-ext-int '(0 0 0 1 2 3 3 3))
      (cat:absm-ext-int '(0 1 1 1 2))
      (cat:absm-int-ext (cat:absm-ext-int '(0 0 0 1 2 3 3 3)))
      (cat:absm-int-ext (cat:absm-ext-int '(0 1 1 1 2))))


(defun dkfll (indx dmns hat)
  (cond ((= 1 dmns)
         (cat:absm 1 (cat:gmsm (first hat))))
        ((= 0 indx)
         (let ((del-1 (cat:absm-int-ext (first hat)))
               (del-2 (cat:absm-int-ext (second hat))))
           (cat:absm-ext-int
            (cons (first del-1)
                  (cons (second del-2) (rest del-1))))))
        ((= 1 indx)
         (let ((del-0 (cat:absm-int-ext (first hat)))
               (del-2 (cat:absm-int-ext (second hat))))
           (cat:absm-ext-int
            (cons (first del-2) del-0))))
        (t
         (let ((del-0 (cat:absm-int-ext (first hat)))
               (del-1 (cat:absm-int-ext (second hat))))
           (cat:absm-ext-int
            (cons (first del-1) del-0))))))


(let ((d (cat:delta-infinity)))
  (cat:smst-kan d #'dkfll)
  (cat:kfll d 0 1 (list (cat:absm 0 1)))
  (cat:kfll d 0 2 (list (cat:absm 0 5) (cat:absm 0 3)))
  (cat:kfll d 1 2 (list (cat:absm 0 6) (cat:absm 0 3)))
  (cat:kfll d 2 2 (list (cat:absm 0 6) (cat:absm 0 5)))
  (cat:kfll d 0 2 (list (cat:absm 0 3) (cat:absm 0 3)))
  (cat:kfll d 1 2 (list (cat:absm 1 2) (cat:absm 0 3))))


(test check-hat
      (let* ((od (cat:gdeltab))
             (s (cat:loop3 0 31 2))
             (faces (mapcar #'(lambda (indx) (cat:face od indx 3 s))
                            '(0 1 2 3))))
        (delete (nth 1 faces) faces)
        (cat:check-hat od 1 3 faces)
        (setf (nth 2 faces) (nth 1 faces))
        (signals #-ecl simple-error
                 #+ecl simple-type-error
                 (cat:check-hat od 1 3 faces))))
