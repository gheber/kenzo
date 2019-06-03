;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test vertex-i
      (let ((d (cat-9:delta-infinity)))
        (cat-9:vertex-i (cat-9:absm 0 1) 0)
        (cat-9:vertex-i (cat-9:absm 1 1) 0)
        (cat-9:vertex-i (cat-9:absm 1 1) 1)
        (cat-9:vertex-i (cat-9:absm 0 3) 0)
        (cat-9:vertex-i (cat-9:absm 0 3) 1)
        (cat-9:vertex-i (cat-9:absm 3 1) 0)
        (cat-9:vertex-i (cat-9:absm 3 1) 1)
        (cat-9:vertex-i (cat-9:absm 3 1) 2)
        (cat-9:vertex-i (cat-9:absm 1 3) 0)
        (cat-9:vertex-i (cat-9:absm 1 3) 1)
        (cat-9:vertex-i (cat-9:absm 1 3) 2)
        (cat-9:vertex-i (cat-9:absm 2 3) 0)
        (cat-9:vertex-i (cat-9:absm 2 3) 1)
        (cat-9:vertex-i (cat-9:absm 2 3) 2)
        (cat-9:vertex-i (cat-9:absm 0 7) 0)
        (cat-9:vertex-i (cat-9:absm 0 7) 1)
        (cat-9:vertex-i (cat-9:absm 0 7) 2)))


(test absm-int-ext
      (cat-9:absm-ext-int '(0 0 0 1 2 3 3 3))
      (cat-9:absm-ext-int '(0 1 1 1 2))
      (cat-9:absm-int-ext (cat-9:absm-ext-int '(0 0 0 1 2 3 3 3)))
      (cat-9:absm-int-ext (cat-9:absm-ext-int '(0 1 1 1 2))))


(defun dkfll (indx dmns hat)
  (cond ((= 1 dmns)
         (cat-9:absm 1 (cat-9:gmsm (first hat))))
        ((= 0 indx)
         (let ((del-1 (cat-9:absm-int-ext (first hat)))
               (del-2 (cat-9:absm-int-ext (second hat))))
           (cat-9:absm-ext-int
            (cons (first del-1)
                  (cons (second del-2) (rest del-1))))))
        ((= 1 indx)
         (let ((del-0 (cat-9:absm-int-ext (first hat)))
               (del-2 (cat-9:absm-int-ext (second hat))))
           (cat-9:absm-ext-int
            (cons (first del-2) del-0))))
        (t
         (let ((del-0 (cat-9:absm-int-ext (first hat)))
               (del-1 (cat-9:absm-int-ext (second hat))))
           (cat-9:absm-ext-int
            (cons (first del-1) del-0))))))


(let ((d (cat-9:delta-infinity)))
  (cat-9:smst-kan d #'dkfll)
  (cat-9:kfll d 0 1 (list (cat-9:absm 0 1)))
  (cat-9:kfll d 0 2 (list (cat-9:absm 0 5) (cat-9:absm 0 3)))
  (cat-9:kfll d 1 2 (list (cat-9:absm 0 6) (cat-9:absm 0 3)))
  (cat-9:kfll d 2 2 (list (cat-9:absm 0 6) (cat-9:absm 0 5)))
  (cat-9:kfll d 0 2 (list (cat-9:absm 0 3) (cat-9:absm 0 3)))
  (cat-9:kfll d 1 2 (list (cat-9:absm 1 2) (cat-9:absm 0 3))))


(test check-hat
      (let* ((od (cat-9:gdeltab))
             (s (cat-9:loop3 0 31 2))
             (faces (mapcar #'(lambda (indx) (cat-9:face od indx 3 s))
                            '(0 1 2 3))))
        (delete (nth 1 faces) faces)
        (cat-9:check-hat od 1 3 faces)
        (setf (nth 2 faces) (nth 1 faces))
        (signals #-ecl simple-error
                 #+ecl simple-type-error
                 (cat-9:check-hat od 1 3 faces))))
