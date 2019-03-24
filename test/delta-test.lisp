;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test soft-delta-cmpr
      (is (equal :less (cat:soft-delta-cmpr (cat:d 12) (cat:d 23)))))

(defun faces (i lst)
  (cat:dlop-int-ext
   (cat:gmsm
    (cat:delta-face i 3 (cat:dlop-ext-int lst)))))

(when (string= (package-name (find-package 'cat)) "CAT-7")
  (test soft-delta-infinity
        (progn
          (cat:cat-init)
          (cat:cmpr (cat:soft-delta-infinity) (cat:d 2) (cat:d 4))
          (cat:face (cat:soft-delta-infinity) 1 2
                    (cat:d (cat:dlop-ext-int '(1 3 5))))
          (cat:cprd (cat:soft-delta-infinity) 3 (cat:d 15))
          (cat:dgnl (cat:soft-delta-infinity) 3 (cat:d 15))
          (cat:? (cat:soft-delta-infinity) 2
                 (cat:d (cat:dlop-ext-int '(0 2 4))))))


  (test soft-delta
        (progn
          (cat:cat-init)
          (let ((d3 (cat:soft-delta 3)))
            (cat:cmpr d3 (cat:d 2) (cat:d 4))
            (cat:basis d3 1)
            (cat:dgnl d3 3 (cat:d 15))
            (cat:face d3 1 2 (cat:d 21))
            (cat:? d3 2 (cat:d 13)))))


  (test delta-dgnl
        (is (cat:cmbn-non-zero-p (cat:delta-dgnl 3 15)))
        (is (cat:cmbn-non-zero-p (cat:delta-dgnl 3 170)))
        (is (cat:cmbn-non-zero-p
             (cat:soft-delta-dgnl 3 (cat:d (cat:dlop-ext-int '(1 3 5 7))))))
        (is (cat:cmbn-non-zero-p (cat:delta-dgnl 0 64))))


  (test delta-bndr
        (is (cat:cmbn-zero-p (cat:delta-bndr 0 4)))
        (is (cat:cmbn-non-zero-p (cat:delta-bndr 1 3)))
        (is (cat:cmbn-non-zero-p (cat:delta-bndr 1 5)))
        (is (cat:cmbn-non-zero-p (cat:delta-bndr 1 10)))
        (is (cat:cmbn-non-zero-p (cat:delta-bndr 5 63)))
        (is (cat:cmbn-non-zero-p (cat:soft-delta-bndr 5 (cat:d (cat:mask 6))))))


  (test delta-face
        (is (equal '(1 2 3) (faces 0 '(0 1 2 3))))
        (is (equal '(0 2 3) (faces 1 '(0 1 2 3))))
        (is (equal '(0 1 3) (faces 2 '(0 1 2 3))))
        (is (equal '(0 1 2) (faces 3 '(0 1 2 3))))
        (is (equal '(2 4 6) (faces 0 '(0 2 4 6))))
        (is (equal '(0 4 6) (faces 1 '(0 2 4 6))))
        (is (equal '(0 2 6) (faces 2 '(0 2 4 6))))
        (is (equal '(0 2 4) (faces 3 '(0 2 4 6))))

        (dotimes (i 4)
          (print (cat:soft-delta-face i 3 (cat:d (cat:mask 4)))))

        (dotimes (i 4)
          (print (cat:soft-delta-face i 3 (cat:d
                                           (cat:dlop-ext-int '(0 2 4 6))))))))


(test delta-infinity
      (is (equal :less (cat:cmpr (cat:delta-infinity) 2 4)))
      (is (equal :equal (cat:cmpr (cat:delta-infinity) 4 4)))
      (is (equal :greater (cat:cmpr (cat:delta-infinity) 8 4)))
      (signals simple-error (cat:basis (cat:delta-infinity) 1))
      (cat:face (cat:delta-infinity) 1 2 21)
      (cat:cprd (cat:delta-infinity) 3 15)
      (cat:dgnl (cat:delta-infinity) 3 15)
      (is (cat:cmbn-non-zero-p (cat:? (cat:delta-infinity) 2 21))))



(test basis
      (let ((basis (cat:delta-n-basis 3))
            (soft-basis (cat:soft-delta-n-basis 3)))
        (dotimes (i 5)
          (print (funcall basis i)))
        (dotimes (i 5)
          (print (funcall soft-basis i)))))

(test delta
      (progn
        (cat:cat-init)
        (let ((d3 (cat:delta 3))
              (d))
          (is (equal :less (cat:cmpr d3 2 4)))
          (is (equal :equal (cat:cmpr d3 4 4)))
          (is (equal :greater (cat:cmpr d3 8 4)))
          (cat:basis d3 1)
          (cat:dgnl d3 3 15)
          (cat:face d3 1 2 21)
          (cat:? d3 2 13)
          (setf d (cat:delta-infinity))
          (cat:basis d))))


(test eat
      (let* ((delta (cat:delta-infinity))
             (d (cat:bndr delta))
             (s14 (cat:mask 15)))
        (cat:cmbn-? d (cat:gnrt-? d 14 s14))
        (defun t1 (n)
          (time (dotimes (i n) (cat:cmbn-? d (cat:gnrt-? d 14 s14)))))
        (compile 't1)
        (t1 500)
        (setf cat:+too-much-time+ -1)
        (t1 500)))


(test deltab2-dgnl
      (dotimes (i 7)
        (print (cat:deltab2-dgnl i (cat:mask (1+ i))))))


(test deltab2-bndr
      (cat:deltab2-bndr 1 5)
      (cat:deltab2-bndr 3 15))
