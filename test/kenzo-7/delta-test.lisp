;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test soft-delta-cmpr
      (is (equal :less (cat-7:soft-delta-cmpr (cat-7:d 12) (cat-7:d 23)))))

(defun faces (i lst)
  (cat-7:dlop-int-ext
   (cat-7:gmsm
    (cat-7:delta-face i 3 (cat-7:dlop-ext-int lst)))))

(test soft-delta-infinity
      (progn
        (cat-7:cat-init)
        (cat-7:cmpr (cat-7:soft-delta-infinity) (cat-7:d 2) (cat-7:d 4))
        (cat-7:face (cat-7:soft-delta-infinity) 1 2
                    (cat-7:d (cat-7:dlop-ext-int '(1 3 5))))
        (cat-7:cprd (cat-7:soft-delta-infinity) 3 (cat-7:d 15))
        (cat-7:dgnl (cat-7:soft-delta-infinity) 3 (cat-7:d 15))
        (cat-7:? (cat-7:soft-delta-infinity) 2
                 (cat-7:d (cat-7:dlop-ext-int '(0 2 4))))))

(test soft-delta
      (progn
        (cat-7:cat-init)
        (let ((d3 (cat-7:soft-delta 3)))
          (cat-7:cmpr d3 (cat-7:d 2) (cat-7:d 4))
          (cat-7:basis d3 1)
          (cat-7:dgnl d3 3 (cat-7:d 15))
          (cat-7:face d3 1 2 (cat-7:d 21))
          (cat-7:? d3 2 (cat-7:d 13)))))

(test delta-dgnl
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-dgnl 3 15)))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-dgnl 3 170)))
      (is (cat-7:cmbn-non-zero-p
           (cat-7:soft-delta-dgnl 3 (cat-7:d (cat-7:dlop-ext-int '(1 3 5 7))))))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-dgnl 0 64))))


(test delta-bndr
      (is (cat-7:cmbn-zero-p (cat-7:delta-bndr 0 4)))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-bndr 1 3)))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-bndr 1 5)))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-bndr 1 10)))
      (is (cat-7:cmbn-non-zero-p (cat-7:delta-bndr 5 63)))
      (is (cat-7:cmbn-non-zero-p (cat-7:soft-delta-bndr 5
                                                        (cat-7:d (cat-7:mask 6))))))

(test delta-face
      (is (equal '(1 2 3) (faces 0 '(0 1 2 3))))
      (is (equal '(0 2 3) (faces 1 '(0 1 2 3))))
      (is (equal '(0 1 3) (faces 2 '(0 1 2 3))))
      (is (equal '(0 1 2) (faces 3 '(0 1 2 3))))
      (is (equal '(2 4 6) (faces 0 '(0 2 4 6))))
      (is (equal '(0 4 6) (faces 1 '(0 2 4 6))))
      (is (equal '(0 2 6) (faces 2 '(0 2 4 6))))
      (is (equal '(0 2 4) (faces 3 '(0 2 4 6))))

      (progn
        (dotimes (i 4)
          (print (cat-7:soft-delta-face i 3 (cat-7:d (cat-7:mask 4)))))
        (dotimes (i 4)
          (print (cat-7:soft-delta-face i 3 (cat-7:d (cat-7:dlop-ext-int '(0 2 4 6))))))))


(test delta-infinity
      (is (equal :less (cat-7:cmpr (cat-7:delta-infinity) 2 4)))
      (is (equal :equal (cat-7:cmpr (cat-7:delta-infinity) 4 4)))
      (is (equal :greater (cat-7:cmpr (cat-7:delta-infinity) 8 4)))
      (signals simple-error (cat-7:basis (cat-7:delta-infinity) 1))
      (cat-7:face (cat-7:delta-infinity) 1 2 21)
      (cat-7:cprd (cat-7:delta-infinity) 3 15)
      (cat-7:dgnl (cat-7:delta-infinity) 3 15)
      (is (cat-7:cmbn-non-zero-p (cat-7:? (cat-7:delta-infinity) 2 21))))


(test basis
      (let ((basis (cat-7:delta-n-basis 3))
            (soft-basis (cat-7:soft-delta-n-basis 3)))
        (dotimes (i 5)
          (print (funcall basis i)))
        (dotimes (i 5)
          (print (funcall soft-basis i)))))

(test delta
      (progn
        (cat-7:cat-init)
        (let ((d3 (cat-7:delta 3))
              (d))
          (is (equal :less (cat-7:cmpr d3 2 4)))
          (is (equal :equal (cat-7:cmpr d3 4 4)))
          (is (equal :greater (cat-7:cmpr d3 8 4)))
          (cat-7:basis d3 1)
          (cat-7:dgnl d3 3 15)
          (cat-7:face d3 1 2 21)
          (cat-7:? d3 2 13)
          (setf d (cat-7:delta-infinity))
          (cat-7:basis d))))


(test eat
      (let* ((delta (cat-7:delta-infinity))
             (d (cat-7:bndr delta))
             (s14 (cat-7:mask 15)))
        (cat-7:cmbn-? d (cat-7:gnrt-? d 14 s14))
        (defun t1 (n)
          (time (dotimes (i n) (cat-7:cmbn-? d (cat-7:gnrt-? d 14 s14)))))
        (compile 't1)
        (t1 500)
        (setf cat-7:+too-much-time+ -1)
        (t1 500)))


(test deltab2-dgnl
      (dotimes (i 7)
        (print (cat-7:deltab2-dgnl i (cat-7:mask (1+ i))))))


(test deltab2-bndr
      (cat-7:deltab2-bndr 1 5)
      (cat-7:deltab2-bndr 3 15))
