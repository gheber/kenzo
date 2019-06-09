;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test disk-pasting-cmpr
      (let* ((c (cat-9:delta 3))
             (cmpr (cat-9:disk-pasting-cmpr (cat-9:cmpr c) 'new)))
        (is (equal :equal (funcall cmpr 'new 'new)))
        (is (equal :less (funcall cmpr 'new 5)))
        (is (equal :greater (funcall cmpr 5 'new)))
        (is (equal :equal (funcall cmpr 5 5)))
        (is (equal :less (funcall cmpr 5 6)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test disk-pasting-basis
      (cat-9:cat-init)
      (let* ((c (cat-9:delta 3))
             (basis (cat-9:disk-pasting-basis (cat-9:basis c) 3 'new)))
        (funcall basis 3)
        (funcall basis 2)))


(test disk-pasting-intr-dffr
      (cat-9:cat-init)
      (let* ((c (cat-9:delta 3))
             intr)
        (signals simple-error
                 (setf intr (cat-9:disk-pasting-intr-dffr
                             (cat-9:dffr c) 3 'new (cat-9:cmbn 2 1 7))))
        (setf intr (cat-9:disk-pasting-intr-dffr (cat-9:dffr c)
                                               3 'new (cat-9:? c 3 15)))
        (funcall intr (cat-9:cmbn 2 1 7))
        (funcall intr (cat-9:cmbn 3))
        (funcall intr (cat-9:cmbn 3 1 15))
        (funcall intr (cat-9:cmbn 3 1 'new 1 15))
        (funcall intr (cat-9:cmbn 3 1 'new -1 15))
        (funcall intr (cat-9:cmbn 3 -1 'new 1 15))
        (funcall intr (cat-9:cmbn 3 -1 'new -1 15))))


(test chcm-disk-pasting
      (cat-9:cat-init)
      (let* ((c (cat-9:delta 3))
             (s3 (cat-9:chcm-disk-pasting c 3 'new (cat-9:? c 3 15))))
        (cat-9:homology s3 0 5)))


(test disk-pasting-face
      (cat-9:cat-init)
      (let* ((c (cat-9:delta 3))
             (face (cat-9:disk-pasting-face (cat-9:cmpr c) (cat-9:face c)
                                          3 'new
                                          (list 14 (cat-9:absm 0 13) 11 7))))
        (funcall face 0 2 7)
        (funcall face 0 3 15)
        (dotimes (i 4) (print (funcall face i 3 'new)))))


(test disk-pasting
      (cat-9:cat-init)
      (let* ((d2 (cat-9:delta 2))
             (s2 (cat-9:disk-pasting d2 2 'new '(6 5 3)))
             s2xs2)
        (cat-9:homology s2 0 4)
        (setf s2xs2 (cat-9:crts-prdc s2 s2))
        (cat-9:homology s2xs2 0 6)))


(test mrph-disk-pasting-intr
      (cat-9:cat-init)
      (let* ((m (cat-9:idnt-mrph (cat-9:delta 3)))
             (intr (cat-9:mrph-disk-pasting-intr m (cat-9:cmpr (cat-9:delta 3))
                                               3 'new (cat-9:cmbn 3 -1 15))))
        (funcall intr (cat-9:cmbn 2 3 7))
        (funcall intr (cat-9:cmbn 3))
        (funcall intr (cat-9:cmbn 3 4 15))
        (funcall intr (cat-9:cmbn 3 1 'new 1 15))
        (funcall intr (cat-9:cmbn 3 -1 'new 1 15))))


(test mrph-disk-pasting
      (cat-9:cat-init)
      (let* ((d (cat-9:delta 3))
             (m (cat-9:idnt-mrph d))
             (sorc (cat-9:chcm-disk-pasting d 3 'new (cat-9:? d 3 15)))
             (nm (cat-9:mrph-disk-pasting m sorc sorc 3 'new
                                        (cat-9:cmbn 3 1 'new))))
        (cat-9:? nm (cat-9:cmbn 3 2 'new 3 15))))


(test disk-pasting1
      (cat-9:cat-init)
      (let* ((d (cat-9:delta 2))
             (s2 (cat-9:disk-pasting d 2 'new '(6 5 3))))
        (cat-9:homology s2 0 4)))
