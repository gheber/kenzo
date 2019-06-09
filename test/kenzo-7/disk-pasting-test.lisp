;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test disk-pasting-cmpr
      (let* ((c (cat-7:delta 3))
             (cmpr (cat-7:disk-pasting-cmpr (cat-7:cmpr c) 'new)))
        (is (equal :equal (funcall cmpr 'new 'new)))
        (is (equal :less (funcall cmpr 'new 5)))
        (is (equal :greater (funcall cmpr 5 'new)))
        (is (equal :equal (funcall cmpr 5 5)))
        (is (equal :less (funcall cmpr 5 6)))
        (is (equal :greater (funcall cmpr 6 5)))))


(test disk-pasting-basis
      (cat-7:cat-init)
      (let* ((c (cat-7:delta 3))
             (basis (cat-7:disk-pasting-basis (cat-7:basis c) 3 'new)))
        (funcall basis 3)
        (funcall basis 2)))


(test disk-pasting-intr-dffr
      (cat-7:cat-init)
      (let* ((c (cat-7:delta 3))
             intr)
        (signals simple-error
                 (setf intr (cat-7:disk-pasting-intr-dffr
                             (cat-7:dffr c) 3 'new (cat-7:cmbn 2 1 7))))
        (setf intr (cat-7:disk-pasting-intr-dffr (cat-7:dffr c)
                                                 3 'new (cat-7:? c 3 15)))
        (funcall intr (cat-7:cmbn 2 1 7))
        (funcall intr (cat-7:cmbn 3))
        (funcall intr (cat-7:cmbn 3 1 15))
        (funcall intr (cat-7:cmbn 3 1 'new 1 15))
        (funcall intr (cat-7:cmbn 3 1 'new -1 15))
        (funcall intr (cat-7:cmbn 3 -1 'new 1 15))
        (funcall intr (cat-7:cmbn 3 -1 'new -1 15))))

(test chcm-disk-pasting
      (cat-7:cat-init)
      (let* ((c (cat-7:delta 3))
             (s3 (cat-7:chcm-disk-pasting c 3 'new (cat-7:? c 3 15))))
        (cat-7:homology s3 0 5)))


(test disk-pasting-face
      (cat-7:cat-init)
      (let* ((c (cat-7:delta 3))
             (face (cat-7:disk-pasting-face (cat-7:cmpr c) (cat-7:face c)
                                            3 'new
                                            (list 14 (cat-7:absm 0 13) 11 7))))
        (funcall face 0 2 7)
        (funcall face 0 3 15)
        (dotimes (i 4) (print (funcall face i 3 'new)))))


(test disk-pasting
      (cat-7:cat-init)
      (let* ((d2 (cat-7:delta 2))
             (s2 (cat-7:disk-pasting d2 2 'new '(6 5 3)))
             s2xs2)
        (cat-7:homology s2 0 4)
        (setf s2xs2 (cat-7:crts-prdc s2 s2))
        (cat-7:homology s2xs2 0 6)))


(test mrph-disk-pasting-intr
      (cat-7:cat-init)
      (let* ((m (cat-7:idnt-mrph (cat-7:delta 3)))
             (intr (cat-7:mrph-disk-pasting-intr m (cat-7:cmpr (cat-7:delta 3))
                                                 3 'new (cat-7:cmbn 3 -1 15))))
        (funcall intr (cat-7:cmbn 2 3 7))
        (funcall intr (cat-7:cmbn 3))
        (funcall intr (cat-7:cmbn 3 4 15))
        (funcall intr (cat-7:cmbn 3 1 'new 1 15))
        (funcall intr (cat-7:cmbn 3 -1 'new 1 15))))


(test mrph-disk-pasting
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 3))
             (m (cat-7:idnt-mrph d))
             (sorc (cat-7:chcm-disk-pasting d 3 'new (cat-7:? d 3 15)))
             (nm (cat-7:mrph-disk-pasting m sorc sorc 3 'new
                                          (cat-7:cmbn 3 1 'new))))
        (cat-7:? nm (cat-7:cmbn 3 2 'new 3 15))))


(test disk-pasting1
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 2))
             (s2 (cat-7:disk-pasting d 2 'new '(6 5 3))))
        (cat-7:homology s2 0 4)))


(test hmeq-disk-pasting
      (cat-7:cat-init)
      (let* ((tcc (cat-7:build-chcm
                   :cmpr #'cat-7:s-cmpr
                   :basis #'(lambda (degr)
                              (case degr
                                (0 (list 'a))
                                (1 (list 'b))
                                (otherwise nil)))
                   :intr-dffr #'(lambda (degr gnrt)
                                  (if (= 1 degr)
                                      (cat-7:cmbn 0 1 'a)
                                      (cat-7:zero-cmbn (1- degr))))
                   :strt :gnrt
                   :orgn '(z-z)))
             (bcc (cat-7:build-chcm
                   :cmpr #'cat-7:s-cmpr
                   :basis #'(lambda (degr) nil)
                   :intr-dffr #'(lambda (degr gnrt) (error "Impossible."))
                   :strt :gnrt
                   :orgn '(zero)))
             (rh (cat-7:build-mrph
                  :sorc tcc :trgt tcc :degr +1
                  :intr #'(lambda (degr gnrt)
                            (if (zerop degr)
                                (cat-7:cmbn 1 1 'b)
                                (cat-7:zero-cmbn 2)))
                  :strt :gnrt
                  :orgn '(rh)))
             (hmeq (cat-7:build-hmeq
                    :lrdct (cat-7:trivial-rdct tcc)
                    :rrdct (cat-7:build-rdct
                            :f (cat-7:zero-mrph tcc bcc 0)
                            :g (cat-7:zero-mrph bcc tcc 0)
                            :h rh
                            :orgn '(rrdct))))
             (nhmeq (cat-7:hmeq-disk-pasting hmeq 1 'new (cat-7:cmbn 0 1 'a))))
        (cat-7:pre-check-rdct (cat-7:lrdct nhmeq))
        (setf cat-7:*tc* (cat-7:cmbn 0 1 'a))
        (setf cat-7:*bc* cat-7:*tc*)
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn 1 1 'new 10 'b))
        (setf cat-7:*bc* cat-7:*tc*)
        (check-rdct)
        (cat-7:pre-check-rdct (cat-7:rrdct nhmeq))
        (setf cat-7:*bc* (cat-7:zero-cmbn 0))
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn 0 1 'a))
        (check-rdct)))
