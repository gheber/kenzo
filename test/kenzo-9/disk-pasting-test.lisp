;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

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
      (cat-9:cat-9-init)
      (let* ((c (cat-9:delta 3))
             (basis (cat-9:disk-pasting-basis (cat-9:basis c) 3 'new)))
        (funcall basis 3)
        (funcall basis 2)))


(test disk-pasting-intr-dffr
      (cat-9:cat-9-init)
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
      (cat-9:cat-9-init)
      (let* ((c (cat-9:delta 3))
             (s3 (cat-9:chcm-disk-pasting c 3 'new (cat-9:? c 3 15))))
        (cat-9:homology s3 0 5)))


(test disk-pasting-face
      (cat-9:cat-9-init)
      (let* ((c (cat-9:delta 3))
             (face (cat-9:disk-pasting-face (cat-9:cmpr c) (cat-9:face c)
                                          3 'new
                                          (list 14 (cat-9:absm 0 13) 11 7))))
        (funcall face 0 2 7)
        (funcall face 0 3 15)
        (dotimes (i 4) (print (funcall face i 3 'new)))))


(test disk-pasting
      (cat-9:cat-9-init)
      (let* ((d2 (cat-9:delta 2))
             (s2 (cat-9:disk-pasting d2 2 'new '(6 5 3)))
             s2xs2)
        (cat-9:homology s2 0 4)
        (setf s2xs2 (cat-9:crts-prdc s2 s2))
        (cat-9:homology s2xs2 0 6)))


(test mrph-disk-pasting-intr
      (cat-9:cat-9-init)
      (let* ((m (cat-9:idnt-mrph (cat-9:delta 3)))
             (intr (cat-9:mrph-disk-pasting-intr m (cat-9:cmpr (cat-9:delta 3))
                                               3 'new (cat-9:cmbn 3 -1 15))))
        (funcall intr (cat-9:cmbn 2 3 7))
        (funcall intr (cat-9:cmbn 3))
        (funcall intr (cat-9:cmbn 3 4 15))
        (funcall intr (cat-9:cmbn 3 1 'new 1 15))
        (funcall intr (cat-9:cmbn 3 -1 'new 1 15))))


(test mrph-disk-pasting
      (cat-9:cat-9-init)
      (let* ((d (cat-9:delta 3))
             (m (cat-9:idnt-mrph d))
             (sorc (cat-9:chcm-disk-pasting d 3 'new (cat-9:? d 3 15)))
             (nm (cat-9:mrph-disk-pasting m sorc sorc 3 'new
                                        (cat-9:cmbn 3 1 'new))))
        (cat-9:? nm (cat-9:cmbn 3 2 'new 3 15))))


(test disk-pasting1
      (cat-9:cat-9-init)
      (let* ((d (cat-9:delta 2))
             (s2 (cat-9:disk-pasting d 2 'new '(6 5 3))))
        (cat-9:homology s2 0 4)))


(when (or (string= (package-name (find-package 'cat)) "CAT-7")
          (string= (package-name (find-package 'cat)) "CAT-8"))
  (test hmeq-disk-pasting
        (cat-9:cat-9-init)
        (let* ((tcc (cat-9:build-chcm
                     :cmpr #'cat-9:s-cmpr
                     :basis #'(lambda (degr)
                                (case degr
                                  (0 (list 'a))
                                  (1 (list 'b))
                                  (otherwise nil)))
                     :intr-dffr #'(lambda (degr gnrt)
                                    (if (= 1 degr)
                                        (cat-9:cmbn 0 1 'a)
                                        (cat-9:zero-cmbn (1- degr))))
                     :strt :gnrt
                     :orgn '(z-z)))
               (bcc (cat-9:build-chcm
                     :cmpr #'cat-9:s-cmpr
                     :basis #'(lambda (degr) nil)
                     :intr-dffr #'(lambda (degr gnrt) (error "Impossible."))
                     :strt :gnrt
                     :orgn '(zero)))
               (rh (cat-9:build-mrph
                    :sorc tcc :trgt tcc :degr +1
                    :intr #'(lambda (degr gnrt)
                              (if (zerop degr)
                                  (cat-9:cmbn 1 1 'b)
                                  (cat-9:zero-cmbn 2)))
                    :strt :gnrt
                    :orgn '(rh)))
               (hmeq (cat-9:build-hmeq
                      :lrdct (cat-9:trivial-rdct tcc)
                      :rrdct (cat-9:build-rdct
                              :f (cat-9:zero-mrph tcc bcc 0)
                              :g (cat-9:zero-mrph bcc tcc 0)
                              :h rh
                              :orgn '(rrdct))))
               (nhmeq (cat-9:hmeq-disk-pasting hmeq 1 'new (cat-9:cmbn 0 1 'a))))
          (cat-9:pre-check-rdct (cat-9:lrdct nhmeq))
          (setf cat-9:*tc* (cat-9:cmbn 0 1 'a))
          (setf cat-9:*bc* cat-9:*tc*)
          (check-rdct)
          (setf cat-9:*tc* (cat-9:cmbn 1 1 'new 10 'b))
          (setf cat-9:*bc* cat-9:*tc*)
          (check-rdct)
          (cat-9:pre-check-rdct (cat-9:rrdct nhmeq))
          (setf cat-9:*bc* (cat-9:zero-cmbn 0))
          (check-rdct)
          (setf cat-9:*tc* (cat-9:cmbn 0 1 'a))
          (check-rdct))))
