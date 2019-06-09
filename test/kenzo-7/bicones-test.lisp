;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test bicn-cmbn
      (let* ((comb-bic (cat-7:cmbn 3 2 (cat-7:bcnb 'b1) 4 (cat-7:bcnb 'b2)
                                 6 (cat-7:bcnb 'b3) 3 (cat-7:bcnc 'c1)
                                 5 (cat-7:bcnc 'c2) 7 (cat-7:bcnd 'd1))))
        (cat-7:cmbn-list comb-bic)
        (cat-7:dispatch-bicn-cmbn (cat-7:cmbn 3 3 (cat-7:bcnb 'b1)
                                          4 (cat-7:bcnb 'b2)
                                          33 (cat-7:bcnc 'c)
                                          333 (cat-7:bcnd 'd)))))

(test bicn-cmbn-cmbn*
      (is (cat-7:cmbn-non-zero-p
           (cat-7:bicn-cmbn-cmbnb (cat-7:cmbn 3 1 (cat-7:bcnb 'a) 2 (cat-7:bcnc 'b)))))
      (is (cat-7:cmbn-zero-p
           (cat-7:bicn-cmbn-cmbnb (cat-7:cmbn 3 2 (cat-7:bcnc 'b)))))
      (is (cat-7:cmbn-non-zero-p
           (cat-7:bicn-cmbn-cmbnc (cat-7:cmbn 3 1 (cat-7:bcnb 'a) 2 (cat-7:bcnc 'b)
                                          3 (cat-7:bcnd 'c)))))
      (is (cat-7:cmbn-zero-p
           (cat-7:bicn-cmbn-cmbnc (cat-7:cmbn 3 1 (cat-7:bcnb 'a) 3 (cat-7:bcnd 'c)))))
      (is (cat-7:cmbn-non-zero-p
           (cat-7:bicn-cmbn-cmbnd (cat-7:cmbn 3 1 (cat-7:bcnb 'a) 2 (cat-7:bcnc 'b)
                                          3 (cat-7:bcnd 'c)))))
      (is (cat-7:cmbn-zero-p
           (cat-7:bicn-cmbn-cmbnd (cat-7:cmbn 3 2 (cat-7:bcnb 'b))))))

(test make-bicn-cmbn
      (multiple-value-call #'cat-7:make-bicn-cmbn
        (cat-7:dispatch-bicn-cmbn
         (cat-7:cmbn 3 3 (cat-7:bcnb 'b1) 4 (cat-7:bcnb 'b2) 33 (cat-7:bcnc 'c)
                   333 (cat-7:bcnd 'd)))))

(test bicn-print
      (let ((a (format nil "~A" (cat-7:bcnb 'a)))
            (b (format nil "~A" (cat-7:bcnc 'b)))
            (c (format nil "~A" (cat-7:bcnd 'c))))
        (is (string= a "<BcnB A>"))
        (is (string= b "<BcnC B>"))
        (is (string= c "<BcnD C>"))))

(test bicone-cmpr
      (let ((r (cat-7:bicone-cmpr #'cat-7:f-cmpr #'cat-7:f-cmpr #'cat-7:f-cmpr)))
        (is (equal :less (funcall r (cat-7:bcnb 1) (cat-7:bcnc 0))))
        (is (equal :less (funcall r (cat-7:bcnb 1) (cat-7:bcnb 2))))))

(test bicone-basis
      (is (equal :locally-effective
                 (cat-7:bicone-basis :locally-effective
                                   :locally-effective
                                   :locally-effective)))
      (let* ((b #'(lambda (degr)
                    (mapcar #'(lambda (item)
                                (cons degr item)) (cat-7:<a-b> 0 degr))))
             (r (cat-7:bicone-basis b b b)))
        (is (not (null (funcall r 3))))))


(test bicone
      (progn
        (cat-7:cat-init)
        (let ((delta3 (cdelta1 3))
              (bic (cat-7:bicone (make-rdct1 3 2) (make-rdct1 4 2))))
          #|
          (princ (cat-7:basis delta3 0))
          (princ (cat-7:basis delta3 1))
          (princ (cat-7:basis delta3 2))
          (princ (cat-7:basis delta3 3))
          (princ (cat-7:basis delta3 4))
          |#
          (princ (cat-7:basis bic 0))
          (princ (cat-7:basis bic 1))
          (princ (cat-7:basis bic 4))
          (princ (cat-7:? bic (cat-7:cmbn 2 3 (cat-7:bcnb '(0 1 3))
                                      4 (cat-7:bcnc '(0 1 2 3))
                                      5 (cat-7:bcnd '(0 1 4)))))
          (is (cat-7:cmbn-zero-p
               (cat-7:? bic
                      (cat-7:? bic (cat-7:cmbn 2 3 (cat-7:bcnb '(0 1 3))
                                           4 (cat-7:bcnc '(0 1 2 3))
                                           5 (cat-7:bcnd '(0 1 4))))))))))


(test cmps
      (progn
        (cat-7:cat-init)
        (let* ((c (cat-7:build-chcm
                   :cmpr #'cat-7:s-cmpr
                   :basis #'(lambda (dmns) '(a))
                   :bsgn 'a
                   :intr-dffr #'cat-7:zero-intr-dffr
                   :strt :cmbn
                   :orgn '(c)))
               (h1 (cat-7:trivial-hmeq c))
               (h2 (cat-7:cmps h1 h1))
               (h3 (cat-7:cmps h2 h2)))
          (cat-7:pre-check-rdct (cat-7:lrdct h2))
          (setf cat-7:*tc* (cat-7:cmbn 3 1 (cat-7:bcnB 'a) 10 (cat-7:bcnC 'a)
                                   100 (cat-7:bcnD 'a)))
          (setf cat-7:*bc* (cat-7:cmbn 3 1 'a))
          (check-rdct)
          (cat-7:pre-check-rdct (cat-7:rrdct h2))
          (check-rdct)
          (setf cat-7:*tc* (cat-7:cmbn 3 1 (cat-7:bcnB (cat-7:bcnB 'a))
                                   10 (cat-7:bcnB (cat-7:bcnC 'a))
                                   100 (cat-7:bcnB (cat-7:bcnD 'a))
                                   1000 (cat-7:bcnC 'a)
                                   10000 (cat-7:bcnD (cat-7:bcnB 'a))
                                   5234 (cat-7:bcnD (cat-7:bcnC 'a))
                                   223 (cat-7:bcnD (cat-7:bcnD 'a))))
          (cat-7:pre-check-rdct (cat-7:lrdct h3))
          (check-rdct)
          (cat-7:pre-check-rdct (cat-7:rrdct h3))
          (check-rdct))))
