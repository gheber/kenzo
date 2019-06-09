;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test bicn-cmbn
      (let* ((comb-bic (cat-9:cmbn 3 2 (cat-9:bcnb 'b1) 4 (cat-9:bcnb 'b2)
                                 6 (cat-9:bcnb 'b3) 3 (cat-9:bcnc 'c1)
                                 5 (cat-9:bcnc 'c2) 7 (cat-9:bcnd 'd1))))
        (cat-9:cmbn-list comb-bic)
        (cat-9:dispatch-bicn-cmbn (cat-9:cmbn 3 3 (cat-9:bcnb 'b1)
                                          4 (cat-9:bcnb 'b2)
                                          33 (cat-9:bcnc 'c)
                                          333 (cat-9:bcnd 'd)))))

(test bicn-cmbn-cmbn*
      (is (cat-9:cmbn-non-zero-p
           (cat-9:bicn-cmbn-cmbnb (cat-9:cmbn 3 1 (cat-9:bcnb 'a) 2 (cat-9:bcnc 'b)))))
      (is (cat-9:cmbn-zero-p
           (cat-9:bicn-cmbn-cmbnb (cat-9:cmbn 3 2 (cat-9:bcnc 'b)))))
      (is (cat-9:cmbn-non-zero-p
           (cat-9:bicn-cmbn-cmbnc (cat-9:cmbn 3 1 (cat-9:bcnb 'a) 2 (cat-9:bcnc 'b)
                                          3 (cat-9:bcnd 'c)))))
      (is (cat-9:cmbn-zero-p
           (cat-9:bicn-cmbn-cmbnc (cat-9:cmbn 3 1 (cat-9:bcnb 'a) 3 (cat-9:bcnd 'c)))))
      (is (cat-9:cmbn-non-zero-p
           (cat-9:bicn-cmbn-cmbnd (cat-9:cmbn 3 1 (cat-9:bcnb 'a) 2 (cat-9:bcnc 'b)
                                          3 (cat-9:bcnd 'c)))))
      (is (cat-9:cmbn-zero-p
           (cat-9:bicn-cmbn-cmbnd (cat-9:cmbn 3 2 (cat-9:bcnb 'b))))))

(test make-bicn-cmbn
      (multiple-value-call #'cat-9:make-bicn-cmbn
        (cat-9:dispatch-bicn-cmbn
         (cat-9:cmbn 3 3 (cat-9:bcnb 'b1) 4 (cat-9:bcnb 'b2) 33 (cat-9:bcnc 'c)
                   333 (cat-9:bcnd 'd)))))

(test bicn-print
      (let ((a (format nil "~A" (cat-9:bcnb 'a)))
            (b (format nil "~A" (cat-9:bcnc 'b)))
            (c (format nil "~A" (cat-9:bcnd 'c))))
        (is (string= a "<BcnB A>"))
        (is (string= b "<BcnC B>"))
        (is (string= c "<BcnD C>"))))

(test bicone-cmpr
      (let ((r (cat-9:bicone-cmpr #'cat-9:f-cmpr #'cat-9:f-cmpr #'cat-9:f-cmpr)))
        (is (equal :less (funcall r (cat-9:bcnb 1) (cat-9:bcnc 0))))
        (is (equal :less (funcall r (cat-9:bcnb 1) (cat-9:bcnb 2))))))

(test bicone-basis
      (is (equal :locally-effective
                 (cat-9:bicone-basis :locally-effective
                                   :locally-effective
                                   :locally-effective)))
      (let* ((b #'(lambda (degr)
                    (mapcar #'(lambda (item)
                                (cons degr item)) (cat-9:<a-b> 0 degr))))
             (r (cat-9:bicone-basis b b b)))
        (is (not (null (funcall r 3))))))


(test bicone
      (progn
        (cat-9:cat-init)
        (let ((delta3 (cdelta1 3))
              (bic (cat-9:bicone (make-rdct1 3 2) (make-rdct1 4 2))))
          #|
          (princ (cat-9:basis delta3 0))
          (princ (cat-9:basis delta3 1))
          (princ (cat-9:basis delta3 2))
          (princ (cat-9:basis delta3 3))
          (princ (cat-9:basis delta3 4))
          |#
          (princ (cat-9:basis bic 0))
          (princ (cat-9:basis bic 1))
          (princ (cat-9:basis bic 4))
          (princ (cat-9:? bic (cat-9:cmbn 2 3 (cat-9:bcnb '(0 1 3))
                                      4 (cat-9:bcnc '(0 1 2 3))
                                      5 (cat-9:bcnd '(0 1 4)))))
          (is (cat-9:cmbn-zero-p
               (cat-9:? bic
                      (cat-9:? bic (cat-9:cmbn 2 3 (cat-9:bcnb '(0 1 3))
                                           4 (cat-9:bcnc '(0 1 2 3))
                                           5 (cat-9:bcnd '(0 1 4))))))))))


(test cmps
      (progn
        (cat-9:cat-init)
        (let* ((c (cat-9:build-chcm
                   :cmpr #'cat-9:s-cmpr
                   :basis #'(lambda (dmns) '(a))
                   :bsgn 'a
                   :intr-dffr #'cat-9:zero-intr-dffr
                   :strt :cmbn
                   :orgn '(c)))
               (h1 (cat-9:trivial-hmeq c))
               (h2 (cat-9:cmps h1 h1))
               (h3 (cat-9:cmps h2 h2)))
          (cat-9:pre-check-rdct (cat-9:lrdct h2))
          (setf cat-9:*tc* (cat-9:cmbn 3 1 (cat-9:bcnB 'a) 10 (cat-9:bcnC 'a)
                                   100 (cat-9:bcnD 'a)))
          (setf cat-9:*bc* (cat-9:cmbn 3 1 'a))
          (check-rdct)
          (cat-9:pre-check-rdct (cat-9:rrdct h2))
          (check-rdct)
          (setf cat-9:*tc* (cat-9:cmbn 3 1 (cat-9:bcnB (cat-9:bcnB 'a))
                                   10 (cat-9:bcnB (cat-9:bcnC 'a))
                                   100 (cat-9:bcnB (cat-9:bcnD 'a))
                                   1000 (cat-9:bcnC 'a)
                                   10000 (cat-9:bcnD (cat-9:bcnB 'a))
                                   5234 (cat-9:bcnD (cat-9:bcnC 'a))
                                   223 (cat-9:bcnD (cat-9:bcnD 'a))))
          (cat-9:pre-check-rdct (cat-9:lrdct h3))
          (check-rdct)
          (cat-9:pre-check-rdct (cat-9:rrdct h3))
          (check-rdct))))
