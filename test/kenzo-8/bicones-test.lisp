;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test bicn-cmbn
      (let* ((comb-bic (cat-8:cmbn 3 2 (cat-8:bcnb 'b1) 4 (cat-8:bcnb 'b2)
                                 6 (cat-8:bcnb 'b3) 3 (cat-8:bcnc 'c1)
                                 5 (cat-8:bcnc 'c2) 7 (cat-8:bcnd 'd1))))
        (cat-8:cmbn-list comb-bic)
        (cat-8:dispatch-bicn-cmbn (cat-8:cmbn 3 3 (cat-8:bcnb 'b1)
                                          4 (cat-8:bcnb 'b2)
                                          33 (cat-8:bcnc 'c)
                                          333 (cat-8:bcnd 'd)))))

(test bicn-cmbn-cmbn*
      (is (cat-8:cmbn-non-zero-p
           (cat-8:bicn-cmbn-cmbnb (cat-8:cmbn 3 1 (cat-8:bcnb 'a) 2 (cat-8:bcnc 'b)))))
      (is (cat-8:cmbn-zero-p
           (cat-8:bicn-cmbn-cmbnb (cat-8:cmbn 3 2 (cat-8:bcnc 'b)))))
      (is (cat-8:cmbn-non-zero-p
           (cat-8:bicn-cmbn-cmbnc (cat-8:cmbn 3 1 (cat-8:bcnb 'a) 2 (cat-8:bcnc 'b)
                                          3 (cat-8:bcnd 'c)))))
      (is (cat-8:cmbn-zero-p
           (cat-8:bicn-cmbn-cmbnc (cat-8:cmbn 3 1 (cat-8:bcnb 'a) 3 (cat-8:bcnd 'c)))))
      (is (cat-8:cmbn-non-zero-p
           (cat-8:bicn-cmbn-cmbnd (cat-8:cmbn 3 1 (cat-8:bcnb 'a) 2 (cat-8:bcnc 'b)
                                          3 (cat-8:bcnd 'c)))))
      (is (cat-8:cmbn-zero-p
           (cat-8:bicn-cmbn-cmbnd (cat-8:cmbn 3 2 (cat-8:bcnb 'b))))))

(test make-bicn-cmbn
      (multiple-value-call #'cat-8:make-bicn-cmbn
        (cat-8:dispatch-bicn-cmbn
         (cat-8:cmbn 3 3 (cat-8:bcnb 'b1) 4 (cat-8:bcnb 'b2) 33 (cat-8:bcnc 'c)
                   333 (cat-8:bcnd 'd)))))

(test bicn-print
      (let ((a (format nil "~A" (cat-8:bcnb 'a)))
            (b (format nil "~A" (cat-8:bcnc 'b)))
            (c (format nil "~A" (cat-8:bcnd 'c))))
        (is (string= a "<BcnB A>"))
        (is (string= b "<BcnC B>"))
        (is (string= c "<BcnD C>"))))

(test bicone-cmpr
      (let ((r (cat-8:bicone-cmpr #'cat-8:f-cmpr #'cat-8:f-cmpr #'cat-8:f-cmpr)))
        (is (equal :less (funcall r (cat-8:bcnb 1) (cat-8:bcnc 0))))
        (is (equal :less (funcall r (cat-8:bcnb 1) (cat-8:bcnb 2))))))

(test bicone-basis
      (is (equal :locally-effective
                 (cat-8:bicone-basis :locally-effective
                                   :locally-effective
                                   :locally-effective)))
      (let* ((b #'(lambda (degr)
                    (mapcar #'(lambda (item)
                                (cons degr item)) (cat-8:<a-b> 0 degr))))
             (r (cat-8:bicone-basis b b b)))
        (is (not (null (funcall r 3))))))


(test bicone
      (progn
        (cat-8:cat-init)
        (let ((delta3 (cdelta1 3))
              (bic (cat-8:bicone (make-rdct1 3 2) (make-rdct1 4 2))))
          #|
          (princ (cat-8:basis delta3 0))
          (princ (cat-8:basis delta3 1))
          (princ (cat-8:basis delta3 2))
          (princ (cat-8:basis delta3 3))
          (princ (cat-8:basis delta3 4))
          |#
          (princ (cat-8:basis bic 0))
          (princ (cat-8:basis bic 1))
          (princ (cat-8:basis bic 4))
          (princ (cat-8:? bic (cat-8:cmbn 2 3 (cat-8:bcnb '(0 1 3))
                                      4 (cat-8:bcnc '(0 1 2 3))
                                      5 (cat-8:bcnd '(0 1 4)))))
          (is (cat-8:cmbn-zero-p
               (cat-8:? bic
                      (cat-8:? bic (cat-8:cmbn 2 3 (cat-8:bcnb '(0 1 3))
                                           4 (cat-8:bcnc '(0 1 2 3))
                                           5 (cat-8:bcnd '(0 1 4))))))))))


(test cmps
      (progn
        (cat-8:cat-init)
        (let* ((c (cat-8:build-chcm
                   :cmpr #'cat-8:s-cmpr
                   :basis #'(lambda (dmns) '(a))
                   :bsgn 'a
                   :intr-dffr #'cat-8:zero-intr-dffr
                   :strt :cmbn
                   :orgn '(c)))
               (h1 (cat-8:trivial-hmeq c))
               (h2 (cat-8:cmps h1 h1))
               (h3 (cat-8:cmps h2 h2)))
          (cat-8:pre-check-rdct (cat-8:lrdct h2))
          (setf cat-8:*tc* (cat-8:cmbn 3 1 (cat-8:bcnB 'a) 10 (cat-8:bcnC 'a)
                                   100 (cat-8:bcnD 'a)))
          (setf cat-8:*bc* (cat-8:cmbn 3 1 'a))
          (check-rdct)
          (cat-8:pre-check-rdct (cat-8:rrdct h2))
          (check-rdct)
          (setf cat-8:*tc* (cat-8:cmbn 3 1 (cat-8:bcnB (cat-8:bcnB 'a))
                                   10 (cat-8:bcnB (cat-8:bcnC 'a))
                                   100 (cat-8:bcnB (cat-8:bcnD 'a))
                                   1000 (cat-8:bcnC 'a)
                                   10000 (cat-8:bcnD (cat-8:bcnB 'a))
                                   5234 (cat-8:bcnD (cat-8:bcnC 'a))
                                   223 (cat-8:bcnD (cat-8:bcnD 'a))))
          (cat-8:pre-check-rdct (cat-8:lrdct h3))
          (check-rdct)
          (cat-8:pre-check-rdct (cat-8:rrdct h3))
          (check-rdct))))
