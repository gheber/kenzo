;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test extract-common-dgop
      (dotimes (i 100)
        (dotimes (j 100)
          (multiple-value-bind (dgop dgop1 dgop2) (cat-7:extract-common-dgop i j)
            (unless (and (= i (cat-7:dgop*dgop dgop dgop1))
                         (= j (cat-7:dgop*dgop dgop dgop2)))
              (error "i = ~D, j = ~D, dgop = ~D, dgop1 = ~D, dgop2 = ~D"
                     i j dgop dgop1 dgop2))))))


(test 2absm-acrpr
      (cat-7:2absm-acrpr (cat-7:absm 5 'a) (cat-7:absm 3 'b)))


(test crts-prdc-cmpr
      (let* ((c (cat-7:cmpr (cat-7:delta-infinity)))
             (rslt (cat-7:crts-prdc-cmpr c c)))
        (is (equal :less (funcall rslt (cat-7:crpr 0 3 0 3) (cat-7:crpr 1 1 0 3))))
        (is (equal :greater
                   (funcall rslt (cat-7:crpr 4 3 0 7) (cat-7:crpr 3 1 0 7))))
        (is (equal :less (funcall rslt (cat-7:crpr 0 3 0 3) (cat-7:crpr 0 3 1 1))))
        (is (equal :less (funcall rslt (cat-7:crpr 0 3 0 3) (cat-7:crpr 0 5 0 3))))
        (is (equal :less (funcall rslt (cat-7:crpr 0 3 0 3) (cat-7:crpr 0 3 0 5))))
        (is (equal :equal
                   (funcall rslt (cat-7:crpr 0 3 0 3) (cat-7:crpr 0 3 0 3))))))


(test crts-prdc-basis
      (let* ((b (cat-7:basis (cat-7:delta 1)))
             (r (cat-7:crts-prdc-basis b b))
             (d3 (cat-7:basis (cat-7:delta 3))))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (funcall r 3)
        (setf r (cat-7:crts-prdc-basis d3 d3))
        (time (dotimes (i 7)
                (print (length (funcall r i)))))
        (setf s3 (cat-7:basis (cat-7:sphere 3)))
        (setf p (cat-7:crts-prdc-basis s3 s3))
        (dotimes (i 8)
          (print (funcall p i)))))


(test crts-prdc-face
      (let* ((d2 (cat-7:delta 2))
             (b2 (cat-7:basis d2))
             (f2 (cat-7:face d2))
             (b (cat-7:crts-prdc-basis b2 b2))
             (r (cat-7:crts-prdc-face f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (funcall b i))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc-face*
      (let* ((d2 (cat-7:delta 2))
             (b2 (cat-7:basis d2))
             (f2 (cat-7:face d2))
             (b (cat-7:crts-prdc-basis b2 b2))
             (r (cat-7:crts-prdc-face* f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item)))))))
      (let* ((s3 (cat-7:sphere 3))
             (b3 (cat-7:basis s3))
             (f3 (cat-7:face s3))
             (b (cat-7:crts-prdc-basis b3 b3))
             (r (cat-7:crts-prdc-face* f3 f3)))
        (dotimes (i 7)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc
      (let ((p (cat-7:crts-prdc (cat-7:delta 3) (cat-7:delta 3))))
        (cat-7:? p 0 (cat-7:crpr 0 1 0 2))
        (cat-7:? p 1 (cat-7:crpr 0 3 0 3))
        (cat-7:? p 2 (cat-7:crpr 0 7 0 7))
        (cat-7:? p 3 (cat-7:crpr 0 15 0 15))
        (cat-7:? p 4 (cat-7:crpr 1 15 2 15))))


(test eat
      (let* ((s5 (cat-7:sphere 5))
             (p (cat-7:crts-prdc s5 s5))
             (b (cat-7:basis p))
             (d (cat-7:bndr p))
             (basis (funcall b 10))
             (c (cat-7:make-cmbn :degr 10
                               :list (mapcar #'(lambda (item)
                                                 (cat-7:term (1+ (random 5))
                                                           item))
                                             basis))))
        (length basis)
        (setf cat-7:+too-much-time+ -1)
        (cat-7:cmbn-? d (cat-7:cmbn-? d c))
        (time
         (dotimes (i 5)
           (cat-7:cmbn-? d (cat-7:cmbn-? d c))))))
