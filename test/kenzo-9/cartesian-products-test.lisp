;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)

(test extract-common-dgop
      (dotimes (i 100)
        (dotimes (j 100)
          (multiple-value-bind (dgop dgop1 dgop2) (cat-9:extract-common-dgop i j)
            (unless (and (= i (cat-9:dgop*dgop dgop dgop1))
                         (= j (cat-9:dgop*dgop dgop dgop2)))
              (error "i = ~D, j = ~D, dgop = ~D, dgop1 = ~D, dgop2 = ~D"
                     i j dgop dgop1 dgop2))))))


(test 2absm-acrpr
      (cat-9:2absm-acrpr (cat-9:absm 5 'a) (cat-9:absm 3 'b)))


(test crts-prdc-cmpr
      (let* ((c (cat-9:cmpr (cat-9:delta-infinity)))
             (rslt (cat-9:crts-prdc-cmpr c c)))
        (is (equal :less (funcall rslt (cat-9:crpr 0 3 0 3) (cat-9:crpr 1 1 0 3))))
        (is (equal :greater
                   (funcall rslt (cat-9:crpr 4 3 0 7) (cat-9:crpr 3 1 0 7))))
        (is (equal :less (funcall rslt (cat-9:crpr 0 3 0 3) (cat-9:crpr 0 3 1 1))))
        (is (equal :less (funcall rslt (cat-9:crpr 0 3 0 3) (cat-9:crpr 0 5 0 3))))
        (is (equal :less (funcall rslt (cat-9:crpr 0 3 0 3) (cat-9:crpr 0 3 0 5))))
        (is (equal :equal
                   (funcall rslt (cat-9:crpr 0 3 0 3) (cat-9:crpr 0 3 0 3))))))


(test crts-prdc-basis
      (let* ((b (cat-9:basis (cat-9:delta 1)))
             (r (cat-9:crts-prdc-basis b b))
             (d3 (cat-9:basis (cat-9:delta 3))))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (funcall r 3)
        (setf r (cat-9:crts-prdc-basis d3 d3))
        (time (dotimes (i 7)
                (print (length (funcall r i)))))
        (setf s3 (cat-9:basis (cat-9:sphere 3)))
        (setf p (cat-9:crts-prdc-basis s3 s3))
        (dotimes (i 8)
          (print (funcall p i)))))


(test crts-prdc-face
      (let* ((d2 (cat-9:delta 2))
             (b2 (cat-9:basis d2))
             (f2 (cat-9:face d2))
             (b (cat-9:crts-prdc-basis b2 b2))
             (r (cat-9:crts-prdc-face f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (funcall b i))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc-face*
      (let* ((d2 (cat-9:delta 2))
             (b2 (cat-9:basis d2))
             (f2 (cat-9:face d2))
             (b (cat-9:crts-prdc-basis b2 b2))
             (r (cat-9:crts-prdc-face* f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item)))))))
      (let* ((s3 (cat-9:sphere 3))
             (b3 (cat-9:basis s3))
             (f3 (cat-9:face s3))
             (b (cat-9:crts-prdc-basis b3 b3))
             (r (cat-9:crts-prdc-face* f3 f3)))
        (dotimes (i 7)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc
      (let ((p (cat-9:crts-prdc (cat-9:delta 3) (cat-9:delta 3))))
        (cat-9:? p 0 (cat-9:crpr 0 1 0 2))
        (cat-9:? p 1 (cat-9:crpr 0 3 0 3))
        (cat-9:? p 2 (cat-9:crpr 0 7 0 7))
        (cat-9:? p 3 (cat-9:crpr 0 15 0 15))
        (cat-9:? p 4 (cat-9:crpr 1 15 2 15))))


(test eat
      (let* ((s5 (cat-9:sphere 5))
             (p (cat-9:crts-prdc s5 s5))
             (b (cat-9:basis p))
             (d (cat-9:bndr p))
             (basis (funcall b 10))
             (c (cat-9:make-cmbn :degr 10
                               :list (mapcar #'(lambda (item)
                                                 (cat-9:term (1+ (random 5))
                                                           item))
                                             basis))))
        (length basis)
        (setf cat-9:+too-much-time+ -1)
        (cat-9:cmbn-? d (cat-9:cmbn-? d c))
        (time
         (dotimes (i 5)
           (cat-9:cmbn-? d (cat-9:cmbn-? d c))))))
