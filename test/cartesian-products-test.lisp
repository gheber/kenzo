;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test extract-common-dgop
      (dotimes (i 100)
        (dotimes (j 100)
          (multiple-value-bind (dgop dgop1 dgop2) (cat:extract-common-dgop i j)
            (unless (and (= i (cat:dgop*dgop dgop dgop1))
                         (= j (cat:dgop*dgop dgop dgop2)))
              (error "i = ~D, j = ~D, dgop = ~D, dgop1 = ~D, dgop2 = ~D"
                     i j dgop dgop1 dgop2))))))


(test 2absm-acrpr
      (cat:2absm-acrpr (cat:absm 5 'a) (cat:absm 3 'b)))


(test crts-prdc-cmpr
      (let* ((c (cat:cmpr (cat:delta-infinity)))
             (rslt (cat:crts-prdc-cmpr c c)))
        (is (equal :less (funcall rslt (cat:crpr 0 3 0 3) (cat:crpr 1 1 0 3))))
        (is (equal :greater
                   (funcall rslt (cat:crpr 4 3 0 7) (cat:crpr 3 1 0 7))))
        (is (equal :less (funcall rslt (cat:crpr 0 3 0 3) (cat:crpr 0 3 1 1))))
        (is (equal :less (funcall rslt (cat:crpr 0 3 0 3) (cat:crpr 0 5 0 3))))
        (is (equal :less (funcall rslt (cat:crpr 0 3 0 3) (cat:crpr 0 3 0 5))))
        (is (equal :equal
                   (funcall rslt (cat:crpr 0 3 0 3) (cat:crpr 0 3 0 3))))))


(test crts-prdc-basis
      (let* ((b (cat:basis (cat:delta 1)))
             (r (cat:crts-prdc-basis b b))
             (d3 (cat:basis (cat:delta 3))))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (funcall r 3)
        (setf r (cat:crts-prdc-basis d3 d3))
        (time (dotimes (i 7)
                (print (length (funcall r i)))))
        (setf s3 (cat:basis (cat:sphere 3)))
        (setf p (cat:crts-prdc-basis s3 s3))
        (dotimes (i 8)
          (print (funcall p i)))))


(test crts-prdc-face
      (let* ((d2 (cat:delta 2))
             (b2 (cat:basis d2))
             (f2 (cat:face d2))
             (b (cat:crts-prdc-basis b2 b2))
             (r (cat:crts-prdc-face f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (funcall b i))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc-face*
      (let* ((d2 (cat:delta 2))
             (b2 (cat:basis d2))
             (f2 (cat:face d2))
             (b (cat:crts-prdc-basis b2 b2))
             (r (cat:crts-prdc-face* f2 f2)))
        (dotimes (i 5)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item)))))))
      (let* ((s3 (cat:sphere 3))
             (b3 (cat:basis s3))
             (f3 (cat:face s3))
             (b (cat:crts-prdc-basis b3 b3))
             (r (cat:crts-prdc-face* f3 f3)))
        (dotimes (i 7)
          (unless (zerop i)
            (dolist (item (coerce (funcall b i) 'list))
              (dotimes (j (1+ i))
                (format t "~%del-~D ~A = ~A"
                        j item (funcall r j i item))))))))


(test crts-prdc
      (let ((p (cat:crts-prdc (cat:delta 3) (cat:delta 3))))
        (cat:? p 0 (cat:crpr 0 1 0 2))
        (cat:? p 1 (cat:crpr 0 3 0 3))
        (cat:? p 2 (cat:crpr 0 7 0 7))
        (cat:? p 3 (cat:crpr 0 15 0 15))
        (cat:? p 4 (cat:crpr 1 15 2 15))))


(test eat
      (let* ((s5 (cat:sphere 5))
             (p (cat:crts-prdc s5 s5))
             (b (cat:basis p))
             (d (cat:bndr p))
             (basis (funcall b 10))
             (c (cat:make-cmbn :degr 10
                               :list (mapcar #'(lambda (item)
                                                 (cat:term (1+ (random 5))
                                                           item))
                                             basis))))
        (length basis)
        (setf cat:+too-much-time+ -1)
        (cat:cmbn-? d (cat:cmbn-? d c))
        (time
         (dotimes (i 5)
           (cat:cmbn-? d (cat:cmbn-? d c))))))
