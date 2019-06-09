;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test tnpr
      (cat-8:tnpr 2 'a 3 'b)
      (setf cat-8:*tnpr-with-degrees* t)
      (cat-8:tnpr 2 'a 3 'b)
      (setf cat-8:*tnpr-with-degrees* nil))


(test 2cmbn-tnpr
      (setf cat-8:*tnpr-with-degrees* t)
      (cat-8:2cmbn-tnpr (cat-8:cmbn 2 3 'a 4 'b -5 'c)
                      (cat-8:cmbn 3 4 'x -3 'y 2 'z)))


(test tnsr-prdc-cmpr
      (let ((cmpr (cat-8:tnsr-prdc-cmpr #'cat-8:s-cmpr #'cat-8:s-cmpr)))
        (is (equal :less (funcall cmpr
                                  (cat-8:tnpr 2 'a 3 'b)
                                  (cat-8:tnpr 3 'a 2 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-8:tnpr 3 'a 2 'b)
                                     (cat-8:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-8:tnpr 2 'a 3 'b)
                                  (cat-8:tnpr 2 'b 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-8:tnpr 2 'b 3 'b)
                                     (cat-8:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-8:tnpr 2 'a 3 'a)
                                  (cat-8:tnpr 2 'a 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-8:tnpr 2 'a 3 'c)
                                     (cat-8:tnpr 2 'a 3 'b))))
        (is (equal :equal (funcall cmpr
                                   (cat-8:tnpr 2 'a 3 'b)
                                   (cat-8:tnpr 2 'a 3 'b))))))


(defun bas (degr)
  (case degr
    (0 '(a b c))
    (1 '(d))
    (2 nil)
    (3 '(x y))))


(test tnsr-prdc-basis
      (let ((bas (cat-8:tnsr-prdc-basis #'bas #'bas)))
        (dotimes (i 8)
          (print (funcall bas i)))))


(test tnsr-prdc-intr-dffr
      (cat-8:cat-init)
      (let* ((chcm (cat-8:build-chcm :cmpr #'cat-8:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-8:zero-mrph
                                   :strt :gnrt :orgn '(test-1)))
             (dffr (cat-8:build-mrph
                    :sorc chcm :trgt chcm :degr -1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-8:cmbn (1- degr) 2 'd1a -3 'd2a))
                                (b (cat-8:cmbn (1- degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-8:tnsr-prdc-intr-dffr dffr dffr)))
        (funcall rslt 4 (cat-8:tnpr 2 'a 2 'b))
        (funcall rslt 5 (cat-8:tnpr 3 'a 2 'b))
        (funcall rslt 5 (cat-8:tnpr 2 'a 3 'b))
        (funcall rslt 6 (cat-8:tnpr 3 'a 3 'b))))


(test tnsr-prdc
      (let ((dd (cat-8:tnsr-prdc (cat-8:delta 2) (cat-8:delta 3))))
        (cat-8:cmpr dd (cat-8:tnpr 2 7 2 11) (cat-8:tnpr 2 7 2 14))
        (cat-8:basis dd 3)
        (cat-8:? dd 4 (cat-8:tnpr 2 7 2 14))
        (cat-8:? dd 3 (cat-8:tnpr 1 6 2 14))
        (cat-8:? dd (cat-8:? dd 4 (cat-8:tnpr 2 7 2 14)))
        (cat-8:? dd (cat-8:? dd 3 (cat-8:tnpr 1 6 2 14)))))


(test tnsr-prdc-intr
      (cat-8:cat-init)
      (let* ((chcm (cat-8:build-chcm :cmpr #'cat-8:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-8:zero-mrph
                                   :strt :gnrt
                                   :orgn '(test-1)))

             (mrph (cat-8:build-mrph
                    :sorc chcm :trgt chcm :degr 0
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-8:cmbn degr 2 'd1a -3 'd2a))
                                (b (cat-8:cmbn degr 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-8:tnsr-prdc-intr mrph mrph)))
        (funcall rslt 6 (cat-8:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-8:tnpr 3 'a 2 'b))
        (setf mrph (cat-8:build-mrph
                    :sorc chcm :trgt chcm :degr 1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-8:cmbn (1+ degr) 2 'd1a -3 'd2a))
                                (b (cat-8:cmbn (1+ degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-3)))
        (setf rslt (cat-8:tnsr-prdc-intr mrph mrph))
        (funcall rslt 6 (cat-8:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-8:tnpr 3 'a 2 'b))))


(test tnsr-prdc
      (cat-8:cat-init)
      (let* ((d (cat-8:dffr (cat-8:delta-infinity)))
             (dd (cat-8:tnsr-prdc d d))
             (ddd (cat-8:cmps dd dd)))
        (is (eq (cat-8:sorc dd) (cat-8:tnsr-prdc (cat-8:delta-infinity)
                                             (cat-8:delta-infinity))))
        (cat-8:? dd 5 (cat-8:tnpr 2 7 3 15))
        (cat-8:? dd 5 (cat-8:tnpr 3 15 2 7))
        (cat-8:? dd (cat-8:? dd 5 (cat-8:tnpr 2 7 3 15)))
        (cat-8:? dd (cat-8:? dd 5 (cat-8:tnpr 3 15 2 7)))
        (cat-8:? ddd 5 (cat-8:tnpr 2 7 3 15))
        (cat-8:? ddd 5 (cat-8:tnpr 3 15 2 7))))


(test tnsr-prdc1
      (cat-8:cat-init)
      (let* ((r (cat-8:ez (cat-8:delta-infinity) (cat-8:delta-infinity)))
             (r2 (cat-8:tnsr-prdc r r)))
        (setf cat-8:*bc* (cat-8:cmbn 4 1 (cat-8:tnpr 2 (cat-8:tnpr 1 3 1 3) 2
                                               (cat-8:tnpr 1 3 1 3)))
              cat-8:*tc* (cat-8:cmbn 2 1 (cat-8:tnpr 1 (cat-8:crpr 0 3 0 3) 1
                                               (cat-8:crpr 0 3 0 3))))
        (cat-8:pre-check-rdct r2)
        (check-rdct)))


(test tnsr-prdc2
      (cat-8:cat-init)
      (let* ((k (cat-8:k-z 2))
             (k2 (cat-8:tnsr-prdc k k)))
        (cat-8:homology k2 0 10)))
