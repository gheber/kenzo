;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test tnpr
      (cat-7:tnpr 2 'a 3 'b)
      (setf cat-7:*tnpr-with-degrees* t)
      (cat-7:tnpr 2 'a 3 'b)
      (setf cat-7:*tnpr-with-degrees* nil))


(test 2cmbn-tnpr
      (setf cat-7:*tnpr-with-degrees* t)
      (cat-7:2cmbn-tnpr (cat-7:cmbn 2 3 'a 4 'b -5 'c)
                      (cat-7:cmbn 3 4 'x -3 'y 2 'z)))


(test tnsr-prdc-cmpr
      (let ((cmpr (cat-7:tnsr-prdc-cmpr #'cat-7:s-cmpr #'cat-7:s-cmpr)))
        (is (equal :less (funcall cmpr
                                  (cat-7:tnpr 2 'a 3 'b)
                                  (cat-7:tnpr 3 'a 2 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-7:tnpr 3 'a 2 'b)
                                     (cat-7:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-7:tnpr 2 'a 3 'b)
                                  (cat-7:tnpr 2 'b 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-7:tnpr 2 'b 3 'b)
                                     (cat-7:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-7:tnpr 2 'a 3 'a)
                                  (cat-7:tnpr 2 'a 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-7:tnpr 2 'a 3 'c)
                                     (cat-7:tnpr 2 'a 3 'b))))
        (is (equal :equal (funcall cmpr
                                   (cat-7:tnpr 2 'a 3 'b)
                                   (cat-7:tnpr 2 'a 3 'b))))))


(defun bas (degr)
  (case degr
    (0 '(a b c))
    (1 '(d))
    (2 nil)
    (3 '(x y))))


(test tnsr-prdc-basis
      (let ((bas (cat-7:tnsr-prdc-basis #'bas #'bas)))
        (dotimes (i 8)
          (print (funcall bas i)))))


(test tnsr-prdc-intr-dffr
      (cat-7:cat-init)
      (let* ((chcm (cat-7:build-chcm :cmpr #'cat-7:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-7:zero-mrph
                                   :strt :gnrt :orgn '(test-1)))
             (dffr (cat-7:build-mrph
                    :sorc chcm :trgt chcm :degr -1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-7:cmbn (1- degr) 2 'd1a -3 'd2a))
                                (b (cat-7:cmbn (1- degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-7:tnsr-prdc-intr-dffr dffr dffr)))
        (funcall rslt 4 (cat-7:tnpr 2 'a 2 'b))
        (funcall rslt 5 (cat-7:tnpr 3 'a 2 'b))
        (funcall rslt 5 (cat-7:tnpr 2 'a 3 'b))
        (funcall rslt 6 (cat-7:tnpr 3 'a 3 'b))))


(test tnsr-prdc
      (let ((dd (cat-7:tnsr-prdc (cat-7:delta 2) (cat-7:delta 3))))
        (cat-7:cmpr dd (cat-7:tnpr 2 7 2 11) (cat-7:tnpr 2 7 2 14))
        (cat-7:basis dd 3)
        (cat-7:? dd 4 (cat-7:tnpr 2 7 2 14))
        (cat-7:? dd 3 (cat-7:tnpr 1 6 2 14))
        (cat-7:? dd (cat-7:? dd 4 (cat-7:tnpr 2 7 2 14)))
        (cat-7:? dd (cat-7:? dd 3 (cat-7:tnpr 1 6 2 14)))))


(test tnsr-prdc-intr
      (cat-7:cat-init)
      (let* ((chcm (cat-7:build-chcm :cmpr #'cat-7:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-7:zero-mrph
                                   :strt :gnrt
                                   :orgn '(test-1)))

             (mrph (cat-7:build-mrph
                    :sorc chcm :trgt chcm :degr 0
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-7:cmbn degr 2 'd1a -3 'd2a))
                                (b (cat-7:cmbn degr 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-7:tnsr-prdc-intr mrph mrph)))
        (funcall rslt 6 (cat-7:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-7:tnpr 3 'a 2 'b))
        (setf mrph (cat-7:build-mrph
                    :sorc chcm :trgt chcm :degr 1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-7:cmbn (1+ degr) 2 'd1a -3 'd2a))
                                (b (cat-7:cmbn (1+ degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-3)))
        (setf rslt (cat-7:tnsr-prdc-intr mrph mrph))
        (funcall rslt 6 (cat-7:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-7:tnpr 3 'a 2 'b))))


(test tnsr-prdc
      (cat-7:cat-init)
      (let* ((d (cat-7:dffr (cat-7:delta-infinity)))
             (dd (cat-7:tnsr-prdc d d))
             (ddd (cat-7:cmps dd dd)))
        (is (eq (cat-7:sorc dd) (cat-7:tnsr-prdc (cat-7:delta-infinity)
                                             (cat-7:delta-infinity))))
        (cat-7:? dd 5 (cat-7:tnpr 2 7 3 15))
        (cat-7:? dd 5 (cat-7:tnpr 3 15 2 7))
        (cat-7:? dd (cat-7:? dd 5 (cat-7:tnpr 2 7 3 15)))
        (cat-7:? dd (cat-7:? dd 5 (cat-7:tnpr 3 15 2 7)))
        (cat-7:? ddd 5 (cat-7:tnpr 2 7 3 15))
        (cat-7:? ddd 5 (cat-7:tnpr 3 15 2 7))))


(test tnsr-prdc1
      (cat-7:cat-init)
      (let* ((r (cat-7:ez (cat-7:delta-infinity) (cat-7:delta-infinity)))
             (r2 (cat-7:tnsr-prdc r r)))
        (setf cat-7:*bc* (cat-7:cmbn 4 1 (cat-7:tnpr 2 (cat-7:tnpr 1 3 1 3) 2
                                               (cat-7:tnpr 1 3 1 3)))
              cat-7:*tc* (cat-7:cmbn 2 1 (cat-7:tnpr 1 (cat-7:crpr 0 3 0 3) 1
                                               (cat-7:crpr 0 3 0 3))))
        (cat-7:pre-check-rdct r2)
        (check-rdct)))


(test tnsr-prdc2
      (cat-7:cat-init)
      (let* ((k (cat-7:k-z 2))
             (k2 (cat-7:tnsr-prdc k k)))
        (cat-7:homology k2 0 10)))
