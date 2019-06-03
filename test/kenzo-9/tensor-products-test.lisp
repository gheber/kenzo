;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)


(test tnpr
      (cat-9:tnpr 2 'a 3 'b)
      (setf cat-9:*tnpr-with-degrees* t)
      (cat-9:tnpr 2 'a 3 'b)
      (setf cat-9:*tnpr-with-degrees* nil))


(test 2cmbn-tnpr
      (setf cat-9:*tnpr-with-degrees* t)
      (cat-9:2cmbn-tnpr (cat-9:cmbn 2 3 'a 4 'b -5 'c)
                      (cat-9:cmbn 3 4 'x -3 'y 2 'z)))


(test tnsr-prdc-cmpr
      (let ((cmpr (cat-9:tnsr-prdc-cmpr #'cat-9:s-cmpr #'cat-9:s-cmpr)))
        (is (equal :less (funcall cmpr
                                  (cat-9:tnpr 2 'a 3 'b)
                                  (cat-9:tnpr 3 'a 2 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-9:tnpr 3 'a 2 'b)
                                     (cat-9:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-9:tnpr 2 'a 3 'b)
                                  (cat-9:tnpr 2 'b 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-9:tnpr 2 'b 3 'b)
                                     (cat-9:tnpr 2 'a 3 'b))))
        (is (equal :less (funcall cmpr
                                  (cat-9:tnpr 2 'a 3 'a)
                                  (cat-9:tnpr 2 'a 3 'b))))
        (is (equal :greater (funcall cmpr
                                     (cat-9:tnpr 2 'a 3 'c)
                                     (cat-9:tnpr 2 'a 3 'b))))
        (is (equal :equal (funcall cmpr
                                   (cat-9:tnpr 2 'a 3 'b)
                                   (cat-9:tnpr 2 'a 3 'b))))))


(defun bas (degr)
  (case degr
    (0 '(a b c))
    (1 '(d))
    (2 nil)
    (3 '(x y))))


(test tnsr-prdc-basis
      (let ((bas (cat-9:tnsr-prdc-basis #'bas #'bas)))
        (dotimes (i 8)
          (print (funcall bas i)))))


(test tnsr-prdc-intr-dffr
      (cat-9:cat-9-init)
      (let* ((chcm (cat-9:build-chcm :cmpr #'cat-9:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-9:zero-mrph
                                   :strt :gnrt :orgn '(test-1)))
             (dffr (cat-9:build-mrph
                    :sorc chcm :trgt chcm :degr -1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-9:cmbn (1- degr) 2 'd1a -3 'd2a))
                                (b (cat-9:cmbn (1- degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-9:tnsr-prdc-intr-dffr dffr dffr)))
        (funcall rslt 4 (cat-9:tnpr 2 'a 2 'b))
        (funcall rslt 5 (cat-9:tnpr 3 'a 2 'b))
        (funcall rslt 5 (cat-9:tnpr 2 'a 3 'b))
        (funcall rslt 6 (cat-9:tnpr 3 'a 3 'b))))


(test tnsr-prdc
      (let ((dd (cat-9:tnsr-prdc (cat-9:delta 2) (cat-9:delta 3))))
        (cat-9:cmpr dd (cat-9:tnpr 2 7 2 11) (cat-9:tnpr 2 7 2 14))
        (cat-9:basis dd 3)
        (cat-9:? dd 4 (cat-9:tnpr 2 7 2 14))
        (cat-9:? dd 3 (cat-9:tnpr 1 6 2 14))
        (cat-9:? dd (cat-9:? dd 4 (cat-9:tnpr 2 7 2 14)))
        (cat-9:? dd (cat-9:? dd 3 (cat-9:tnpr 1 6 2 14)))))


(test tnsr-prdc-intr
      (cat-9:cat-9-init)
      (let* ((chcm (cat-9:build-chcm :cmpr #'cat-9:s-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-9:zero-mrph
                                   :strt :gnrt
                                   :orgn '(test-1)))

             (mrph (cat-9:build-mrph
                    :sorc chcm :trgt chcm :degr 0
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-9:cmbn degr 2 'd1a -3 'd2a))
                                (b (cat-9:cmbn degr 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-2)))
             (rslt (cat-9:tnsr-prdc-intr mrph mrph)))
        (funcall rslt 6 (cat-9:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-9:tnpr 3 'a 2 'b))
        (setf mrph (cat-9:build-mrph
                    :sorc chcm :trgt chcm :degr 1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cat-9:cmbn (1+ degr) 2 'd1a -3 'd2a))
                                (b (cat-9:cmbn (1+ degr) 3 'd1b -4 'd2b))))
                    :strt :gnrt :orgn '(test-3)))
        (setf rslt (cat-9:tnsr-prdc-intr mrph mrph))
        (funcall rslt 6 (cat-9:tnpr 2 'a 4 'b))
        (funcall rslt 5 (cat-9:tnpr 3 'a 2 'b))))


(test tnsr-prdc
      (cat-9:cat-9-init)
      (let* ((d (cat-9:dffr (cat-9:delta-infinity)))
             (dd (cat-9:tnsr-prdc d d))
             (ddd (cat-9:cmps dd dd)))
        (is (eq (cat-9:sorc dd) (cat-9:tnsr-prdc (cat-9:delta-infinity)
                                             (cat-9:delta-infinity))))
        (cat-9:? dd 5 (cat-9:tnpr 2 7 3 15))
        (cat-9:? dd 5 (cat-9:tnpr 3 15 2 7))
        (cat-9:? dd (cat-9:? dd 5 (cat-9:tnpr 2 7 3 15)))
        (cat-9:? dd (cat-9:? dd 5 (cat-9:tnpr 3 15 2 7)))
        (cat-9:? ddd 5 (cat-9:tnpr 2 7 3 15))
        (cat-9:? ddd 5 (cat-9:tnpr 3 15 2 7))))


(test tnsr-prdc1
      (cat-9:cat-9-init)
      (let* ((r (cat-9:ez (cat-9:delta-infinity) (cat-9:delta-infinity)))
             (r2 (cat-9:tnsr-prdc r r)))
        (setf cat-9:*bc* (cat-9:cmbn 4 1 (cat-9:tnpr 2 (cat-9:tnpr 1 3 1 3) 2
                                               (cat-9:tnpr 1 3 1 3)))
              cat-9:*tc* (cat-9:cmbn 2 1 (cat-9:tnpr 1 (cat-9:crpr 0 3 0 3) 1
                                               (cat-9:crpr 0 3 0 3))))
        (cat-9:pre-check-rdct r2)
        (check-rdct)))


(test tnsr-prdc2
      (cat-9:cat-9-init)
      (let* ((k (cat-9:k-z 2))
             (k2 (cat-9:tnsr-prdc k k)))
        (cat-9:homology k2 0 10)))
