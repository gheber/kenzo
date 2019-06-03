;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test abar
      (cat-9:abar )
      (cat-9:abar '(2 a 3 b))
      (cat-9:abar 2 'a 3 'b)
      (signals simple-error (cat-9:abar 2 'a 3)))

(test bar-cmpr
      (let ((r (cat-9:bar-cmpr #'cat-9:s-cmpr)))
        (is (equal :equal (funcall r (cat-9:abar) (cat-9:abar))))
        (is (equal :greater (funcall r (cat-9:abar 3 'a) (cat-9:abar))))
        (is (equal :less (funcall r (cat-9:abar 3 'a) (cat-9:abar 2 'a 1 'b))))
        (is (equal :less (funcall r (cat-9:abar 3 'a) (cat-9:abar 3 'b))))
        (is (equal :equal (funcall r (cat-9:abar 3 'a) (cat-9:abar 3 'a))))))

(test bar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
        (cat-9:bar-basis-length basis 2 1)
        (cat-9:bar-basis-length basis 2 2)
        (cat-9:bar-basis-length basis 3 1)
        (cat-9:bar-basis-length basis 3 2)
        (cat-9:bar-basis-length basis 4 1)
        (cat-9:bar-basis-length basis 4 2)
        (cat-9:bar-basis-length basis 4 3)
        (cat-9:bar-basis-length basis 4 4)
        (cat-9:bar-basis-length basis 8 1)
        (cat-9:bar-basis-length basis 8 2)
        (cat-9:bar-basis-length basis 8 3)
        (cat-9:bar-basis-length basis 8 4)
        (cat-9:bar-basis-length basis 8 5)
        (cat-9:bar-basis-length basis 8 6)
        (cat-9:bar-basis-length basis 8 11)))


(test bar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
             (r (cat-9:bar-basis basis)))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (dotimes (i 7)
          (print (funcall r i)))
        (cat-9:bar-basis :locally-effective)))


(test bar-intr-vrtc-dffr
      (let* ((d (cat-9:soft-delta-infinity))
             (r (cat-9:bar-intr-vrtc-dffr (cat-9:dffr d))))
        (funcall r 0 (cat-9:abar))
        (funcall r 3 (cat-9:abar 3 (cat-9:d 7)))
        (funcall r 5 (cat-9:abar 3 (cat-9:d 7) 2 (cat-9:d 3)))
        (funcall r 5 (cat-9:abar 2 (cat-9:d 3) 3 (cat-9:d 7)))))


(defun random-abar1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat-9:mask 6)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-9:brgn (1+ dmns) gmsm) rslt))))
    (cat-9:make-abar :list rslt)))


(test vrtc-bar
      (progn
        (cat-9:cat-init)
        (let ((v (cat-9:vrtc-bar (cat-9:delta-infinity))))
          (dotimes (i 10)
            (print (random-abar1 5)))
          (dotimes (i 10)
            (let ((abar (random-abar1 3)))
              (print abar)
              (print (cat-9:? v (apply #'+ (mapcar #'car (cat-9:abar-list abar)))
                            abar))
              (print (cat-9:? v (cat-9:? v (apply #'+ (mapcar #'car
                                                          (cat-9:abar-list abar)))
                                     abar))))))))


(test bar-intr-hrzn-dffr
      (let* ((k (cat-9:k-z-1))
             (r (cat-9:bar-intr-hrzn-dffr (cat-9:aprd k))))
        (funcall r 0 (cat-9:abar ))
        (funcall r 3 (cat-9:abar 3 '(2 3)))
        (funcall r 6 (cat-9:abar 3 '(2 3) 3 '(-2 -3)))
        (funcall r 9 (cat-9:abar 3 '(2 3) 3 '(-2 -3) 3 '(2 3)))
        (funcall r 11 (cat-9:abar 3 '(2 3) 3 '(-2 -3) 2 '(-2) 3 '(-2 -3)))))


(test bar-hrzn-dffr
      (progn
        (cat-9:cat-init)
        (let ((h (cat-9:bar-hrzn-dffr (cat-9:k-z-1)))
              (abar))
          (dotimes (i 10)
            (print (random-abar 10 5)))
          (setf abar (random-abar 10 5))
          (cat-9:? h (apply #'+ (mapcar #'car (cat-9:abar-list abar))) abar)
          (cat-9:? h (cat-9:? h (apply #'+ (mapcar #'car
                                               (cat-9:abar-list abar))) abar))
          (dotimes (i 10)
            (setf abar (random-abar 10 3))
            (print abar)
            (print (cat-9:? h (apply #'+ (mapcar #'car (cat-9:abar-list abar)))
                          abar))
            (print (cat-9:? h (cat-9:? h (apply #'+ (mapcar #'car
                                                        (cat-9:abar-list abar)))
                                   abar)))))))


(test bar
      (progn
        (cat-9:cat-init)
        (let ((b (cat-9:bar (cat-9:k-z-1)))
              (abar (random-abar 10 3)))
          (cat-9:? b (apply #'+ (mapcar #'car (cat-9:abar-list abar))) abar)
          (cat-9:? b (cat-9:? b (apply #'+ (mapcar #'car (cat-9:abar-list abar)))
                          abar))
          (dotimes (i 10)
            (let ((abar (random-abar 10 3)))
              (print abar)
              (print (cat-9:? b (apply #'+ (mapcar #'car (cat-9:abar-list abar)))
                            abar))
              (print (cat-9:? b
                            (cat-9:? b
                                   (apply #'+ (mapcar #'car
                                                      (cat-9:abar-list abar)))
                                   abar))))))))


(test ncmbn-bar
      (cat-9:ncmbn-bar nil)
      (cat-9:ncmbn-bar (list (cat-9:cmbn 3 2 'a 3 'b)))
      (cat-9:ncmbn-bar (list (cat-9:cmbn 1 2 'a 3 'b) (cat-9:cmbn 2 4 'c 5 'd)))
      (cat-9:ncmbn-bar (list (cat-9:cmbn 1 2 'a 3 'b) (cat-9:cmbn 1 4 'c 5 'd)
                           (cat-9:cmbn 1 6 'e 7 'f))))


(test mrph-vrtc-bar-intr
      (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                 :basis :locally-effective
                                 :intr-dffr #'cat-9:zero-mrph
                                 :strt :cmbn))
             (m (cat-9:build-mrph :sorc cc :trgt cc :degr 0 :intr
                                #'(lambda (degr gnrt)
                                    (cat-9:cmbn degr 2 gnrt 3 (1+ gnrt)))
                                :strt :gnrt :orgn '(test)))
             (r (cat-9:mrph-vrtc-bar-intr m)))
        (funcall r 4 (cat-9:abar 2 3 2 4))))

(test vrtc-bar
      (progn
        (cat-9:cat-init)
        (let* ((f (cat-9:aw (cat-9:soft-delta-infinity)
                            (cat-9:soft-delta-infinity)))
               (cf (cat-9:vrtc-bar f)))
          (cat-9:? cf 6 (cat-9:abar 3 (cat-9:crpr 0 (cat-9::make-delta :cdr 7)
                                                  0 (cat-9::make-delta :cdr 7))
                                    3
                                    (cat-9:crpr 0 (cat-9::make-delta :cdr 56)
                                                0 (cat-9::make-delta :cdr 56)))))))


(test hmtp-vrtc-bar-intr
      (progn
        (cat-9:cat-init)
        (let* ((ez (cat-9:ez (cat-9:delta-infinity) (cat-9:delta-infinity)))
               (h (cat-9:h ez))
               (gf (cat-9:cmps (cat-9:g ez) (cat-9:f ez)))
               (r (cat-9:hmtp-vrtc-bar-intr h gf)))
          (funcall r 3 (cat-9:abar 3 (cat-9:crpr 0 7 0 7)))
          (funcall r 9 (cat-9:abar 3 (cat-9:crpr 0 7 0 7) 3 (cat-9:crpr 0 14 0 14)
                                 3 (cat-9:crpr 0 14 0 14))))))


#|
(test vrtc-bar1
      (progn
        (cat-9:cat-init)

        (let* ((tcc (cat-9:build-chcm
                     :cmpr #'cat-9:s-cmpr
                     :basis #'(lambda (degr) '(a b c d))
                     :bsgn 'd
                     :intr-dffr #'(lambda (degr gnrt)
                                    (ecase gnrt
                                      (a (cat-9:cmbn (1- degr) 1 'b 1 'd))
                                      ((b d) (cat-9:cmbn (1- degr)))
                                      (c (cat-9:cmbn (1- degr) 1 'd))))
                     :strt :gnrt
                     :orgn '(tcc)))
               (bcc (cat-9:build-chcm
                     :cmpr #'cat-9:s-cmpr
                     :basis #'(lambda (degr) '(c d))
                     :bsgn 'd
                     :intr-dffr #'(lambda (degr gnrt)
                                    (ecase gnrt
                                      (d (cat-9:cmbn (1- degr)))
                                      (c (cat-9:cmbn (1- degr) 1 'd))))
                     :strt :gnrt
                     :orgn '(bcc)))
               (f (cat-9:build-mrph :sorc tcc :trgt bcc :degr 0
                                  :intr #'(lambda (degr gnrt)
                                            (ecase gnrt
                                              (a (cat-9:cmbn degr 1 'c 1 'd))
                                              (b (cat-9:cmbn degr))
                                              ((c d) (cat-9:cmbn degr 1 gnrt))))
                                  :strt :gnrt :orgn '(f)))
               (g (cat-9:build-mrph :sorc bcc :trgt tcc :degr 0
                                  :intr #'identity :strt :cmbn :orgn '(g)))
               (h (cat-9:build-mrph :sorc tcc :trgt tcc :degr +1
                                  :intr #'(lambda (degr gnrt)
                                            (ecase gnrt
                                              ((a b) (cat-9:cmbn
                                                      (1+ degr)
                                                      1 'a -1 'b -1 'c -1 'd))
                                              ((c d) (cat-9:cmbn (1+ degr)))))
                                  :strt :gnrt :orgn '(h)))
               (rdct (cat-9:build-rdct :f f :g g :h h :orgn '(rdct)))
               (bar))
          (cat-9:tcc rdct 3 'a)
          (cat-9:g rdct (cat-9:f rdct 3 'a))
          (cat-9:h rdct 3 'a)
          (setf bar (cat-9:vrtc-bar rdct))
          (cat-9:pre-check-rdct bar)
          (aleat-tc)
          (aleat-bc)
          ;;(loop (c))
          (dotimes (i 10) (c))))) ;; degrees >= 15 is possible => error.
|#

(test homology
      (progn
        (cat-9:cat-init)
        (let* ((h (cat-9:efhm (cat-9:k-z-1)))
               (b (cat-9:bar h)))
          (cat-9:homology (cat-9:rbcc b) 0 11))))
