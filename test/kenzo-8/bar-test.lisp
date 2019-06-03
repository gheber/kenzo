;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test abar
      (cat-8:abar )
      (cat-8:abar '(2 a 3 b))
      (cat-8:abar 2 'a 3 'b)
      (signals simple-error (cat-8:abar 2 'a 3)))

(test bar-cmpr
      (let ((r (cat-8:bar-cmpr #'cat-8:s-cmpr)))
        (is (equal :equal (funcall r (cat-8:abar) (cat-8:abar))))
        (is (equal :greater (funcall r (cat-8:abar 3 'a) (cat-8:abar))))
        (is (equal :less (funcall r (cat-8:abar 3 'a) (cat-8:abar 2 'a 1 'b))))
        (is (equal :less (funcall r (cat-8:abar 3 'a) (cat-8:abar 3 'b))))
        (is (equal :equal (funcall r (cat-8:abar 3 'a) (cat-8:abar 3 'a))))))

(test bar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
        (cat-8:bar-basis-length basis 2 1)
        (cat-8:bar-basis-length basis 2 2)
        (cat-8:bar-basis-length basis 3 1)
        (cat-8:bar-basis-length basis 3 2)
        (cat-8:bar-basis-length basis 4 1)
        (cat-8:bar-basis-length basis 4 2)
        (cat-8:bar-basis-length basis 4 3)
        (cat-8:bar-basis-length basis 4 4)
        (cat-8:bar-basis-length basis 8 1)
        (cat-8:bar-basis-length basis 8 2)
        (cat-8:bar-basis-length basis 8 3)
        (cat-8:bar-basis-length basis 8 4)
        (cat-8:bar-basis-length basis 8 5)
        (cat-8:bar-basis-length basis 8 6)
        (cat-8:bar-basis-length basis 8 11)))

(test bar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
             (r (cat-8:bar-basis basis)))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (dotimes (i 7)
          (print (funcall r i)))
        (cat-8:bar-basis :locally-effective)))

(test bar-intr-vrtc-dffr
      (let* ((d (cat-8:soft-delta-infinity))
             (r (cat-8:bar-intr-vrtc-dffr (cat-8:dffr d))))
        (funcall r 0 (cat-8:abar))
        (funcall r 3 (cat-8:abar 3 (cat-8:d 7)))
        (funcall r 5 (cat-8:abar 3 (cat-8:d 7) 2 (cat-8:d 3)))
        (funcall r 5 (cat-8:abar 2 (cat-8:d 3) 3 (cat-8:d 7)))))

(defun random-abar1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat-8:mask 6)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-8:brgn (1+ dmns) gmsm) rslt))))
    (cat-8:make-abar :list rslt)))

(test vrtc-bar
      (progn
        (cat-8:cat-init)
        (let ((v (cat-8:vrtc-bar (cat-8:delta-infinity))))
          (dotimes (i 10)
            (print (random-abar1 5)))
          (dotimes (i 10)
            (let ((abar (random-abar1 3)))
              (print abar)
              (print (cat-8:? v (apply #'+ (mapcar #'car (cat-8:abar-list abar)))
                              abar))
              (print (cat-8:? v (cat-8:? v (apply #'+ (mapcar #'car
                                                              (cat-8:abar-list abar)))
                                         abar))))))))

(test bar-intr-hrzn-dffr
      (let* ((k (cat-8:k-z-1))
             (r (cat-8:bar-intr-hrzn-dffr (cat-8:aprd k))))
        (funcall r 0 (cat-8:abar ))
        (funcall r 3 (cat-8:abar 3 '(2 3)))
        (funcall r 6 (cat-8:abar 3 '(2 3) 3 '(-2 -3)))
        (funcall r 9 (cat-8:abar 3 '(2 3) 3 '(-2 -3) 3 '(2 3)))
        (funcall r 11 (cat-8:abar 3 '(2 3) 3 '(-2 -3) 2 '(-2) 3 '(-2 -3)))))

(test bar-hrzn-dffr
      (progn
        (cat-8:cat-init)
        (let ((h (cat-8:bar-hrzn-dffr (cat-8:k-z-1)))
              (abar))
          (dotimes (i 10)
            (print (random-abar 10 5)))
          (setf abar (random-abar 10 5))
          (cat-8:? h (apply #'+ (mapcar #'car (cat-8:abar-list abar))) abar)
          (cat-8:? h (cat-8:? h (apply #'+ (mapcar #'car
                                                   (cat-8:abar-list abar))) abar))
          (dotimes (i 10)
            (setf abar (random-abar 10 3))
            (print abar)
            (print (cat-8:? h (apply #'+ (mapcar #'car (cat-8:abar-list abar)))
                            abar))
            (print (cat-8:? h (cat-8:? h (apply #'+ (mapcar #'car
                                                            (cat-8:abar-list abar)))
                                       abar)))))))

(test bar
      (progn
        (cat-8:cat-init)
        (let ((b (cat-8:bar (cat-8:k-z-1)))
              (abar (random-abar 10 3)))
          (cat-8:? b (apply #'+ (mapcar #'car (cat-8:abar-list abar))) abar)
          (cat-8:? b (cat-8:? b (apply #'+ (mapcar #'car (cat-8:abar-list abar)))
                              abar))
          (dotimes (i 10)
            (let ((abar (random-abar 10 3)))
              (print abar)
              (print (cat-8:? b (apply #'+ (mapcar #'car (cat-8:abar-list abar)))
                              abar))
              (print (cat-8:? b
                              (cat-8:? b
                                       (apply #'+ (mapcar #'car
                                                          (cat-8:abar-list abar)))
                                       abar))))))))

(test ncmbn-bar
      (cat-8:ncmbn-bar nil)
      (cat-8:ncmbn-bar (list (cat-8:cmbn 3 2 'a 3 'b)))
      (cat-8:ncmbn-bar (list (cat-8:cmbn 1 2 'a 3 'b) (cat-8:cmbn 2 4 'c 5 'd)))
      (cat-8:ncmbn-bar (list (cat-8:cmbn 1 2 'a 3 'b) (cat-8:cmbn 1 4 'c 5 'd)
                             (cat-8:cmbn 1 6 'e 7 'f))))

(test mrph-vrtc-bar-intr
      (let* ((cc (cat-8:build-chcm :cmpr #'cat-8:f-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-8:zero-mrph
                                   :strt :cmbn))
             (m (cat-8:build-mrph :sorc cc :trgt cc :degr 0 :intr
                                  #'(lambda (degr gnrt)
                                      (cat-8:cmbn degr 2 gnrt 3 (1+ gnrt)))
                                  :strt :gnrt :orgn '(test)))
             (r (cat-8:mrph-vrtc-bar-intr m)))
        (funcall r 4 (cat-8:abar 2 3 2 4))))


(test vrtc-bar
      (progn
        (cat-8:cat-init)
        (let* ((f (cat-8:aw (cat-8:soft-delta-infinity)
                            (cat-8:soft-delta-infinity)))
               (cf (cat-8:vrtc-bar f)))
          (cat-8:? cf 6 (cat-8:abar 3 (cat-8:crpr 0 (cat-8::make-delta :cdr 7)
                                                  0 (cat-8::make-delta :cdr 7))
                                    3
                                    (cat-8:crpr 0 (cat-8::make-delta :cdr 56)
                                                0 (cat-8::make-delta :cdr 56)))))))

(test hmtp-vrtc-bar-intr
      (progn
        (cat-8:cat-init)
        (let* ((ez (cat-8:ez (cat-8:delta-infinity) (cat-8:delta-infinity)))
               (h (cat-8:h ez))
               (gf (cat-8:cmps (cat-8:g ez) (cat-8:f ez)))
               (r (cat-8:hmtp-vrtc-bar-intr h gf)))
          (funcall r 3 (cat-8:abar 3 (cat-8:crpr 0 7 0 7)))
          (funcall r 9 (cat-8:abar 3 (cat-8:crpr 0 7 0 7) 3 (cat-8:crpr 0 14 0 14)
                                   3 (cat-8:crpr 0 14 0 14))))))

#|
(test vrtc-bar1
(progn
(cat-8:cat-init)

(let* ((tcc (cat-8:build-chcm
:cmpr #'cat-8:s-cmpr
:basis #'(lambda (degr) '(a b c d))
:bsgn 'd
:intr-dffr #'(lambda (degr gnrt)
(ecase gnrt
(a (cat-8:cmbn (1- degr) 1 'b 1 'd))
((b d) (cat-8:cmbn (1- degr)))
(c (cat-8:cmbn (1- degr) 1 'd))))
:strt :gnrt
:orgn '(tcc)))
(bcc (cat-8:build-chcm
:cmpr #'cat-8:s-cmpr
:basis #'(lambda (degr) '(c d))
:bsgn 'd
:intr-dffr #'(lambda (degr gnrt)
(ecase gnrt
(d (cat-8:cmbn (1- degr)))
(c (cat-8:cmbn (1- degr) 1 'd))))
:strt :gnrt
:orgn '(bcc)))
(f (cat-8:build-mrph :sorc tcc :trgt bcc :degr 0
:intr #'(lambda (degr gnrt)
(ecase gnrt
(a (cat-8:cmbn degr 1 'c 1 'd))
(b (cat-8:cmbn degr))
((c d) (cat-8:cmbn degr 1 gnrt))))
:strt :gnrt :orgn '(f)))
(g (cat-8:build-mrph :sorc bcc :trgt tcc :degr 0
:intr #'identity :strt :cmbn :orgn '(g)))
(h (cat-8:build-mrph :sorc tcc :trgt tcc :degr +1
:intr #'(lambda (degr gnrt)
(ecase gnrt
((a b) (cat-8:cmbn
(1+ degr)
1 'a -1 'b -1 'c -1 'd))
((c d) (cat-8:cmbn (1+ degr)))))
:strt :gnrt :orgn '(h)))
(rdct (cat-8:build-rdct :f f :g g :h h :orgn '(rdct)))
(bar))
(cat-8:tcc rdct 3 'a)
(cat-8:g rdct (cat-8:f rdct 3 'a))
(cat-8:h rdct 3 'a)
(setf bar (cat-8:vrtc-bar rdct))
(cat-8:pre-check-rdct bar)
(aleat-tc)
(aleat-bc)
;;(loop (c))
(dotimes (i 10) (c))))) ;; degrees >= 15 is possible => error.
|#

(test homology
      (progn
        (cat-8:cat-init)
        (let* ((h (cat-8:efhm (cat-8:k-z-1)))
               (b (cat-8:bar h)))
          (cat-8:homology (cat-8:rbcc b) 0 11))))
