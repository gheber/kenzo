;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test abar
      (cat-7:abar )
      (cat-7:abar '(2 a 3 b))
      (cat-7:abar 2 'a 3 'b)
      (signals simple-error (cat-7:abar 2 'a 3)))

(test bar-cmpr
      (let ((r (cat-7:bar-cmpr #'cat-7:s-cmpr)))
        (is (equal :equal (funcall r (cat-7:abar) (cat-7:abar))))
        (is (equal :greater (funcall r (cat-7:abar 3 'a) (cat-7:abar))))
        (is (equal :less (funcall r (cat-7:abar 3 'a) (cat-7:abar 2 'a 1 'b))))
        (is (equal :less (funcall r (cat-7:abar 3 'a) (cat-7:abar 3 'b))))
        (is (equal :equal (funcall r (cat-7:abar 3 'a) (cat-7:abar 3 'a))))))

(test bar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
        (cat-7:bar-basis-length basis 2 1)
        (cat-7:bar-basis-length basis 2 2)
        (cat-7:bar-basis-length basis 3 1)
        (cat-7:bar-basis-length basis 3 2)
        (cat-7:bar-basis-length basis 4 1)
        (cat-7:bar-basis-length basis 4 2)
        (cat-7:bar-basis-length basis 4 3)
        (cat-7:bar-basis-length basis 4 4)
        (cat-7:bar-basis-length basis 8 1)
        (cat-7:bar-basis-length basis 8 2)
        (cat-7:bar-basis-length basis 8 3)
        (cat-7:bar-basis-length basis 8 4)
        (cat-7:bar-basis-length basis 8 5)
        (cat-7:bar-basis-length basis 8 6)
        (cat-7:bar-basis-length basis 8 11)))

(test bar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
             (r (cat-7:bar-basis basis)))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (dotimes (i 7)
          (print (funcall r i)))
        (cat-7:bar-basis :locally-effective)))

(test bar-intr-vrtc-dffr
      (let* ((d (cat-7:soft-delta-infinity))
             (r (cat-7:bar-intr-vrtc-dffr (cat-7:dffr d))))
        (funcall r 0 (cat-7:abar))
        (funcall r 3 (cat-7:abar 3 (cat-7:d 7)))
        (funcall r 5 (cat-7:abar 3 (cat-7:d 7) 2 (cat-7:d 3)))
        (funcall r 5 (cat-7:abar 2 (cat-7:d 3) 3 (cat-7:d 7)))))

(defun random-abar1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat-7:mask 6)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-7:brgn (1+ dmns) gmsm) rslt))))
    (cat-7:make-abar :list rslt)))

(test vrtc-bar
      (progn
        (cat-7:cat-init)
        (let ((v (cat-7:vrtc-bar (cat-7:delta-infinity))))
          (dotimes (i 10)
            (print (random-abar1 5)))
          (dotimes (i 10)
            (let ((abar (random-abar1 3)))
              (print abar)
              (print (cat-7:? v (apply #'+ (mapcar #'car (cat-7:abar-list abar)))
                              abar))
              (print (cat-7:? v (cat-7:? v (apply #'+ (mapcar #'car
                                                              (cat-7:abar-list abar)))
                                         abar))))))))

(test bar-intr-hrzn-dffr
      (let* ((k (cat-7:k-z-1))
             (r (cat-7:bar-intr-hrzn-dffr (cat-7:aprd k))))
        (funcall r 0 (cat-7:abar ))
        (funcall r 3 (cat-7:abar 3 '(2 3)))
        (funcall r 6 (cat-7:abar 3 '(2 3) 3 '(-2 -3)))
        (funcall r 9 (cat-7:abar 3 '(2 3) 3 '(-2 -3) 3 '(2 3)))
        (funcall r 11 (cat-7:abar 3 '(2 3) 3 '(-2 -3) 2 '(-2) 3 '(-2 -3)))))

(test bar-hrzn-dffr
      (progn
        (cat-7:cat-init)
        (let ((h (cat-7:bar-hrzn-dffr (cat-7:k-z-1)))
              (abar))
          (dotimes (i 10)
            (print (random-abar 10 5)))
          (setf abar (random-abar 10 5))
          (cat-7:? h (apply #'+ (mapcar #'car (cat-7:abar-list abar))) abar)
          (cat-7:? h (cat-7:? h (apply #'+ (mapcar #'car
                                                   (cat-7:abar-list abar))) abar))
          (dotimes (i 10)
            (setf abar (random-abar 10 3))
            (print abar)
            (print (cat-7:? h (apply #'+ (mapcar #'car (cat-7:abar-list abar)))
                            abar))
            (print (cat-7:? h (cat-7:? h (apply #'+ (mapcar #'car
                                                            (cat-7:abar-list abar)))
                                       abar)))))))

(test bar
      (progn
        (cat-7:cat-init)
        (let ((b (cat-7:bar (cat-7:k-z-1)))
              (abar (random-abar 10 3)))
          (cat-7:? b (apply #'+ (mapcar #'car (cat-7:abar-list abar))) abar)
          (cat-7:? b (cat-7:? b (apply #'+ (mapcar #'car (cat-7:abar-list abar)))
                              abar))
          (dotimes (i 10)
            (let ((abar (random-abar 10 3)))
              (print abar)
              (print (cat-7:? b (apply #'+ (mapcar #'car (cat-7:abar-list abar)))
                              abar))
              (print (cat-7:? b
                              (cat-7:? b
                                       (apply #'+ (mapcar #'car
                                                          (cat-7:abar-list abar)))
                                       abar))))))))

(test ncmbn-bar
      (cat-7:ncmbn-bar nil)
      (cat-7:ncmbn-bar (list (cat-7:cmbn 3 2 'a 3 'b)))
      (cat-7:ncmbn-bar (list (cat-7:cmbn 1 2 'a 3 'b) (cat-7:cmbn 2 4 'c 5 'd)))
      (cat-7:ncmbn-bar (list (cat-7:cmbn 1 2 'a 3 'b) (cat-7:cmbn 1 4 'c 5 'd)
                             (cat-7:cmbn 1 6 'e 7 'f))))

(test mrph-vrtc-bar-intr
      (let* ((cc (cat-7:build-chcm :cmpr #'cat-7:f-cmpr
                                   :basis :locally-effective
                                   :intr-dffr #'cat-7:zero-mrph
                                   :strt :cmbn))
             (m (cat-7:build-mrph :sorc cc :trgt cc :degr 0 :intr
                                  #'(lambda (degr gnrt)
                                      (cat-7:cmbn degr 2 gnrt 3 (1+ gnrt)))
                                  :strt :gnrt :orgn '(test)))
             (r (cat-7:mrph-vrtc-bar-intr m)))
        (funcall r 4 (cat-7:abar 2 3 2 4))))

(test vrtc-bar
      (progn
        (cat-7:cat-init)
        (let* ((f (cat-7:aw (cat-7:soft-delta-infinity)
                            (cat-7:soft-delta-infinity)))
               (cf (cat-7:vrtc-bar f)))
          (cat-7:? cf 6 (cat-7:abar 3 (cat-7:crpr 0 (cat-7:d 7) 0 (cat-7:d 7)) 3
                                    (cat-7:crpr 0 (cat-7:d 56) 0 (cat-7:d 56)))))))

(test hmtp-vrtc-bar-intr
      (progn
        (cat-7:cat-init)
        (let* ((ez (cat-7:ez (cat-7:delta-infinity) (cat-7:delta-infinity)))
               (h (cat-7:h ez))
               (gf (cat-7:cmps (cat-7:g ez) (cat-7:f ez)))
               (r (cat-7:hmtp-vrtc-bar-intr h gf)))
          (funcall r 3 (cat-7:abar 3 (cat-7:crpr 0 7 0 7)))
          (funcall r 9 (cat-7:abar 3 (cat-7:crpr 0 7 0 7) 3 (cat-7:crpr 0 14 0 14)
                                   3 (cat-7:crpr 0 14 0 14))))))

#|
(test vrtc-bar1
(progn
(cat-7:cat-init)

(let* ((tcc (cat-7:build-chcm
:cmpr #'cat-7:s-cmpr
:basis #'(lambda (degr) '(a b c d))
:bsgn 'd
:intr-dffr #'(lambda (degr gnrt)
(ecase gnrt
(a (cat-7:cmbn (1- degr) 1 'b 1 'd))
((b d) (cat-7:cmbn (1- degr)))
(c (cat-7:cmbn (1- degr) 1 'd))))
:strt :gnrt
:orgn '(tcc)))
(bcc (cat-7:build-chcm
:cmpr #'cat-7:s-cmpr
:basis #'(lambda (degr) '(c d))
:bsgn 'd
:intr-dffr #'(lambda (degr gnrt)
(ecase gnrt
(d (cat-7:cmbn (1- degr)))
(c (cat-7:cmbn (1- degr) 1 'd))))
:strt :gnrt
:orgn '(bcc)))
(f (cat-7:build-mrph :sorc tcc :trgt bcc :degr 0
:intr #'(lambda (degr gnrt)
(ecase gnrt
(a (cat-7:cmbn degr 1 'c 1 'd))
(b (cat-7:cmbn degr))
((c d) (cat-7:cmbn degr 1 gnrt))))
:strt :gnrt :orgn '(f)))
(g (cat-7:build-mrph :sorc bcc :trgt tcc :degr 0
:intr #'identity :strt :cmbn :orgn '(g)))
(h (cat-7:build-mrph :sorc tcc :trgt tcc :degr +1
:intr #'(lambda (degr gnrt)
(ecase gnrt
((a b) (cat-7:cmbn
(1+ degr)
1 'a -1 'b -1 'c -1 'd))
((c d) (cat-7:cmbn (1+ degr)))))
:strt :gnrt :orgn '(h)))
(rdct (cat-7:build-rdct :f f :g g :h h :orgn '(rdct)))
(bar))
(cat-7:tcc rdct 3 'a)
(cat-7:g rdct (cat-7:f rdct 3 'a))
(cat-7:h rdct 3 'a)
(setf bar (cat-7:vrtc-bar rdct))
(cat-7:pre-check-rdct bar)
(aleat-tc)
(aleat-bc)
;;(loop (c))
(dotimes (i 10) (c))))) ;; degrees >= 15 is possible => error.
|#

(test homology
      (progn
        (cat-7:cat-init)
        (let* ((h (cat-7:efhm (cat-7:k-z-1)))
               (b (cat-7:bar h)))
          (cat-7:homology (cat-7:rbcc b) 0 11))))
