;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test allp
      (cat-7:allp )
      (cat-7:allp '(2 a 3 b))
      (cat-7:allp 2 'a 3 'b)
      (signals simple-error (cat-7:allp 2 'a 3)))


(test cobar-cmpr
      (let ((r (cat-7:cobar-cmpr #'cat-7:s-cmpr)))
        (is (equal :equal (funcall r (cat-7:allp) (cat-7:allp))))
        (is (equal :greater (funcall r (cat-7:allp 3 'a) (cat-7:allp))))
        (is (equal :less (funcall r (cat-7:allp 3 'a) (cat-7:allp 2 'a 1 'b))))
        (is (equal :less (funcall r (cat-7:allp 3 'a) (cat-7:allp 3 'b))))
        (is (equal :equal (funcall r (cat-7:allp 3 'a) (cat-7:allp 3 'a))))))


(test cobar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
        (cat-7:cobar-basis-length basis 1 1)
        (cat-7:cobar-basis-length basis 2 1)
        (cat-7:cobar-basis-length basis 2 2)
        (cat-7:cobar-basis-length basis 3 1)
        (cat-7:cobar-basis-length basis 3 2)
        (cat-7:cobar-basis-length basis 3 3)))


(test cobar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
             (r (cat-7:cobar-basis basis)))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (dotimes (i 7)
          (print (funcall r i)))
        (cat-7:cobar-basis :locally-effective)))


(test cobar-intr-vrtc-dffr
      (let* ((d (cat-7:soft-delta-infinity))
             (r (cat-7:cobar-intr-vrtc-dffr (cat-7:dffr d))))
        (funcall r 0 (cat-7:allp))
        (funcall r 3 (cat-7:allp 3 (cat-7:d 15)))
        (funcall r 5 (cat-7:allp 3 (cat-7:d (cat-7:mask 5)) 2 (cat-7:d (cat-7:mask 4))))
        (funcall r 5 (cat-7:allp 2 (cat-7:d (cat-7:mask 4))
                               3 (cat-7:d (cat-7:mask 5))))))


(test vrtc-cobar
      (cat-7:cat-init)
      (let ((v (cat-7:vrtc-cobar (cat-7:soft-delta-infinity))))
        (dotimes (i 10) (print (random-allp 5)))
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (print allp)
            (print (cat-7:? v (apply #'+ (mapcar #'car (cat-7:allp-list allp)))
                          allp))
            (print (cat-7:? v (cat-7:? v (apply #'+ (mapcar #'car
                                                        (cat-7:allp-list allp)))
                                   allp)))))))


(test cobar-intr-hrzn-dffr
      (let* ((d (cat-7:soft-delta-infinity))
             (r (cat-7:cobar-intr-hrzn-dffr (cat-7:cprd d))))
        (funcall r 0 (cat-7:allp))
        (funcall r 3 (cat-7:allp 3 (cat-7:d (cat-7:mask 5))))
        (funcall r 5 (cat-7:allp 3 (cat-7:d (cat-7:mask 5)) 2 (cat-7:d (cat-7:mask 4))))
        (funcall r 5 (cat-7:allp 2 (cat-7:d (cat-7:mask 4))
                               3 (cat-7:d (cat-7:mask 5))))))


#|
(test cobar-hrzn-dffr
      (cat-7:cat-init)
      (let ((h (cat-7:cobar-hrzn-dffr (cat-7:soft-delta-infinity)))
            (allp (random-allp 4)))
        (dotimes (i 10) (print (random-allp 5)))
        (cat-7:? h (apply #'+ (mapcar #'car (cat-7:allp-list allp))) allp)
        (cat-7:? h (cat-7:? h (apply #'+ (mapcar #'car (cat-7:allp-list allp)))
                        allp))
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (print allp)
            (print (cat-7:? h (apply #'+ (mapcar #'car (cat-7:allp-list allp)))
                          allp))
            (print (cat-7:? h (cat-7:? h (apply #'+ (mapcar #'car
                                                        (cat-7:allp-list allp)))
                                   allp)))))))
|#

(defun random-allp1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat-7:mask 9)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-7:cbgn (1- dmns) gmsm) rslt))))
    (cat-7:make-allp :list rslt)))


#|
(test cobar
      (cat-7:cat-init)
      (let ((c (cat-7:cobar (cat-7:deltab)))
            (allp (random-allp1 4)))
        (dotimes (i 10) (print (random-allp1 5)))
        (cat-7:? c (apply #'+ (mapcar #'car (cat-7:allp-list allp))) allp)
        (cat-7:? c (cat-7:? c (apply #'+ (mapcar #'car (cat-7:allp-list allp)))
                        allp))
        (dotimes (i 10)
          (let ((allp (random-allp1 3)))
            (print allp)
            (print (cat-7:? c (apply #'+ (mapcar #'car (cat-7:allp-list allp)))
                          allp))
            (print (cat-7:? c (cat-7:? c (apply #'+ (mapcar #'car
                                                        (cat-7:allp-list allp)))
                                   allp)))))))
|#

(test ncmbn-cobar
      (cat-7:ncmbn-cobar nil)
      (cat-7:ncmbn-cobar (list (cat-7:cmbn 3 2 'a 3 'b)))
      (cat-7:ncmbn-cobar (list (cat-7:cmbn 1 2 'a 3 'b) (cat-7:cmbn 2 4 'c 5 'd)))
      (cat-7:ncmbn-cobar (list (cat-7:cmbn 1 2 'a 3 'b) (cat-7:cmbn 1 4 'c 5 'd)
                             (cat-7:cmbn 1 6 'e 7 'f))))


(test mrph-vrtc-cobar-intr
      (let* ((cc (cat-7:build-chcm :cmpr #'cat-7:f-cmpr
                                 :basis :locally-effective
                                 :intr-dffr #'identity :strt :cmbn))
             (m (cat-7:build-mrph :sorc cc :trgt cc :degr 0 :intr
                                #'(lambda (degr gnrt)
                                    (cat-7:cmbn degr 2 gnrt 3 (1+ gnrt)))
                                :strt :gnrt :orgn '(test)))
             (r (cat-7:mrph-vrtc-cobar-intr m)))
        (funcall r 4 (cat-7:allp 2 3 2 4))))

(test vrtc-cobar
      (cat-7:cat-init)
      (let* ((f (cat-7:aw (cat-7:soft-delta-infinity) (cat-7:soft-delta-infinity)))
             (cf (cat-7:vrtc-cobar f)))
        (cat-7:? cf 2 (cat-7:allp 1 (cat-7:crpr 0 (cat-7:d 7) 0 (cat-7:d 7))
                                  1 (cat-7:crpr 0 (cat-7:d 56) 0 (cat-7:d 56))))))


(test hmtp-vrtc-cobar-intr
      (cat-7:cat-init)
      (let* ((ez (cat-7:ez (cat-7:delta-infinity) (cat-7:delta-infinity)))
             (h (cat-7:h ez))
             (gf (cat-7:cmps (cat-7:g ez) (cat-7:f ez)))
             (r (cat-7:hmtp-vrtc-cobar-intr h gf)))
        (funcall r 3 (cat-7:allp 1 (cat-7:crpr 0 7 0 7)
                               1 (cat-7:crpr 0 14 0 14)
                               1 (cat-7:crpr 0 14 0 14)))))


#|
(test rdct
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
                                            ((a b) (cat-7:cmbn (1+ degr) 1 'a
                                                             -1 'b -1 'c
                                                             -1 'd))
                                            ((c d) (cat-7:cmbn (1+ degr)))))
                                :strt :gnrt :orgn '(h)))
             (rdct (cat-7:build-rdct :f f :g g :h h :orgn '(rdct)))
             cobar)
        (cat-7:tcc rdct 3 'a)
        (cat-7:g rdct (cat-7:f rdct 3 'a))
        (cat-7:h rdct 3 'a)
        (setf cobar (cat-7:vrtc-cobar rdct))
        (cat-7:pre-check-rdct cobar)
        (aleat-tc)
        ;;(aleat-bc)
        ;;(loop (c))
        ;; degrees >= 15 is possible => error.
        ))
|#

#|
(cat-init)
(setf tcc (build-chcm
           :cmpr #'s-cmpr
           :basis #'(lambda (degr) '(a b c d))
           :bsgn 'd
           :intr-dffr #'(lambda (degr gnrt)
                          (ecase gnrt
                            (a (cmbn (1- degr) 1 'b 1 'd))
                            ((b d) (cmbn (1- degr)))
                            (c (cmbn (1- degr) 1 'd))))
           :strt :gnrt
           :orgn '(tcc)))
(setf bcc (build-chcm
           :cmpr #'s-cmpr
           :basis #'(lambda (degr) '(c d))
           :bsgn 'd
           :intr-dffr #'(lambda (degr gnrt)
                          (ecase gnrt
                            (d (cmbn (1- degr)))
                            (c (cmbn (1- degr) 1 'd))))
           :strt :gnrt
           :orgn '(bcc)))
(setf f (build-mrph :sorc tcc :trgt bcc :degr 0
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                (a (cmbn degr 1 'c 1 'd))
                                (b (cmbn degr))
                                ((c d) (cmbn degr 1 gnrt))))
                    :strt :gnrt :orgn '(f)))
(setf g (build-mrph :sorc bcc :trgt tcc :degr 0
                    :intr #'identity :strt :cmbn :orgn '(g)))
(setf h (build-mrph :sorc tcc :trgt tcc :degr +1
                    :intr #'(lambda (degr gnrt)
                              (ecase gnrt
                                ((a b) (cmbn (1+ degr) 1 'a -1 'b -1 'c -1 'd))
                                ((c d) (cmbn (1+ degr)))))
                    :strt :gnrt :orgn '(h)))
(setf rdct (build-rdct :f f :g g :h h :orgn '(rdct)))
(tcc rdct 3 'a)
(g rdct (f rdct 3 'a))
(h rdct 3 'a)
(setf cobar (vrtc-cobar rdct))
(pre-check-rdct cobar)
(defun aleat-tc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (+ 65 (random 4))) 'string))
             (intern (coerce (vector (+ 65 (random 4))) 'string)))
       (rslt nil (cons (cbgn degr gnrt) rslt)))
      ((> tdegr 10) (setf *tc* (cmbn tdegr 1 (make-allp :list rslt))))))
(aleat-tc)
(defun aleat-bc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (+ 67 (random 2))) 'string))
             (intern (coerce (vector (+ 67 (random 2))) 'string)))
       (rslt nil (cons (cbgn degr gnrt) rslt)))
      ((> tdegr 10) (setf *bc* (cmbn tdegr 1 (make-allp :list rslt))))))
(aleat-bc)
(defun c ()
  (aleat-tc)
  (aleat-bc)
  (check-rdct))
(loop (c))  ;; degrees >= 15 is possible => error.
|#


#|
(test left-hmeq
      (cat-7:cat-init)
      (let* ((h (cat-7:left-hmeq (cat-7:sphere 3)))
             (c (cat-7:cobar h)))
        (inspect c)))
|#
