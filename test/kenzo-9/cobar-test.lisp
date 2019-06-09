;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test allp
      (cat-9:allp )
      (cat-9:allp '(2 a 3 b))
      (cat-9:allp 2 'a 3 'b)
      (signals simple-error (cat-9:allp 2 'a 3)))


(test cobar-cmpr
      (let ((r (cat-9:cobar-cmpr #'cat-9:s-cmpr)))
        (is (equal :equal (funcall r (cat-9:allp) (cat-9:allp))))
        (is (equal :greater (funcall r (cat-9:allp 3 'a) (cat-9:allp))))
        (is (equal :less (funcall r (cat-9:allp 3 'a) (cat-9:allp 2 'a 1 'b))))
        (is (equal :less (funcall r (cat-9:allp 3 'a) (cat-9:allp 3 'b))))
        (is (equal :equal (funcall r (cat-9:allp 3 'a) (cat-9:allp 3 'a))))))


(test cobar-basis-length
      (let ((basis #'(lambda (degr) (list degr))))
        (cat-9:cobar-basis-length basis 1 1)
        (cat-9:cobar-basis-length basis 2 1)
        (cat-9:cobar-basis-length basis 2 2)
        (cat-9:cobar-basis-length basis 3 1)
        (cat-9:cobar-basis-length basis 3 2)
        (cat-9:cobar-basis-length basis 3 3)))


(test cobar-basis
      (let* ((basis #'(lambda (degr) (list degr)))
             (r (cat-9:cobar-basis basis)))
        (funcall r 0)
        (funcall r 1)
        (funcall r 2)
        (dotimes (i 7)
          (print (funcall r i)))
        (cat-9:cobar-basis :locally-effective)))


(test cobar-intr-vrtc-dffr
      (let* ((d (cat-9:soft-delta-infinity))
             (r (cat-9:cobar-intr-vrtc-dffr (cat-9:dffr d))))
        (funcall r 0 (cat-9:allp))
        (funcall r 3 (cat-9:allp 3 (cat-9:d 15)))
        (funcall r 5 (cat-9:allp 3 (cat-9:d (cat-9:mask 5)) 2 (cat-9:d (cat-9:mask 4))))
        (funcall r 5 (cat-9:allp 2 (cat-9:d (cat-9:mask 4))
                               3 (cat-9:d (cat-9:mask 5))))))


(test vrtc-cobar
      (cat-9:cat-init)
      (let ((v (cat-9:vrtc-cobar (cat-9:soft-delta-infinity))))
        (dotimes (i 10) (print (random-allp 5)))
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (print allp)
            (print (cat-9:? v (apply #'+ (mapcar #'car (cat-9:allp-list allp)))
                          allp))
            (print (cat-9:? v (cat-9:? v (apply #'+ (mapcar #'car
                                                        (cat-9:allp-list allp)))
                                   allp)))))))


(test cobar-intr-hrzn-dffr
      (let* ((d (cat-9:soft-delta-infinity))
             (r (cat-9:cobar-intr-hrzn-dffr (cat-9:cprd d))))
        (funcall r 0 (cat-9:allp))
        (funcall r 3 (cat-9:allp 3 (cat-9:d (cat-9:mask 5))))
        (funcall r 5 (cat-9:allp 3 (cat-9:d (cat-9:mask 5)) 2 (cat-9:d (cat-9:mask 4))))
        (funcall r 5 (cat-9:allp 2 (cat-9:d (cat-9:mask 4))
                               3 (cat-9:d (cat-9:mask 5))))))


#|
(test cobar-hrzn-dffr
      (cat-9:cat-init)
      (let ((h (cat-9:cobar-hrzn-dffr (cat-9:soft-delta-infinity)))
            (allp (random-allp 4)))
        (dotimes (i 10) (print (random-allp 5)))
        (cat-9:? h (apply #'+ (mapcar #'car (cat-9:allp-list allp))) allp)
        (cat-9:? h (cat-9:? h (apply #'+ (mapcar #'car (cat-9:allp-list allp)))
                        allp))
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (print allp)
            (print (cat-9:? h (apply #'+ (mapcar #'car (cat-9:allp-list allp)))
                          allp))
            (print (cat-9:? h (cat-9:? h (apply #'+ (mapcar #'car
                                                        (cat-9:allp-list allp)))
                                   allp)))))))
|#

(defun random-allp1 (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat-9:mask 9)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-9:cbgn (1- dmns) gmsm) rslt))))
    (cat-9:make-allp :list rslt)))


#|
(test cobar
      (cat-9:cat-init)
      (let ((c (cat-9:cobar (cat-9:deltab)))
            (allp (random-allp1 4)))
        (dotimes (i 10) (print (random-allp1 5)))
        (cat-9:? c (apply #'+ (mapcar #'car (cat-9:allp-list allp))) allp)
        (cat-9:? c (cat-9:? c (apply #'+ (mapcar #'car (cat-9:allp-list allp)))
                        allp))
        (dotimes (i 10)
          (let ((allp (random-allp1 3)))
            (print allp)
            (print (cat-9:? c (apply #'+ (mapcar #'car (cat-9:allp-list allp)))
                          allp))
            (print (cat-9:? c (cat-9:? c (apply #'+ (mapcar #'car
                                                        (cat-9:allp-list allp)))
                                   allp)))))))
|#

(test ncmbn-cobar
      (cat-9:ncmbn-cobar nil)
      (cat-9:ncmbn-cobar (list (cat-9:cmbn 3 2 'a 3 'b)))
      (cat-9:ncmbn-cobar (list (cat-9:cmbn 1 2 'a 3 'b) (cat-9:cmbn 2 4 'c 5 'd)))
      (cat-9:ncmbn-cobar (list (cat-9:cmbn 1 2 'a 3 'b) (cat-9:cmbn 1 4 'c 5 'd)
                             (cat-9:cmbn 1 6 'e 7 'f))))


(test mrph-vrtc-cobar-intr
      (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                 :basis :locally-effective
                                 :intr-dffr #'identity :strt :cmbn))
             (m (cat-9:build-mrph :sorc cc :trgt cc :degr 0 :intr
                                #'(lambda (degr gnrt)
                                    (cat-9:cmbn degr 2 gnrt 3 (1+ gnrt)))
                                :strt :gnrt :orgn '(test)))
             (r (cat-9:mrph-vrtc-cobar-intr m)))
        (funcall r 4 (cat-9:allp 2 3 2 4))))

(test vrtc-cobar
      (cat-9:cat-init)
      (let* ((f (cat-9:aw (cat-9:soft-delta-infinity) (cat-9:soft-delta-infinity)))
             (cf (cat-9:vrtc-cobar f)))
        (cat-9:? cf 2 (cat-9:allp 1 (cat-9:crpr 0 (cat-9::make-delta :cdr 7)
                                                0 (cat-9::make-delta :cdr 7))
                                  1 (cat-9:crpr 0 (cat-9::make-delta :cdr 56)
                                                0 (cat-9::make-delta :cdr 56))))))

(test hmtp-vrtc-cobar-intr
      (cat-9:cat-init)
      (let* ((ez (cat-9:ez (cat-9:delta-infinity) (cat-9:delta-infinity)))
             (h (cat-9:h ez))
             (gf (cat-9:cmps (cat-9:g ez) (cat-9:f ez)))
             (r (cat-9:hmtp-vrtc-cobar-intr h gf)))
        (funcall r 3 (cat-9:allp 1 (cat-9:crpr 0 7 0 7)
                               1 (cat-9:crpr 0 14 0 14)
                               1 (cat-9:crpr 0 14 0 14)))))


#|
(test rdct
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
                                            ((a b) (cat-9:cmbn (1+ degr) 1 'a
                                                             -1 'b -1 'c
                                                             -1 'd))
                                            ((c d) (cat-9:cmbn (1+ degr)))))
                                :strt :gnrt :orgn '(h)))
             (rdct (cat-9:build-rdct :f f :g g :h h :orgn '(rdct)))
             cobar)
        (cat-9:tcc rdct 3 'a)
        (cat-9:g rdct (cat-9:f rdct 3 'a))
        (cat-9:h rdct 3 'a)
        (setf cobar (cat-9:vrtc-cobar rdct))
        (cat-9:pre-check-rdct cobar)
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
      (cat-9:cat-init)
      (let* ((h (cat-9:left-hmeq (cat-9:sphere 3)))
             (c (cat-9:cobar h)))
        (inspect c)))
|#
