;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test absm-loopabsm
      (let ((cmpr (cat-7:cmpr (cat-7:deltab))))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 4 7) (cat-7:absm 7 cat-7:+null-loop+))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 0 15) (cat-7:absm 7 cat-7:+null-loop+))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 7 4)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 7 4 1 12 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 7 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 7 -1 1 12 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 7 4)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 0 (cat-7:loop3 2 7 4 1 14 -1)))
        ;; in principle illegal but works
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 0 (cat-7:loop3 2 7 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 0 (cat-7:loop3 2 7 -1 1 14 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 0 (cat-7:loop3 0 15 -1 1 14 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 0 15)
                           (cat-7:absm 2 (cat-7:loop3 0 7 -1 0 14 -1)))
        (cat-7:absm-loopabsm cmpr 3 (cat-7:absm 2 7)
                           (cat-7:absm 2 (cat-7:loop3 0 28 -1 0 14 -1)))))


(test twisted-crts-prdc
      (cat-7:cat-init)
      (let* ((p (cat-7:r-proj-space 3))
             (tw (cat-7:twisted-crts-prdc p))
             (cmpr (cat-7:cmpr tw))
             (c (random-cmbn cmpr 8 10 4 4 100)))
        (time (cat-7:? tw (cat-7:? tw c)))
        (time (cat-7:? tw (cat-7:? tw c)))))


(test dtau-d-intr
      (cat-7:cat-init)
      (let* ((d (cat-7:deltab))
             (p (cat-7:crts-prdc d (cat-7:loop-space d)))
             (tw (cat-7:twisted-crts-prdc d))
             (crts-prdc-cmpr (cat-7:cmpr p))
             (crts-prdc-face (cat-7:face p))
             (twisted-crts-prdc-face (cat-7:face tw))
             (delta (cat-7:dtau-d-intr crts-prdc-cmpr crts-prdc-face
                                     twisted-crts-prdc-face)))
        (funcall delta 3 (cat-7:crpr 0 15 0 (cat-7:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-7:crpr 10 15 5 (cat-7:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-7:crpr 5 15 10 (cat-7:loop3 0 (- 2048 64) 1)))))


(test szczarba
      (cat-7:cat-init)
      (let ((rdct (cat-7:szczarba (cat-7:deltab)))
            bcc)
        (cat-7:pre-check-rdct rdct)
        (setf cat-7:*tc* (cat-7:cmbn 2 1 (cat-7:crpr 0 7 0
                                               (cat-7:loop3 0 (- 512 32) 1)))
              cat-7:*bc* (cat-7:cmbn 2 1 (cat-7:tnpr 0 1 2
                                               (cat-7:loop3 0 (- 512 32) 1))
                                 10 (cat-7:tnpr 1 (- 4096 1024) 1
                                              (cat-7:loop3 0 7 1))
                                 100 (cat-7:tnpr 2 7 0
                                               (cat-7:loop3 0 (- 4096 1024) 1))))
        (check-rdct)
        (setf bcc (cat-7:bcc rdct))
        (time (cat-7:? bcc 5 (cat-7:tnpr 5 (cat-7:mask 6) 0 cat-7:+null-loop+)))
        (time (cat-7:? bcc 6 (cat-7:tnpr 6 (cat-7:mask 7) 0 cat-7:+null-loop+)))))


(test pop-first-absm
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list cat-7:+null-loop+)))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a -2))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a +2))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a -1))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a +1))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 1 'a -2 2 'b 3))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a +2 2 'b -3))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 1 'a -1 2 'b 3))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 0 'a +1 2 'b 5))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 2 'a -1 1 'b 3))))
      (cat-7:pop-first-absm 4 (cdr (cat-7:loop-list (cat-7:loop3 2 'a +1 1 'b 3)))))


(test crts-contraction-intr
      (cat-7:cat-init)
      (let* ((delta (cat-7:deltab))
             (tw (cat-7:twisted-crts-prdc delta))
             (d (cat-7:bndr tw))
             (base-cmpr (cat-7:cmpr delta))
             (base-bspn (cat-7:bspn delta))
             (base-face (cat-7:face delta))
             (crts-cmpr (cat-7:cmpr tw))
             (r (cat-7:crts-contraction-intr base-cmpr base-bspn base-face
                                           crts-cmpr))
             (h (cat-7:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(crts-contraction ,delta)))
             (z (cat-7:i-sbtr (cat-7:idnt-mrph tw) (cat-7:cmps d h) (cat-7:cmps h d))))
        (cat-7:? z 0 (cat-7:crpr 0 1 0 cat-7:+null-loop+))
        (funcall r 0 (cat-7:crpr 0 1 0 (cat-7:loop3 0 96 1)))
        (cat-7:? z 0 (cat-7:crpr 0 1 0 (cat-7:loop3 0 96 1)))
        (cat-7:? z 0 (cat-7:crpr 0 1 0 (cat-7:loop3 0 96 1 0 (+ 256 128) 1)))
        (cat-7:? z 0 (cat-7:crpr 0 1 0 (cat-7:loop3 0 96 1 0 (+ 256 128)
                                              1 0 (+ 512 1024) 1)))
        (cat-7:? z 0 (cat-7:crpr 0 1 0 (cat-7:loop3 0 96 2)))
        (cat-7:? z 1 (cat-7:crpr 0 3 0 (cat-7:loop3 0 (+ 32 64 128) 1)))
        (cat-7:? z 2 (cat-7:crpr 0 7 0 (cat-7:loop3 0 (+ 32 64 128 256) 1)))
        (cat-7:? z 3 (cat-7:crpr 2 7 1 (cat-7:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-7:? z 3 (cat-7:crpr 2 7 4 (cat-7:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-7:? z 3 (cat-7:crpr 2 7 1 (cat-7:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-7:? z 3 (cat-7:crpr 2 7 4 (cat-7:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))
        (cat-7:? z 3 (cat-7:crpr 1 7 2 (cat-7:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-7:? z 3 (cat-7:crpr 4 7 2 (cat-7:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-7:? z 3 (cat-7:crpr 1 7 2 (cat-7:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-7:? z 3 (cat-7:crpr 4 7 2 (cat-7:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test crts-contraction
      (cat-7:cat-init)
      (let* ((p (cat-7:r-proj-space 3))
             (tw (cat-7:twisted-crts-prdc p))
             (cmpr (cat-7:cmpr tw))
             (r (cat-7:crts-contraction-intr (cat-7:cmpr p) (cat-7:bspn p)
                                           (cat-7:face p) (cat-7:cmpr tw)))
             (h (cat-7:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(cat-7:crts-contraction ,p)))
             (d (cat-7:bndr tw))
             (z (cat-7:i-sbtr (cat-7:idnt-mrph tw) (cat-7:cmps h d) (cat-7:cmps d h)))
             (c (random-cmbn cmpr 5 10 2 2 1)))
        (cat-7:? z c)
        (setf c (random-cmbn cmpr 6 10 3 3 3))
        (cat-7:? z c)
        (setf c (random-cmbn cmpr 7 10 4 4 6))
        (cat-7:? z c)
        (setf c (random-cmbn cmpr 8 10 4 4 20))
        (cat-7:? z c)
        (cat-7:? h c)
        (cat-7:? d (cat-7:? h c))
        (print (length (cat-7:cmbn-list (cat-7:? d (cat-7:? h c)))))))


(test crts-contraction1
      (let* ((h (cat-7:crts-contraction (cat-7:deltab)))
             (d (cat-7:bndr (cat-7:twisted-crts-prdc (cat-7:deltab))))
             (z (cat-7:i-sbtr (cat-7:idnt-mrph (cat-7:sorc d)) (cat-7:cmps d h)
                            (cat-7:cmps h d))))
        (cat-7:? z 3 (cat-7:crpr 4 7 2 (cat-7:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test tnpr-contraction
      (cat-7:cat-init)
      (let* ((delta (cat-7:deltab))
             (sz (cat-7:szczarba (cat-7:deltab)))
             (tw (cat-7:bcc sz))
             (h (cat-7:tnpr-contraction delta))
             (z (cat-7:i-sbtr (cat-7:idnt-mrph tw) (cat-7:cmps tw h)
                            (cat-7:cmps h tw))))
        (time (cat-7:? z 3 (cat-7:tnpr 0 1 3 (cat-7:loop3 0 (cat-7:mask 5)
                                                    2 0 (* 32 (cat-7:mask 5))
                                                    -1))))
        (cat-7:? h 4 (cat-7:tnpr 0 1 0 (cat-7:loop3 0 (cat-7:mask 6) 1)))))
