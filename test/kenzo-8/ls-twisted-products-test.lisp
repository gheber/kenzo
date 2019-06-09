;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test absm-loopabsm
      (let ((cmpr (cat-8:cmpr (cat-8:deltab))))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 4 7) (cat-8:absm 7 cat-8:+null-loop+))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 0 15) (cat-8:absm 7 cat-8:+null-loop+))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 7 4)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 7 4 1 12 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 7 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 7 -1 1 12 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 7 4)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 0 (cat-8:loop3 2 7 4 1 14 -1)))
        ;; in principle illegal but works
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 0 (cat-8:loop3 2 7 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 0 (cat-8:loop3 2 7 -1 1 14 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 0 (cat-8:loop3 0 15 -1 1 14 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 0 15)
                           (cat-8:absm 2 (cat-8:loop3 0 7 -1 0 14 -1)))
        (cat-8:absm-loopabsm cmpr 3 (cat-8:absm 2 7)
                           (cat-8:absm 2 (cat-8:loop3 0 28 -1 0 14 -1)))))


(test twisted-crts-prdc
      (cat-8:cat-init)
      (let* ((p (cat-8:r-proj-space 3))
             (tw (cat-8:twisted-crts-prdc p))
             (cmpr (cat-8:cmpr tw))
             (c (random-cmbn cmpr 8 10 4 4 100)))
        (time (cat-8:? tw (cat-8:? tw c)))
        (time (cat-8:? tw (cat-8:? tw c)))))


(test dtau-d-intr
      (cat-8:cat-init)
      (let* ((d (cat-8:deltab))
             (p (cat-8:crts-prdc d (cat-8:loop-space d)))
             (tw (cat-8:twisted-crts-prdc d))
             (crts-prdc-cmpr (cat-8:cmpr p))
             (crts-prdc-face (cat-8:face p))
             (twisted-crts-prdc-face (cat-8:face tw))
             (delta (cat-8:dtau-d-intr crts-prdc-cmpr crts-prdc-face
                                     twisted-crts-prdc-face)))
        (funcall delta 3 (cat-8:crpr 0 15 0 (cat-8:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-8:crpr 10 15 5 (cat-8:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-8:crpr 5 15 10 (cat-8:loop3 0 (- 2048 64) 1)))))


(test szczarba
      (cat-8:cat-init)
      (let ((rdct (cat-8:szczarba (cat-8:deltab)))
            bcc)
        (cat-8:pre-check-rdct rdct)
        (setf cat-8:*tc* (cat-8:cmbn 2 1 (cat-8:crpr 0 7 0
                                               (cat-8:loop3 0 (- 512 32) 1)))
              cat-8:*bc* (cat-8:cmbn 2 1 (cat-8:tnpr 0 1 2
                                               (cat-8:loop3 0 (- 512 32) 1))
                                 10 (cat-8:tnpr 1 (- 4096 1024) 1
                                              (cat-8:loop3 0 7 1))
                                 100 (cat-8:tnpr 2 7 0
                                               (cat-8:loop3 0 (- 4096 1024) 1))))
        (check-rdct)
        (setf bcc (cat-8:bcc rdct))
        (time (cat-8:? bcc 5 (cat-8:tnpr 5 (cat-8:mask 6) 0 cat-8:+null-loop+)))
        (time (cat-8:? bcc 6 (cat-8:tnpr 6 (cat-8:mask 7) 0 cat-8:+null-loop+)))))


(test pop-first-absm
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list cat-8:+null-loop+)))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a -2))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a +2))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a -1))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a +1))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 1 'a -2 2 'b 3))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a +2 2 'b -3))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 1 'a -1 2 'b 3))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 0 'a +1 2 'b 5))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 2 'a -1 1 'b 3))))
      (cat-8:pop-first-absm 4 (cdr (cat-8:loop-list (cat-8:loop3 2 'a +1 1 'b 3)))))


(test crts-contraction-intr
      (cat-8:cat-init)
      (let* ((delta (cat-8:deltab))
             (tw (cat-8:twisted-crts-prdc delta))
             (d (cat-8:bndr tw))
             (base-cmpr (cat-8:cmpr delta))
             (base-bspn (cat-8:bspn delta))
             (base-face (cat-8:face delta))
             (crts-cmpr (cat-8:cmpr tw))
             (r (cat-8:crts-contraction-intr base-cmpr base-bspn base-face
                                           crts-cmpr))
             (h (cat-8:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(crts-contraction ,delta)))
             (z (cat-8:i-sbtr (cat-8:idnt-mrph tw) (cat-8:cmps d h) (cat-8:cmps h d))))
        (cat-8:? z 0 (cat-8:crpr 0 1 0 cat-8:+null-loop+))
        (funcall r 0 (cat-8:crpr 0 1 0 (cat-8:loop3 0 96 1)))
        (cat-8:? z 0 (cat-8:crpr 0 1 0 (cat-8:loop3 0 96 1)))
        (cat-8:? z 0 (cat-8:crpr 0 1 0 (cat-8:loop3 0 96 1 0 (+ 256 128) 1)))
        (cat-8:? z 0 (cat-8:crpr 0 1 0 (cat-8:loop3 0 96 1 0 (+ 256 128)
                                              1 0 (+ 512 1024) 1)))
        (cat-8:? z 0 (cat-8:crpr 0 1 0 (cat-8:loop3 0 96 2)))
        (cat-8:? z 1 (cat-8:crpr 0 3 0 (cat-8:loop3 0 (+ 32 64 128) 1)))
        (cat-8:? z 2 (cat-8:crpr 0 7 0 (cat-8:loop3 0 (+ 32 64 128 256) 1)))
        (cat-8:? z 3 (cat-8:crpr 2 7 1 (cat-8:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-8:? z 3 (cat-8:crpr 2 7 4 (cat-8:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-8:? z 3 (cat-8:crpr 2 7 1 (cat-8:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-8:? z 3 (cat-8:crpr 2 7 4 (cat-8:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))
        (cat-8:? z 3 (cat-8:crpr 1 7 2 (cat-8:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-8:? z 3 (cat-8:crpr 4 7 2 (cat-8:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-8:? z 3 (cat-8:crpr 1 7 2 (cat-8:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-8:? z 3 (cat-8:crpr 4 7 2 (cat-8:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test crts-contraction
      (cat-8:cat-init)
      (let* ((p (cat-8:r-proj-space 3))
             (tw (cat-8:twisted-crts-prdc p))
             (cmpr (cat-8:cmpr tw))
             (r (cat-8:crts-contraction-intr (cat-8:cmpr p) (cat-8:bspn p)
                                           (cat-8:face p) (cat-8:cmpr tw)))
             (h (cat-8:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(cat-8:crts-contraction ,p)))
             (d (cat-8:bndr tw))
             (z (cat-8:i-sbtr (cat-8:idnt-mrph tw) (cat-8:cmps h d) (cat-8:cmps d h)))
             (c (random-cmbn cmpr 5 10 2 2 1)))
        (cat-8:? z c)
        (setf c (random-cmbn cmpr 6 10 3 3 3))
        (cat-8:? z c)
        (setf c (random-cmbn cmpr 7 10 4 4 6))
        (cat-8:? z c)
        (setf c (random-cmbn cmpr 8 10 4 4 20))
        (cat-8:? z c)
        (cat-8:? h c)
        (cat-8:? d (cat-8:? h c))
        (print (length (cat-8:cmbn-list (cat-8:? d (cat-8:? h c)))))))


(test crts-contraction1
      (let* ((h (cat-8:crts-contraction (cat-8:deltab)))
             (d (cat-8:bndr (cat-8:twisted-crts-prdc (cat-8:deltab))))
             (z (cat-8:i-sbtr (cat-8:idnt-mrph (cat-8:sorc d)) (cat-8:cmps d h)
                            (cat-8:cmps h d))))
        (cat-8:? z 3 (cat-8:crpr 4 7 2 (cat-8:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test tnpr-contraction
      (cat-8:cat-init)
      (let* ((delta (cat-8:deltab))
             (sz (cat-8:szczarba (cat-8:deltab)))
             (tw (cat-8:bcc sz))
             (h (cat-8:tnpr-contraction delta))
             (z (cat-8:i-sbtr (cat-8:idnt-mrph tw) (cat-8:cmps tw h)
                            (cat-8:cmps h tw))))
        (time (cat-8:? z 3 (cat-8:tnpr 0 1 3 (cat-8:loop3 0 (cat-8:mask 5)
                                                    2 0 (* 32 (cat-8:mask 5))
                                                    -1))))
        (cat-8:? h 4 (cat-8:tnpr 0 1 0 (cat-8:loop3 0 (cat-8:mask 6) 1)))))
