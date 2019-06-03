;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)


(test absm-loopabsm
      (let ((cmpr (cat-9:cmpr (cat-9:deltab))))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 4 7) (cat-9:absm 7 cat-9:+null-loop+))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 0 15) (cat-9:absm 7 cat-9:+null-loop+))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 7 4)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 7 4 1 12 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 7 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 7 -1 1 12 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 7 4)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 0 (cat-9:loop3 2 7 4 1 14 -1)))
        ;; in principle illegal but works
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 0 (cat-9:loop3 2 7 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 0 (cat-9:loop3 2 7 -1 1 14 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 0 (cat-9:loop3 0 15 -1 1 14 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 0 15)
                           (cat-9:absm 2 (cat-9:loop3 0 7 -1 0 14 -1)))
        (cat-9:absm-loopabsm cmpr 3 (cat-9:absm 2 7)
                           (cat-9:absm 2 (cat-9:loop3 0 28 -1 0 14 -1)))))


(test twisted-crts-prdc
      (cat-9:cat-9-init)
      (let* ((p (cat-9:r-proj-space 3))
             (tw (cat-9:twisted-crts-prdc p))
             (cmpr (cat-9:cmpr tw))
             (c (random-cmbn cmpr 8 10 4 4 100)))
        (time (cat-9:? tw (cat-9:? tw c)))
        (time (cat-9:? tw (cat-9:? tw c)))))


(test dtau-d-intr
      (cat-9:cat-9-init)
      (let* ((d (cat-9:deltab))
             (p (cat-9:crts-prdc d (cat-9:loop-space d)))
             (tw (cat-9:twisted-crts-prdc d))
             (crts-prdc-cmpr (cat-9:cmpr p))
             (crts-prdc-face (cat-9:face p))
             (twisted-crts-prdc-face (cat-9:face tw))
             (delta (cat-9:dtau-d-intr crts-prdc-cmpr crts-prdc-face
                                     twisted-crts-prdc-face)))
        (funcall delta 3 (cat-9:crpr 0 15 0 (cat-9:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-9:crpr 10 15 5 (cat-9:loop3 0 (- 2048 64) 1)))
        (funcall delta 5 (cat-9:crpr 5 15 10 (cat-9:loop3 0 (- 2048 64) 1)))))


(test szczarba
      (cat-9:cat-9-init)
      (let ((rdct (cat-9:szczarba (cat-9:deltab)))
            bcc)
        (cat-9:pre-check-rdct rdct)
        (setf cat-9:*tc* (cat-9:cmbn 2 1 (cat-9:crpr 0 7 0
                                               (cat-9:loop3 0 (- 512 32) 1)))
              cat-9:*bc* (cat-9:cmbn 2 1 (cat-9:tnpr 0 1 2
                                               (cat-9:loop3 0 (- 512 32) 1))
                                 10 (cat-9:tnpr 1 (- 4096 1024) 1
                                              (cat-9:loop3 0 7 1))
                                 100 (cat-9:tnpr 2 7 0
                                               (cat-9:loop3 0 (- 4096 1024) 1))))
        (check-rdct)
        (setf bcc (cat-9:bcc rdct))
        (time (cat-9:? bcc 5 (cat-9:tnpr 5 (cat-9:mask 6) 0 cat-9:+null-loop+)))
        (time (cat-9:? bcc 6 (cat-9:tnpr 6 (cat-9:mask 7) 0 cat-9:+null-loop+)))))


(test pop-first-absm
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list cat-9:+null-loop+)))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a -2))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a +2))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a -1))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a +1))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 1 'a -2 2 'b 3))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a +2 2 'b -3))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 1 'a -1 2 'b 3))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 0 'a +1 2 'b 5))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 2 'a -1 1 'b 3))))
      (cat-9:pop-first-absm 4 (cdr (cat-9:loop-list (cat-9:loop3 2 'a +1 1 'b 3)))))


(test crts-contraction-intr
      (cat-9:cat-9-init)
      (let* ((delta (cat-9:deltab))
             (tw (cat-9:twisted-crts-prdc delta))
             (d (cat-9:bndr tw))
             (base-cmpr (cat-9:cmpr delta))
             (base-bspn (cat-9:bspn delta))
             (base-face (cat-9:face delta))
             (crts-cmpr (cat-9:cmpr tw))
             (r (cat-9:crts-contraction-intr base-cmpr base-bspn base-face
                                           crts-cmpr))
             (h (cat-9:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(crts-contraction ,delta)))
             (z (cat-9:i-sbtr (cat-9:idnt-mrph tw) (cat-9:cmps d h) (cat-9:cmps h d))))
        (cat-9:? z 0 (cat-9:crpr 0 1 0 cat-9:+null-loop+))
        (funcall r 0 (cat-9:crpr 0 1 0 (cat-9:loop3 0 96 1)))
        (cat-9:? z 0 (cat-9:crpr 0 1 0 (cat-9:loop3 0 96 1)))
        (cat-9:? z 0 (cat-9:crpr 0 1 0 (cat-9:loop3 0 96 1 0 (+ 256 128) 1)))
        (cat-9:? z 0 (cat-9:crpr 0 1 0 (cat-9:loop3 0 96 1 0 (+ 256 128)
                                              1 0 (+ 512 1024) 1)))
        (cat-9:? z 0 (cat-9:crpr 0 1 0 (cat-9:loop3 0 96 2)))
        (cat-9:? z 1 (cat-9:crpr 0 3 0 (cat-9:loop3 0 (+ 32 64 128) 1)))
        (cat-9:? z 2 (cat-9:crpr 0 7 0 (cat-9:loop3 0 (+ 32 64 128 256) 1)))
        (cat-9:? z 3 (cat-9:crpr 2 7 1 (cat-9:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-9:? z 3 (cat-9:crpr 2 7 4 (cat-9:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-9:? z 3 (cat-9:crpr 2 7 1 (cat-9:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-9:? z 3 (cat-9:crpr 2 7 4 (cat-9:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))
        (cat-9:? z 3 (cat-9:crpr 1 7 2 (cat-9:loop3 2 (+ 32 64 128)
                                              2 4 (+ 32 64 128) -2)))
        (cat-9:? z 3 (cat-9:crpr 4 7 2 (cat-9:loop3 2 (+ 32 64 128)
                                              2 1 (+ 32 64 128) -2)))
        (cat-9:? z 3 (cat-9:crpr 1 7 2 (cat-9:loop3 2 (+ 32 64 128)
                                              -2 4 (+ 32 64 128) 2)))
        (cat-9:? z 3 (cat-9:crpr 4 7 2 (cat-9:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test crts-contraction
      (cat-9:cat-9-init)
      (let* ((p (cat-9:r-proj-space 3))
             (tw (cat-9:twisted-crts-prdc p))
             (cmpr (cat-9:cmpr tw))
             (r (cat-9:crts-contraction-intr (cat-9:cmpr p) (cat-9:bspn p)
                                           (cat-9:face p) (cat-9:cmpr tw)))
             (h (cat-9:build-mrph :sorc tw :trgt tw :degr +1
                                :intr r :strt :gnrt
                                :orgn `(cat-9:crts-contraction ,p)))
             (d (cat-9:bndr tw))
             (z (cat-9:i-sbtr (cat-9:idnt-mrph tw) (cat-9:cmps h d) (cat-9:cmps d h)))
             (c (random-cmbn cmpr 5 10 2 2 1)))
        (cat-9:? z c)
        (setf c (random-cmbn cmpr 6 10 3 3 3))
        (cat-9:? z c)
        (setf c (random-cmbn cmpr 7 10 4 4 6))
        (cat-9:? z c)
        (setf c (random-cmbn cmpr 8 10 4 4 20))
        (cat-9:? z c)
        (cat-9:? h c)
        (cat-9:? d (cat-9:? h c))
        (print (length (cat-9:cmbn-list (cat-9:? d (cat-9:? h c)))))))


(test crts-contraction1
      (let* ((h (cat-9:crts-contraction (cat-9:deltab)))
             (d (cat-9:bndr (cat-9:twisted-crts-prdc (cat-9:deltab))))
             (z (cat-9:i-sbtr (cat-9:idnt-mrph (cat-9:sorc d)) (cat-9:cmps d h)
                            (cat-9:cmps h d))))
        (cat-9:? z 3 (cat-9:crpr 4 7 2 (cat-9:loop3 2 (+ 32 64 128)
                                              -2 1 (+ 32 64 128) 2)))))


(test tnpr-contraction
      (cat-9:cat-9-init)
      (let* ((delta (cat-9:deltab))
             (sz (cat-9:szczarba (cat-9:deltab)))
             (tw (cat-9:bcc sz))
             (h (cat-9:tnpr-contraction delta))
             (z (cat-9:i-sbtr (cat-9:idnt-mrph tw) (cat-9:cmps tw h)
                            (cat-9:cmps h tw))))
        (time (cat-9:? z 3 (cat-9:tnpr 0 1 3 (cat-9:loop3 0 (cat-9:mask 5)
                                                    2 0 (* 32 (cat-9:mask 5))
                                                    -1))))
        (cat-9:? h 4 (cat-9:tnpr 0 1 0 (cat-9:loop3 0 (cat-9:mask 6) 1)))))
