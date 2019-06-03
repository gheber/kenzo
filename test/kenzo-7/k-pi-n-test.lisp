;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test k-z-1-cmpr
      (is (equal :less (cat-7:k-z-1-cmpr '(1 1 2) '(1 2 2))))
      (is (equal :equal (cat-7:k-z-1-cmpr '(1 1 2) '(1 1 2))))
      (is (equal :greater (cat-7:k-z-1-cmpr '(1 1 2) '(1 1 -1)))))

(test k-z-1-face
      (cat-7:k-z-1-face 0 1 '(3))
      (cat-7:k-z-1-face 1 1 '(3))
      (cat-7:k-z-1-face 0 3 '(1 2 3))
      (cat-7:k-z-1-face 1 3 '(1 2 3))
      (cat-7:k-z-1-face 2 3 '(1 2 3))
      (cat-7:k-z-1-face 2 3 '(1 2 -2))
      (cat-7:k-z-1-face 3 3 '(1 2 3)))

(test z-absm-bar
      (cat-7:z-absm-bar (cat-7:absm 0 '()))
      (cat-7:z-absm-bar (cat-7:absm 1 '()))
      (cat-7:z-absm-bar (cat-7:absm 0 '(2)))
      (dotimes (i 8)
        (print (cat-7:z-absm-bar (cat-7:absm i '(3 6))))))


(test z-bar-absm
      (cat-7:z-bar-absm (cat-7:z-absm-bar (cat-7:absm 0 '())))
      (cat-7:z-bar-absm (cat-7:z-absm-bar (cat-7:absm 1 '())))
      (cat-7:z-bar-absm (cat-7:z-absm-bar (cat-7:absm 0 '(2))))
      (dotimes (i 8)
        (print (cat-7:z-bar-absm (cat-7:z-absm-bar (cat-7:absm i '(3 6)))))))


(test k-z-1-grml
      (cat-7:k-z-1-grml 0 (cat-7:crpr 0 nil 0 nil))
      (cat-7:k-z-1-grml 1 (cat-7:crpr 0 '(3) 0 '(4)))
      (cat-7:k-z-1-grml 1 (cat-7:crpr 0 '(3) 0 '(-3)))
      (cat-7:k-z-1-grml 1 (cat-7:crpr 0 '(3) 1 '()))
      (cat-7:k-z-1-grml 1 (cat-7:crpr 1 '() 0 '(3)))
      (cat-7:k-z-1-grml 1 (cat-7:crpr 1 '() 1 '()))
      (cat-7:k-z-1-grml 5 (cat-7:crpr 24 '(2 2 3) 20 '(-2 2 4))))


(test k-z-1-grin
      (cat-7:k-z-1-grin 3 '(2 3 -4))
      (let ((gmsm '(2 3 -4)))
        (cat-7:k-z-1-grml 3 (cat-7:crpr 0 gmsm 0
                                    (cat-7:gmsm (cat-7:k-z-1-grin 3 gmsm))))))


(defun aleat-list (max length)
  (let ((rslt nil)
        (2max (+ max max)))
    (dotimes (i length)
      (push (let ((k (- (random 2max) max)))
              (if (zerop k)
                  max
                  k))
            rslt))
    rslt))


(test k-z-1
      (let ((k (cat-7:k-z-1))
            zero?)
        (cat-7:? k (cat-7:? k 14 (aleat-list 200 14)))
        (setf zero? (cat-7:add (cat-7:idnt-mrph k) (cat-7:grin k)))
        (cat-7:? zero? 14 (aleat-list 200 14))))


(test circle
      (let ((c (cat-7:circle)))
        (cat-7:cmpr c '* '*)
        (cat-7:basis c 1)
        (cat-7:? c 1 's1)))


(test kz1-rdct-f-intr
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 0))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 0 4 '()))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 1))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 1 4 '(3)))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 1 4 '(3) 5 '(2)))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn 1 4 '(3) -3 '(4)))
      (cat-7:kz1-rdct-f-intr (cat-7:cmbn -3)))


(test kz1-rdct-h-intr
      (cat-7:kz1-rdct-h-intr 0 nil)
      (cat-7:kz1-rdct-h-intr 1 '(-4))
      (cat-7:kz1-rdct-h-intr 1 '(-1))
      (cat-7:kz1-rdct-h-intr 1 '(1))
      (cat-7:kz1-rdct-h-intr 1 '(4))
      (cat-7:kz1-rdct-h-intr 3 '(-4 3 5))
      (cat-7:kz1-rdct-h-intr 3 '(-1 3 -1))
      (cat-7:kz1-rdct-h-intr 3 '(1 2 2))
      (cat-7:kz1-rdct-h-intr 3 '(4 3 5)))


(test kz1-rdct
      (cat-7:cat-init)
      (cat-7:pre-check-rdct (cat-7:kz1-rdct))
      (setf cat-7:*tc* (cat-7:cmbn 0 1 '()))
      (setf cat-7:*bc* (cat-7:cmbn 0 1 '*))
      (check-rdct)
      (setf cat-7:*tc* (cat-7:cmbn 1 1 '(-4) 10 '(-1) 100 '(1) 1000 '(5)))
      (setf cat-7:*bc* (cat-7:cmbn 1 1 's1))
      (check-rdct)
      (setf cat-7:*tc* (cat-7:cmbn 2 1 '(-4 2) 10 '(-1 3) 100 '(1 -4) 1000 '(5 5)))
      (check-rdct)
      (setf cat-7:*tc* (cat-7:cmbn 3 1 '(-4 -4 5) 10 '(-1 2 1)
                               100 '(1 4 2) 1000 '(5 1 -1)))
      (check-rdct))


(test k-z-1-homology
      (cat-7:cat-init)
      (cat-7:homology (cat-7:k-z-1) 1))


(test k-z2-1-homology
      (let ((k (cat-7:k-z2-1)))
        (cat-7:homology k 0 4)))


(test k-z2-homology
      (let ((k3 (cat-7:k-z2 3)))
        (cat-7:homology k3 7)))


(test z2-bar-absm
      (dotimes (i 8)    ;;; not really legal
        (dotimes (j 8)
          (print (cat-7:z2-bar-absm (cat-7:z2-absm-bar (cat-7:absm i j)))))))


(test z-fundamental-gmsm
      (cat-7:z-fundamental-gmsm 1 33)
      (cat-7:z-fundamental-gmsm 2 33)
      (cat-7:z-fundamental-gmsm 3 33)
      (cat-7:z-fundamental-gmsm 4 33))


(test interesting-faces
      (let* ((d (cat-7:delta 14))
             (f (cat-7:face d)))
        (do ((i 5 (1- i)))
            ((minusp i))
          (print (cat-7:interesting-faces f i 5 (cat-7:mask 6))))))


(test gmsm-cocycle
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 10))
             (chml-clss (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -2
                                        :intr #'(lambda (dmns gmsm)
                                                  (cat-7:term-cmbn 0 gmsm
                                                                 :gnrt-z))
                                        :strt :gnrt
                                        :orgn '(essai-1))))
        (cat-7:gmsm-cocycle (cat-7:face d) 2 4 31 chml-clss)
        (setf chml-clss (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -1
                                        :intr #'(lambda (dmns gmsm)
                                                  (cat-7:term-cmbn 0 gmsm
                                                                 :gnrt-z))
                                        :strt :gnrt
                                        :orgn '(essai-2)))
        (cat-7:gmsm-cocycle (cat-7:face d) 1 4 31 chml-clss)))

(test z-cocycle-gbar
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 10))
             (chml-clss (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -1
                                        :intr #'(lambda (dmns gmsm)
                                                  (cat-7:term-cmbn 0 gmsm
                                                                 :gnrt-z))
                                        :strt :gnrt
                                        :orgn '(essai-1))))
        (cat-7:gmsm-cocycle (cat-7:face d) 1 4 31 chml-clss)
        (cat-7:z-cocycle-gbar 1 4 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  1 4 31 chml-clss))
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -2
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 gmsm :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-2)))
        (cat-7:gmsm-cocycle (cat-7:face d) 2 2 7 chml-clss)
        (cat-7:z-cocycle-gbar 2 2 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  2 2 7 chml-clss))
        (cat-7:gmsm-cocycle (cat-7:face d) 2 2 0 chml-clss) ;; normally illegal
        (cat-7:z-cocycle-gbar 2 2 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  2 2 0 chml-clss))
        (cat-7:gmsm-cocycle (cat-7:face d) 2 3 15 chml-clss)
        (cat-7:z-cocycle-gbar 2 3 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  2 3 15 chml-clss))
        (cat-7:gmsm-cocycle (cat-7:face d) 2 4 31 chml-clss)
        (cat-7:z-cocycle-gbar 2 4 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  2 4 31 chml-clss))
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 gmsm :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-3)))
        (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss)
        (cat-7:z-cocycle-gbar 3 4 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  3 4 31 chml-clss))
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:zero-cmbn 0))
                              :strt :gnrt
                              :orgn '(essai-33)))
        (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss)
        (cat-7:z-cocycle-gbar 3 4 (cat-7:gmsm-cocycle (cat-7:face d)
                                                  3 4 31 chml-clss))))


(test z-cocycle-gbar-head
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 10))
             (chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -1
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 gmsm :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-1)))
             (x (cat-7:gmsm-cocycle (cat-7:face d) 1 4 31 chml-clss)))
        (cat-7:z-cocycle-gbar 1 4 x)
        (signals simple-error (cat-7:z-cocycle-gbar-head 1 4 x))
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -2
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 gmsm :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-2)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 7 chml-clss))
        (cat-7:z-cocycle-gbar 2 2 x)
        (cat-7:z-cocycle-gbar-head 2 2 x)
        ;; normally illegal
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 0 chml-clss))
        (cat-7:z-cocycle-gbar 2 2 x)
        (cat-7:z-cocycle-gbar-head 2 2 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 3 15 chml-clss))
        (cat-7:z-cocycle-gbar 2 3 x)
        (cat-7:z-cocycle-gbar-head 2 3 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 4 31 chml-clss))
        (cat-7:z-cocycle-gbar 2 4 x)
        (cat-7:z-cocycle-gbar-head 2 4 x)
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 gmsm :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-3)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss))
        (cat-7:z-cocycle-gbar 3 4 x)
        (cat-7:z-cocycle-gbar-head 3 4 x)
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:zero-cmbn 0))
                              :strt :gnrt
                              :orgn '(essai-33)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss))
        (cat-7:z-cocycle-gbar 3 4 x)
        (cat-7:z-cocycle-gbar-head 3 4 x)))


(test Z2-fundamental-gmsm
      (cat-7:Z2-fundamental-gmsm 1 1)
      (cat-7:Z2-fundamental-gmsm 2 1)
      (cat-7:Z2-fundamental-gmsm 3 1)
      (cat-7:Z2-fundamental-gmsm 4 1))


(test z2-cocycle-gbar
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 10))
             (chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -1
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-1)))
             (x (cat-7:gmsm-cocycle (cat-7:face d) 1 4 31 chml-clss)))
        (cat-7:z2-cocycle-gbar 1 4 x)
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -2
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-2)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 7 chml-clss))
        (cat-7:z2-cocycle-gbar 2 2 x)
        ;; normally illegal
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 0 chml-clss))
        (cat-7:z2-cocycle-gbar 2 2 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 3 15 chml-clss))
        (cat-7:z2-cocycle-gbar 2 3 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 4 31 chml-clss))
        (cat-7:z2-cocycle-gbar 2 4 x)
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-3)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss))
        (cat-7:z2-cocycle-gbar 3 4 x)))


(test z2-cocycle-gbar-head
      (cat-7:cat-init)
      (let* ((d (cat-7:delta 10))
             (chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -1
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-1)))
             (x (cat-7:gmsm-cocycle (cat-7:face d) 1 4 31 chml-clss)))
        (cat-7:z2-cocycle-gbar 1 4 x)
        (signals simple-error (cat-7:z2-cocycle-gbar-head 1 4 x))
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -2
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-2)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 7 chml-clss))
        (cat-7:z2-cocycle-gbar 2 2 x)
        (cat-7:z2-cocycle-gbar-head 2 2 x)
        ;; normally illegal
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 2 0 chml-clss))
        (cat-7:z2-cocycle-gbar 2 2 x)
        (cat-7:z2-cocycle-gbar-head 2 2 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 3 15 chml-clss))
        (cat-7:z2-cocycle-gbar 2 3 x)
        (cat-7:z2-cocycle-gbar-head 2 3 x)
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 2 4 31 chml-clss))
        (cat-7:z2-cocycle-gbar 2 4 x)
        (cat-7:z2-cocycle-gbar-head 2 4 x)
        (setf chml-clss
              (cat-7:build-mrph :sorc d :trgt (cat-7:z-chcm) :degr -3
                              :intr #'(lambda (dmns gmsm)
                                        (cat-7:term-cmbn 0 (mod gmsm 2) :gnrt-z))
                              :strt :gnrt
                              :orgn '(essai-3)))
        (setf x (cat-7:gmsm-cocycle (cat-7:face d) 3 4 31 chml-clss))
        (cat-7:z2-cocycle-gbar 3 4 x)
        (cat-7:z2-cocycle-gbar-head 3 4 x)))


(test k-z-fundamental-class
      (cat-7:cat-init)
      (let ((c1 (cat-7:k-z-fundamental-class 1))
            (c3 (cat-7:k-z-fundamental-class 3)))
        (cat-7:? c1 1 '(34))
        (cat-7:? c1 2 '(34 45))
        (cat-7:? c3 3 (cat-7:z-fundamental-gmsm 3 45))))


(test k-z2-fundamental-class
      (cat-7:cat-init)
      (let ((c1 (cat-7:k-z2-fundamental-class 1))
            (c3 (cat-7:k-z2-fundamental-class 3)))
        (cat-7:? c1 1 1)
        (cat-7:? c1 2 2)
        (cat-7:? c3 3 (cat-7:z2-fundamental-gmsm 3 1))))
