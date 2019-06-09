;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test normalize-loop
      (cat-7:normalize-loop 4 cat-7:+empty-list+)
      (cat-7:normalize-loop 4 '((1 a . 1)))
      (cat-7:normalize-loop 4 '((1 a . 1) (3 b . 1) (5 b . 1))))


(test unnormalize-loop
      (cat-7:unnormalize-loop (cat-7:absm 1 (cat-7:loop3 2 'a 3))))


(test loop3
      (cat-7:loop3 nil)
      (cat-7:loop3 '(1 a 2 3 b -2))
      (signals simple-error (cat-7:loop3 '(1 a 2 3 b b)))
      (signals simple-error (cat-7:loop3 '(1 a 2 3 b)))
      (cat-7:loop3 1 'a 2 3 'b 4)
      (signals simple-error (cat-7:loop3 1 'a 2 3 'b)))


(test loop-print
      (print (cat-7:loop3 nil))
      (print (cat-7:loop3 '(0 a 2)))
      (print (cat-7:loop3 '(1 a 2)))
      (print (cat-7:loop3 '(3 a 2 5 b -1 6 c 4)))
      (print (cat-7:loop3 '(7 a 1 11 b 1 13 c 1 14 d 1)))
      (print (cat-7:loop3 '(7 a 1 11 b 1 13 c 1 7 d 1)))
      (print (cat-7:loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))))


(test apowr-niloop
      (cat-7:apowr-niloop #'cat-7:s-cmpr '(0 a . 1) cat-7:+empty-list+)
      (cat-7:apowr-niloop #'cat-7:s-cmpr '(0 a . 1) '((1 a . 1)))
      (cat-7:apowr-niloop #'cat-7:s-cmpr '(0 a . 1) '((0 b . 1)))
      (cat-7:apowr-niloop #'cat-7:s-cmpr '(0 a . 1) '((0 a . 1) (0 b . 1)))
      (cat-7:apowr-niloop #'cat-7:s-cmpr '(0 a . 1) '((0 a . -1) (0 b . 1))))


(test loop-space-cmpr
      (let ((cmpr (cat-7:loop-space-cmpr #'cat-7:s-cmpr)))
        (is (equal :greater (funcall cmpr (cat-7:loop3 '(1 a 2))
                                     (cat-7:loop3 '(1 a -1)))))
        (is (equal :greater (funcall cmpr (cat-7:loop3 '(3 b 4 1 a 2))
                                     (cat-7:loop3 '(3 b 4 1 a -1)))))))


(test apowr-face4
      (cat-7:cat-init)
      (let ((face (cat-7:face (cat-7:delta-infinity))))
        (cat-7:apowr-face4 face 3 4 (cat-7:apowr 1 31 5))
        (setf face (cat-7:face (cat-7:sphere 5)))
        (cat-7:apowr-face4 face 2 5 (cat-7:apowr 1 's5 4))
        (cat-7:apowr-face4 face 2 5 (cat-7:apowr 2 's5 4))))


(test apowr-lastface4
      (cat-7:cat-init)
      (let ((cmpr #'cat-7:f-cmpr)
            (face (cat-7:face (cat-7:delta-infinity))))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 63 1))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 63 -1))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 63 3))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 63 -3))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 8 31 1))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-7:absm 8 'a))
                         (4 (cat-7:absm 8 'a)))))
        (setf cmpr #'cat-7:s-cmpr)
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-7:absm 1 'a))
                         (4 (cat-7:absm 8 'a)))))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-7:absm 1 'a))
                         (4 (cat-7:absm 1 'a)))))
        (cat-7:apowr-lastface4 cmpr face 4 (cat-7:apowr 0 'a 2))))


(test loop-space-face
      (cat-7:cat-init)
      (let* ((cmpr #'cat-7:f-cmpr)
             (face (cat-7:face (cat-7:delta-infinity)))
             (rslt (cat-7:loop-space-face cmpr face)))
        (funcall rslt 2 4 (cat-7:loop3 '(0 63 3 1 31 -3)))
        (funcall rslt 2 4 (cat-7:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-7:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-7:loop3 '(0 63 3)))))


(test loop-space-grin-sintr
      (cat-7:loop-space-grin-sintr 5 (cat-7:loop3 0 'a 2 3 'b -3)))


(test loop-space-grml-sintr
      (let* ((cmpr #'cat-7:s-cmpr)
             (ml (cat-7:loop-space-grml-sintr cmpr)))
        (funcall ml 2 (cat-7:crpr 3 (cat-7:loop3) 0 (cat-7:loop3 0 'a 2)))
        (funcall ml 2 (cat-7:crpr 0 (cat-7:loop3 0 'a 2) 3 (cat-7:loop3)))
        (funcall ml 2 (cat-7:crpr 0 (cat-7:loop3 0 'a 2) 0 (cat-7:loop3 0 'a -2)))
        (funcall ml 2 (cat-7:crpr 0 (cat-7:loop3 0 'a 2) 0 (cat-7:loop3 0 'a -3)))
        (funcall ml 2 (cat-7:crpr 0 (cat-7:loop3 0 'a 2) 0 (cat-7:loop3 0 'b -3)))
        (funcall ml 2 (cat-7:crpr 0 (cat-7:loop3 0 'a 2 0 'b -3)
                                0 (cat-7:loop3 0 'b +3 0 'a -2)))))


(test loop-space
      (cat-7:cat-init)
      (let ((g (cat-7:gdeltab))
            basis cmbn cmbn2 dd echcm efhm hat loop-list rslt x)
        (is (equal :equal (cat-7:cmpr g (cat-7:loop3 0 3 2) (cat-7:loop3 0 3 2))))
        (is (equal :less (cat-7:cmpr g (cat-7:loop3 0 3 2) (cat-7:loop3 0 3 3))))
        (is (equal :less (cat-7:cmpr g (cat-7:loop3 0 3 2) (cat-7:loop3 0 5 2))))
        (is (equal :greater (cat-7:cmpr g (cat-7:loop3 0 5 2) (cat-7:loop3 0 3 2))))
        (cat-7:face g 3 3 (cat-7:loop3 12 7 3))
        (cat-7:face g 3 3 (cat-7:loop3 5 7 3))
        (cat-7:face g 3 3 (cat-7:loop3 6 7 3 5 7 3 3 7 3))
        (cat-7:face g 4 4 (cat-7:absm 4 (cat-7:loop3 6 7 3 5 7 3 3 7 3)))
        (cat-7:check-faces (cat-7:cmpr g) (cat-7:face g) 3
                         (cat-7:loop3 6 7 -3 5 7 -3 3 7 -3))
        (setf dd (cat-7:cmps g g))
        (cat-7:? dd 3 (cat-7:loop3 6 7 3 5 7 3 3 7 3))
        (cat-7:grml g 2 (cat-7:crpr 0 (cat-7:loop3 0 15 3) 0 (cat-7:loop3 0 15 4)))
        (cat-7:grin g 2 (cat-7:loop3 0 15 -2))
        (setf rslt (cat-7:loop3 0 31 1))
        (setf hat (mapcar #'(lambda (i) (cat-7:face g i 3 rslt))
                          (cat-7:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-7:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-7:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-7:loop-space (cat-7:sphere 2)))
        (setf rslt (cat-7:loop3 3 's2 2 5 's2 -2 6 's2 2))
        (setf hat (mapcar #'(lambda (i) (cat-7:face g i 3 rslt))
                          (cat-7:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-7:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-7:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-7:loop-space (cat-7:sphere 3) 2))
        (setf x (first (cat-7:basis (cat-7:echcm g) 4)))
        (setf efhm (cat-7:efhm g))
        (setf x (cat-7:lf efhm (cat-7:rg efhm 4 x)))
        (setf rslt (cat-7:gnrt (first (cat-7:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-7:face g i 4 rslt))
                          (cat-7:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-7:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-7:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf rslt (cat-7:gnrt (third (cat-7:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-7:face g i 4 rslt))
                          (cat-7:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-7:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-7:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-7:loop-space (cat-7:sphere 3) 2))
        (setf efhm (cat-7:efhm g))
        (setf echcm (cat-7:rbcc efhm))
        (setf basis (cat-7:basis echcm 4))
        (setf cmbn (cat-7:cmbn 4 1 (first basis) 10 (second basis)
                             100 (third basis)))
        (setf cmbn2 (cat-7:lf efhm (cat-7:rg efhm cmbn)))
        (setf loop-list (mapcar #'cdr (cat-7:cmbn-list cmbn2)))
        (dolist (loop loop-list)
          (setf hat (mapcar #'(lambda (i) (cat-7:face g i 4 loop))
                            (cat-7:<a-b> 0 4)))
          (dotimes (i 5)
            (print (cat-7:kfll g i 4 (remove (nth i hat) hat)))
            (cat-7:check-kan g i 4 (remove (nth i hat) hat))))))


(test loop-space2
      (cat-7:cat-init)
      (let* ((p (cat-7:r-proj-space 3))
             (op (cat-7:loop-space p))
             cmpr c)
        (dotimes (i 15) (print (random-apowr 8 3)))
        (dotimes (i 20)
          (print (cat-7:normalize-loop 7 (random-niloop 7 3 3))))

        (setf cmpr (cat-7:cmpr op))
        (setf cat-7:+too-much-time+ -1)
        (setf c (random-loop-cmbn cmpr 8 10 4 4 100))
        (time (cat-7:? op (cat-7:? op c)))
        (time (cat-7:? op (cat-7:? op c)))))
