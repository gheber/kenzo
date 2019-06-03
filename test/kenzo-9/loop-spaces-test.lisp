;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)


(test normalize-loop
      (cat-9:normalize-loop 4 cat-9:+empty-list+)
      (cat-9:normalize-loop 4 '((1 a . 1)))
      (cat-9:normalize-loop 4 '((1 a . 1) (3 b . 1) (5 b . 1))))


(test unnormalize-loop
      (cat-9:unnormalize-loop (cat-9:absm 1 (cat-9:loop3 2 'a 3))))


(test loop3
      (cat-9:loop3 nil)
      (cat-9:loop3 '(1 a 2 3 b -2))
      (signals simple-error (cat-9:loop3 '(1 a 2 3 b b)))
      (signals simple-error (cat-9:loop3 '(1 a 2 3 b)))
      (cat-9:loop3 1 'a 2 3 'b 4)
      (signals simple-error (cat-9:loop3 1 'a 2 3 'b)))


(test loop-print
      (print (cat-9:loop3 nil))
      (print (cat-9:loop3 '(0 a 2)))
      (print (cat-9:loop3 '(1 a 2)))
      (print (cat-9:loop3 '(3 a 2 5 b -1 6 c 4)))
      (print (cat-9:loop3 '(7 a 1 11 b 1 13 c 1 14 d 1)))
      (print (cat-9:loop3 '(7 a 1 11 b 1 13 c 1 7 d 1)))
      (print (cat-9:loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))))


(test apowr-niloop
      (cat-9:apowr-niloop #'cat-9:s-cmpr '(0 a . 1) cat-9:+empty-list+)
      (cat-9:apowr-niloop #'cat-9:s-cmpr '(0 a . 1) '((1 a . 1)))
      (cat-9:apowr-niloop #'cat-9:s-cmpr '(0 a . 1) '((0 b . 1)))
      (cat-9:apowr-niloop #'cat-9:s-cmpr '(0 a . 1) '((0 a . 1) (0 b . 1)))
      (cat-9:apowr-niloop #'cat-9:s-cmpr '(0 a . 1) '((0 a . -1) (0 b . 1))))


(test loop-space-cmpr
      (let ((cmpr (cat-9:loop-space-cmpr #'cat-9:s-cmpr)))
        (is (equal :greater (funcall cmpr (cat-9:loop3 '(1 a 2))
                                     (cat-9:loop3 '(1 a -1)))))
        (is (equal :greater (funcall cmpr (cat-9:loop3 '(3 b 4 1 a 2))
                                     (cat-9:loop3 '(3 b 4 1 a -1)))))))


(test apowr-face4
      (cat-9:cat-9-init)
      (let ((face (cat-9:face (cat-9:delta-infinity))))
        (cat-9:apowr-face4 face 3 4 (cat-9:apowr 1 31 5))
        (setf face (cat-9:face (cat-9:sphere 5)))
        (cat-9:apowr-face4 face 2 5 (cat-9:apowr 1 's5 4))
        (cat-9:apowr-face4 face 2 5 (cat-9:apowr 2 's5 4))))


(test apowr-lastface4
      (cat-9:cat-9-init)
      (let ((cmpr #'cat-9:f-cmpr)
            (face (cat-9:face (cat-9:delta-infinity))))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 63 1))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 63 -1))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 63 3))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 63 -3))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 8 31 1))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-9:absm 8 'a))
                         (4 (cat-9:absm 8 'a)))))
        (setf cmpr #'cat-9:s-cmpr)
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-9:absm 1 'a))
                         (4 (cat-9:absm 8 'a)))))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-9:absm 1 'a))
                         (4 (cat-9:absm 1 'a)))))
        (cat-9:apowr-lastface4 cmpr face 4 (cat-9:apowr 0 'a 2))))


(test loop-space-face
      (cat-9:cat-9-init)
      (let* ((cmpr #'cat-9:f-cmpr)
             (face (cat-9:face (cat-9:delta-infinity)))
             (rslt (cat-9:loop-space-face cmpr face)))
        (funcall rslt 2 4 (cat-9:loop3 '(0 63 3 1 31 -3)))
        (funcall rslt 2 4 (cat-9:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-9:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-9:loop3 '(0 63 3)))))


(test loop-space-grin-sintr
      (cat-9:loop-space-grin-sintr 5 (cat-9:loop3 0 'a 2 3 'b -3)))


(test loop-space-grml-sintr
      (let* ((cmpr #'cat-9:s-cmpr)
             (ml (cat-9:loop-space-grml-sintr cmpr)))
        (funcall ml 2 (cat-9:crpr 3 (cat-9:loop3) 0 (cat-9:loop3 0 'a 2)))
        (funcall ml 2 (cat-9:crpr 0 (cat-9:loop3 0 'a 2) 3 (cat-9:loop3)))
        (funcall ml 2 (cat-9:crpr 0 (cat-9:loop3 0 'a 2) 0 (cat-9:loop3 0 'a -2)))
        (funcall ml 2 (cat-9:crpr 0 (cat-9:loop3 0 'a 2) 0 (cat-9:loop3 0 'a -3)))
        (funcall ml 2 (cat-9:crpr 0 (cat-9:loop3 0 'a 2) 0 (cat-9:loop3 0 'b -3)))
        (funcall ml 2 (cat-9:crpr 0 (cat-9:loop3 0 'a 2 0 'b -3)
                                0 (cat-9:loop3 0 'b +3 0 'a -2)))))


(test loop-space
      (cat-9:cat-9-init)
      (let ((g (cat-9:gdeltab))
            basis cmbn cmbn2 dd echcm efhm hat loop-list rslt x)
        (is (equal :equal (cat-9:cmpr g (cat-9:loop3 0 3 2) (cat-9:loop3 0 3 2))))
        (is (equal :less (cat-9:cmpr g (cat-9:loop3 0 3 2) (cat-9:loop3 0 3 3))))
        (is (equal :less (cat-9:cmpr g (cat-9:loop3 0 3 2) (cat-9:loop3 0 5 2))))
        (is (equal :greater (cat-9:cmpr g (cat-9:loop3 0 5 2) (cat-9:loop3 0 3 2))))
        (cat-9:face g 3 3 (cat-9:loop3 12 7 3))
        (cat-9:face g 3 3 (cat-9:loop3 5 7 3))
        (cat-9:face g 3 3 (cat-9:loop3 6 7 3 5 7 3 3 7 3))
        (cat-9:face g 4 4 (cat-9:absm 4 (cat-9:loop3 6 7 3 5 7 3 3 7 3)))
        (cat-9:check-faces (cat-9:cmpr g) (cat-9:face g) 3
                         (cat-9:loop3 6 7 -3 5 7 -3 3 7 -3))
        (setf dd (cat-9:cmps g g))
        (cat-9:? dd 3 (cat-9:loop3 6 7 3 5 7 3 3 7 3))
        (cat-9:grml g 2 (cat-9:crpr 0 (cat-9:loop3 0 15 3) 0 (cat-9:loop3 0 15 4)))
        (cat-9:grin g 2 (cat-9:loop3 0 15 -2))
        (setf rslt (cat-9:loop3 0 31 1))
        (setf hat (mapcar #'(lambda (i) (cat-9:face g i 3 rslt))
                          (cat-9:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-9:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-9:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-9:loop-space (cat-9:sphere 2)))
        (setf rslt (cat-9:loop3 3 's2 2 5 's2 -2 6 's2 2))
        (setf hat (mapcar #'(lambda (i) (cat-9:face g i 3 rslt))
                          (cat-9:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-9:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-9:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-9:loop-space (cat-9:sphere 3) 2))
        (setf x (first (cat-9:basis (cat-9:echcm g) 4)))
        (setf efhm (cat-9:efhm g))
        (setf x (cat-9:lf efhm (cat-9:rg efhm 4 x)))
        (setf rslt (cat-9:gnrt (first (cat-9:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-9:face g i 4 rslt))
                          (cat-9:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-9:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-9:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf rslt (cat-9:gnrt (third (cat-9:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-9:face g i 4 rslt))
                          (cat-9:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-9:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-9:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-9:loop-space (cat-9:sphere 3) 2))
        (setf efhm (cat-9:efhm g))
        (setf echcm (cat-9:rbcc efhm))
        (setf basis (cat-9:basis echcm 4))
        (setf cmbn (cat-9:cmbn 4 1 (first basis) 10 (second basis)
                             100 (third basis)))
        (setf cmbn2 (cat-9:lf efhm (cat-9:rg efhm cmbn)))
        (setf loop-list (mapcar #'cdr (cat-9:cmbn-list cmbn2)))
        (dolist (loop loop-list)
          (setf hat (mapcar #'(lambda (i) (cat-9:face g i 4 loop))
                            (cat-9:<a-b> 0 4)))
          (dotimes (i 5)
            (print (cat-9:kfll g i 4 (remove (nth i hat) hat)))
            (cat-9:check-kan g i 4 (remove (nth i hat) hat))))))


(test loop-space2
      (cat-9:cat-9-init)
      (let* ((p (cat-9:r-proj-space 3))
             (op (cat-9:loop-space p))
             cmpr c)
        (dotimes (i 15) (print (random-apowr 8 3)))
        (dotimes (i 20)
          (print (cat-9:normalize-loop 7 (random-niloop 7 3 3))))

        (setf cmpr (cat-9:cmpr op))
        (setf cat-9:+too-much-time+ -1)
        (setf c (random-loop-cmbn cmpr 8 10 4 4 100))
        (time (cat-9:? op (cat-9:? op c)))
        (time (cat-9:? op (cat-9:? op c)))))
