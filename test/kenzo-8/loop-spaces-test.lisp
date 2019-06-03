;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test normalize-loop
      (cat-8:normalize-loop 4 cat-8:+empty-list+)
      (cat-8:normalize-loop 4 '((1 a . 1)))
      (cat-8:normalize-loop 4 '((1 a . 1) (3 b . 1) (5 b . 1))))


(test unnormalize-loop
      (cat-8:unnormalize-loop (cat-8:absm 1 (cat-8:loop3 2 'a 3))))


(test loop3
      (cat-8:loop3 nil)
      (cat-8:loop3 '(1 a 2 3 b -2))
      (signals simple-error (cat-8:loop3 '(1 a 2 3 b b)))
      (signals simple-error (cat-8:loop3 '(1 a 2 3 b)))
      (cat-8:loop3 1 'a 2 3 'b 4)
      (signals simple-error (cat-8:loop3 1 'a 2 3 'b)))


(test loop-print
      (print (cat-8:loop3 nil))
      (print (cat-8:loop3 '(0 a 2)))
      (print (cat-8:loop3 '(1 a 2)))
      (print (cat-8:loop3 '(3 a 2 5 b -1 6 c 4)))
      (print (cat-8:loop3 '(7 a 1 11 b 1 13 c 1 14 d 1)))
      (print (cat-8:loop3 '(7 a 1 11 b 1 13 c 1 7 d 1)))
      (print (cat-8:loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))))


(test apowr-niloop
      (cat-8:apowr-niloop #'cat-8:s-cmpr '(0 a . 1) cat-8:+empty-list+)
      (cat-8:apowr-niloop #'cat-8:s-cmpr '(0 a . 1) '((1 a . 1)))
      (cat-8:apowr-niloop #'cat-8:s-cmpr '(0 a . 1) '((0 b . 1)))
      (cat-8:apowr-niloop #'cat-8:s-cmpr '(0 a . 1) '((0 a . 1) (0 b . 1)))
      (cat-8:apowr-niloop #'cat-8:s-cmpr '(0 a . 1) '((0 a . -1) (0 b . 1))))


(test loop-space-cmpr
      (let ((cmpr (cat-8:loop-space-cmpr #'cat-8:s-cmpr)))
        (is (equal :greater (funcall cmpr (cat-8:loop3 '(1 a 2))
                                     (cat-8:loop3 '(1 a -1)))))
        (is (equal :greater (funcall cmpr (cat-8:loop3 '(3 b 4 1 a 2))
                                     (cat-8:loop3 '(3 b 4 1 a -1)))))))


(test apowr-face4
      (cat-8:cat-init)
      (let ((face (cat-8:face (cat-8:delta-infinity))))
        (cat-8:apowr-face4 face 3 4 (cat-8:apowr 1 31 5))
        (setf face (cat-8:face (cat-8:sphere 5)))
        (cat-8:apowr-face4 face 2 5 (cat-8:apowr 1 's5 4))
        (cat-8:apowr-face4 face 2 5 (cat-8:apowr 2 's5 4))))


(test apowr-lastface4
      (cat-8:cat-init)
      (let ((cmpr #'cat-8:f-cmpr)
            (face (cat-8:face (cat-8:delta-infinity))))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 63 1))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 63 -1))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 63 3))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 63 -3))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 8 31 1))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-8:absm 8 'a))
                         (4 (cat-8:absm 8 'a)))))
        (setf cmpr #'cat-8:s-cmpr)
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-8:absm 1 'a))
                         (4 (cat-8:absm 8 'a)))))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 'a 2))
        (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                       (case indx
                         (5 (cat-8:absm 1 'a))
                         (4 (cat-8:absm 1 'a)))))
        (cat-8:apowr-lastface4 cmpr face 4 (cat-8:apowr 0 'a 2))))


(test loop-space-face
      (cat-8:cat-init)
      (let* ((cmpr #'cat-8:f-cmpr)
             (face (cat-8:face (cat-8:delta-infinity)))
             (rslt (cat-8:loop-space-face cmpr face)))
        (funcall rslt 2 4 (cat-8:loop3 '(0 63 3 1 31 -3)))
        (funcall rslt 2 4 (cat-8:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-8:loop3 '(0 63 3 0 63 -3)))
        (funcall rslt 4 4 (cat-8:loop3 '(0 63 3)))))


(test loop-space-grin-sintr
      (cat-8:loop-space-grin-sintr 5 (cat-8:loop3 0 'a 2 3 'b -3)))


(test loop-space-grml-sintr
      (let* ((cmpr #'cat-8:s-cmpr)
             (ml (cat-8:loop-space-grml-sintr cmpr)))
        (funcall ml 2 (cat-8:crpr 3 (cat-8:loop3) 0 (cat-8:loop3 0 'a 2)))
        (funcall ml 2 (cat-8:crpr 0 (cat-8:loop3 0 'a 2) 3 (cat-8:loop3)))
        (funcall ml 2 (cat-8:crpr 0 (cat-8:loop3 0 'a 2) 0 (cat-8:loop3 0 'a -2)))
        (funcall ml 2 (cat-8:crpr 0 (cat-8:loop3 0 'a 2) 0 (cat-8:loop3 0 'a -3)))
        (funcall ml 2 (cat-8:crpr 0 (cat-8:loop3 0 'a 2) 0 (cat-8:loop3 0 'b -3)))
        (funcall ml 2 (cat-8:crpr 0 (cat-8:loop3 0 'a 2 0 'b -3)
                                0 (cat-8:loop3 0 'b +3 0 'a -2)))))


(test loop-space
      (cat-8:cat-init)
      (let ((g (cat-8:gdeltab))
            basis cmbn cmbn2 dd echcm efhm hat loop-list rslt x)
        (is (equal :equal (cat-8:cmpr g (cat-8:loop3 0 3 2) (cat-8:loop3 0 3 2))))
        (is (equal :less (cat-8:cmpr g (cat-8:loop3 0 3 2) (cat-8:loop3 0 3 3))))
        (is (equal :less (cat-8:cmpr g (cat-8:loop3 0 3 2) (cat-8:loop3 0 5 2))))
        (is (equal :greater (cat-8:cmpr g (cat-8:loop3 0 5 2) (cat-8:loop3 0 3 2))))
        (cat-8:face g 3 3 (cat-8:loop3 12 7 3))
        (cat-8:face g 3 3 (cat-8:loop3 5 7 3))
        (cat-8:face g 3 3 (cat-8:loop3 6 7 3 5 7 3 3 7 3))
        (cat-8:face g 4 4 (cat-8:absm 4 (cat-8:loop3 6 7 3 5 7 3 3 7 3)))
        (cat-8:check-faces (cat-8:cmpr g) (cat-8:face g) 3
                         (cat-8:loop3 6 7 -3 5 7 -3 3 7 -3))
        (setf dd (cat-8:cmps g g))
        (cat-8:? dd 3 (cat-8:loop3 6 7 3 5 7 3 3 7 3))
        (cat-8:grml g 2 (cat-8:crpr 0 (cat-8:loop3 0 15 3) 0 (cat-8:loop3 0 15 4)))
        (cat-8:grin g 2 (cat-8:loop3 0 15 -2))
        (setf rslt (cat-8:loop3 0 31 1))
        (setf hat (mapcar #'(lambda (i) (cat-8:face g i 3 rslt))
                          (cat-8:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-8:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-8:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-8:loop-space (cat-8:sphere 2)))
        (setf rslt (cat-8:loop3 3 's2 2 5 's2 -2 6 's2 2))
        (setf hat (mapcar #'(lambda (i) (cat-8:face g i 3 rslt))
                          (cat-8:<a-b> 0 3)))
        (dotimes (i 4)
          (print (cat-8:kfll g i 3 (remove (nth i hat) hat :test #'equal)))
          (cat-8:check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-8:loop-space (cat-8:sphere 3) 2))
        (setf x (first (cat-8:basis (cat-8:echcm g) 4)))
        (setf efhm (cat-8:efhm g))
        (setf x (cat-8:lf efhm (cat-8:rg efhm 4 x)))
        (setf rslt (cat-8:gnrt (first (cat-8:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-8:face g i 4 rslt))
                          (cat-8:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-8:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-8:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf rslt (cat-8:gnrt (third (cat-8:cmbn-list x))))
        (setf hat (mapcar #'(lambda (i) (cat-8:face g i 4 rslt))
                          (cat-8:<a-b> 0 4)))
        (dotimes (i 5)
          (print (cat-8:kfll g i 4 (remove (nth i hat) hat :test #'equal)))
          (cat-8:check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

        (setf g (cat-8:loop-space (cat-8:sphere 3) 2))
        (setf efhm (cat-8:efhm g))
        (setf echcm (cat-8:rbcc efhm))
        (setf basis (cat-8:basis echcm 4))
        (setf cmbn (cat-8:cmbn 4 1 (first basis) 10 (second basis)
                             100 (third basis)))
        (setf cmbn2 (cat-8:lf efhm (cat-8:rg efhm cmbn)))
        (setf loop-list (mapcar #'cdr (cat-8:cmbn-list cmbn2)))
        (dolist (loop loop-list)
          (setf hat (mapcar #'(lambda (i) (cat-8:face g i 4 loop))
                            (cat-8:<a-b> 0 4)))
          (dotimes (i 5)
            (print (cat-8:kfll g i 4 (remove (nth i hat) hat)))
            (cat-8:check-kan g i 4 (remove (nth i hat) hat))))))


(test loop-space2
      (cat-8:cat-init)
      (let* ((p (cat-8:r-proj-space 3))
             (op (cat-8:loop-space p))
             cmpr c)
        (dotimes (i 15) (print (random-apowr 8 3)))
        (dotimes (i 20)
          (print (cat-8:normalize-loop 7 (random-niloop 7 3 3))))

        (setf cmpr (cat-8:cmpr op))
        (setf cat-8:+too-much-time+ -1)
        (setf c (random-loop-cmbn cmpr 8 10 4 4 100))
        (time (cat-8:? op (cat-8:? op c)))
        (time (cat-8:? op (cat-8:? op c)))))
