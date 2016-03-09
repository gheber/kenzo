;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)

(test dgop-ext-int
      (is (= 0 (cat:dgop-ext-int '())))
      (signals simple-error (cat:dgop-ext-int '(2 2)))
      (is (= 17 (cat:dgop-ext-int '(4 0)))))


(test dgop-int-ext
      (is (null (cat:dgop-int-ext 0)))
      (is (equal '(2) (cat:dgop-int-ext 4)))
      (is (equal '(5 4 3 2 1 0) (cat:dgop-int-ext 63)))
      (dotimes (i 33)
        (is (= i (cat:dgop-ext-int (cat:dgop-int-ext i))))))


(test hyphenize-list
      (is (string= "-" (cat:hyphenize-list '())))
      (is (string= "3" (cat:hyphenize-list '(3))))
      (is (string= "5-3-1" (cat:hyphenize-list '(5 3 1)))))


(test moore
      (let ((m (cat:moore 2 2)))
        (dotimes (i 4)
          (print (cat:basis m i :dgnr)))))

#|
(test intr-diagonal
      (let* ((cmpr #'cat:f-cmpr)
             (face #'cat:delta-face)
             (diag (cat:intr-diagonal face)))
        (funcall diag 4 (cat:mask 5))
        (funcall diag 0 4)
        (setf cmpr #'cat:s-cmpr face (cat:sphere-face 4))
        (setf diag (cat:intr-diagonal face))
        (funcall diag 4 's4)
        (funcall diag 0 '*)
        (setf s4 (cat:sphere 4))
        (cat:dgnl s4 4 's4)))
|#

(test face-bndr
      (let* ((face #'(lambda (indx dmns gmsm)
                       (cat:absm 0 (append
                                    (subseq gmsm 0 indx)
                                    (subseq gmsm (1+ indx))))))
             (bndr (cat:face-bndr #'cat:l-cmpr face)))
        (funcall bndr 0 '(a))
        (funcall bndr 1 '(a b))
        (funcall bndr 2 '(a b c))
        (funcall bndr 3 '(a b c d))
        (funcall bndr 3 '(d c b a))
        (funcall bndr 1 '(a a))
        (funcall bndr 2 '(a a a))
        (funcall bndr 3 '(a a a a))
        (setf face #'(lambda (index dmns gmsm)
                       (cat:absm (cat:dgop-ext-int
                                  (nreverse (cat:<a-b< 0 (1- dmns)))) 'a)))
        (setf bndr (cat:face-bndr #'cat:s-cmpr face))
        (funcall bndr 1 '(a b))
        (funcall bndr 2 '(a b c))))


(test face*-bndr
      (let* ((face* #'(lambda (indx dmns gmsm)
                        (if (and (= dmns 3) (evenp indx))
                            'm2
                            :degenerate)))
             (bndr (cat:face*-bndr #'cat:s-cmpr face*)))
        (funcall bndr 3 'm3)
        (funcall bndr 2 'm2)
        (funcall bndr 0 '*)
        (setf face* #'(lambda (indx dmns gmsm)
                        (if (and (= dmns 3) (< indx 2))
                            'm2
                            :degenerate)))
        (setf bndr (cat:face*-bndr #'cat:s-cmpr face*))
        (funcall bndr 3 'm3)
        (funcall bndr 2 'm2)
        (funcall bndr 0 '*)))


(test a-cmpr3
      (is (equal :less (cat:a-cmpr3 #'cat:s-cmpr
                                    (cat:absm 0 'a)
                                    (cat:absm 1 'b))))
      (is (equal :greater (cat:a-cmpr3 #'cat:s-cmpr
                                       (cat:absm 2 'a)
                                       (cat:absm 1 'b))))
      (is (equal :less (cat:a-cmpr3 #'cat:s-cmpr
                                    (cat:absm 1 'a)
                                    (cat:absm 1 'b))))
      (is (equal :greater (cat:a-cmpr3 #'cat:s-cmpr
                                       (cat:absm 1 'c)
                                       (cat:absm 1 'b))))
      (is (equal :equal (cat:a-cmpr3 #'cat:s-cmpr
                                     (cat:absm 1 'a)
                                     (cat:absm 1 'a)))))


(test bspn-p
      (let* ((d (cat:delta-infinity))
             (cmpr (cat:cmpr d)))
        (is (cat:bspn-p cmpr 1 5 (cat:absm (cat:mask 5) 1)))
        (is (not (cat:bspn-p cmpr 1 5 (cat:absm (cat:mask 5) 2))))
        (is (not (cat:bspn-p cmpr 1 5 (cat:absm (cat:mask 4) 3))))))


(test dlop-ext-int
      (is (= 0 (cat:dlop-ext-int '())))
      (signals simple-error (cat:dlop-ext-int '(2 2)))
      (is (= 17 (cat:dlop-ext-int '(0 4)))))


(test dlop-ext-int
      (is (null (cat:dlop-int-ext 0)))
      (is (equal '(2) (cat:dlop-int-ext 4)))
      (is (equal '(0 1 2 3 4 5) (cat:dlop-int-ext 63)))
      (dotimes (i 33)
        (is (= i (cat:dlop-ext-int (cat:dlop-int-ext i))))))


(test 1dgop*dgop
      (dotimes (i 20)
        (dotimes (j 5)
          (format t "~% (~D) o ~A = ~A"
                  j (cat:dgop-int-ext i)
                  (cat:dgop-int-ext (cat:1dgop*dgop j i))))))


(test dgop*dgop
      (dotimes (i 10)
        (dotimes (j 10)
          (format t "~%~A o ~A = ~A"
                  (cat:dgop-int-ext i) (cat:dgop-int-ext j)
                  (cat:dgop-int-ext (cat:dgop*dgop i j))))))


(test remove-bit
      (is (= 51 (cat:remove-bit 107 3))))


(test dgop/dgop
      (is (= 67 (cat:dgop/dgop 67 0)))
      (is (= 32 (cat:dgop/dgop 68 4)))
      (is (= 7 (cat:dgop/dgop 31 5))))


(test 1dlop-dgop
      (dotimes (i 5)
        (dotimes (j 17)
          (multiple-value-bind (dgop 1dlop) (cat:1dlop-dgop i j)
            (format t "~% del-~D o ~A = ~A o del-~A"
                    i (cat:dgop-int-ext j) (cat:dgop-int-ext dgop) 1dlop)))))

#|
(test a-face4
      (let ((simp (cat:dlop-ext-int '(0 1 2 3 4 5 6))))
        (dotimes (i 5)
          (dotimes (j 17)
            (let ((face (cat:a-face4 (cat:face (cat:delta-infinity))
                                     i (+ 6 (logcount j))
                                     (cat:absm j simp))))
              (format t "~%del-~D o ~A (0 1 2 3 4 5 6) = ~A ~A"
                      i (cat:dgop-int-ext j)
                      (cat:dgop-int-ext (cat:dgop face))
                      (cat:dlop-int-ext (cat:gmsm face))))))))
|#


(test 1dgnr
      (print (cat:1dgnr 2 (cat:absm 17 'a))))


(test ndgnr
      (print (cat:ndgnr 2 (cat:absm 17 'a))))


(test nface
      (let ((f (cat:face (cat:delta-infinity)))
            p)
        (dotimes (dlop 31)
          (print (cat:dlop-int-ext (cat:gmsm (cat:nface f dlop 4 31)))))
        (setf p (cat:r-proj-space))
        (setf f (cat:face p))
        (dotimes (dlop 31)
          (format t "~%~A ~A"
                  (cat:hyphenize-list (cat:dlop-int-ext dlop))
                  (cat:nface f dlop 4 4)))))


(test check-faces
      (let ((d (cat:delta-infinity)))
        (is (cat:check-faces #'cat:f-cmpr (cat:face d) 4 31))))


(test check-smst
      (signals simple-error (cat:check-smst (cat:delta-infinity) 5))
      (is (cat:check-smst (cat:delta 3) 2))
      (is (cat:check-smst (cat:delta 3) 2 4)))
