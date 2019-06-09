;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test finite-ss-pre-table
      (let ((p (cat-9:finite-ss-pre-table '(0 v0 v1 v2))))
        (signals simple-error (cat-9:finite-ss-pre-table '(0 v0 v0 v2)))
        (setf p (cat-9:finite-ss-pre-table '(0 v0 v1 1 e0 e1 e2)))
        (signals simple-error (cat-9:finite-ss-pre-table '(0 v0 v1 1 e0 e1 v1)))
        (setf p (cat-9:finite-ss-pre-table '(0 v0 (v0 v0))))
        (setf p (cat-9:finite-ss-pre-table '(0 v0 v1 v2 0 v3)))
        (signals simple-error (cat-9:finite-ss-pre-table '(0 v0 (v0 v0)
                                                         (v1 v1))))
        (signals simple-error (cat-9:finite-ss-pre-table '(0 (v0 v0) (v1 v1))))
        (signals #-(or ecl sbcl) simple-error #+(or ecl sbcl) type-error
                 (cat-9:finite-ss-pre-table '(0 v0 (v0 v0) #(1 2))))))


(test finite-ss-pre-table-table
      (let ((p (cat-9:finite-ss-pre-table-table
                (cat-9:finite-ss-pre-table '(2 v0 (e1 e2) v1 1 e0 e1 e2)))))))


(test finite-ss-table
      (cat-9:finite-ss-table '(*))
      (cat-9:finite-ss-table '(a b))
      (cat-9:finite-ss-table '(a b 1 c (b a)))
      (cat-9:finite-ss-table '(* 2 s2 3 s3))
      (cat-9:finite-ss-table '(s0 s1 s2 1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
                             2 s012 (s12 s02 s01)))
      (cat-9:finite-ss-table '(* 4 s4 ((2 1 0 *))))
      (signals simple-error (cat-9:finite-ss-table '(* 4 s4 ((1 1 0 *)))))
      (signals simple-error (cat-9:finite-ss-table '(* 1 s (t))))
      (signals simple-error (cat-9:finite-ss-table '(* 4 s4 ((1 0 *))))))


#|
TR is NIL. Why?

(test build-finite-ss
      (cat-9:cat-init)
      (let (tr (cat-9:build-finite-ss '(s0 s1 s2
                                      1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
                                      2 s012 (s12 s02 s01))))
        (cat-9:cmpr tr 's01 's02)
        (cat-9:basis tr 2)
        (cat-9:bspn tr)
        (cat-9:face tr 1 2 's012)
        (cat-9:? tr 2 's012)
        (cat-9:? tr (cat-9:? tr 2 's012))
        (mapcar #'(lambda (s) (length (eval s))) cat-9:*list-list*)
        (setf tr (cat-9:build-finite-ss '(s0 s1 s2
                                        1 s01 (s1 s1) s02 (s2 s0) s12 (s2 s1)
                                        2 s012 (s12 s02 s01))))
        (mapcar #'(lambda (s) (length (eval s))) cat-9:*list-list*)))
|#

(test sphere
      (cat-9:cat-init)
      (let ((s3 (cat-9:sphere 3))
            d)
        (funcall (cat-9:cmpr s3) 's3 's3)
        (dotimes (i 5)
          (print (funcall (cat-9:basis s3) i)))
        (mapcar #'(lambda (i) (funcall (cat-9:face s3) i 3 's3)) (cat-9:<a-b> 0 3))
        (cat-9:? s3 3 's3)
        (cat-9:smst (cat-9:idnm s3))
        (cat-9:chcm (cat-9:idnm s3))
        (setf d (cat-9:bndr s3))
        (cat-9:add d d)))


(test sphere-wedge
      (cat-9:cat-init)
      (let ((w (cat-9:sphere-wedge 3 2 3)))
        (funcall (cat-9:cmpr w) 's3-1 's3-2)
        (dotimes (i 5) (print (funcall (cat-9:basis w) i)))
        (funcall (cat-9:face w) 2 3 's3-1)
        (cat-9:gnrt-? (cat-9:bndr w) 3 's3-2)))


(test moore
      (cat-9:cat-init)
      (let ((m4 (cat-9:moore 2 4)))
        (cat-9:cmpr m4 'n5 'n5)
        (dotimes (i 7)
          (print (cat-9:basis m4 i)))
        (mapcar #'(lambda (i) (cat-9:face m4 i 5 'n5)) (cat-9:<a-b> 0 5))
        (cat-9:? m4 4 'm4)
        (cat-9:? m4 5 'n5)))


(test r-proj-space-basis
      (let ((b (cat-9:r-proj-space-basis 1 :infinity)))
        (dotimes (i 5) (print (funcall b i)))
        (setf b (cat-9:r-proj-space-basis 1 5))
        (dotimes (i 8) (print (funcall b i)))
        (setf b (cat-9:r-proj-space-basis 3 :infinity))
        (dotimes (i 5) (print (funcall b i)))
        (setf b (cat-9:r-proj-space-basis 3 6))
        (dotimes (i 10) (print (funcall b i)))))


(test R-proj-space
      (cat-9:cat-init)
      (let ((p (cat-9:R-proj-space)))
        (cat-9:basis p 4)
        (dotimes (i 5)
          (print (cat-9:face p i 4 4)))
        (dotimes (i 5)
          (print (cat-9:? p i i)))
        (setf dd (cat-9:cmps p p))
        (dotimes (i 6)
          (print (cat-9:? dd i i)))
        (setf p (cat-9:R-proj-space 3))
        (dotimes (i 7)
          (print (cat-9:basis p i)))
        (dotimes (i 5)
          (print (cat-9:face p i 4 4)))
        (dotimes (i 7)
          (print (cat-9:? p i i)))
        (setf dd (cat-9:cmps p p))
        (dotimes (i 7)
          (print (cat-9:? dd i i)))))
