;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)

(test finite-ss-pre-table
      (let ((p (cat-7:finite-ss-pre-table '(0 v0 v1 v2))))
        (signals simple-error (cat-7:finite-ss-pre-table '(0 v0 v0 v2)))
        (setf p (cat-7:finite-ss-pre-table '(0 v0 v1 1 e0 e1 e2)))
        (signals simple-error (cat-7:finite-ss-pre-table '(0 v0 v1 1 e0 e1 v1)))
        (setf p (cat-7:finite-ss-pre-table '(0 v0 (v0 v0))))
        (setf p (cat-7:finite-ss-pre-table '(0 v0 v1 v2 0 v3)))
        (signals simple-error (cat-7:finite-ss-pre-table '(0 v0 (v0 v0)
                                                         (v1 v1))))
        (signals simple-error (cat-7:finite-ss-pre-table '(0 (v0 v0) (v1 v1))))
        (signals #-(or ecl sbcl) simple-error #+(or ecl sbcl) type-error
                 (cat-7:finite-ss-pre-table '(0 v0 (v0 v0) #(1 2))))))


(test finite-ss-pre-table-table
      (let ((p (cat-7:finite-ss-pre-table-table
                (cat-7:finite-ss-pre-table '(2 v0 (e1 e2) v1 1 e0 e1 e2)))))))


(test finite-ss-table
      (cat-7:finite-ss-table '(*))
      (cat-7:finite-ss-table '(a b))
      (cat-7:finite-ss-table '(a b 1 c (b a)))
      (cat-7:finite-ss-table '(* 2 s2 3 s3))
      (cat-7:finite-ss-table '(s0 s1 s2 1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
                             2 s012 (s12 s02 s01)))
      (cat-7:finite-ss-table '(* 4 s4 ((2 1 0 *))))
      (signals simple-error (cat-7:finite-ss-table '(* 4 s4 ((1 1 0 *)))))
      (signals simple-error (cat-7:finite-ss-table '(* 1 s (t))))
      (signals simple-error (cat-7:finite-ss-table '(* 4 s4 ((1 0 *))))))


#|
TR is NIL. Why?

(test build-finite-ss
      (cat-7:cat-init)
      (let (tr (cat-7:build-finite-ss '(s0 s1 s2
                                      1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
                                      2 s012 (s12 s02 s01))))
        (cat-7:cmpr tr 's01 's02)
        (cat-7:basis tr 2)
        (cat-7:bspn tr)
        (cat-7:face tr 1 2 's012)
        (cat-7:? tr 2 's012)
        (cat-7:? tr (cat-7:? tr 2 's012))
        (mapcar #'(lambda (s) (length (eval s))) cat-7:*list-list*)
        (setf tr (cat-7:build-finite-ss '(s0 s1 s2
                                        1 s01 (s1 s1) s02 (s2 s0) s12 (s2 s1)
                                        2 s012 (s12 s02 s01))))
        (mapcar #'(lambda (s) (length (eval s))) cat-7:*list-list*)))
|#

(test sphere
      (cat-7:cat-init)
      (let ((s3 (cat-7:sphere 3))
            d)
        (funcall (cat-7:cmpr s3) 's3 's3)
        (dotimes (i 5)
          (print (funcall (cat-7:basis s3) i)))
        (mapcar #'(lambda (i) (funcall (cat-7:face s3) i 3 's3)) (cat-7:<a-b> 0 3))
        (cat-7:? s3 3 's3)
        (cat-7:smst (cat-7:idnm s3))
        (cat-7:chcm (cat-7:idnm s3))
        (setf d (cat-7:bndr s3))
        (cat-7:add d d)))


(test sphere-wedge
      (cat-7:cat-init)
      (let ((w (cat-7:sphere-wedge 3 2 3)))
        (funcall (cat-7:cmpr w) 's3-1 's3-2)
        (dotimes (i 5) (print (funcall (cat-7:basis w) i)))
        (funcall (cat-7:face w) 2 3 's3-1)
        (cat-7:gnrt-? (cat-7:bndr w) 3 's3-2)))


(test moore
      (cat-7:cat-init)
      (let ((m4 (cat-7:moore 2 4)))
        (cat-7:cmpr m4 'n5 'n5)
        (dotimes (i 7)
          (print (cat-7:basis m4 i)))
        (mapcar #'(lambda (i) (cat-7:face m4 i 5 'n5)) (cat-7:<a-b> 0 5))
        (cat-7:? m4 4 'm4)
        (cat-7:? m4 5 'n5)))


(test r-proj-space-basis
      (let ((b (cat-7:r-proj-space-basis 1 :infinity)))
        (dotimes (i 5) (print (funcall b i)))
        (setf b (cat-7:r-proj-space-basis 1 5))
        (dotimes (i 8) (print (funcall b i)))
        (setf b (cat-7:r-proj-space-basis 3 :infinity))
        (dotimes (i 5) (print (funcall b i)))
        (setf b (cat-7:r-proj-space-basis 3 6))
        (dotimes (i 10) (print (funcall b i)))))


(test R-proj-space
      (cat-7:cat-init)
      (let ((p (cat-7:R-proj-space)))
        (cat-7:basis p 4)
        (dotimes (i 5)
          (print (cat-7:face p i 4 4)))
        (dotimes (i 5)
          (print (cat-7:? p i i)))
        (setf dd (cat-7:cmps p p))
        (dotimes (i 6)
          (print (cat-7:? dd i i)))
        (setf p (cat-7:R-proj-space 3))
        (dotimes (i 7)
          (print (cat-7:basis p i)))
        (dotimes (i 5)
          (print (cat-7:face p i 4 4)))
        (dotimes (i 7)
          (print (cat-7:? p i i)))
        (setf dd (cat-7:cmps p p))
        (dotimes (i 7)
          (print (cat-7:? dd i i)))))

(test gmsms-subsmst
      (cat-7:cat-init)
      (let* ((k (cat-7:k-z 2))
             (efhm (cat-7:efhm k))
             g cmbn kf incl cone s2 cl2 f2 s3

             )
        (setf cat-7:*homology-verbose* nil)
        (setf g (first (cat-7:chcm-homology-gen (cat-7:rbcc efhm) 4)))
        (setf cmbn (cat-7:lf efhm (cat-7:rg efhm g)))
        (multiple-value-setq (kf incl) (cat-7:gmsms-subsmst k cmbn))
        (setf cone (cat-7:cone incl))
        (cat-7:efhm cone)
        (cat-7:homology kf 0 5)
        (cat-7:homology (cat-7:cone incl) 0 6)

        (setf g (first (cat-7:chcm-homology-gen (cat-7:rbcc efhm) 8)))
        (setf cmbn (cat-7:lf efhm (cat-7:rg efhm g)))
        (multiple-value-setq (kf incl) (cat-7:gmsms-subsmst k cmbn))
        (setf cone (cat-7:cone incl))
        (cat-7:efhm cone)
        (cat-7:homology kf 0 9)
        (cat-7:homology (cat-7:cone incl) 0 10)
        (dotimes (i 9) (print (length (cat-7:basis kf i))))

        (setf s2 (cat-7:sphere 2))
        (setf cl2 (cat-7:chml-clss s2 2))
        (setf f2 (cat-7:z-whitehead s2 cl2))
        (setf s3 (cat-7:fibration-total f2))
        (setf efhm (cat-7:efhm s3))
        (cat-7:basis (cat-7:rbcc efhm) 3)
        (setf cmbn (cat-7:lf efhm (cat-7:rg efhm 3 (first (cat-7:basis
                                                           (cat-7:rbcc efhm) 3)))))
        (setf kf (cat-7:gmsms-subsmst s3 cmbn))
        (cat-7:homology kf 0 4)))
