
(in-package :kenzo-test)

(in-suite :kenzo)

(test finite-ss-pre-table
      (let ((p (cat:finite-ss-pre-table '(0 v0 v1 v2))))
	(signals simple-error (cat:finite-ss-pre-table '(0 v0 v0 v2)))
	(setf p (cat:finite-ss-pre-table '(0 v0 v1 1 e0 e1 e2)))
	(signals simple-error (cat:finite-ss-pre-table '(0 v0 v1 1 e0 e1 v1)))
	(setf p (cat:finite-ss-pre-table '(0 v0 (v0 v0))))
	(setf p (cat:finite-ss-pre-table '(0 v0 v1 v2 0 v3)))
	(signals simple-error (cat:finite-ss-pre-table '(0 v0 (v0 v0)
							 (v1 v1))))
	(signals simple-error (cat:finite-ss-pre-table '(0 (v0 v0) (v1 v1))))
	(signals #-(or ecl sbcl) simple-error #+(or ecl sbcl) type-error
		 (cat:finite-ss-pre-table '(0 v0 (v0 v0) #(1 2))))))


(test finite-ss-pre-table-table
      (let ((p (cat:finite-ss-pre-table-table
		(cat:finite-ss-pre-table '(2 v0 (e1 e2) v1 1 e0 e1 e2)))))))


(test finite-ss-table
      (cat:finite-ss-table '(*))
      (cat:finite-ss-table '(a b))
      (cat:finite-ss-table '(a b 1 c (b a)))
      (cat:finite-ss-table '(* 2 s2 3 s3))
      (cat:finite-ss-table '(s0 s1 s2 1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
			     2 s012 (s12 s02 s01)))
      (cat:finite-ss-table '(* 4 s4 ((2 1 0 *))))
      (signals simple-error (cat:finite-ss-table '(* 4 s4 ((1 1 0 *)))))
      (signals simple-error (cat:finite-ss-table '(* 1 s (t))))
      (signals simple-error (cat:finite-ss-table '(* 4 s4 ((1 0 *))))))


#|
TR is NIL. Why?

(test build-finite-ss
      (cat:cat-init)
      (let (tr (cat:build-finite-ss '(s0 s1 s2
				      1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
				      2 s012 (s12 s02 s01))))
	(cat:cmpr tr 's01 's02)
        (cat:basis tr 2)
	(cat:bspn tr)
	(cat:face tr 1 2 's012)
	(cat:? tr 2 's012)
	(cat:? tr (cat:? tr 2 's012))
	(mapcar #'(lambda (s) (length (eval s))) cat:*list-list*)
	(setf tr (cat:build-finite-ss '(s0 s1 s2
					1 s01 (s1 s1) s02 (s2 s0) s12 (s2 s1)
					2 s012 (s12 s02 s01))))
	(mapcar #'(lambda (s) (length (eval s))) cat:*list-list*)))
|#

(test sphere
      (cat:cat-init)
      (let ((s3 (cat:sphere 3))
	    d)
	(funcall (cat:cmpr s3) 's3 's3)
	(dotimes (i 5)
	  (print (funcall (cat:basis s3) i)))
	(mapcar #'(lambda (i) (funcall (cat:face s3) i 3 's3)) (cat:<a-b> 0 3))
	(cat:? s3 3 's3)
	(cat:smst (cat:idnm s3))
	(cat:chcm (cat:idnm s3))
	(setf d (cat:bndr s3))
	(cat:add d d)))


(test sphere-wedge
      (cat:cat-init)
      (let ((w (cat:sphere-wedge 3 2 3)))
	(funcall (cat:cmpr w) 's3-1 's3-2)
	(dotimes (i 5) (print (funcall (cat:basis w) i)))
	(funcall (cat:face w) 2 3 's3-1)
	(cat:gnrt-? (cat:bndr w) 3 's3-2)))


(test moore
      (cat:cat-init)
      (let ((m4 (cat:moore 2 4)))
	(cat:cmpr m4 'n5 'n5)
	(dotimes (i 7)
	  (print (cat:basis m4 i)))
	(mapcar #'(lambda (i) (cat:face m4 i 5 'n5)) (cat:<a-b> 0 5))
	(cat:? m4 4 'm4)
	(cat:? m4 5 'n5)))


(test r-proj-space-basis
      (let ((b (cat:r-proj-space-basis 1 :infinity)))
	(dotimes (i 5) (print (funcall b i)))
	(setf b (cat:r-proj-space-basis 1 5))
	(dotimes (i 8) (print (funcall b i)))
	(setf b (cat:r-proj-space-basis 3 :infinity))
	(dotimes (i 5) (print (funcall b i)))
	(setf b (cat:r-proj-space-basis 3 6))
	(dotimes (i 10) (print (funcall b i)))))


(test R-proj-space
      (cat:cat-init)
      (let ((p (cat:R-proj-space)))
	(cat:basis p 4)
	(dotimes (i 5)
	  (print (cat:face p i 4 4)))
	(dotimes (i 5)
	  (print (cat:? p i i)))
	(setf dd (cat:cmps p p))
	(dotimes (i 6)
	  (print (cat:? dd i i)))
	(setf p (cat:R-proj-space 3))
	(dotimes (i 7)
	  (print (cat:basis p i)))
	(dotimes (i 5)
	  (print (cat:face p i 4 4)))
	(dotimes (i 7)
	  (print (cat:? p i i)))
	(setf dd (cat:cmps p p))
	(dotimes (i 7)
	  (print (cat:? dd i i)))))


(test gmsms-subsmst
      (cat:cat-init)
      (let* ((k (cat:k-z 2))
	     (efhm (cat:efhm k))
	     g cmbn kf incl cone s2 cl2 f2 s3

	     )
	(setf cat:*homology-verbose* nil)
	(setf g (first (cat:chcm-homology-gen (cat:rbcc efhm) 4)))
	(setf cmbn (cat:lf efhm (cat:rg efhm g)))
	(multiple-value-setq (kf incl) (cat:gmsms-subsmst k cmbn))
	(setf cone (cat:cone incl))
	(cat:efhm cone)
	(cat:homology kf 0 5)
	(cat:homology (cat:cone incl) 0 6)

	(setf g (first (cat:chcm-homology-gen (cat:rbcc efhm) 8)))
	(setf cmbn (cat:lf efhm (cat:rg efhm g)))
	(multiple-value-setq (kf incl) (cat:gmsms-subsmst k cmbn))
	(setf cone (cat:cone incl))
	(cat:efhm cone)
	(cat:homology kf 0 9)
	(cat:homology (cat:cone incl) 0 10)
	(dotimes (i 9) (print (length (cat:basis kf i))))

	(setf s2 (cat:sphere 2))
	(setf cl2 (cat:chml-clss s2 2))
	(setf f2 (cat:z-whitehead s2 cl2))
	(setf s3 (cat:fibration-total f2))
	(setf efhm (cat:efhm s3))
	(cat:basis (cat:rbcc efhm) 3)
	(setf cmbn (cat:lf efhm (cat:rg efhm 3 (first (cat:basis
						       (cat:rbcc efhm) 3)))))
	(setf kf (cat:gmsms-subsmst s3 cmbn))
	(cat:homology kf 0 4)))
