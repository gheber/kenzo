;; Fundamental classes
(IN-PACKAGE "COMMON-LISP-USER")


(DEFUN Zp-FUNDAMENTAL-GMSM (p dmns pi-elm)  ;; pi-elm not "equal" to 0
   (declare (fixnum dmns pi-elm))
   (the gmsm
      (if (= 1 dmns)
         (list pi-elm)
         (let ((bspn (if (= 2 dmns) nil +null-gbar+)))
            (declare (type gmsm bspn))
            (make-gbar :dmns dmns
               :list (cons (absm 0 (Zp-fundamental-gmsm p (1- dmns) pi-elm))
                        (mapcar
                           #'(lambda (k)
                                (declare (fixnum k))
                                (absm (mask k) bspn))
                           (nreverse (<a-b> 0 (- dmns 2))))))))))
#|

  (Zp-fundamental-gmsm 2 1 1)
  (Zp-fundamental-gmsm 2 2 1)
  (Zp-fundamental-gmsm 2 3 1)
  (Zp-fundamental-gmsm 2 4 1) 

  (Zp-fundamental-gmsm 5 1 1)
  (Zp-fundamental-gmsm 5 4 1)
  
|#


(DEFUN Zp-COCYCLE-GBAR (p n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z/pZ
  (declare
    (fixnum p n dmns)
    (list cocycle))
  (the absm
    (progn
      (mapc
        #'(lambda (cons)
	    (declare (cons cons))
            (setf (cdr cons)
                  (mod (cdr cons) p)))
       cocycle)
      (cond ((= n 1)
	     (g-bar-absm (cyclicgroup p) (nreverse (mapcar #'cdr cocycle))))
	    ((= n dmns)
	     (let ((value (cdr (first cocycle))))
	       (declare (fixnum value))
	       (if (zerop value)
		   (absm (mask dmns) +null-gbar+)
		 (absm 0 (zp-fundamental-gmsm p dmns value)))))
	    (t
	     (let ((cocycle1 +empty-list+)
		   (cocycle2 +empty-list+))
	       (declare (list cocycle1 cocycle2))
	       (dolist (cons cocycle)
		 (declare (cons cons))
		 (if (evenp (car cons))
		     (push (cons (ash (car cons) -1) (cdr cons))
			   cocycle1)
		   (push (cons (ash (car cons) -1) (cdr cons))
			 cocycle2)))
	       (setf cocycle1 (nreverse cocycle1)
		     cocycle2 (nreverse cocycle2))
	       (mapc
		#'(lambda (cons)
		    (declare (cons cons))
		    (when (evenp (car cons))
			  (setf (cdr cons)
				(mod (- (cdr cons)
					(cdr (assoc (1+ (car cons)) cocycle1)))
				     p))))
		cocycle2)
	       (let ((head-absm (zp-cocycle-gbar p (1- n) (1- dmns) cocycle2))
		     (tail-absm (zp-cocycle-gbar p n (1- dmns) cocycle1)))
		 (declare (type absm head-absm tail-absm))
		 (normalize-gbar
		  (cons dmns
			(cons head-absm
			      (rest (unnormalize-gbar tail-absm
						      (if (= n 2) () +null-gbar+)))))))))))))


(DEFUN Zp-COCYCLE-GBAR-HEAD (p n dmns cocycle)
  ;; cocycle \in Z^n(\Delta^{dmns}, \pi) with \pi = Z/pZ
  (declare
    (fixnum n dmns)
    (list cocycle))
  (the absm
    (progn
      (mapc
        #'(lambda (cons)
	    (declare (cons cons))
            (setf (cdr cons)
                  (mod (cdr cons) p)))
       cocycle)
      (cond ((= n 1)
	     (error "In Zp-COCYCLE-GBAR-HEAD, this point should not have been reached."))
		 ; (zp-bar-absm (nreverse (mapcar #'cdr cocycle))))
	    ((= n dmns)
	     (let ((value (cdr (first cocycle))))
	       (declare (fixnum value))
	       (if (zerop value)
		   (if (= n 2)
		       (absm 1 +empty-list+)
		     (absm (mask (1- dmns)) +null-gbar+))
		 (absm 0 (zp-fundamental-gmsm p (1- dmns) value)))))
	    (t
	     (let ((cocycle1 +empty-list+)
		   (cocycle2 +empty-list+))
	       (declare (list cocycle1 cocycle2))
	       (dolist (cons cocycle)
		 (declare (cons cons))
		 (if (evenp (car cons))
		     (push (cons (ash (car cons) -1) (cdr cons))
			   cocycle1)
		   (push (cons (ash (car cons) -1) (cdr cons))
			 cocycle2)))
	       (setf cocycle1 (nreverse cocycle1)
		     cocycle2 (nreverse cocycle2))
	       (mapc
		#'(lambda (cons)
		    (declare (cons cons))
		    (when (evenp (car cons))
			  (setf (cdr cons)
				(mod (- (cdr cons)
					(cdr (assoc (1+ (car cons)) cocycle1)))
				     p))))
		cocycle2)
	       (zp-cocycle-gbar p (1- n) (1- dmns) cocycle2)))))))





(DEFUN Zp-WHITEHEAD-SINTR (p smst n chml-clss
				&aux (face (face smst))
				     (k-zp-n-1 (k-zp p (1- n)))
				     (idnt (bspn k-zp-n-1))
				     (efhm (efhm smst)))
   (declare
      (type simplicial-set smst)
      (fixnum n)
      (type morphism chml-clss)
      (type face face)
      (type ab-simplicial-group k-zp-n-1)
      (type gmsm idnt)
      (type homotopy-equivalence efhm))
   (setf chml-clss
	 (i-cmps chml-clss (rf efhm) (lg efhm)))
   (flet ((rslt (dmns gmsm)
	    (declare
	      (fixnum dmns)
	      (type gmsm gmsm))
	    (if (< dmns n)
	        (absm (mask (1- dmns)) idnt)
	      (zp-cocycle-gbar-head p n dmns
		(gmsm-cocycle face n dmns gmsm chml-clss)))))
     (the sintr #'rslt)))

(DEFUN Zp-WHITEHEAD (p smst chml-clss &aux (n (- (degr chml-clss))))
   (declare
      (type simplicial-set smst)
      (fixnum p n)
      (type morphism chml-clss))
   (the fibration
      (build-smmr
         :sorc smst :trgt (k-zp p (1- n)) :degr -1
         :sintr (Zp-whitehead-sintr p smst n chml-clss)
         :orgn `(Zp-whitehead ,p ,smst))))


