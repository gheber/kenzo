;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES
;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES
;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES

(IN-PACKAGE #:cat)

(PROVIDE "loop-spaces")

#-sbcl (DEFINE-CONSTANT +NULL-LOOP+ (make-loop :list +empty-list+))
#+sbcl (DEFPARAMETER +NULL-LOOP+ (make-loop :list +empty-list+))

;;; NILOOP = non-normalized iloop; a common dgop is possible.

(DEFUN NORMALIZE-LOOP (dmns niloop)
  (declare (list niloop))
  (the absm
       (progn
         (unless niloop
	   (return-from normalize-loop
	     (absm (mask dmns) +null-loop+)))
         (let ((dgop (apply #'logand (mapcar #'car niloop))))
	   (declare (fixnum dgop))
	   (if (zerop dgop)
               (absm 0 (make-loop :list niloop))
               (absm dgop
		     (make-loop
		      :list (mapcar #'(lambda (apowr)
                                        (cons
					 (dgop/dgop (apdgop apowr) dgop)
					 (cdr apowr)))
				    niloop))))))))


(DEFUN UNNORMALIZE-LOOP (absm)
  (declare (type absm absm))
  (the iloop
       (with-absm (dgop loop) absm
		  (mapcar #'(lambda (apowr)
			      (declare (type apowr apowr))
			      (cons (dgop*dgop dgop (car apowr))
				    (cdr apowr)))
			  (loop-list loop)))))


(DEFUN LOOP3 (&rest list)
  (when (and (= 1 (length list))
             (listp (first list)))
    (setf list (first list)))
  (unless (zerop (mod (length list) 3))
    (error "In LOOP3, 3 should divide the list length."))
  (flet ((fixnumpp (object)
	   (unless (typep object 'fixnum)
	     (error "In LOOP3, ~A should be a fixnum." object))
	   object))
    (do ((rslt +empty-list+ (cons (apowr
                                   (fixnumpp (car mark))
                                   (cadr mark)
                                   (fixnumpp (caddr mark)))
                                  rslt))
         (mark list (cdddr mark)))
        ((endp mark) (make-loop :list (nreverse rslt)))
      (declare (list rslt mark)))))


(DEFUN LOOP-PRINT (loop stream depth)
  (declare
   (type loop loop) (stream stream)
   (ignore depth))
  (the loop
       (progn
	 (format stream "<<Loop")
	 (dolist (apowr (loop-list loop))
	   (declare (type apowr apowr))
	   (with-apowr (dgop gmsm expn) apowr
		       (format stream "[")
		       (unless (zerop dgop)
			 (format stream "~A " (hyphenize-list (dgop-int-ext dgop))))
		       (format stream "~A" gmsm)
		       (unless (= 1 expn)
			 (format stream "\\~D" expn))
		       (format stream "]")))
	 (format stream ">>")
	 loop)))


(DEFUN APOWR-NILOOP (cmpr apowr niloop)
  (declare
   (type cmprf cmpr)
   (type apowr apowr)
   (type iloop niloop))
  (the iloop
       (progn
         (unless niloop
	   (return-from apowr-niloop
	     (list apowr)))
         (let ((apowr2 (first niloop)))
	   (declare (type apowr apowr2))
	   (with-apowr (dgop2 gmsm2 expn2) apowr2
		       (unless (and (= (apdgop apowr) dgop2)
				    (eq :equal (funcall cmpr (apgmsm apowr)
							gmsm2)))
			 (return-from apowr-niloop
			   (cons apowr niloop)))
		       (incf expn2 (apexpn apowr))
		       (if (zerop expn2)
			   (rest niloop)
			   (cons (apowr dgop2 gmsm2 expn2) (rest niloop))))))))


(DEFUN LOOP-SPACE-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((2apowr-cmpr (apowr1 apowr2)
           (declare (type apowr apowr1 apowr2))
           (lexico
	    (f-cmpr (apdgop apowr1) (apdgop apowr2))
	    (funcall cmpr (apgmsm apowr1) (apgmsm apowr2))
	    (f-cmpr (apexpn apowr1) (apexpn apowr2)))))
    (flet ((rslt (loop1 loop2)
	     (declare (type loop loop1 loop2))
	     (maplexico #'2apowr-cmpr (loop-list loop1) (loop-list loop2))))
      (the cmprf #'rslt))))


(DEFUN APOWR-FACE4 (face indx dmns apowr)
  ;; dmns = dimension in the loop-space
  (declare
   (type face face)
   (type apowr apowr))
  (with-apowr (dgop gmsm expn) apowr
	      (let ((face (a-face4 face indx (1+ dmns) (absm dgop gmsm))))
		(declare (type absm face))
		(with-absm (dgop gmsm) face
			   (if (logbitp (1- dmns) dgop)
			       nil
			       (apowr dgop gmsm expn))))))


(DEFUN APOWR-LASTFACE4 (cmpr face dmns apowr)
  ;; indx =dmns
  (declare
   (type cmprf cmpr)
   (type face face)
   (fixnum dmns)
   (type apowr apowr))
  (the list
       (with-apowr
	   (dgop gmsm expn) apowr
	   (let ((absm (absm dgop gmsm)))
	     (declare (type absm absm))
	     (let ((face-n (a-face4 face dmns (1+ dmns) absm))
		   (face-n+1 (a-face4 face (1+ dmns) (1+ dmns) absm)))
	       (declare (type absm face-n face-n+1))
	       (with-absm
		   (dgop1 gmsm1) face-n
		   (with-absm
		       (dgop2 gmsm2) face-n+1
		       (if (logbitp (1- dmns) dgop1)
			   (if (logbitp (1- dmns) dgop2)
			       (return-from apowr-lastface4 +empty-list+)
			       (return-from apowr-lastface4
				 (list (apowr dgop2 gmsm2 (- expn)))))
			   (when (logbitp (1- dmns) dgop2)
			     (return-from apowr-lastface4
			       (list (apowr dgop1 gmsm1 expn)))))
		       (when (and (= dgop1 dgop2)
				  (eq :equal (funcall cmpr gmsm1 gmsm2)))
			 (return-from apowr-lastface4 +empty-list+))
		       (case expn
			 (+1 (list (apowr dgop2 gmsm2 -1) (apowr dgop1 gmsm1
								 +1)))
			 (-1 (list (apowr dgop1 gmsm1 -1) (apowr dgop2 gmsm2
								 +1)))
			 (otherwise
			  (if (plusp expn)
			      (do ((rslt +empty-list+ (cons apowr2
							    (cons apowr1
								  rslt)))
				   (apowr1 (apowr dgop1 gmsm1 +1))
				   (apowr2 (apowr dgop2 gmsm2 -1))
				   (nark expn (1- nark)))
				  ((zerop nark) rslt)
				(declare
				 (list rslt)
				 (type apowr apowr1 apowr2)))
			      (do ((rslt +empty-list+ (cons apowr1
							    (cons apowr2
								  rslt)))
				   (apowr1 (apowr dgop1 gmsm1 -1))
				   (apowr2 (apowr dgop2 gmsm2 +1))
				   (nark expn (1+ nark)))
				  ((zerop nark) rslt)
				(declare
				 (list rslt)
				 (type apowr apowr1 apowr2)))))))))))))


(DEFUN LOOP-SPACE-FACE (cmpr face)
  (declare
   (type cmprf cmpr)
   (type face face))
  (flet ((rslt (indx dmns loop)
	   (declare
	    (fixnum indx dmns)
	    (type loop loop))
	   (the absm
                (let ((rslt +empty-list+))
		  (declare (type iloop rslt))
		  (if (< indx dmns)
                      (dolist (apowr (loop-list loop))
			(declare (type apowr apowr))
			(let ((lface (apowr-face4 face indx dmns apowr)))
			  (declare (type (or null apowr) lface))
			  (when lface (setf rslt (apowr-niloop cmpr lface
							       rslt)))))
                      (dolist (apowr (loop-list loop))
			(declare (type apowr apowr))
			(let ((lface (apowr-lastface4 cmpr face dmns apowr)))
			  (declare (list lface))
			  (dolist (apowr lface)
			    (declare (type apowr apowr))
			    (setf rslt (apowr-niloop cmpr apowr rslt))))))
		  (normalize-loop (1- dmns) (nreverse rslt))))))
    (the face #'rslt)))


(DEFUN LOOP-SPACE-FACE* (cmpr face)
  (declare
   (type cmprf cmpr)
   (type face face))
  (flet ((rslt (indx dmns loop)
	   (declare
	    (fixnum indx dmns)
	    (type loop loop))
	   (the (or loop (eql :degenerate))
                (let ((rslt +empty-list+))
		  (declare (type iloop rslt))
		  (if (< indx dmns)
                      (dolist (apowr (loop-list loop))
			(declare (type apowr apowr))
			(let ((lface (apowr-face4 face indx dmns apowr)))
			  (declare (type (or null apowr) lface))
			  (when lface (setf rslt (apowr-niloop cmpr lface
							       rslt)))))
                      (dolist (apowr (loop-list loop))
			(declare (type apowr apowr))
			(let ((lface (apowr-lastface4 cmpr face dmns apowr)))
			  (declare (list lface))
			  (dolist (apowr lface)
			    (declare (type apowr apowr))
			    (setf rslt (apowr-niloop cmpr apowr rslt))))))
		  (if (zerop (apply #'logand (mapcar #'car rslt)))
                      (make-loop :list (nreverse rslt))
                      :degenerate)))))
    (the face* #'rslt)))


(DEFUN LOOP-SPACE-GRIN-SINTR (dmns loop)
  (declare
   (ignore dmns)
   (type loop loop))
  (the absm
       (absm 0 (make-loop
		:list (mapcar #'(lambda (apowr)
				  (declare (type apowr apowr))
				  (with-apowr (dgop gmsm expn) apowr
					      (apowr dgop gmsm (- expn))))
			      (reverse (loop-list loop)))))))


(DEFUN LOOP-SPACE-GRML-SINTR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (dmns crpr)
	   (declare
	    (fixnum dmns)
	    (type crpr crpr))
	   (the absm
		(with-crpr
		    (absm1 absm2) crpr
		    (let ((niloop1 (nreverse (unnormalize-loop absm1)))
			  (niloop2 (unnormalize-loop absm2)))
		      (declare (type iloop niloop1 niloop2))
		      (loop
			 (unless niloop1 (return))
			 (unless niloop2 (return))
			 (let ((apowr1 (first niloop1))
			       (apowr2 (first niloop2)))
			   (declare (type apowr apowr1 apowr2))
			   (with-apowr
			       (dgop1 gmsm1 expn1) apowr1
			       (with-apowr
				   (dgop2 gmsm2 expn2) apowr2
				   (unless (and (= dgop1 dgop2)
						(eq :equal (funcall cmpr
								    gmsm1
								    gmsm2)))
				     (return))
				   (incf expn1 expn2)
				   (cond ((zerop expn1)
					  (pop niloop1)
					  (pop niloop2))
					 (t
					  (pop niloop1)
					  (setf niloop2
						(cons (apowr dgop2 gmsm2 expn1)
						      (rest niloop2)))
					  (return)))))))
		      (normalize-loop dmns
				      (nreconc niloop1 niloop2)))))))
    (the sintr #'rslt)))


(DEFUN LOOP-SPACE (smst &optional (n 1))
  (declare
   (type simplicial-set smst)
   (fixnum n))
  (the simplicial-group
       (if (= n 1)
	   (with-slots (cmpr face) smst
	     (declare
	      (type cmprf cmpr)
	      (type face face))
	     (build-smgr
	      :cmpr (loop-space-cmpr cmpr)
	      :basis :locally-effective
	      :bspn +null-loop+
	      :face (loop-space-face cmpr face)
	      :face* (loop-space-face* cmpr face)
	      :sintr-grml (loop-space-grml-sintr cmpr)
	      :sintr-grin #'loop-space-grin-sintr
	      :orgn `(loop-space ,smst)))
	   (loop-space (loop-space smst) (1- n)))))


(DEFUN GDELTAB ()
  (loop-space (deltab)))
