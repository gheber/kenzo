;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN
;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN
;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN

(IN-PACKAGE #:cat)

(provide "kan")

(DEFUN KAN (idnm)
  (declare (fixnum idnm))
  (the (or null simplicial-group)
       (find idnm *kan-list* :key #'idnm)))


#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((kan kan) stream)
  (the kan
       (progn
	 (format stream "[K~D Kan-Simplicial-Set]" (idnm kan))
	 kan)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))


(DEFUN SMST-KAN (smst kfll)
  (declare
   (type simplicial-set smst)
   (type kfll kfll))
  (the kan
       (progn
	 (change-class smst 'kan)
	 (setf (slot-value smst 'kfll) kfll)
	 (pushnew smst *kan-list*)
	 smst)))


(DEFUN CHECK-HAT (kan indx dmns hat)
  (declare
   (type kan kan)
   (fixnum indx dmns)  ;;; dmns like in (funcall kfll ...)
   (list hat))
  (assert (= dmns (length hat)))
  (setf hat
	(append (subseq hat 0 indx)
		(list :hole)
		(nthcdr indx hat)))
  (do ((cmpr (cmpr kan))
       (j 0 (1+ j)))
      ((> j dmns) t)
    (declare
     (type cmprf cmpr)
     (fixnum j))
    (unless (= j indx)
      (let ((del-j (nth j hat)))
	(declare (type absm del-j))
	(do ((i j (1+ i)))
	    ((= i dmns))
	  (declare (fixnum i))
	  (unless (= i (1- indx))
	    (let ((del-i-del-j (face kan i (1- dmns) del-j))
		  (del-j-del-i+1 (face kan j (1- dmns)
				       (nth (1+ i) hat))))
	      (declare (type absm del-i-del-j del-j-del-i+1))
	      (assert (eq :equal
			  (a-cmpr3 cmpr del-i-del-j del-j-del-i+1))))))))))


(DEFUN CHECK-KAN (kan indx dmns hat)
  (declare
   (type kan kan)
   (fixnum indx dmns)
   (list hat))
  (the (values)
       (let ((rslt (kfll kan indx dmns hat))
	     (cmpr (cmpr kan))
	     (face (face kan)))
	 (declare
	  (type absm rslt)
	  (type cmprf cmpr)
	  (type face face))
	 (check-hat kan indx dmns hat)
	 (do ((i 0 (1+ i)))
	     ((> i dmns))
	   (declare (fixnum i))
	   (unless (= i indx)
	     (let ((x-i (pop hat))
		   (del-i-rslt (a-face4 face i dmns rslt)))
	       (declare (type absm x-i del-i-rslt))
	       (with-absm (dgop1 gmsm1) x-i
			  (with-absm
			      (dgop2 gmsm2) del-i-rslt
			      (unless (and (= dgop1 dgop2)
					   (eq (funcall cmpr gmsm1 gmsm2)
					       :equal))
				(error "Wrong result:~@
                            Fill = ~A~@
                            Del-~D(Fill) = ~A~@
                            Hat-~D = ~A"
				       rslt i del-i-rslt i x-i)))))))
	 (done))))
