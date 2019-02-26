;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN
;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN
;;;  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN  KAN

(IN-PACKAGE #:cat-9)

(provide "kan")

(DEFVAR *KAN-LIST*)
(SETF *KAN-LIST* +empty-list+)
(PUSHNEW '*KAN-LIST* *list-list*)

(DEFUN KAN (idnm)
  (declare (type fixnum idnm))
  (the (or null simplicial-group)
     (find idnm *kan-list* :key #'idnm)))

(DEFMETHOD PRINT-OBJECT ((kan kan) stream)
  (declare (type stream stream))
  (the kan
     (progn
       (format stream "[K~D Kan-Simplicial-Set]" (idnm kan))
       kan)))

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

#|
()
(setf d (delta-infinity))
(defun vertex-i (absm i)
  (with-absm (dgop gmsm) absm
             (let ((dgop (nreverse (dgop-int-ext dgop)))
                   (gmsm (dlop-int-ext gmsm)))
               (do ((dgop-mark dgop)
                    (gmsm-mark gmsm)
                    (ii 0 (1+ ii)))
                   ((= i ii) (car gmsm-mark))
                 (if (and dgop-mark
                          (= ii (car dgop-mark)))
                     (pop dgop-mark)
                   (pop gmsm-mark))))))
(vertex-i (absm 0 1) 0)
(vertex-i (absm 1 1) 0)
(vertex-i (absm 1 1) 1)
(vertex-i (absm 0 3) 0)
(vertex-i (absm 0 3) 1)
(vertex-i (absm 3 1) 0)
(vertex-i (absm 3 1) 1)
(vertex-i (absm 3 1) 2)
(vertex-i (absm 1 3) 0)
(vertex-i (absm 1 3) 1)
(vertex-i (absm 1 3) 2)
(vertex-i (absm 2 3) 0)
(vertex-i (absm 2 3) 1)
(vertex-i (absm 2 3) 2)
(vertex-i (absm 0 7) 0)
(vertex-i (absm 0 7) 1)
(vertex-i (absm 0 7) 2)
(defun absm-int-ext (absm)
  (with-absm (dgop gmsm) absm
             (do ((dgop (nreverse (dgop-int-ext dgop)))
             (gmsm (rest (dlop-int-ext gmsm)))
                  (i 0 (1+ i))
                  (rslt (list (first (dlop-int-ext gmsm)))))
                 ((and (endp dgop) (endp gmsm)) (nreverse rslt))
               (if dgop
                   (if (= i (car dgop))
                       (progn
                         (pop dgop)
                         (push (first rslt) rslt))
                     (push (pop gmsm) rslt))
                 (return (nreconc rslt gmsm))))))
(defun absm-ext-int (vlist)
  (do ((dgop nil)
       (gmsm (list (first vlist)))
       (mark (rest vlist) (cdr mark))
       (idgop 0 (1+ idgop)))
      ((endp mark) (absm (dgop-ext-int dgop)
                         (dgop-ext-int gmsm)))
    (if (= (first gmsm) (car mark))
        (push idgop dgop)
      (push (car mark) gmsm))))
(absm-ext-int '(0 0 0 1 2 3 3 3))
(absm-ext-int '(0 1 1 1 2))
(absm-int-ext (absm-ext-int '(0 0 0 1 2 3 3 3)))
(absm-int-ext (absm-ext-int '(0 1 1 1 2)))
(defun dkfll (indx dmns hat)
  (cond ((= 1 dmns)
         (absm 1 (gmsm (first hat))))
        ((= 0 indx)
         (let ((del-1 (absm-int-ext (first hat)))
               (del-2 (absm-int-ext (second hat))))
           (absm-ext-int
            (cons (first del-1)
                  (cons (second del-2) (rest del-1))))))
        ((= 1 indx)
         (let ((del-0 (absm-int-ext (first hat)))
               (del-2 (absm-int-ext (second hat))))
              (absm-ext-int
               (cons (first del-2) del-0))))
        (t
         (let ((del-0 (absm-int-ext (first hat)))
               (del-1 (absm-int-ext (second hat))))
              (absm-ext-int
               (cons (first del-1) del-0))))))
(smst-kan d #'dkfll)
(kfll d 0 1 (list (absm 0 1)))
(kfll d 0 2 (list (absm 0 5) (absm 0 3)))
(kfll d 1 2 (list (absm 0 6) (absm 0 3)))
(kfll d 2 2 (list (absm 0 6) (absm 0 5)))
(kfll d 0 2 (list (absm 0 3) (absm 0 3)))
(kfll d 1 2 (list (absm 1 2) (absm 0 3)))
|#

(DEFUN CHECK-HAT (kan indx dmns hat)
  (declare
     (type kan kan)
     (type fixnum indx dmns)  ;;; dmns like in (funcall kfll ...)
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
	 (type fixnum j))
     (unless (= j indx)
       (let ((del-j (nth j hat)))
	 (declare (type absm del-j))
	 (do ((i j (1+ i)))
	     ((= i dmns))
	     (declare (type fixnum i))
	    (unless (= i (1- indx))
	      (let ((del-i-del-j (face kan i (1- dmns) del-j))
		    (del-j-del-i+1 (face kan j (1- dmns)
					 (nth (1+ i) hat))))
		(declare (type absm del-i-del-j del-j-del-i+1))
		(assert (eq :equal
			    (a-cmpr3 cmpr del-i-del-j del-j-del-i+1))))))))))

#|
  (setf od (gdeltab))
  (setf s (loop3 0 31 2))
  (setf faces (mapcar #'(lambda (indx) (face od indx 3 s))
                      '(0 1 2 3)))
  (delete (nth 1 faces) faces)
  (check-hat od 1 3 faces)
  (setf (nth 2 faces) (nth 1 faces))
  (check-hat od 1 3 faces)
|#


(DEFUN CHECK-KAN (kan indx dmns hat)
  (declare
     (type kan kan)
     (type fixnum indx dmns)
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
	  (declare (type fixnum i))
	  (unless (= i indx)
	    (let ((x-i (pop hat))
		  (del-i-rslt (a-face4 face i dmns rslt)))
	      (declare (type absm x-i del-i-rslt))
	      (with-absm (dgop1 gmsm1) x-i
	      (with-absm (dgop2 gmsm2) del-i-rslt
		 (unless (and (= dgop1 dgop2)
			      (eq (funcall cmpr gmsm1 gmsm2) :equal))
		    (error "Wrong result:~@
                            Fill = ~A~@
                            Del-~D(Fill) = ~A~@
                            Hat-~D = ~A"
		       rslt i del-i-rslt i x-i)))))))
       (done))))
