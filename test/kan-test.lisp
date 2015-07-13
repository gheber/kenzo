
(in-package :kenzo-test)

(in-suite :kenzo)

(defun vertex-i (absm i)
  (cat:with-absm
      (dgop gmsm) absm
      (let ((dgop (nreverse (cat:dgop-int-ext dgop)))
	    (gmsm (cat:dlop-int-ext gmsm)))
	(do ((dgop-mark dgop)
	     (gmsm-mark gmsm)
	     (ii 0 (1+ ii)))
	    ((= i ii) (car gmsm-mark))
	  (if (and dgop-mark
		   (= ii (car dgop-mark)))
	      (pop dgop-mark)
	      (pop gmsm-mark))))))


(test vertex-i
      (let ((d (cat:delta-infinity)))
	(vertex-i (cat:absm 0 1) 0)
	(vertex-i (cat:absm 1 1) 0)
	(vertex-i (cat:absm 1 1) 1)
	(vertex-i (cat:absm 0 3) 0)
	(vertex-i (cat:absm 0 3) 1)
	(vertex-i (cat:absm 3 1) 0)
	(vertex-i (cat:absm 3 1) 1)
	(vertex-i (cat:absm 3 1) 2)
	(vertex-i (cat:absm 1 3) 0)
	(vertex-i (cat:absm 1 3) 1)
	(vertex-i (cat:absm 1 3) 2)
	(vertex-i (cat:absm 2 3) 0)
	(vertex-i (cat:absm 2 3) 1)
	(vertex-i (cat:absm 2 3) 2)
	(vertex-i (cat:absm 0 7) 0)
	(vertex-i (cat:absm 0 7) 1)
	(vertex-i (cat:absm 0 7) 2)))


(defun absm-int-ext (absm)
  (cat:with-absm
      (dgop gmsm) absm
      (do ((dgop (nreverse (cat:dgop-int-ext dgop)))
	   (gmsm (rest (cat:dlop-int-ext gmsm)))
	   (i 0 (1+ i))
	   (rslt (list (first (cat:dlop-int-ext gmsm)))))
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
      ((endp mark) (cat:absm (cat:dgop-ext-int dgop)
			     (cat:dgop-ext-int gmsm)))
    (if (= (first gmsm) (car mark))
	(push idgop dgop)
	(push (car mark) gmsm))))


(test absm-int-ext
      (absm-ext-int '(0 0 0 1 2 3 3 3))
      (absm-ext-int '(0 1 1 1 2))
      (absm-int-ext (absm-ext-int '(0 0 0 1 2 3 3 3)))
      (absm-int-ext (absm-ext-int '(0 1 1 1 2))))


(defun dkfll (indx dmns hat)
  (cond ((= 1 dmns)
	 (cat:absm 1 (cat:gmsm (first hat))))
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


(let ((d (cat:delta-infinity)))
  (cat:smst-kan d #'dkfll)
  (cat:kfll d 0 1 (list (cat:absm 0 1)))
  (cat:kfll d 0 2 (list (cat:absm 0 5) (cat:absm 0 3)))
  (cat:kfll d 1 2 (list (cat:absm 0 6) (cat:absm 0 3)))
  (cat:kfll d 2 2 (list (cat:absm 0 6) (cat:absm 0 5)))
  (cat:kfll d 0 2 (list (cat:absm 0 3) (cat:absm 0 3)))
  (cat:kfll d 1 2 (list (cat:absm 1 2) (cat:absm 0 3))))


(test check-hat
      (let* ((od (cat:gdeltab))
	     (s (cat:loop3 0 31 2))
	     (faces (mapcar #'(lambda (indx) (cat:face od indx 3 s))
			    '(0 1 2 3))))
	(delete (nth 1 faces) faces)
	(cat:check-hat od 1 3 faces)
	(setf (nth 2 faces) (nth 1 faces))
	(signals simple-error (cat:check-hat od 1 3 faces))))
