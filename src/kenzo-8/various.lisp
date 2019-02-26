;;;  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS
;;;  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS
;;;  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS  VARIOUS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "various")

(DEFINE-CONSTANT +EMPTY-LIST+ '())

(DEFINE-CONSTANT +F-EMPTY-VECTOR+
   (make-array 0 :element-type 'fixnum))

(DEFINE-CONSTANT +S-EMPTY-VECTOR+
   #())

(DEFINE-CONSTANT +TRUE+ t)

(DEFINE-CONSTANT +FALSE+ nil)

(DEFINE-CONSTANT +2-EXP+
  (let ((rslt (make-array (integer-length most-positive-fixnum)
                        :element-type 'fixnum)))
    (declare (type (array fixnum 1) rslt))
    (dotimes (i (integer-length most-positive-fixnum) rslt)
      (setf (aref rslt i) (the fixnum (expt 2 i))))))

(DEFINE-CONSTANT +MASK+
  (let ((rslt (make-array (integer-length most-positive-fixnum)
                        :element-type 'fixnum)))
    (declare (type (array fixnum 1) rslt))
    (dotimes (i (integer-length most-positive-fixnum) rslt)
      (setf (aref rslt i) (the fixnum (1- (expt 2 i)))))))

(DEFUN BINOMIAL-N-P (n p)
  (declare (fixnum n p))
  (when (> (2-exp p) n)
     (setf p (- n p)))
  (the fixnum
     (do ((nn n (1- nn))
	  (pp 1 (1+ pp))
	  (rslt 1 (/ (* rslt nn) pp)))
	 ((> pp p) rslt)
	(declare (fixnum nn pp rslt)))))

#|
  (dotimes (i 6)
     (print (binomial-n-p 5 i)))
  (dotimes (i 6)
     (print (binomial-p-q (- 5 i) i))))
|#

(SETF *PRINT-LEVEL* 5
  *PRINT-LENGTH* 10)

(DEFUN <A-B< (a b)
   (declare (fixnum a b))
   (the list
      (do ((i (1- b) (1- i))
           (rslt +empty-list+ (cons i rslt)))
          ((< i a) rslt)
         (declare (fixnum i) (list rslt)))))
 
(DEFUN <A-B> (a b)
  (declare (fixnum a b))
 (the list
   (do ((i b (1- i))
       (rslt +empty-list+ (cons i rslt)))
      ((< i a) rslt)
    (declare (fixnum i) (list rslt)))))

(DEFUN >A-B< (a b)
  (declare (fixnum a b))
 (the list
  (do ((i (1- b) (1- i))
       (rslt +empty-list+ (cons i rslt)))
      ((= i a) rslt)
    (declare (fixnum i) (list rslt)))))

(DEFUN >A-B> (a b)
  (declare (fixnum a b))
 (the list
  (do ((i b (1- i))
       (rslt +empty-list+ (cons i rslt)))
      ((= i a) rslt)
    (declare (fixnum i) (list rslt)))))

#|
  (<a-b< 0 5)
  (<a-b> 0 5)
  (>a-b< 0 5)
  (>a-b> 0 5))
|#

(DEFUN V<A-B> (a b)
   (declare (fixnum a b))
 (the (vector fixnum)
    (let ((rslt (make-array (1+ (- b a)) :element-type 'fixnum)))
       (declare (type (vector fixnum) rslt))
       (do ((i a (1+ i))
            (mark 0 (1+ mark)))
           ((> i b))
          (declare (fixnum i mark))
          (setf (aref rslt mark) i))
       rslt)))

#|
  (v<a-b> -5 5))
|#

(DEFUN SRANDOM (max)
   (declare (fixnum max))
   (the fixnum
      (let ((rslt (- (random (+ max max)) max)))
         (declare (fixnum rslt))
         (if (zerop rslt)
            max
            rslt))))

#|
  (dotimes (i 20) (print (srandom 3))))
|#

(DEFUN CLOCK ()
  (multiple-value-bind (sec min hour day month year)
		       (get-decoded-time)
    (format t "~%;; Clock -> ~4D-~2,'0D-~2,'0D, ~Dh ~Dm ~Ds.~%"
	      year month day hour min sec))
  (values))

#|
  (clock))
|#

(DEFUN DONE ()
   (format t "~%---done---")
   (values))
   
#|
  (done))
|#



