;;;  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH
;;;  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH
;;;  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH  SMITH

(IN-PACKAGE #:cat-8)

(PROVIDE "smith")

(DEFTYPE MATRIX () '(array fixnum (* *)))


(DEFUN RANDOM-MATRIX (line-n column-n max)
  (declare (fixnum line-n column-n max))
  (the matrix
    (let ((rslt (make-array (list line-n column-n)
			    :element-type 'fixnum))
	  (2max+1 (+ max max 1)))
      (declare
        (type matrix rslt)
	(fixnum 2max+1))
      (dotimes (i line-n)
	(declare (fixnum i))
	(dotimes (j column-n)
	  (declare (fixnum j))
	  (setf (aref rslt i j)
		(- (random 2max+1) max))))
      rslt)))

#|
()
(random-matrix 2 3 10)
|#

(DEFUN IDNT-MTRX (n)
  (declare (fixnum n))
  (the matrix
    (let ((rslt (if (zerop n)
                    (make-array '(0 0)
                                :element-type 'fixnum)
                  (make-array (list n n)
                              :element-type 'fixnum
                              :initial-element 0))))
      (declare (type matrix rslt))
      (dotimes (i n)
	(declare (fixnum i))
	(setf (aref rslt i i) 1))
      rslt)))

#|
()
(idnt-mtrx 3)
|#

(DEFUN COPY-MTRX (mtrx)
  (declare (type matrix mtrx))
  (the matrix
    (let ((line-n (line-number mtrx))
	  (column-n (column-number mtrx)))
      (declare (fixnum line-n column-n))
      (let ((rslt (make-array (list line-n column-n)
			      :element-type 'fixnum)))
	(declare (type matrix rslt))
	(dotimes (il line-n)
	  (declare (fixnum il))
	(dotimes (ic column-n)
	  (declare (fixnum ic))
	  (setf (aref rslt il ic) (aref mtrx il ic))))
	rslt))))

#|
()
(setf m (random-matrix 3 4 10))
(equalp m (copy-mtrx m))
|#

(DEFUN LEFT-SUBMATRIX (mtrx k)
  (declare (type matrix mtrx))
  (the matrix
    (let ((line-n (line-number mtrx))
          (column-n (column-number mtrx)))
      (declare (fixnum line-n column-n))
      (assert (<= k column-n))
      (let ((rslt (make-array (list line-n k)
			      :element-type 'fixnum)))
	(declare (type matrix rslt))
	(dotimes (il line-n)
	  (declare (fixnum il))
	(dotimes (ic k)
	  (declare (fixnum ic))
	  (setf (aref rslt il ic) (aref mtrx il ic))))
	rslt))))

#|
()
(setf m (random-matrix 3 4 10))
(left-submatrix m 2)
|#

(DEFUN MTRX-PRDC (mtrx1 mtrx2)
  (declare (type matrix mtrx1 mtrx2))
  (the matrix
    (let ((rslt-line-n (line-number mtrx1))
	  (mtrx1-column-n (column-number mtrx1))
	  (mtrx2-line-n (line-number mtrx2))
	  (rslt-column-n (column-number mtrx2)))
      (declare (fixnum rslt-line-n mtrx1-column-n
		       mtrx2-line-n rslt-column-n))
      (unless (= mtrx1-column-n mtrx2-line-n)
	(error "In MTRX-PRDC, bad line or column number."))
      (let ((rslt (make-array (list rslt-line-n rslt-column-n)
			      :element-type 'fixnum)))
	(declare (type matrix rslt))
	(dotimes (il rslt-line-n)
	  (declare (fixnum il))
	(dotimes (ic rslt-column-n)
	  (declare (fixnum ic))
	  (do ((k 0 (1+ k))
	       (sum 0 (+ sum
			 (* (aref mtrx1 il k) (aref mtrx2 k ic)))))
	      ((= k mtrx1-column-n) (setf (aref rslt il ic) sum)))))
	rslt))))

#|
()
(setf m1 (random-matrix 2 3 10))
(setf m2 (random-matrix 3 2 10))
(mtrx-prdc m1 m2)
(mtrx-prdc m2 m1)
|#

(DEFUN CHCM-MTRX (chcm degr)
  (declare
   (type chain-complex chcm)
   (fixnum degr))
  (the matrix
    (let ((cmpr (cmpr1 chcm))
          (dffr (dffr chcm))
          (sbasis (basis chcm degr))
          (tbasis (basis chcm (1- degr))))
      (declare
       (type cmprf cmpr)
       (type morphism dffr)
       (list sbasis tbasis))
      (let ((srank (length sbasis))
            (trank (length tbasis)))
        (declare (fixnum srank trank))
        (let ((rslt (make-array (list trank srank)
                                :element-type 'fixnum
                                :initial-element 0)))
          (declare (type matrix rslt))
          (do ((j 0 (1+ j))
               (mark sbasis (cdr mark)))
              ((endp mark))
            (declare
             (fixnum j)
             (list mark))
            (let ((cmbn (gnrt-? dffr degr (car mark))))
              (declare (type cmbn cmbn))
              (do ((mark1 (cmbn-list cmbn) (cdr mark1))
                   (mark2 tbasis)
                   (i 0))
                  ((endp mark1))
                (declare
                 (list mark1 mark2)
                 (fixnum i))
                (with--term (cffc gnrt) mark1
                            (loop
                              (cond ((eq :equal (funcall cmpr gnrt (car mark2)))
                                     (setf (aref rslt i j) cffc)
                                     (incf i)
                                     (pop mark2)
                                     (return))
                                    (t (incf i)
                                       (pop mark2))))))))
          rslt)))))

#|
()
(setf d (delta 5))
(chcm-mtrx d 3)
(mtrx-prdc (chcm-mtrx d 2) (chcm-mtrx d 3))
(setf m (moore 2 2))
(dotimes (i 5)
  (print (chcm-mtrx m i)))
(dotimes (i 6)
  (print (array-dimensions (chcm-mtrx m i))))
|#

(DEFUN LINE-OP (mtrx begin lambda line1 line2)
  ;; line2 := line2 + lambda * line1
  ;; but leaving unchanged the terms in columns [0..begin[
  (declare
   (type matrix mtrx)
   (fixnum begin lambda line1 line2))
  (the matrix
    (progn
      (do ((column-number (column-number mtrx))
           (j begin (1+ j)))
          ((= j column-number))
        (declare (fixnum column-number j))
        (incf (aref mtrx line2 j)
              (* lambda (aref mtrx line1 j))))
      mtrx)))
	
#|
()
(setf m (random-matrix 3 4 10))
(line-op m 1 3 2 0)
|#

;;; mtrx-list = (P P^-1 M Q Q^-1)

;;; All the standard operations on lines and colums
;;; have a zzz-5 version organised as follows.
;;; The list returned leaves unchanged the product P M Q^(-1).

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(line-op-5 list 0 3 1 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
|#

(DEFUN COLUMN-OP (mtrx begin lambda column1 column2)
  ;; column2 := column2 + lambda * column1
  ;; but leaving unchanged the terms in lines [0..begin[
  (declare
    (type matrix mtrx)
    (fixnum begin lambda column1 column2))
  (the matrix
  (progn
  (do ((line-number (line-number mtrx))
       (i begin (1+ i)))
      ((= i line-number))
      (declare (fixnum line-number i))
    (incf (aref mtrx i column2)
	  (* lambda (aref mtrx i column1))))
  mtrx)))
	
#|
()
(setf m (random-matrix 3 4 10))
(column-op m 1 3 2 0)
|#

#|
()
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(column-op-5 list 0 3 1 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#


(DEFUN LINE-SWAP (mtrx begin line1 line2)
  ;; swaps line1 and line2, leaving unchanged
  ;; the terms of column-index < begin
  (declare
    (type matrix mtrx)
    (fixnum begin line1 line2))
  (the matrix
  (progn
  (do ((column-number (column-number mtrx))
       (j begin (1+ j)))       
      ((= j column-number))
      (declare (fixnum column-number j))
    (rotatef (aref mtrx line1 j) (aref mtrx line2 j)))
  mtrx)))

#|
(setf m (random-matrix 3 4 10))
(line-swap m 1 0 2)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(line-swap-5 list 0 1 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN COLUMN-SWAP (mtrx begin column1 column2)
  ;; swaps column1 and column2, leaving unchanged
  ;; the terms of line-index < begin
  (declare
    (type matrix mtrx)
    (fixnum begin column1 column2))
  (the matrix (progn
  (do ((line-number (line-number mtrx))
       (i begin (1+ i)))       
      ((= i line-number))
      (declare (fixnum line-number i))
    (rotatef (aref mtrx i column1) (aref mtrx i column2)))
  mtrx)))

#|
(setf m (random-matrix 3 4 10))
(column-swap m 1 0 2)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(column-swap-5 list 0 1 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN LINE-MINUS (mtrx begin line)
  ;; line := - line
  ;; leaving unchanged the terms of column-index < begin.
  (declare
    (type matrix mtrx)
    (fixnum begin line))
  (the matrix (progn
  (do ((column-number (column-number mtrx))
       (j begin (1+ j)))
      ((= j column-number))
      (declare (fixnum column-number j))
    (setf (aref mtrx line j) (- (aref mtrx line j))))
  mtrx)))

(DEFUN COLUMN-MINUS (mtrx begin column)
  ;; column := - column
  ;; leaving unchanged the terms of line-index < begin.
  (declare
    (type matrix mtrx)
    (fixnum begin column))
  (the matrix (progn
  (do ((line-number (line-number mtrx))
       (i begin (1+ i)))
      ((= i line-number))
      (declare (fixnum line-number i))
    (setf (aref mtrx i column) (- (aref mtrx i column))))
  mtrx)))

#|
(setf m (random-matrix 3 4 10))
(line-minus m 1 2)
(column-minus m 1 2)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(line-minus-5 list 0 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
(column-minus-5 list 0 2)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN MINIMAL-TERM (matrix begin)
  (declare
   (type matrix matrix)
   (fixnum begin))
  (the (values fixnum fixnum fixnum)
    (do ((line-number (line-number matrix))
         (column-number (column-number matrix))
         (min 0)
         (min-il -1)
         (min-ic -1)
         (il begin (1+ il)))
        ((= il line-number) (values min min-il min-ic))
      (declare (fixnum line-number column-number
                       min min-il min-ic il))
      (do ((ic begin (1+ ic)))
          ((= ic column-number))
        (declare (fixnum ic))
        (let ((term (abs (aref matrix il ic))))
          (declare (fixnum term))
          (when (= term 1)
            (return-from minimal-term (values 1 il ic)))
          (when (plusp term)
            (when (or (< term min)
                      (zerop min))               
              (setf min term
                min-il il
                min-ic ic))))))))

#|
()
(setf m (random-matrix 4 5 10))
(minimal-term m 1)
|#

(DEFUN MINIMAL-REST-1 (matrix begin)
  ;; Let c (= corner) the term M_{b,b} (b = begin).
  ;; This function looks for the minimal rest of the division
  ;; of M_{il,ic} by c
  ;;              for il = begin and ic > begin
  ;;               or ic = begin and il > begin
  (declare
   (type matrix matrix)
   (fixnum begin))
  (the (values fixnum fixnum fixnum)
    (let ((line-number (line-number matrix))
          (column-number (column-number matrix))
          (corner (aref matrix begin begin))
          (min 0)
          (min-il -1)
          (min-ic -1))
      (declare (fixnum corner min min-il min-ic))
      (do ((ic (1+ begin) (1+ ic)))
          ((= ic column-number))
        (declare (fixnum ic))
        (let ((term (abs (second (multiple-value-list (round (aref matrix begin ic) 
                                                             corner))))))
          (declare (fixnum term))
          (when (= term 1)
            (return-from minimal-rest-1 (values 1 begin ic)))
          (when (plusp term)
            (when (or (< term min)
                      (zerop min))               
              (setf min term
                min-il begin
                min-ic ic)))))
      (do ((il (1+ begin) (1+ il)))
          ((= il line-number))
        (declare (fixnum il))
        (let ((term (abs (second (multiple-value-list (round (aref matrix il begin)
                                                             corner))))))
          (declare (fixnum term))
          (when (= term 1)
            (return-from minimal-rest-1 (values 1 il begin)))
          (when (plusp term)
            (when (or (< term min)
                      (zerop min))               
              (setf min term
                min-il il
                min-ic begin)))))
      (values min min-il min-ic))))

#|
()
(setf m (random-matrix 4 5 10))
(minimal-rest-1 m 1)
|#

(DEFUN MINIMAL-REST-2 (matrix begin)
  ;; Let c (= corner) the term M_{b,b} (b = begin).
  ;; This function looks for the minimal rest of the division
  ;; of M_{il,ic} by c for il > begin and ic > begin.
  (declare
   (type matrix matrix)
   (fixnum begin))
  (the (values fixnum fixnum fixnum)
    (let ((line-number (line-number matrix))
          (column-number (column-number matrix))		      
          (corner (aref matrix begin begin))
          (min 0)
          (min-il  -1)
          (min-ic -1))
      (declare (fixnum corner min min-il min-ic))
      (do ((il (1+ begin) (1+ il)))
          ((= il line-number))
        (declare (fixnum il))
        (do ((ic (1+ begin) (1+ ic)))
            ((= ic column-number))
          (declare (fixnum il))
          (let ((term (abs (second (multiple-value-list (round (aref matrix il ic)
                                                               corner))))))
            (declare (fixnum term))
            (when (= 1 term)
              (return-from minimal-rest-2 (values 1 il ic)))
            (when (plusp term)
              (when (or (< term min)
                        (zerop min))
                (setf min term
                  min-il il
                  min-ic ic))))))
      (values min min-il min-ic))))

#|
()
(setf m (random-matrix 4 5 10))
(minimal-rest-2 m 1)
|#

(DEFUN MINIMAL-TERM-TOP-LEFT (mtrx-list begin il ic)
  (declare
   (list mtrx-list)
   (fixnum begin il ic))
  (the list
    (progn
      (when (< begin il)
        (line-swap-5 mtrx-list begin begin il))
      (when (< begin ic)
        (column-swap-5 mtrx-list begin begin ic))
      (when (minusp (aref (third mtrx-list) begin begin))
        (line-minus-5 mtrx-list begin begin))
      mtrx-list)))

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(minimal-term-top-left list 0 1 3)
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1))
|#

(DEFUN PIVOTT (mtrx-list begin)
  (declare
   (list mtrx-list)
   (fixnum begin))
  (the list (progn
    (let ((line-number (line-number (third mtrx-list)))
          (column-number (column-number (third mtrx-list)))
          (corner (aref (third mtrx-list) begin begin)))
      (declare (fixnum line-number column-number corner))
      (do ((il (1+ begin) (1+ il)))
          ((= il line-number))
        (declare (fixnum il))
        (line-op-5 mtrx-list begin
                   (- (round (aref (third mtrx-list) il begin) corner))
                   begin il))
      (do ((ic (1+ begin) (1+ ic)))
          ((= ic column-number))
        (declare (fixnum ic))
        (column-op-5 mtrx-list begin
                     (- (round (aref (third mtrx-list) begin ic) corner))
                     begin ic)))
              mtrx-list)))

#|
()
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf (aref m 0 0) -1)
m
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(pivott list 0)
m
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN LIST-SMITH (mtrx-list)
  (declare (list mtrx-list))
  (the list (progn
  (let ((matrix (third mtrx-list))
	(begin 0))
    (declare
      (type matrix matrix)
      (fixnum begin))
   (loop
      (multiple-value-bind (term il ic) (minimal-term matrix begin)
	(declare (fixnum term il ic))
        ;; (format t "~%*BEGIN* = ~D ; MIN = ~D." begin term)
	(when (zerop term)
	  (return-from list-smith mtrx-list))
	(minimal-term-top-left mtrx-list begin il ic))
      (loop
       (multiple-value-bind (term il ic) (minimal-rest-1 matrix begin)
	 (declare (fixnum term il ic))
	 (cond ((zerop term)
		(pivott mtrx-list begin)
		(multiple-value-bind (term il ic) (minimal-rest-2 matrix begin)
		  (declare
		    (fixnum term il)
		    (ignore ic))
		  (if (zerop term)
		      (return)
		    (line-op-5 mtrx-list begin 1 il begin))))
	       ((= il begin)
		(column-op-5 mtrx-list begin
			     (- (round (aref matrix begin ic)
				       (aref matrix begin begin)))
			     begin ic)
		(column-swap-5 mtrx-list begin begin ic)
		(when (minusp (aref matrix begin begin))
                      (column-minus-5 mtrx-list begin begin)))
	       (t
		(line-op-5 mtrx-list begin
			   (- (round (aref matrix il begin)
				     (aref matrix begin begin)))
			    begin il)
		(line-swap-5 mtrx-list begin begin il)
		(when (minusp (aref matrix begin begin))
		      (column-minus-5 mtrx-list begin begin))))))
      ;; (Format t "~%  Finally the diagonal term is ~D." (aref matrix begin begin))
      (incf begin)))
  mtrx-list)))

#|
()
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(third (list-smith list))
(equalp t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN SMITH (matrix)
  (declare (type matrix matrix))
  (the list
    (let ((line-n (line-number matrix))
          (column-n (column-number matrix)))
      (declare (fixnum line-n column-n))
      (list-smith
       (list (idnt-mtrx line-n) (idnt-mtrx line-n)
             matrix
             (idnt-mtrx column-n) (idnt-mtrx column-n))))))


;;; ECHCM -> The same without the first epimorphism
;;;
;;; epi: C_f <- C_{f+1}
;;  rank(C_f) = n
;;  rank(C_{f+1}) = m
;;  f = first

(eval-when (:compile-toplevel :load-toplevel :execute)
  (UNLESS (FIND-PACKAGE "GNRTS")
    (make-package "GNRTS"))
  (DEFINE-CONSTANT +GNRTS-PCKG+
    (find-package "GNRTS")))

(DEFUN GNRT-NAME-BASIS (n)
  (declare (fixnum n))
  (the list
  (do ((i (1- n) (1- i))
       (rslt +empty-list+ (cons (gnrt-name i) rslt)))
      ((minusp i) rslt)
      (declare
        (fixnum i)
	(list rslt)))))

#|
(gnrt-name 4)
(gnrt-name-basis 4)
|#

(DEFUN ECHCM-KILL-EPI-F-INTR (cmpr first n m f+1-basis mtrx-list)
  (declare
    (type cmprf cmpr)
    (fixnum first n m)
    (list f+1-basis mtrx-list))
  (let ((f+1 (1+ first))
	(m-n (- m n))
	(q-1 (fifth mtrx-list)))
    (declare
      (fixnum f+1 m-n)
      (type matrix q-1))
    (flet ((rslt (cmbn)
	     (declare (type cmbn cmbn))
	     (with-cmbn (degr list) cmbn
	       (when (= degr first)
		 (return-from rslt (zero-cmbn degr)))
	       (unless (= degr f+1)
		 (return-from rslt cmbn))
                 (let ((rslt-cffcs (make-array m-n :element-type 'fixnum
                                               :initial-element 0)))
                   (declare (type (array fixnum *) rslt-cffcs))
                   (do ((cmbn-mark list (cdr cmbn-mark))
                        (basis-mark f+1-basis)
                        (ic 0))
                       ((endp cmbn-mark))
                     (declare
                      (list cmbn-mark basis-mark)
                      (fixnum ic))
                     (with--term (cffc gnrt) cmbn-mark
                                 (loop (when (eq :equal (funcall cmpr gnrt (car basis-mark)))
                                         (return))
                                       (pop basis-mark)
                                       (incf ic))
                                 (dotimes (il m-n)
                                   (incf (aref rslt-cffcs il)
                                         (* cffc (aref q-1 (+ n il) ic))))
                                 (pop basis-mark)
                                 (incf ic)))
                   (do ((term-list +empty-list+)
                        (il (1- m-n) (1- il)))
                       ((minusp il) (make-cmbn :degr degr
                                               :list term-list))
                     (declare
                      (list term-list)
                      (fixnum il))
                     (let ((cffc (aref rslt-cffcs il)))
                       (declare (fixnum cffc))
                       (unless (zerop cffc)
                         (push (term cffc (gnrt-name il)) term-list))))))))
      (the intr-mrph #'rslt))))
	      
#|
()
(setf q-1 (random-matrix 5 5 10))
(setf f (echcm-kill-epi-f-intr #'s-cmpr 2 2 5 '(a b c d e)
		      (list 0 0 0 0 q-1)))
(funcall f (cmbn 2 1 'a))
(funcall f (cmbn 4 1 'a))
(funcall f (cmbn 3 1 'a 10 'b 100 'c 1000 'd 10000 'e))
|#

(DEFUN ECHCM-KILL-EPI-G-INTR (first n m f+1-basis mtrx-list)
  (declare
   (fixnum first n m)
   (list f+1-basis mtrx-list))
  (let ((f+1 (1+ first))
        (m-n (- m n))
        (q (fourth mtrx-list)))
    (declare
     (fixnum f+1 m-n)
     (type matrix q))
    (flet ((rslt (cmbn)
                 (declare (type cmbn cmbn))
                 (with-cmbn (degr list) cmbn
                            (unless (= degr f+1)
                              (return-from rslt cmbn))
                            (let ((rslt-cffcs (make-array m :element-type 'fixnum
                                                          :initial-element 0)))
                              (declare (type (array fixnum *) rslt-cffcs))
                              (do ((cmbn-mark list (cdr cmbn-mark))
                                   (basis-mark (gnrt-name-basis m-n))
                                   (ic 0))
                                  ((endp cmbn-mark))
                                (declare
                                 (list cmbn-mark basis-mark)
                                 (fixnum ic))
                                (with--term (cffc gnrt) cmbn-mark
                                            (loop (when (eq :equal (s-cmpr gnrt (car basis-mark)))
                                                    (return))
                                                  (pop basis-mark)
                                                  (incf ic))
                                            (dotimes (il m)
                                              (incf (aref rslt-cffcs il)
                                                    (* cffc (aref q il (+ n ic)))))
                                            (pop basis-mark)
                                            (incf ic)))
                              (do ((term-list +empty-list+)
                                   (il (1- m) (1- il)))
                                  ((minusp il) (make-cmbn :degr degr
                                                          :list term-list))
                                (declare
                                 (list term-list)
                                 (fixnum il))
                                (let ((cffc (aref rslt-cffcs il)))
                                  (declare (fixnum cffc))
                                  (unless (zerop cffc)
                                    (push (term cffc (nth il f+1-basis)) term-list))))))))
      (the intr-mrph #'rslt))))
    
#|
()
(setf q (random-matrix 5 5 10))
(setf g (echcm-kill-epi-g-intr 2 2 5 '(a b c d e)
		      (list 0 0 0 q 0)))
(funcall g (cmbn 2))
(funcall g (cmbn 4 1 'a))
(funcall g (cmbn 3 1 :gn-0 10 :gn-1 100 :gn-2))
|#

(DEFUN ECHCM-KILL-EPI-H-INTR (cmpr first n m f-basis f+1-basis mtrx-list)
  (declare
   (type cmprf cmpr)
   (fixnum first n m)
   (list f-basis f+1-basis mtrx-list))
  (let ((lqxp-1 (mtrx-prdc (left-submatrix (fourth mtrx-list) n)
                           (second mtrx-list))))
    (declare (type matrix lqxp-1))
    (flet ((rslt (cmbn)
                 (declare (type cmbn cmbn))
                 (with-cmbn (degr list) cmbn
                            (unless (= degr first)
                              (return-from rslt (zero-cmbn (1+ degr))))
                            (let ((rslt-cffcs (make-array m :element-type 'fixnum
                                                          :initial-element 0)))
                              (declare (type (array fixnum *) rslt-cffcs))
                              (do ((cmbn-mark list (cdr cmbn-mark))
                                   (basis-mark f-basis)
                                   (ic 0))
                                  ((endp cmbn-mark))
                                (declare
                                 (list cmbn-mark basis-mark)
                                 (fixnum ic))
                                (with--term (cffc gnrt) cmbn-mark
                                            (loop (when (eq :equal (funcall cmpr gnrt (car basis-mark)))
                                                    (return))
                                                  (pop basis-mark)
                                                  (incf ic))
                                            (dotimes (il m)
                                              (incf (aref rslt-cffcs il)
                                                    (* cffc (aref lqxp-1 il ic))))
                                            (pop basis-mark)
                                            (incf ic)))
                              (do ((term-list +empty-list+)
                                   (il (1- m) (1- il)))
                                  ((minusp il) (make-cmbn :degr (1+ degr)
                                                          :list term-list))
                                (declare
                                 (list term-list)
                                 (fixnum il))
                                (let ((cffc (aref rslt-cffcs il)))
                                  (declare (fixnum cffc))
                                  (unless (zerop cffc)
                                    (push (term cffc (nth il f+1-basis)) term-list))))))))
      (the intr-mrph #'rslt))))
	      
#|
()
(setf p-1 (random-matrix 2 2 10))
(setf q (random-matrix 5 5 10))
(setf h (echcm-kill-epi-h-intr #'s-cmpr 2 2 5 '(a b) '(a b c d e)
		      (list 0 p-1 0 q 0)))
(funcall h (cmbn 2 1 'a 1000 'b))
(funcall h (cmbn 4 1 'a))
|#

(DEFUN ECHCM-WITHOUT-EPI (echcm first n m intr-f)
  (declare
    (type chain-complex echcm)
    (fixnum first n m)
    (type intr-mrph intr-f))
  (the chain-complex
  (with-slots (cmpr basis dffr orgn) echcm
    (declare
      (type cmprf cmpr)
      (type basis basis)
      (type morphism dffr)
      (list orgn))
    (build-chcm
     :cmpr #'(lambda (gnrt1 gnrt2)
	       (if (and (symbolp gnrt1)
			(eq (symbol-package gnrt1)
			    +gnrts-pckg+))
		   (s-cmpr gnrt1 gnrt2)
		 (funcall cmpr gnrt1 gnrt2)))
     :basis #'(lambda (degr)
		(declare (fixnum degr))
		(cond ((= degr first)
		       +empty-list+)
		      ((= degr (1+ first))
		       (gnrt-name-basis (- m n)))
		      (t
		       (funcall basis degr))))
     :intr-dffr #'(lambda (cmbn)
		    (declare (type cmbn cmbn))
		    (case (- (cmbn-degr cmbn) first)
		      (1 (zero-cmbn first))
		      (2 (funcall intr-f
			   (cmbn-? dffr cmbn)))
		      (otherwise
		         (cmbn-? dffr cmbn))))
     :strt :cmbn
     :orgn `(echcm-without-epi ,echcm)))))

(DEFUN ECHCM-KILL-EPI (echcm first)
  (declare
    (type chain-complex echcm)
    (fixnum first))
  (the reduction
  (with-slots (cmpr basis) echcm
    (declare
      (type cmprf cmpr)
      (type basis basis))
    (assert (not (eq basis :locally-effective)))
    (let* ((f-basis (funcall basis first))
	   (f+1-basis (funcall basis (1+ first)))
	   (mtrx-list (smith (chcm-mtrx echcm (1+ first))))
	   (smith (third mtrx-list))
	   (m (column-number smith))
	   (n (line-number smith))
	   (intr-f (echcm-kill-epi-f-intr cmpr first n m
				    f+1-basis
				    mtrx-list))
	   (intr-g (echcm-kill-epi-g-intr first n m f+1-basis mtrx-list))
	   (intr-h (echcm-kill-epi-h-intr cmpr first n m
				    f-basis f+1-basis
				    mtrx-list))
	   (echcm2 (echcm-without-epi echcm first n m intr-f)))				      
      (declare
        (list mtrx-list)
	(type matrix smith)
	(fixnum m n)
	(type intr-mrph intr-f intr-g intr-h)
	(type chain-complex echcm2))
      (assert (dotimes (i n +true+)
		(unless (= 1 (aref smith i i))
		  (return +false+))))
      (build-rdct
       :f (build-mrph
	    :sorc echcm :trgt echcm2 :degr 0
	    :intr intr-f :strt :cmbn
	    :orgn `(echcm-kill-epi-f ,echcm))
       :g (build-mrph
	    :sorc echcm2 :trgt echcm :degr 0
	    :intr intr-g :strt :cmbn
	    :orgn `(echcm-kill-epi-g ,echcm))
       :h (build-mrph
	    :sorc echcm :trgt echcm :degr +1
	    :intr intr-h :strt :cmbn
	    :orgn `(echcm-kill-epi-h ,echcm))
       :orgn `(echcm-kill-epi ,echcm))))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf s3-chml-clss (chml-clss 's3))
(setf s3-fibration (z-whitehead s3 3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(setf ecc (echcm s3-4))
(setf rdct (echcm-kill-epi ecc 2))
(pre-check-rdct rdct)
(setf *tc* (cmbn 0 1 (bsgn ecc))
      *bc* *tc*)
(check-rdct)
(setf *tc* (cmbn 2 1 (first (basis ecc 2))))
(check-rdct)
(setf *tc* (cmbn 3 1 (first (basis ecc 3))))
(check-rdct)
(setf *tc* (cmbn 4 1 (first (basis ecc 4)))
      *bc* *tc*)
(check-rdct)
(setf s3-4-chml-clss (chml-clss 's3-4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(setf ecc (echcm s3-5))
(dotimes (i 7)
  (format t "~%DIM = ~D ; LENGTH = ~D" i (length (basis ecc i))))
(setf rdct1 (echcm-kill-epi ecc 2))
(setf rdct2 (echcm-kill-epi (bcc rdct1) 3))
(setf rdct3 (echcm-kill-epi (bcc rdct2) 4))
(setf rdct12 (cmps rdct2 rdct1))
(setf rdct123 (cmps rdct3 rdct12))
(pre-check-rdct rdct123)
(setf *tc* (cmbn 0 1 (bsgn ecc)) *bc* *tc*)
(check-rdct)
(setf *tc* (cmbn 2 1 (first (basis ecc 2))))
(check-rdct)
(let ((b3 (basis ecc 3)))
  (setf *tc* (cmbn 3 1 (first b3) 10 (second b3))))
(check-rdct)
(let ((b4 (basis ecc 4)))
  (setf *tc* (cmbn 4 1 (first b4) 10 (second b4))))
(check-rdct)
(let ((b5 (basis ecc 5)))
  (setf *tc* (cmbn 5 1 (first b5)
		     10 (second b5)
		     100 (third b5)
		     1000 (fourth b5))))
(check-rdct)
(let ((b6 (basis ecc 6)))
  (setf *tc* (cmbn 6 1 (first b6)
		     10 (second b6)
		     100 (third b6)
		     1000 (fourth b6)
		     10000 (fifth b6)
		     100000 (sixth b6)
		     1000000 (seventh b6))))
(check-rdct)
|#
 
(DEFUN KILL-EPI (chcm first)
  (declare
    (type chain-complex chcm)
    (fixnum first))
  (the homotopy-equivalence
    (let ((efhm (efhm chcm))
	  (echcm (echcm chcm)))
      (declare
        (type homotopy-equivalence efhm)
	(type chain-complex echcm))
      (let ((last-rdct (echcm-kill-epi echcm first)))
	(declare (type reduction last-rdct))
	(setf (slot-value chcm 'efhm)
	      (build-hmeq
	        :lrdct (lrdct efhm)
	        :rrdct (cmps last-rdct (rrdct efhm))
	        :orgn `(kill-epi ,chcm ,first)))))))

(DEFUN KILL-EPIS (chcm first end)
  (declare
    (type chain-complex chcm)
    (fixnum first end))
  (the homotopy-equivalence (progn
    (do ((indx first (1+ indx)))
	((= indx end))
        (declare (fixnum indx))
      (kill-epi chcm indx))
    (efhm chcm))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf s3-chml-clss (chml-clss 's3))
(setf s3-fibration (z-whitehead s3 3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(setf s3-4-chml-clss (chml-clss 's3-4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(time (homology s3-5 6))
(kill-epis s3-5 2 5)
(homology s3-5 0 7)

(cat-init)
(setf s3 (sphere 3))
(setf s3-chml-clss (chml-clss 's3))
(setf s3-fibration (z-whitehead s3 3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(kill-epi s3-4 2)
(setf s3-4-chml-clss (chml-clss 's3-4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(time (homology s3-5 6))
|#

(DEFUN CHML-CLSS-INTR (chcm first)
  (declare
    (type chain-complex chcm)
    (fixnum first))
  (let* ((echcm (echcm chcm))
	 (cmpr (cmpr echcm))
	 (basis (basis echcm))
	 (f-basis (funcall basis first))
	 (mtrx-list (smith (chcm-mtrx echcm (1+ first))))
	 (p-1 (second mtrx-list))
	 (smith (third mtrx-list))
	 (n (line-number smith))
	 (m (column-number smith))
         (diag-indx (dotimes (indx (min n m)
			       (if (> n m)
				   m
				 (error "In CHML-CLSS, the cohomology-ring ~@
                                      is null.")))
		      (declare (fixnum indx))
		      (unless (= 1 (aref smith indx indx))
			(return indx)))))
    (declare
      (type chain-complex echcm)
      (type cmprf cmpr)
      (type basis basis)
      (fixnum n m diag-indx)
      (list f-basis mtrx-list)
      (type matrix p-1 smith))
    (flet ((rslt (cmbn)
	     (declare (type cmbn cmbn))
	     (with-cmbn (degr list) cmbn
	       (unless (= degr first)
		 (return-from rslt (zero-cmbn (- degr first))))
	       (do ((rslt 0)
		    (bmark f-basis)
		    (ic 0)
		    (cmark list (cdr cmark)))
		   ((endp cmark)
		    (if (zerop rslt)
			(zero-cmbn 0)
		      (term-cmbn 0 rslt :z-gnrt)))
		   (declare
		     (fixnum rslt)
		     (list bmark cmark))
		 (with--term (cffc gnrt) cmark
		   (loop
		    (when (eq :equal (funcall cmpr gnrt (car bmark)))
		      (return))
		    (pop bmark)
		    (incf ic))
		   (incf rslt (* cffc (aref p-1 diag-indx ic)))
		   (pop bmark)
		   (incf ic))))))
      (the intr-mrph #'rslt))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf c (chml-clss-intr s3 3))
(funcall c (cmbn 3 5 's3))
(setf s3-chml-clss (chml-clss 's3))
(setf s3-fibration (z-whitehead s3 3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(kill-epi s3-4 2)
(setf c (chml-clss-intr s3-4 4))
(funcall c (cmbn 4 5 (first (basis (echcm s3-4) 4))))
(setf s3-4-chml-clss (chml-clss 's3-4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(kill-epis s3-5 3 5)
(setf c (chml-clss-intr s3-5 5))
(let ((b5 (basis (echcm s3-5) 5)))
  (funcall c (cmbn 5 1 (first b5) 10 (second b5))))
|#

(DEFUN CHML-CLSS (chcm first)
  (declare
    (type chain-complex chcm)
    (fixnum first))
  (the morphism
    (build-mrph
      :sorc (echcm chcm) :trgt (z-chcm) :degr (- first)
      :intr (chml-clss-intr chcm first)
      :strt :cmbn
      :orgn `(chml-clss ,chcm ,first))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf s3-chml-clss (chml-clss s3 3))
(setf s3-fibration (z-whitehead s3 3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(homology s3-4 0 6)
(kill-epi s3-4 2)
(setf s3-4-chml-clss (chml-clss s3-4 4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(homology s3-5 0 6)
(kill-epis s3-5 3 5)
(setf s3-5-chml-clss (chml-clss s3-5 5))
(setf s3-5-fibration (z2-whitehead s3-5 5 s3-5-chml-clss))
(setf s3-6 (fibration-total s3-5-fibration))
(homology s3-6 0 7)
|#
