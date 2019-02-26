;;;  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH
;;;  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH
;;;  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH  NEW-SMITH

(IN-PACKAGE #:cat-9)

(PROVIDE "smith")

;; New-version uses the other type MATRICE = sparse-matrices
;; (DEFTYPE MATRIX () '(array fixnum (* *)))


;; Replaces (DEFUN RANDOM-MATRIX (line-n column-n max)...

(DEFMACRO RANDOM-MATRIX (line-n column-n dnst max)
  `(mat-aleat ,line-n ,column-n ,dnst ,max))

#|
(random-matrix 5 10 .2 100)
|#
  
;; Replaces (DEFUN IDNT-MTRX (n)...

(DEFMACRO IDNT-MTRX (n)
  `(identite ,n))

#|
(idnt-mtrx 3)
|#

;; Replaces (DEFUN COPY-MTRX (mtrx)...
(DEFMACRO COPY-MTRX (mtrx)
  `(copier-matrice ,mtrx))

#|
(setf m (random-matrix 3 4 .4 10))
(copy-mtrx m)
;; Warning: The next (equalp ) cannot work
;;     because of the new circular structure of m.
;;   Generates an ACL crash !
;; (equalp m (copy-mtrx m))
|#

#|
(setf m (random-matrix 3 4 .4 10))
(line-number m)
(column-number m)
|#

;; In the former smith, the indices start from 0
;;   and the intervals have the format [0 .. n-1[

;; Now the indices start from 1
;;   and the intervals have the format [1 .. n]

;; So that the below submatrix has k columns numbered [1 .. k]

(DEFUN LEFT-SUBMATRIX (mtrx k)
  ;;; keeps the columns of index <= k
  (declare (type matrice mtrx) (type fixnum k))
  (the matrice
    (let ((line-number (line-number mtrx)))
      (declare (type fixnum line-number))
      (let ((rslt (creer-matrice line-number k)))
        (declare (type matrice rslt))
        (do ((basecol (uplig mtrx))
             (il 1 (1+ il)))
            ((> il line-number))
          (declare (type array basecol) (type fixnum il))
          (let ((spl0 (baselig mtrx il)))
            (declare (type t-mat spl0))
            (do ((spl (left (chercher-hor spl0 k)) (left spl))
                 (tpl (baselig rslt il) (left tpl)))
                ((eq spl spl0))
              (declare (type t-mat spl tpl))
              (inserer-terme tpl (aref basecol (icol spl)) (val spl)))))
        rslt))))

#|
(setf m (random-matrix 5 10 .6 10))
(left-submatrix m 5)
(left-submatrix m 10)
(left-submatrix m 0)
(setf m (creer-matrice 5 10))
(left-submatrix m 5)
|#


(DEFUN MTRX-PRDC (mtrx1 mtrx2)
  (declare (type matrice mtrx1 mtrx2))
  (the matrice
    (let ((nl (line-number mtrx1))
          (nc (column-number mtrx2))
          (n (column-number mtrx1)))
      (declare (type fixnum nl nc n))
      (assert (eql n (line-number mtrx2)))
      (let ((rslt (creer-matrice nl nc)))
        (declare (type matrice rslt))
        (do ((il 1 (1+ il)))
            ((> il nl))
          (declare (type fixnum il))
          (do ((pl01 (baselig mtrx1 il))
               (pl0r (baselig rslt il))               
               (ic 1 (1+ ic)))
              ((> ic nc))
            (declare (type t-mat pl01 pl0r) (type fixnum ic))
            (let ((pl (left pl01))
                  (pc (up (basecol mtrx2 ic)))
                  (sum 0))
              (declare (type t-mat pl pc) (type fixnum sum))
              (loop
                (let ((ilic (icol pl))
                      (icil (ilig pc)))
                  (declare (type fixnum ilic icil))
                  (when (zerop ilic) (return))
                  (when (zerop icil) (return))
                  (cond ((eql ilic icil)
                         (incf sum (safe-* (val pl) (val pc)))
                         (setf pl (left pl) pc (up pc)))
                        ((> ilic icil)
                         (setf pl (left pl)))
                        (t
                         (setf pc (up pc))))))
              (unless (zerop sum)
                (inserer-terme pl0r (basecol rslt ic) sum)))))
        rslt))))

#|
(setf m (random-matrix 1 1 1 10))
(mtrx-prdc m m)
(setf m1 (random-matrix 2 3 .8 5))
(setf m2 (random-matrix 3 2 .8 5))
(mtrx-prdc m1 m2)
(setf m1 (random-matrix 3 5 .8 2))
(setf m2 (random-matrix 5 1 .8 2))
(mtrx-prdc m1 m2)
(setf m1 (creer-matrice 3 5))
(setf m2 (random-matrix 5 4 .8 10))
(mtrx-prdc m1 m2)
(setf m1 (creer-matrice 3 5))
(setf m2 (random-matrix 6 3 .8 10))
(mtrx-prdc m2 m1)
(setf m1 (creer-matrice 3 5))
(setf m2 (creer-matrice 5 4))
(mtrx-prdc m1 m2)
(setf m1 (random-matrix 1 2 2 10))
(setf m2 (creer-matrice 2 1))
(inserer-terme (baselig m2 1) (basecol m2 1) (val (up (basecol m1 2))))
(inserer-terme (baselig m2 2) (basecol m2 1) (- (val (up (basecol m1 1)))))
m2
(mtrx-prdc m1 m2)
|#

(DEFMACRO CHCM-MTRX (chcm degr)
  `(chcm-mat ,chcm ,degr))

#|
(setf d (delta 5))
(chcm-mtrx d 3)
(mtrx-prdc (chcm-mtrx d 2) (chcm-mtrx d 3))
(setf m (moore 2 2))
(dotimes (i 5)
  (print (chcm-mtrx m i)))
(dotimes (i 6)
  (print (list (length (leftcol (chcm-mtrx m i)))
               (length (uplig (chcm-mtrx m i))))))
|#

(DEFUN EXTRACT-LINE (mtrx ilig)
  (declare (type matrice mtrx) (type fixnum ilig))
  (the list
    (let ((pl0 (baselig mtrx ilig)))
      (declare (type t-mat pl0))
      (do ((pl (left pl0) (left pl))
           (rslt +empty-list+ (cons (list (icol pl) (val pl)) rslt)))
          ((eq pl pl0) rslt)
        (declare (type t-mat pl) (type list rslt))))))

#|
(setf m (random-matrix 4 10 1 10))
(extract-line m 3)
|#

(DEFUN EXTRACT-COLUMN (mtrx icol)
  (declare (type matrice mtrx) (type fixnum icol))
  (the list
    (let ((pc0 (basecol mtrx icol)))
      (declare (type t-mat pc0))
      (do ((pc (up pc0) (up pc))
           (rslt +empty-list+ (cons (list (ilig pc) (val pc)) rslt)))
          ((eq pc pc0) rslt)
        (declare (type t-mat pc) (type list rslt))))))

#|
(setf m (random-matrix 4 10 1 10))
(extract-column m 3)
|#

(DEFUN EXTRACT-TERM (matrix il ic)
  (declare (type matrice matrix) (type fixnum ic il))
  (the fixnum
    (do ((p (left (baselig matrix il)) (left p)))
        (nil)
      (declare (type t-mat p))
      (let ((p-ic (icol p)))
        (declare (type fixnum p-ic))
        (when (<= p-ic ic)
          (return-from extract-term
            (if (eql p-ic ic)
                (val p)
              0)))))))

#|
(setf m (random-matrix 2 3 1 10))
(dotimes (il 2)
  (dotimes (ic 3)
    (print (list il ic (extract-term m (1+ il) (1+ ic))))))
|#

(DEFUN NEW-LINE (mtrx ilig list)
  (declare (type matrice mtrx) (type fixnum ilig) (type list list))
  (the matrice
    (let ((peigne (peigne-ver mtrx (basecol mtrx 0) ilig))
          (pl0 (baselig mtrx ilig)))
      (declare (type list peigne) (type t-mat pl0))
      (map nil #'(lambda (item)
                   (declare (type t-mat item))
                   (when (eq (left pl0) (up item))
                     (supprimer-terme pl0 item)))
        peigne)
      (do ((markp (nreverse peigne) (cdr markp))
           (icol 1 (1+ icol))
           (markl list))
          ((endp markl))
        (declare (type list markp markl) (type fixnum icol))
        (when (eql (caar markl) icol)
          (inserer-terme pl0 (car markp) (cadar markl))
          (setf markl (cdr markl))))      
      mtrx)))

#|
(setf m (random-matrix 5 10 1 10))
(new-line m 3 nil)
(setf m (random-matrix 5 10 1 10))
(new-line m 3 '((2 5) (10 10)))
|#

(DEFUN NEW-COLUMN (mtrx icol list)
  (declare (type matrice mtrx) (type fixnum icol) (type list list))
  (the matrice
    (let ((peigne (peigne-hor mtrx (baselig mtrx 0) icol))
          (pc0 (basecol mtrx icol)))
      (declare (type list peigne) (type t-mat pc0))
      (map nil #'(lambda (item)
                   (declare (type t-mat item))
                   (when (eq (up pc0) (left item))
                     (supprimer-terme item pc0)))
        peigne)
      (do ((markp (nreverse peigne) (cdr markp))
           (ilig 1 (1+ ilig))
           (markc list))
          ((endp markc))
        (declare (type list markp markc) (type fixnum ilig))
        (when (eql (caar markc) ilig)
          (inserer-terme (car markp) pc0 (cadar markc))
          (setf markc (cdr markc))))      
      mtrx)))

#|
(setf m (random-matrix 5 10 1 10))
(new-column m 3 nil)
(setf m (random-matrix 5 10 1 10))
(new-column m 3 '((2 5) (5 10)))
|#

(DEFUN SAFE-* (arg1 arg2)
  (declare (type fixnum arg1 arg2))
  (let ((rslt (* arg1 arg2)))
    (declare (type integer rslt))
    (if (typep rslt 'fixnum)
        rslt
      (error "Fixnum-overflow in (safe-* ~D ~D)." arg1 arg2))))

#|
(safe-* 23170 23170)
(safe-* 23170 23171)
|#

(DEFUN SAFE-+ (arg1 arg2)
  (declare (type fixnum arg1 arg2))
  (let ((rslt (+ arg1 arg2)))
    (declare (type integer rslt))
    (if (typep rslt 'fixnum)
        rslt
      (error "Fixnum-overflow in (safe-+ ~D ~D)." arg1 arg2))))

#|
(safe-+ 268435456 268435455)
(safe-+ 268435456 268435456)
|#


(DEFUN LINE-OP (mtrx lambda iline1 iline2)
  ;; line2 := line2 + lambda * line1
  (declare
   (type matrice mtrx)
   (type fixnum lambda iline1 iline2))
  (the matrice
    (let ((mark1 (extract-line mtrx iline1))
          (mark2 (extract-line mtrx iline2))
          (new-line2 +empty-list+))
      (declare (type list mark1 mark2 new-line2))
      (loop
        (when (endp mark1)
          (setf new-line2 (nreconc new-line2 mark2))
          (return))
        (when (endp mark2)
          (setf new-line2
            (nreconc new-line2
                     (mapcar #'(lambda (item)
                                 (declare (type list item))
                                 (list (first item)
                                       (safe-* lambda (second item))))
                       mark1)))
          (return))
        (let ((icol1 (caar mark1))
              (icol2 (caar mark2)))
          (declare (type fixnum icol1 icol2))
          (cond ((< icol1 icol2)
                 (push (list icol1 (safe-* lambda (second (pop mark1))))
                       new-line2))
                ((> icol1 icol2)
                 (push (list icol2 (second (pop mark2))) new-line2))
                (t (let ((new-val (safe-+ (safe-* lambda (second (pop mark1)))
                                     (second (pop mark2)))))
                     (declare (type fixnum new-val))
                     (unless (zerop new-val)
                       (push (list icol1 new-val) new-line2)))))))
      (new-line mtrx iline2 new-line2)
      mtrx)))

#|
(setf m (random-matrix 5 10 1 10))
(line-op m 2 3 4)
|#
    
(DEFUN COLUMN-OP (mtrx lambda icol1 icol2)
  ;; col2 := col2 + lambda * col1
  (declare
   (type matrice mtrx)
   (type fixnum lambda icol1 icol2))
  (the matrice
    (let ((mark1 (extract-column mtrx icol1))
          (mark2 (extract-column mtrx icol2))
          (new-column2 +empty-list+))
      (declare (type list mark1 mark2 new-column2))
      (loop
        (when (endp mark1)
          (setf new-column2 (nreconc new-column2 mark2))
          (return))
        (when (endp mark2)
          (setf new-column2
            (nreconc new-column2
                     (mapcar #'(lambda (item)
                                 (declare (type list item))
                                 (list (first item)
                                       (safe-* lambda (second item))))
                       mark1)))
          (return))
        (let ((ilig1 (caar mark1))
              (ilig2 (caar mark2)))
          (declare (type fixnum ilig1 ilig2))
          (cond ((< ilig1 ilig2)
                 (push (list ilig1 (safe-* lambda (second (pop mark1))))
                       new-column2))
                ((> ilig1 ilig2)
                 (push (list ilig2 (second (pop mark2))) new-column2))
                (t (let ((new-val (safe-+ (safe-* lambda (second (pop mark1)))
                                     (second (pop mark2)))))
                     (declare (type fixnum new-val))
                     (unless (zerop new-val)
                       (push (list ilig1 new-val) new-column2)))))))
      (new-column mtrx icol2 new-column2)
      mtrx)))

#|
(setf m (random-matrix 5 10 1 10))
(column-op m 2 3 4)
|#

;;; The matrices cannot be compared by equalp,
;;;   because of their circular structure,
;;;   needing a specific equality function.

(DEFUN EQUAL-MATRIX (mtrx1 mtrx2)
  (declare (type matrice mtrx1 mtrx2))
  (the boolean
    (let ((line-number (line-number mtrx1)))
      (declare (type fixnum line-number))
      (unless (eql line-number (line-number mtrx2))
        (return-from equal-matrix +false+))
      (unless (eql (column-number mtrx1) (column-number mtrx2))
        (return-from equal-matrix +false+))
      (do ((il 1 (1+ il)))
          ((> il line-number))
        (declare (type fixnum il))
        (let ((p10 (baselig mtrx1 il))
              (p20 (baselig mtrx2 il)))
          (declare (type t-mat p10 p20))
          (do ((p1 (left p10) (left p1))
               (p2 (left p20) (left p2)))
              (nil)
            (declare (type t-mat p1 p2))
            (when (eq p1 p10)
              (unless (eq p2 p20)
                (return-from equal-matrix +false+))
              (return))
            (when (eq p2 p20)
              (unless (eq p1 p10)
                (return-from equal-matrix +false+)))
            (unless (eql (icol p1) (icol p2))
              (return-from equal-matrix +false+))
            (unless (eql (val p1) (val p2))
              (return-from equal-matrix +false+)))))
      (return-from equal-matrix +true+))))

#|
(setf m1 (creer-matrice 2 3))
(setf m2 (creer-matrice 3 3))
(equal-matrix m1 m2)
(setf m1 (creer-matrice 2 3))
(setf m2 (creer-matrice 2 4))
(equal-matrix m1 m2)
(setf m1 (creer-matrice 1 1))
(setf m2 (creer-matrice 1 1))
(new-line m2 1 '((1 1)))
(equal-matrix m1 m2)
(new-line m1 1 '((1 1)))
(new-line m2 1 nil)
(equal-matrix m1 m2)
(setf m1 (creer-matrice 1 2))
(setf m2 (creer-matrice 1 2))
(new-line m1 1 '((2 1)))
(new-line m2 1 '((1 1)))
(equal-matrix m1 m2)
(new-line m2 1 '((2 2)))
(equal-matrix m1 m2)
(new-line m2 1 '((2 1)))
(equal-matrix m1 m2)
(setf m1 (random-matrix 5 10 1 10))
(setf m2 (copier-matrice m1))
(equal-matrix m1 m2)          
|#

;;; mtrx-list = (P P^-1 M Q Q^-1)

;;; All the standard operations on lines and colums
;;; have a zzz-5 version organised as follows.
;;; The list returned leaves unchanged the product P M Q^(-1).

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))  
(line-op-5 list 3 1 3)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(column-op-5 list 3 1 3)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#
  
(DEFUN LINE-SWAP (mtrx iline1 iline2)
  ;; swaps line1 and line2
  (declare
    (type matrice mtrx) (type fixnum iline1 iline2))
  (the matrice
    (let ((new-line1 (extract-line mtrx iline2))
          (new-line2 (extract-line mtrx iline1)))
      (declare (type list new-line1 new-line2))
      (new-line mtrx iline1 new-line1)
      (new-line mtrx iline2 new-line2)
      mtrx)))

#|
(setf m (random-matrix 3 4 1 10))
(line-swap m 1 2)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(line-swap-5 list 1 3)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN COLUMN-SWAP (mtrx icol1 icol2)
  ;; swaps column1 and column2
  (declare
    (type matrice mtrx) (type fixnum icol1 icol2))
  (the matrice
    (let ((new-column1 (extract-column mtrx icol2))
          (new-column2 (extract-column mtrx icol1)))
      (declare (type list new-column1 new-column2))
      (new-column mtrx icol1 new-column1)
      (new-column mtrx icol2 new-column2)
      mtrx)))

#|
(setf m (random-matrix 3 4 1 10))
(column-swap m 3 2)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(column-swap-5 list 1 3)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN SAFE-- (arg)
  (declare (type fixnum arg))
  (let ((rslt (- arg)))
    (declare (type integer rslt))
    (if (typep rslt 'fixnum)
        rslt
      (error "Fixnum-overflow in (SAFE-- ~D)." arg))))

#|
(safe-- -536870911)
(safe-- -536870912)
|#

(DEFUN LINE-MINUS (mtrx iline)
  ;; line := - line
  (declare (type matrice mtrx) (type fixnum iline))
  (the matrice
    (new-line mtrx iline
              (mapcar #'(lambda (item)
                          (declare (type list item))
                          (the list
                            (list (first item)
                                  (safe-- (second item)))))
                (extract-line mtrx iline)))))

(DEFUN COLUMN-MINUS (mtrx icol)
  ;; column := - column
  (declare (type matrice mtrx) (type fixnum icol))
  (the matrice
    (new-column mtrx icol
                (mapcar #'(lambda (item)
                            (declare (type list item))
                            (the list
                              (list (first item)
                                    (safe-- (second item)))))
                  (extract-column mtrx icol)))))


#|
(setf m (random-matrix 3 4 1 10))
(line-minus m 2)
(column-minus m 1)
|#

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(line-minus-5 list 3)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
(column-minus-5 list 2)
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN MINIMAL-TERM (matrix begin)
  (declare (type matrice matrix) (type fixnum begin))
  (the (values fixnum fixnum fixnum)
    (do ((il (line-number matrix) (1- il))
         (min 0)
         (min-il -1)
         (min-ic -1))
        ((< il begin)
         (return-from minimal-term
           (values min min-il min-ic)))
      (declare (type fixnum il min min-il min-ic))
      (do ((p (left (baselig matrix il)) (left p)))
          ((< (icol p) begin))
        (declare (type t-mat p))
        (let ((term (abs (val p))))
          (declare (type fixnum term))
          (when (eql term 1)
            (return-from minimal-term (values 1 il (icol p))))
          (when (plusp term)
            (when (or (< term min) (zerop min))
              (setf min term
                min-il il
                min-ic (icol p)))))))))

#|
(setf m (random-matrix 4 5 1 10))
(minimal-term m 1)
(setf m (creer-matrice 3 5))
(minimal-term m 1)
(setf m (random-matrix 4 5 1 10))
(setf m2 (copier-matrice m))
(minimal-term m 1)
(minimal-term m 2)
(minimal-term m 3)
(minimal-term m 4)
(minimal-term m 5)
|#

(DEFUN MINIMAL-REST-1 (matrix begin)
  ;; Let c (= corner) the term M_{b,b} (b = begin).
  ;; This function looks for the minimal rest of the division
  ;; of M_{il,ic} by c
  ;;              for il = begin and ic > begin
  ;;               or ic = begin and il > begin
  ;; asserts c defined and non-null
  (declare (type matrice matrix) (type fixnum begin))
  (the (values fixnum fixnum fixnum)
    (let ((min 0) (min-il -1) (min-ic -1)
          (corner (do ((p (left (baselig matrix begin)) (left p)))
                      ((eql (icol p) begin) (val p))
                    (declare (type t-mat p))
                    (when (zerop (icol p))
                      (error "Illegal matrix in MINIMAL-REST-1")))))
      (declare (type fixnum min min-il min-ic corner))
      (assert (not (zerop corner)))
      (do ((p (left (baselig matrix begin)) (left p)))
          ((eql (icol p) begin))
        (declare (type t-mat p))
        (let ((term (abs (second (multiple-value-list
                                  (round (val p) corner))))))
          (declare (type fixnum term))
          (when (= term 1)
            (return-from minimal-rest-1 (values 1 begin (icol p))))
          (when (plusp term)
            (when (or (< term min) (zerop min))
              (setf min term
                min-il begin
                min-ic (icol p))))))
      (do ((p (up (basecol matrix begin)) (up p)))
          ((eql (ilig p) begin))
        (declare (type t-mat p))
        (let ((term (abs (second (multiple-value-list
                                  (round (val p) corner))))))
          (declare (type fixnum term))
          (when (= term 1)
            (return-from minimal-rest-1 (values 1 (ilig p) begin)))
          (when (plusp term)
            (when (or (< term min) (zerop min))
              (setf min term
                min-il (ilig p)
                min-ic begin)))))
      (values min min-il min-ic))))
      

#|
(setf m (random-matrix 4 5 1 10))
;;; pay attention corner /= 0
|#

(DEFUN MINIMAL-REST-2 (matrix begin)
  ;; Let c (= corner) the term M_{b,b} (b = begin).
  ;; This function looks for the minimal rest of the division
  ;; of M_{il,ic} by c for il > begin and ic > begin.
  (declare (type matrice matrix) (type fixnum begin))
  (the (values fixnum fixnum fixnum)
    (let ((min 0) (min-il -1) (min-ic -1)
          (corner (do ((p (left (baselig matrix begin)) (left p)))
                      ((eql (icol p) begin) (val p))
                    (declare (type t-mat p))
                    (when (zerop (icol p))
                      (error "Illegal matrix in MINIMAL-REST-1")))))
      (declare (type fixnum min min-il min-ic corner))
      (assert (not (zerop corner)))
      (do ((il (line-number matrix) (1- il)))
          ((eql il begin))
        (declare (type fixnum il))
        (do ((p (left (baselig matrix il)) (left p)))
            ((<= (icol p) begin))
          (declare (type t-mat p))
          (let ((term (abs (second (multiple-value-list
                                    (round (val p) corner))))))
            (declare (type fixnum term))
            (when (= 1 term)
              (return-from minimal-rest-2 (values 1 il (icol p))))
            (when (plusp term)
              (when (or (< term min) (zerop min))
                (setf min term
                  min-il il
                  min-ic (icol p)))))))
      (values min min-il min-ic))))

#|
(setf m (random-matrix 6 8 1 10))
|#


(DEFUN MINIMAL-TERM-TOP-LEFT (mtrx-list begin il ic)
  (declare (list mtrx-list)  (type fixnum begin il ic))
  (the list
    (progn
      (assert (<= begin il))
      (assert (<= begin ic))
      (when (< begin il)
        (line-swap-5 mtrx-list begin il))
      (when (< begin ic)
        (column-swap-5 mtrx-list begin ic))
      (let ((corner (extract-term (third mtrx-list) begin begin)))
        (declare (type fixnum corner))
        (when (minusp corner)
          (line-minus-5 mtrx-list begin)))
      mtrx-list)))

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
  m (random-matrix 4 5 1 10)
  mm (copier-matrice m)
  q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(minimal-term-top-left list 1 1 3)
mm
m
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)

(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
  m (random-matrix 4 5 1 10)
  mm (copier-matrice m)
  q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(minimal-term-top-left list 2 4 2)
mm
m
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)

(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
  m (random-matrix 4 5 1 10)
  mm (copier-matrice m)
  q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(minimal-term-top-left list 2 3 4)
mm
m
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)

|#

(DEFUN PIVOTT (mtrx-list begin &aux (mtrx (third mtrx-list)))
  (declare (type list mtrx-list) (type fixnum begin)
           (type matrice mtrx))
  (the list
    (let ((corner (extract-term mtrx begin begin))
          (pc0 (basecol mtrx begin))
          (pl0 (baselig mtrx begin)))
      (declare (type fixnum corner) (type t-mat pc0 pl0))
      (do ((p (up pc0) (up pc0)))
          (nil)
        (declare (type t-mat p))
        (let ((il (ilig p)))
          (declare (type fixnum il))
          (when (eql il begin)
            (return))
          (let ((lambda (safe-- (/ (val p) corner))))
            (declare (type fixnum lambda))
            (line-op-5 mtrx-list lambda begin il))))
      (do ((p (left pl0) (left pl0)))
          (nil)
        (declare (type t-mat p))
        (let ((ic (icol p)))
          (declare (type fixnum ic))
          (when (eql ic begin)
            (return))
          (let ((lambda (safe-- (/ (val p) corner))))
            (declare (type fixnum lambda))
            (column-op-5 mtrx-list lambda begin ic))))
      mtrx-list)))

#|
(setf p (idnt-mtrx 4) p-1 (idnt-mtrx 4)
      m (random-matrix 4 5 1 10)
      q (idnt-mtrx 5) q-1 (idnt-mtrx 5))
(setf list (list p p-1 m q q-1))
m
(new-line m 1 '((1 1) (2 2) (4 4)))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(pivott list 1)
m
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(mtrx-prdc p p-1)
(mtrx-prdc q q-1)
|#

(DEFUN LIST-SMITH (mtrx-list)
  (declare (list mtrx-list))
  (the list
    (progn
      (let ((matrix (third mtrx-list))
            (begin 1))
        (declare (type matrice matrix) (type fixnum begin))
        (loop
          (multiple-value-bind (term il ic) (minimal-term matrix begin)
            (declare (type fixnum term il ic))
            ;; (format t "~%*BEGIN* = ~D ; MIN = ~D." begin term)
            (when (zerop term)
              (return-from list-smith mtrx-list))
            (minimal-term-top-left mtrx-list begin il ic)
            ; (print (list 1 begin il ic t2))
            ; (break)
            )
          (loop
            (multiple-value-bind (term il ic) (minimal-rest-1 matrix begin)
              (declare (type fixnum term il ic))
              (cond ((zerop term)
                     (pivott mtrx-list begin)
                     ; (print (list 2 t2))
                     ; (break)
                     (multiple-value-bind (term il ic) (minimal-rest-2 matrix begin)
                       (declare (type fixnum term il) (ignore ic))
                       (cond ((zerop term)
                              (return))
                             (t
                              (line-op-5 mtrx-list 1 il begin)
                              ; (print (list 3 t2))
                              ; (break)
                              ))))
                    ((= il begin)
                     (column-op-5 mtrx-list
                                  (safe-- (round (extract-term matrix begin ic)
                                            (extract-term matrix begin begin)))
                                  begin ic)
                     ; (print (list 4 t2))
                     ; (break)
                     (column-swap-5 mtrx-list begin ic)
                     ; (print (list 5 t2))
                     ; (break)
                     (when (minusp (extract-term matrix begin begin))
                       (column-minus-5 mtrx-list begin)
                       ; (print (list 6 t2))
                       ; (break)
                       ))
                    (t
                     (line-op-5 mtrx-list
                                (safe-- (round (extract-term matrix il begin)
                                          (extract-term matrix begin begin)))
                                begin il)
                     ; (print (list 7 t2))
                     ; (break)
                     (line-swap-5 mtrx-list begin il)
                     ; (print (list 8 t2))
                     ; (break)
                     (when (minusp (extract-term matrix begin begin))
                       (column-minus-5 mtrx-list begin)
                       ; (print (list 9 t2))
                       ; (break)
                       )))))
          ;; (Format t "~%  Finally the diagonal term is ~D." (aref matrix begin begin))
          (incf begin)))
      mtrx-list)))

#| ;; the choice of 2 in random-matrix makes list-mith work correctly
(setf p (idnt-mtrx 20) p-1 (idnt-mtrx 20)
      m (random-matrix 20 30 0.1 2)
      q (idnt-mtrx 30) q-1 (idnt-mtrx 30))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(third (list-smith list))
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(equal-matrix (idnt-mtrx 20) (mtrx-prdc p p-1))
(equal-matrix (idnt-mtrx 30) (mtrx-prdc q q-1))
|#

#| ;; the choice of 5 in random-matrix makes list-mith work NOT correctly
(setf p (idnt-mtrx 20) p-1 (idnt-mtrx 20)
      m (random-matrix 20 30 0.1 5)
      q (idnt-mtrx 30) q-1 (idnt-mtrx 30))
(setf list (list p p-1 m q q-1))
(setf t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(third (list-smith list))
(equal-matrix t1 (mtrx-prdc p (mtrx-prdc m q-1)))
(equal-matrix (idnt-mtrx 20) (mtrx-prdc p p-1))
(equal-matrix (idnt-mtrx 30) (mtrx-prdc q q-1))
|#

(DEFUN SMITH (matrix)
  (declare (type matrice matrix))
  (the list
    (let ((line-n (line-number matrix))
          (column-n (column-number matrix)))
      (declare (type fixnum line-n column-n))
      (list-smith
       (list (idnt-mtrx line-n) (idnt-mtrx line-n)
             matrix
             (idnt-mtrx column-n) (idnt-mtrx column-n))))))

;;;
;;;  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING
;;;  No protection via safe-x from now on
;;; 



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
  (declare (type fixnum n))
  (the list
  (do ((i (1- n) (1- i))
       (rslt +empty-list+ (cons (gnrt-name i) rslt)))
      ((minusp i) rslt)
      (declare
        (type fixnum i)
	(list rslt)))))

#|
(gnrt-name 4)
(gnrt-name-basis 4)
|#

(DEFUN ECHCM-KILL-EPI-F-INTR (cmpr first n m f+1-basis mtrx-list)
  (declare
   (type cmprf cmpr)
   (type fixnum first n m)
   (list f+1-basis mtrx-list))
  (let ((f+1 (1+ first))
        (m-n (- m n))
        (q-1 (fifth mtrx-list)))
    (declare (type fixnum f+1 m-n) (type matrice q-1))
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
                          (ic #|0|# 1))
                         ((endp cmbn-mark))
                       (declare
                        (list cmbn-mark basis-mark)
                        (type fixnum ic))
                       (with--term (cffc gnrt) cmbn-mark
                         (loop (when (eq :equal (funcall cmpr gnrt (car basis-mark)))
                                 (return))
                               (pop basis-mark)
                               (incf ic))
                         (dotimes (il m-n)
                           (incf (aref rslt-cffcs il)
                                 (* cffc (terme q-1 (+ n il #||# 1) ic))))
                         (pop basis-mark)
                         (incf ic)))
                     (do ((term-list +empty-list+)
                          (il (1- m-n) (1- il)))
                         ((minusp il) (make-cmbn :degr degr
                                                 :list term-list))
                       (declare
                        (list term-list)
                        (type fixnum il))
                       (let ((cffc (aref rslt-cffcs il)))
                         (declare (type fixnum cffc))
                         (unless (zerop cffc)
                           (push (term cffc (gnrt-name il)) term-list))))))))
      (the intr-mrph #'rslt))))
 
#|
(setf q-1 (creer-matrice 2 2))
(maj-matrice q-1 '((1 ((1 2) (2 3))) (2 ((1 1) (2 2)))))
(setf f (echcm-kill-epi-f-intr #'s-cmpr 2 1 2 '(a b)
                               (list 0 0 0 0 q-1)))
(funcall f (cmbn 2 1 'a))
(funcall f (cmbn 4 1 'a))
(funcall f (cmbn 3 1 'a 10 'b))

(setf q-1 (random-matrix 5 5 .5 10))
(setf f (echcm-kill-epi-f-intr #'s-cmpr 2 2 5 '(a b c d e)
		      (list 0 0 0 0 q-1)))
(funcall f (cmbn 2 1 'a))
(funcall f (cmbn 4 1 'a))
(funcall f (cmbn 3 1 'a 10 'b 100 'c 1000 'd 10000 'e))
|#

(DEFUN ECHCM-KILL-EPI-G-INTR (first n m f+1-basis mtrx-list)
  (declare (type fixnum first n m) (list f+1-basis mtrx-list))
  (let ((f+1 (1+ first))
        (m-n (- m n))
        (q (fourth mtrx-list)))
    (declare
     (type fixnum f+1 m-n)
     (type matrice q))
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
                          (ic #|0|# 1))
                         ((endp cmbn-mark))
                       (declare (list cmbn-mark basis-mark) (type fixnum ic))
                       (with--term (cffc gnrt) cmbn-mark
                         (loop (when (eq :equal (s-cmpr gnrt (car basis-mark)))
                                 (return))
                               (pop basis-mark)
                               (incf ic))
                         (dotimes (il m)
                           (incf (aref rslt-cffcs il)
                                 (* cffc (terme q #|il|# (+ il 1) (+ n ic)))))
                         (pop basis-mark)
                         (incf ic)))
                     (do ((term-list +empty-list+)
                          (il (1- m) (1- il)))
                         ((minusp il) (make-cmbn :degr degr
                                                 :list term-list))
                       (declare (list term-list) (type fixnum il))
                       (let ((cffc (aref rslt-cffcs il)))
                         (declare (type fixnum cffc))
                         (unless (zerop cffc)
                           (push (term cffc (nth il f+1-basis)) term-list))))))))
      (the intr-mrph #'rslt))))
    
#|
(setf q (random-matrix 5 5 0.5 10))
(setf g (echcm-kill-epi-g-intr 2 2 5 '(a b c d e)
		      (list 0 0 0 q 0)))
(funcall g (cmbn 2))
(funcall g (cmbn 4 1 'a))
(funcall g (cmbn 3 1 :gn-00000 10 :gn-00001 100 :gn-00002))
|#

(DEFUN ECHCM-KILL-EPI-H-INTR (cmpr first n m f-basis f+1-basis mtrx-list)
  (declare
   (type cmprf cmpr)
   (type fixnum first n m)
   (list f-basis f+1-basis mtrx-list))
  (let ((lqxp-1 (mtrx-prdc (left-submatrix (fourth mtrx-list) n) ;;; ***
                           (second mtrx-list))))
    (declare (type matrice lqxp-1))
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
                          (ic 1))
                         ((endp cmbn-mark))
                       (declare
                        (list cmbn-mark basis-mark)
                        (type fixnum ic))
                       (with--term (cffc gnrt) cmbn-mark
                         (loop (when (eq :equal (funcall cmpr gnrt (car basis-mark)))
                                 (return))
                               (pop basis-mark)
                               (incf ic))
                         (dotimes (il m)
                           (incf (aref rslt-cffcs il)
                                 (* cffc (terme lqxp-1 (1+ il) ic))))
                         (pop basis-mark)
                         (incf ic)))
                     (do ((term-list +empty-list+)
                          (il (1- m) (1- il)))
                         ((minusp il) (make-cmbn :degr (1+ degr)
                                                 :list term-list))
                       (declare (list term-list) (type fixnum il))
                       (let ((cffc (aref rslt-cffcs il)))
                         (declare (type fixnum cffc))
                         (unless (zerop cffc)
                           (push (term cffc (nth il f+1-basis)) term-list))))))))
      (the intr-mrph #'rslt))))

#|
(setf p-1 (random-matrix 2 2 0.5 10))
(setf q (random-matrix 5 5 0.5 10))
(setf h (echcm-kill-epi-h-intr #'s-cmpr 2 2 5 '(a b) '(a b c d e)
		      (list 0 p-1 0 q 0)))
(funcall h (cmbn 2 1 'a 1000 'b))
(funcall h (cmbn 4 1 'a))
|#

(DEFUN ECHCM-WITHOUT-EPI (echcm first n m intr-f)
  (declare
   (type chain-complex echcm)
   (type fixnum first n m)
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
                  (declare (type fixnum degr))
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
  (declare (type chain-complex echcm) (type fixnum first))
  (the reduction
    (with-slots (cmpr basis) echcm
      (declare
       (type cmprf cmpr)
       (type basis basis))
      (assert (not (eq basis :locally-effective)))
      (let* ((f-basis (funcall basis first))
             (f+1-basis (funcall basis (1+ first)))
             (mtrx-list (smith (chcm-mat echcm (1+ first)))) ;;; ***
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
         (type matrice smith)
         (type fixnum m n)
         (type intr-mrph intr-f intr-g intr-h)
         (type chain-complex echcm2))
        (assert (dotimes (i n +true+)
                  (unless (= 1 (terme smith (1+ i) (1+ i)))
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
(setf s3 (sphere 3))  ;;;  xxx
(setf s3-chml-clss (chml-clss s3 3))
(setf s3-fibration (z-whitehead s3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(homology s3-4 0 5)
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
(setf s3-4-chml-clss (chml-clss s3-4 4))
(setf s3-4-fibration (z2-whitehead s3-4 s3-4-chml-clss))
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
  (declare (type chain-complex chcm) (type fixnum first))
  (the reduction
    (let ((rdct (efhm chcm))
          (echcm (echcm chcm)))
      (declare (type reduction rdct) (type chain-complex echcm))
      (let ((last-rdct (echcm-kill-epi echcm first)))
        (declare (type reduction last-rdct))
        (setf (slot-value chcm 'efhm)
          (cmps last-rdct rdct))
        (setf (slot-value (efhm chcm) 'orgn )
          `(kill-epi ,chcm ,first))
        (efhm chcm)))))

(DEFUN KILL-EPIS (chcm first end)
  (declare (type chain-complex chcm) (type fixnum first end))
  (the equivalence
    (progn
      (do ((indx first (1+ indx)))
          ((= indx end))
        (declare (type fixnum indx))
        (kill-epi chcm indx))
      (efhm chcm))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf s3-chml-clss (chml-clss s3 3))
(setf s3-fibration (z-whitehead s3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(setf s3-4-chml-clss (chml-clss s3-4 4))
(setf s3-4-fibration (z2-whitehead s3-4 s3-4-chml-clss))
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
  (declare (type chain-complex chcm) (type fixnum first))
  (let* ((echcm (echcm chcm))
         (cmpr (cmpr echcm))
         (basis (basis echcm))
         (f-basis (funcall basis first))
         (mtrx-list (smith (chcm-mtrx echcm (1+ first))))
         (p-1 (second mtrx-list))
         (smith (third mtrx-list))
         (n (line-number smith))
         (m (column-number smith))       
#|       (diag-indx (dotimes (indx (min n m)
                                   (if (> n m)
                                       m
                                     (error "In CHML-CLSS, the cohomology-group ~@
                                      is null.")))
                      (declare (type fixnum indx))
                      (unless (= 1 (terme smith indx indx))
                        (return indx)))))
|#
         (diag-indx (do ((indx 1 (1+ indx))
                         (diag-length (min n m)))
                        ((> indx diag-length)
                         (if (> n m)
                             (1+ m)
                           (error "In CHML-CLSS, the cohomology-group ~@
                                      is null.")))
                      (declare (type fixnum indx diag-length))
                      (unless (= 1 (terme smith indx indx))
                        (return indx)))))
    (declare
     (type chain-complex echcm)
     (type cmprf cmpr)
     (type basis basis)
     (type fixnum n m diag-indx)
     (list f-basis mtrx-list)
     (type matrice p-1 smith))
    (flet ((rslt (cmbn)
                 (declare (type cmbn cmbn))
                 (with-cmbn (degr list) cmbn
                   (unless (= degr first)
                     (return-from rslt (zero-cmbn (- degr first))))
                   (do ((rslt 0)
                        (bmark f-basis)
                        (ic 1)
                        (cmark list (cdr cmark)))
                       ((endp cmark)
                        (if (zerop rslt)
                            (zero-cmbn 0)
                          (term-cmbn 0 rslt :z-gnrt)))
                     (declare
                      (type fixnum rslt)
                      (list bmark cmark))
                     (with--term (cffc gnrt) cmark
                       (loop
                         (when (eq :equal (funcall cmpr gnrt (car bmark)))
                           (return))
                         (pop bmark)
                         (incf ic))
                       (incf rslt (* cffc (terme p-1 diag-indx ic)))
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

(DEFUN CHML-CLSS-LAST (chcm first)
  (declare
    (type chain-complex chcm)
    (type fixnum first))
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
(setf s3-fibration (z-whitehead s3 s3-chml-clss))
(setf s3-4 (fibration-total s3-fibration))
(homology s3-4 0 6)
(kill-epi s3-4 2)
(setf s3-4-chml-clss (chml-clss s3-4 4))
(setf s3-4-fibration (z2-whitehead s3-4 4 s3-4-chml-clss))
(setf s3-5 (fibration-total s3-4-fibration))
(homology s3-5 0 6)
(kill-epis s3-5 3 5)
(setf s3-5p_i-chml-clss (chml-clss s3-5 5))
(setf s3-5-fibration (z2-whitehead s3-5 5 s3-5-chml-clss))
(setf s3-6 (fibration-total s3-5-fibration))
(homology s3-6 0 7)
|#

(DEFUN CHML-CLSS (chcm first)
  (declare (type chain-complex chcm) (type fixnum first))
  (the morphism
    (let* ((efhm (efhm chcm))
           (echcm (echcm chcm))
           (chml-clss-last (chml-clss-last echcm first)))
      (declare (type effective-homology efhm)
               (type chain-complex echcm)
               (type morphism chml-clss-last))
      (etypecase efhm
        (chain-complex chml-clss-last)
        (reduction (cmps chml-clss-last (f efhm)))
        (equivalence (cmps chml-clss-last (cmps (rf efhm) (lg efhm))))))))

#|
(cat-init)
(setf s3 (sphere 3))
(setf cc (chml-clss s3 3))
(? cc 3 's3)
(setf k (k-z 3))
(setf cc (chml-clss k 3))
(? cc 3 (gbar 3 0 (gbar 2 0 '(12) 0 nil) 1 (gbar 0) 0 (gbar 0)))
|#
