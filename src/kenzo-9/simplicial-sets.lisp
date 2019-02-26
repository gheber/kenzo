;;;  SIMPLICIAL-SETS  SIMPLICIAL-SETS  SIMPLICIAL-SETS
;;;  SIMPLICIAL-SETS  SIMPLICIAL-SETS  SIMPLICIAL-SETS
;;;  SIMPLICIAL-SETS  SIMPLICIAL-SETS  SIMPLICIAL-SETS

(IN-PACKAGE #:cat-9)

(PROVIDE "simplicial-sets")


(DEFVAR *SMST-LIST*)
(SETF *SMST-LIST* +empty-list+)
(PUSHNEW '*SMST-LIST* *list-list*)

(DEFUN ABSM-PRINT (absm stream depth)
  (declare
   (type absm absm)
   (type stream stream)
   (ignore depth))
  (format stream
      "<AbSm ~A ~A>"
      (hyphenize-list (dgop-int-ext (dgop absm)))
      (gmsm absm))
  absm)

(DEFUN FACE4 (smst indx dmns gmsm-or-absm)
   (if (typep gmsm-or-absm 'absm)
      (a-face4 (face1 smst) indx dmns gmsm-or-absm)
      (funcall (face1 smst) indx dmns gmsm-or-absm)))

(DEFMETHOD PRINT-OBJECT ((smst simplicial-set) stream)
  (declare (type stream stream))
  (the simplicial-set
    (progn
      (format stream "[K~D Simplicial-Set]" (idnm smst))
      smst)))


(DEFUN SMST (n)
   (declare (type fixnum n))
   (the (or simplicial-set null)
      (find n *smst-list* :key #'idnm)))

(DEFUN BUILD-SMST
    (&key cmpr basis bspn face face* intr-bndr bndr-strt
	  intr-dgnl dgnl-strt orgn)
   (declare
      (type cmprf cmpr)
      (type basis basis)
      (type gmsm bspn)
      (type face face)
      (type (or null face*) face*)
      (type (or intr-mrph null) intr-bndr intr-dgnl)
         (type (or strt null) bndr-strt dgnl-strt)
      (list orgn))
   (let ((already (find orgn *smst-list* :test #'equal :key #'orgn)))
      (declare (type (or simplicial-set null) already))
      (when already
         (return-from build-smst already)))
   (if intr-bndr
      (unless bndr-strt
            (error "In BUILD-SMST, an intr-bndr is given but not its strategy."))
      (cond (face*
                (setf bndr-strt :gnrt
                      intr-bndr (face*-bndr cmpr face*)))
            (face
               (setf bndr-strt :gnrt
                     intr-bndr (face-bndr cmpr face)))))
   (if intr-dgnl
      (unless dgnl-strt
            (error "In BUILD-SMST, an intr-dgnl is given but not its strategy."))
      (setf dgnl-strt :gnrt
            intr-dgnl (intr-diagonal face)))
   (the simplicial-set
      (let ((rslt (build-chcm :cmpr cmpr :basis basis :bsgn bspn
                     :intr-dffr intr-bndr
                        :strt bndr-strt :orgn orgn)))
         (change-chcm-to-clgb rslt
          :intr-cprd intr-dgnl :cprd-strt dgnl-strt)
         (setf (slot-value (dgnl rslt) 'orgn)
               `(diagonal ,rslt))
         (change-class rslt 'simplicial-set)
         (setf (slot-value rslt 'face) face)
         (push rslt *smst-list*)
         rslt)))

(DEFUN A-BASIS2 (basis dmns)
  (declare
     (type basis basis)
     (type fixnum dmns))
  (the list
    (let ((basis-array (make-array (1+ dmns) :element-type 'list)))
      (declare (type (vector list) basis-array))
      (dotimes (i (1+ dmns))
	(declare (type fixnum i))
	(setf (aref basis-array i) (funcall basis i)))
      (do ((dgop (mask dmns) (1- dgop))
	   (rslt +empty-list+
		 (nconc (mapcar
			 #'(lambda (gmsm)
			     (declare (type gmsm gmsm))
			     (absm dgop gmsm))
			 (aref basis-array (- dmns (logcount dgop))))
			rslt)))
	  ((minusp dgop) rslt)))))

#|
(setf m (moore 2 2))
(dotimes (i 4)
  (print (basis m i :dgnr)))
|#

(DEFUN A-BASIS1 (basis)
  (declare
   (type basis basis))
  (the basis
    (flet
        ((frslt
          (dmns)
          (declare (type fixnum dmns))
          (the list
            (let ((basis-array (make-array (1+ dmns) :element-type 'list)))
              (declare (type (vector list) basis-array))
              (dotimes (i (1+ dmns))
                (declare (type fixnum i))
                (setf (aref basis-array i) (funcall basis i)))
              (do ((dgop (mask dmns) (1- dgop))
                   (rslt +empty-list+
                         (nconc (mapcar
                                    #'(lambda (gmsm)
                                        (declare (type gmsm gmsm))
                                        (absm dgop gmsm))
                                  (aref basis-array (- dmns (logcount dgop))))
                                rslt)))
                  ((minusp dgop) rslt)
                (declare (type fixnum dgop) (type list rslt)))))))
      #'frslt)))

#|
(setf m (moore 2 2))
(setf b (a-basis1 (basis m)))
(dotimes (i 4)
  (print (funcall b i))
|#

(DEFUN INTR-DIAGONAL (face)
  (declare (type face face))
  (flet ((rslt (dmns gmsm)
               (declare
                (type fixnum dmns)
                (type gmsm gmsm))
               (the cmbn
                 (let ((del-0-s +empty-list+)
                       (rslt +empty-list+))
                   (declare (list del-0-s rslt))
                   (do ((dmns dmns (1- dmns))
                        (absm (absm 0 gmsm) (a-face4 face 0 dmns absm)))
                       ((zerop dmns) (push absm del-0-s))
                     (declare (type fixnum dmns) (type absm absm))
                     (push absm del-0-s))
                   (do ((ldmns dmns (1- ldmns))
                        (rdmns 0 (1+ rdmns))
                        (absm (absm 0 gmsm) (a-face4 face ldmns ldmns absm))
                        (mark-del-0 del-0-s (cdr mark-del-0)))
                       ((zerop ldmns)
                        (push (term 1 (tnpr 0 (gmsm absm) dmns gmsm))
                              rslt))
                     (declare
                      (type fixnum ldmns rdmns)
                      (type gmsm gmsm)
                      (list mark-del-0))
                     (unless (or (plusp (dgop absm))
                                 (plusp (dgop (car mark-del-0))))
                       (push (term 1 (tnpr ldmns (gmsm absm)
                                           rdmns (gmsm (car mark-del-0))))
                             rslt)))
                   (make-cmbn :degr dmns :list rslt)))))
    (the intr-mrph #'rslt)))

#|
()
(setf cmpr #'f-cmpr
      face #'delta-face)
(setf diag (intr-diagonal face))
(funcall diag 4 (mask 5))
(funcall diag 0 4)
(setf cmpr #'s-cmpr
      face (sphere-face 4))
(setf diag (intr-diagonal face))
(funcall diag 4 's4)
(funcall diag 0 '*)
(setf s4 (sphere 4))
(dgnl s4 4 's4)
|#                 

(DEFUN FACE-BNDR (cmpr face)
   (declare
      (type cmprf cmpr)
      (type face face))
   (flet ((rslt (dmns gmsm)
             (declare
                (type fixnum dmns)
                (type gmsm gmsm))
             (the cmbn
                (progn
                   (when (zerop dmns)
                      (return-from rslt +zero-negative-cmbn+))
                   (let ((pre-rslt +empty-list+))
                      (declare (list pre-rslt))
                             ;; (list (cons cffc face))
                      (dotimes (indx (1+ dmns))
                         (declare (type fixnum indx))
                         (let ((face (funcall face indx dmns gmsm)))
                            (declare (type absm face))
                            (unless (degenerate-p face)
                               (push (term (-1-expt-n indx) (gmsm face))
                                  pre-rslt))))
                      (apply #'nterm-add cmpr (1- dmns) pre-rslt))))))
      (the intr-mrph #'rslt)))

#|
  (setf face #'(lambda (indx dmns gmsm)
                  (absm 0 (append
                             (subseq gmsm 0 indx)
                             (subseq gmsm (1+ indx))))))
  (setf bndr (face-bndr #'l-cmpr face))
  (funcall bndr 0 '(a))
  (funcall bndr 1 '(a b))
  (funcall bndr 2 '(a b c))
  (funcall bndr 3 '(a b c d))
  (funcall bndr 3 '(d c b a))
  (funcall bndr 1 '(a a))
  (funcall bndr 2 '(a a a))
  (funcall bndr 3 '(a a a a))
  (setf face #'(lambda (index dmns gmsm)
                  (absm (dgop-ext-int (nreverse (<a-b< 0 (1- dmns)))) 'a)))
  (setf bndr (face-bndr #'s-cmpr face))
  (funcall bndr 1 '(a b))
  (funcall bndr 2 '(a b c)))
|#

(DEFUN FACE*-BNDR (cmpr face*)
   (declare
      (type cmprf cmpr)
      (type face* face*))
   (flet ((rslt (dmns gmsm)
             (declare
                (type fixnum dmns)
                (type gmsm gmsm))
             (the cmbn
                (progn
                   (when (zerop dmns)
                      (return-from rslt +zero-negative-cmbn+))
                   (let ((pre-rslt +empty-list+))
                      (declare (list pre-rslt))
                             ;; (list (list gmsm fixnum))
                      (dotimes (indx (1+ dmns))
                         (declare (type fixnum indx))
                         (let ((face (funcall face* indx dmns gmsm)))
                            (declare (type (or gmsm (eql :degenerate)) face))
                            (unless (eq face :degenerate)
                               (push (term (-1-expt-n indx) face) pre-rslt))))
                      (apply #'nterm-add cmpr (1- dmns) pre-rslt))))))
      (the intr-mrph #'rslt)))

#|
  (setf face* 
        #'(lambda (indx dmns gmsm)
             (if (and (= dmns 3) (evenp indx))
                'm2
                :degenerate)))
  (setf bndr (face*-bndr #'s-cmpr face*))
  (funcall bndr 3 'm3)
  (funcall bndr 2 'm2)
  (funcall bndr 0 '*)
  (setf face* 
        #'(lambda (indx dmns gmsm)
             (if (and (= dmns 3) (< indx 2))
                'm2
                :degenerate)))
  (setf bndr (face*-bndr #'s-cmpr face*))
  (funcall bndr 3 'm3)
  (funcall bndr 2 'm2)
  (funcall bndr 0 '*))
|#

(DEFUN A-CMPR3 (cmpr absm1 absm2)
   (declare
      (type cmprf cmpr)
      (type absm absm1 absm2))
   (the cmpr
      (with-absm (dgop1 gmsm1) absm1
      (with-absm (dgop2 gmsm2) absm2
         (lexico
            (f-cmpr dgop1 dgop2)
            (funcall cmpr gmsm1 gmsm2))))))

#|
  (a-cmpr3 #'s-cmpr (absm 0 'a) (absm 1 'b))
  (a-cmpr3 #'s-cmpr (absm 2 'a) (absm 1 'b))
  (a-cmpr3 #'s-cmpr (absm 1 'a) (absm 1 'b))
  (a-cmpr3 #'s-cmpr (absm 1 'c) (absm 1 'b))
  (a-cmpr3 #'s-cmpr (absm 1 'a) (absm 1 'a))
|#

(DEFUN BSPN-P (cmpr bspn dmns absm)
   ;; this function makes sense only in the simplicial context
   ;; so that the basg (= base-generator) is called bspn (= base-point)
   (declare
      (type gmsm bspn)
      (type fixnum dmns)
      (type absm absm))
   (the boolean
      (with-absm (dgop gmsm) absm
         (and (= dgop (mask dmns))
              (eq :equal (funcall cmpr bspn gmsm))))))

#|
  (setf d (delta-infinity))
  (setf cmpr (cmpr d))
  (bspn-p cmpr 1 5 (absm (mask 5) 1))
  (bspn-p cmpr 1 5 (absm (mask 5) 2))
  (bspn-p cmpr 1 5 (absm (mask 4) 3))
|#

(DEFUN DLOP-EXT-INT (ext-dlop)
   (declare (list ext-dlop))
          ;; (list fixnum)
   (when ext-dlop   
      (unless (apply #'< ext-dlop)
         (error "In DLOP-EXT-INT, the external dlop ~A is not increasing." ext-dlop)))
   (the fixnum
      (apply #'logxor
         (mapcar #'(lambda (item)
                      (declare (type fixnum item))
                      (the fixnum (2-exp item)))
            ext-dlop))))
#|
  (dlop-ext-int '())
  (dlop-ext-int '(2 2))
  (dlop-ext-int '(0 4)))
|#

(DEFUN DLOP-INT-EXT (dlop)
   (declare (type fixnum dlop))
   (the list
      (nreverse (dgop-int-ext dlop))))

#|
  (dlop-int-ext 0)
  (dlop-int-ext 4)
  (dlop-int-ext 63)
  (dotimes (i 33)
     (print (dlop-ext-int (dlop-int-ext i)))))
|#

(DEFUN 1DGOP*DGOP (1dgop dgop)
   (declare (type fixnum 1dgop dgop))
   ;; 1dgop is an ordinary integer
   ;; dgop is a bit string
   (the fixnum
      (let ((share (ash -1 1dgop)))
         (declare (type fixnum share))
         (logxor
            (ash (logand share dgop) 1)
            (2-exp 1dgop)
            (logandc1 share dgop)))))
      
#|
  (dotimes (i 20)
     (dotimes (j 5)
       (format t "~% (~D) o ~A = ~A"
                 j (dgop-int-ext i) (dgop-int-ext (1dgop*dgop j i))))))
|#

(DEFUN DGOP*DGOP (dgop1 dgop2)
   (declare (type fixnum dgop1 dgop2))
   (let ((dgop 0)
         (bmark 0))
      (declare (type fixnum dgop bmark))
      (loop
         (when (zerop dgop1)
            (return-from dgop*dgop
               (logxor dgop (ash dgop2 bmark))))
         (when (zerop dgop2)
            (return-from dgop*dgop
               (logxor dgop (ash dgop1 bmark))))
         (cond ((evenp dgop1)
                (when (oddp dgop2)
                   (incf dgop (2-exp bmark)))
                (setf dgop2 (ash dgop2 -1)))
               (t
                  (incf dgop (2-exp bmark))))
         (setf dgop1 (ash dgop1 -1))
         (incf bmark))))

#|
  (dotimes (i 10)
     (dotimes (j 10)
        (format t "~%~A o ~A = ~A"
                  (dgop-int-ext i) (dgop-int-ext j)
                  (dgop-int-ext (dgop*dgop i j))))))
|#

(DEFUN REMOVE-BIT (n indx)
   (declare (type fixnum n indx))
   (the fixnum
      (let ((cut (2-exp indx)))
         (multiple-value-bind (quotient remainder) (truncate n cut)
            (declare (type fixnum quotient remainder))
            (+ (ash (ash quotient -1) indx) remainder)))))

#|
  (remove-bit 107 3)  ;; 51
|#

(DEFUN DGOP/DGOP (dgop1 dgop2)
   (declare (type fixnum dgop1 dgop2))
   (the fixnum
      (do ((indx (1- (integer-length dgop2)) (1- indx)))
          ((minusp indx) dgop1)
         (declare (type fixnum indx))
         (when (logbitp indx dgop2)
            (setf dgop1 (remove-bit dgop1 indx))))))

#|
  (dgop/dgop 67 0)
  (dgop/dgop 68 4)
  (dgop/dgop 31 5)
|#      

(DEFUN 1DLOP-DGOP (1dlop dgop)
   (declare (type fixnum 1dlop dgop))
   ;; 1dlop is an ordinary fixnum
   ;; dgop is a bit string
   (the (values fixnum (or fixnum null))
      (progn
         (when (logbitp 1dlop dgop)
            (let ((share (ash -1 1dlop)))
               (declare (type fixnum share))
               (return-from 1dlop-dgop
                  (values
                     (logxor
                        (logand share (ash dgop -1))
                        (logandc1 share dgop))
                     nil))))
         (when (and (plusp 1dlop)
                    (logbitp (1- 1dlop) dgop))
            (let ((share (ash -1 1dlop)))   ;;;
               (declare (type fixnum share))
	       (setf share (ash share -1))  ;;; because of the compiler bug
               (return-from 1dlop-dgop
                  (values
                     (logxor
                        (logand share (ash dgop -1))
                        (logandc1 share dgop))
                     nil))))
         (let ((share (ash -1 1dlop)))
            (declare (type fixnum share))
            (let ((right (logandc1 share dgop)))
               (declare (type fixnum right))
               (values
                  (logxor right (logand share (ash dgop -1)))
                  (- 1dlop (logcount right))))))))

#|
  (dotimes (i 5)
     (dotimes (j 17)
        (multiple-value-bind (dgop 1dlop) (1dlop-dgop i j)
           (format t "~% del-~D o ~A = ~A o del-~A"
                     i (dgop-int-ext j) (dgop-int-ext dgop) 1dlop)))))
|#


(DEFUN A-FACE4 (face indx dmns absm) 
  (declare
   (type face face)
   (type fixnum indx dmns)
   (type absm absm))
  (the absm
    (with-absm (dgop gmsm) absm
      (multiple-value-bind (dgop2 1dlop) (1dlop-dgop indx dgop)
;       (declare (type fixnum dgop2)
;                #-lispworks (type (or fixnum nil) 1dlop))
;                 ;; Bug LispWorks if this or-type defined
        (declare (type fixnum dgop2) (type (or fixnum null) 1dlop))
        (unless 1dlop
          (return-from a-face4 (absm dgop2 gmsm)))
        (locally (declare (type fixnum 1dlop))
          (let ((gmsm-face (funcall face 1dlop (- dmns (logcount dgop)) gmsm)))
            (declare (type absm gmsm-face))
            (with-absm (dgop3 gmsm3) gmsm-face
              (absm (dgop*dgop dgop2 dgop3) gmsm3))))))))
#|
()
(require "special-smsts")
(let ((simp (dlop-ext-int '(0 1 2 3 4 5 6))))
   (dotimes (i 5)
      (dotimes (j 17)
         (let ((face (a-face4 (face (delta-infinity))
                        i (+ 6 (logcount j)) (absm j simp))))
            (format t "~%del-~D o ~A (0 1 2 3 4 5 6) = ~A ~A"
                      i (dgop-int-ext j)
                      (dgop-int-ext (dgop face))
                      (dlop-int-ext (gmsm face)))))))
|#

(DEFUN 1DGNR (indx absm)
   (declare
      (type fixnum indx)
      (type absm absm))
   (the absm
      (with-absm (dgop gmsm) absm
         (absm (1dgop*dgop indx dgop) gmsm))))

#|
(1dgnr 2 (absm 17 'a))
|#

(DEFUN NDGNR (dgop absm)
   (declare
      (type fixnum dgop)
      (type absm absm))
   (the absm
      (with-absm (dgop2 gmsm) absm
         (absm (dgop*dgop dgop dgop2) gmsm))))

#|
  (ndgnr 2 (absm 17 'a))
|#


(DEFUN NFACE (face dlop dmns gmsm)
  (declare
     (type face face)
     (type dlop dlop)
     (type fixnum dmns)
     (type gmsm gmsm))
  (do* ((gmsm gmsm)
	(dmns dmns)
	(dgop 0)
	(dlop dlop (- dlop (2-exp bark)))
	(bark (1- (integer-length dlop))
	      (1- (integer-length dlop))))
       ((zerop dlop) (absm dgop gmsm))
      (declare
         (type gmsm gmsm)
	 (type fixnum dgop dlop bark))
      (multiple-value-bind (dgop2 indx) (1dlop-dgop bark dgop)
	 (declare
	    (type fixnum dgop2)
	    (type (or null fixnum) indx))
	 (if indx
	    (locally (declare (type fixnum indx))
	       (let ((new-absm (funcall face indx dmns gmsm)))
		  (declare (type absm new-absm))
		  (with-absm (dgop3 gmsm2) new-absm
		     (setf gmsm gmsm2
			   dmns (- dmns 1
				   (logcount dgop3))
			   dgop (dgop*dgop dgop2 dgop3)))))
  	     (setf dgop dgop2)))))

#|
  (setf f (face (delta-infinity)))
  (dotimes (dlop 31)
    (print (dlop-int-ext (gmsm (nface f dlop 4 31)))))
  (setf p (r-proj-space))
  (setf f (face p))
  (dotimes (dlop 31)
    (format t "~%~A ~A"
       (hyphenize-list (dlop-int-ext dlop))
       (nface f dlop 4 4)))
|#

(DEFUN CHECK-FACES (cmpr face dmns gmsm)
   (declare
      (type cmprf cmpr)
      (type face face)
      (type fixnum dmns)
      (type gmsm gmsm))
   (the boolean
      (progn      
         (when (> dmns 1)
            (let ((dmns-1 (1- dmns)))
               (declare (type fixnum dmns-1))
               (dotimes (i dmns)
                  (declare (type fixnum i))
                  (dotimes (j (1+ i))
                     (declare (type fixnum j))
                     (let ((rslt1 (a-face4 face i dmns-1 (funcall face j dmns gmsm)))
                           (rslt2 (a-face4 face j dmns-1 (funcall face (1+ i) dmns gmsm))))
                        (declare (type absm rslt1 rslt2))
                        (unless (eq (a-cmpr3 cmpr rslt1 rslt2) :equal)
                           (cerror "CHECK-FACES will return NIL."
                              "Noncoherent boundary operators detected by CHECK-FACES :~@
                               Simplex => ~A~@
                               del_~D o del_~D => ~A~@
                               del_~D o del_~D => ~A"
                              gmsm i j rslt1 j (1+ i) rslt2)
                              (return-from check-faces nil)))))))
         t)))

#|
  (setf d (delta-infinity))
  (check-faces #'f-cmpr (face d) 4 31)
|#

(DEFUN CHECK-SMST (smst dmns1 &optional (dmns2 (1+ dmns1)))
   (declare
      (type simplicial-set smst)
      (type fixnum dmns1 dmns2))
   (with-slots (cmpr basis face) smst
      (declare
         (type cmprf cmpr)
         (type face face))
   (when (eq basis :locally-effective)
      (error "In CHECK-SMST, the locally-effective simplicial-set ~A~@
              cannot be checked." smst))
      (do ((rslt t)
           (dmns dmns1 (1+ dmns)))
          ((>= dmns dmns2) rslt)
         (declare
            (type boolean rslt)
            (type fixnum dmns))
         (format t "~%Checking the ~D-simplices..." dmns)
         (dolist (gmsm (funcall basis dmns))
            (declare (type gmsm gmsm))
            (unless (check-faces cmpr face dmns gmsm)
               (setf rslt nil))))))

#|
  (check-smst (delta-infinity) 5)
  (check-smst (delta 3) 2)
  (check-smst (delta 3) 2 4)
|#

(DEFUN SMST-ACHCM-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (the cmprf
    (flet
        ((rslt
          (absm1 absm2)
          (declare (type absm absm1 absm2))
          (the cmpr
            (lexico
             (f-cmpr (dgop absm1) (dgop absm2))
             (funcall cmpr (gmsm absm1) (gmsm absm2))))))
      (the function #'rslt))))

#|
(setf d (delta-infinity))
(setf c (smst-achcm-cmpr (cmpr d)))
(funcall c (absm 0 3) (absm 1 0))
(funcall c (absm 1 0) (absm 0 3))
(funcall c (absm 0 3) (absm 0 6))
(funcall c (absm 0 6) (absm 0 3))
(funcall c (absm 0 6) (absm 0 6))
|#

#|#|

(DEFUN SMST-ACHCM-DFFR-IMPL

(DEFUN BUILD-CHCM (&key cmpr basis bsgn intr-dffr strt orgn))

(DEFUN SMST-ACHCM (smst)
  (declare (type simplicial-set smst))
  (the chain-complex
    (build-chcm :cmpr (smst-achcm-cmpr (cmpr smst))
                :basis (a-basis1 smst)
                :bsgn (bspn smst)
                
                
                
                
|#|#
