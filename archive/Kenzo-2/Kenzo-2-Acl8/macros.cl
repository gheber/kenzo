;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS
;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS
;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS

(IN-PACKAGE "CAT")

(PROVIDE "macros")

;;;
;;;  VARIOUS
;;;

(DEFMACRO -1-EXPT-N (n)
  `(the fixnum (if (evenp (the fixnum ,n)) 1 -1)))

(DEFMACRO -1-EXPT-N+1 (n)
  `(the fixnum (if (evenp (the fixnum ,n)) -1 1)))

(DEFMACRO -1-EXPT-N-1 (n)
  `(-1-expt-n+1 ,n))

#|
  (-1-expt-n 5)
  (-1-expt-n 6)
  (-1-expt-n+1 5)
  (-1-expt-n+1 6)
  (-1-expt-n-1 5)
  (-1-expt-n-1 6))
|#

;;; (DEFCONSTANT +2-EXP+ (make-array (integer-length most-positive-fixnum)
;;;                         :element-type 'fixnum))
;;; 
;;; (dotimes (i (integer-length most-positive-fixnum))
;;;    (setf (aref +2-exp+ i) (the fixnum (expt 2 i))))


(DEFCONSTANT +2-EXP+
  (let ((rslt (make-array (integer-length most-positive-fixnum)
                        :element-type 'fixnum)))
    (declare (type (array fixnum 1) rslt))
    (dotimes (i (integer-length most-positive-fixnum) rslt)
      (setf (aref rslt i) (the fixnum (expt 2 i))))))

(DEFMACRO 2-EXP (n)
   `(aref +2-exp+ ,n))

;;; (DEFCONSTANT +MASK+ (make-array (1+ (integer-length most-positive-fixnum))
;;;                         :element-type 'fixnum))
;;; 
;;; (dotimes (i (1+ (integer-length most-positive-fixnum)))
;;;    (setf (aref +mask+ i) (the fixnum (1- (expt 2 i)))))

(DEFCONSTANT +MASK+
  (let ((rslt (make-array (integer-length most-positive-fixnum)
                        :element-type 'fixnum)))
    (declare (type (array fixnum 1) rslt))
    (dotimes (i (integer-length most-positive-fixnum) rslt)
      (setf (aref rslt i) (the fixnum (1- (expt 2 i)))))))


(DEFMACRO MASK (n)
   `(aref +mask+ ,n))

(DEFMACRO BINOMIAL-P-Q (p q)
  `(binomial-n-p (+ ,p ,q) ,p))

;;;
;;;  COMBINATIONS
;;;

(DEFMACRO LEXICO (&rest rest)
   (unless (cdr rest)
      (return-from lexico (first rest)))
   (let ((vrslt (gensym)))
      `(let ((,vrslt ,(first rest)))
          (declare (type cmpr ,vrslt))
          (if (eq ,vrslt :equal)
             (lexico ,@(rest rest))
             ,vrslt))))

#|
  (let ((#+ACLPC allegro:*top-print-level*
         #-ACLPC     *print-level* nil))
    (declare (special
                #+aclpc allegro:*top-print-level*
                #-aclpc *print-level*))
     (pprint (macroexpand-1
               '(lexico
                   comparison1
                   comparison2
                   comparison3
                   comparison4)))))
|#

(DEFMACRO TERM (cffc gnrt)
  `(cons ,cffc ,gnrt))

(DEFMACRO -TERM (mark)
   `(car ,mark))

(DEFMACRO CFFC (term)
   `(car ,term))

(DEFMACRO -CFFC (mark)
   `(caar ,mark))

(DEFMACRO GNRT (term)
  `(cdr ,term))

(DEFMACRO -GNRT (mark)
   `(cdar ,mark))

(DEFMACRO WITH-TERM ((cffc gnrt) term . body)
   `(let (,@(if cffc `((,cffc (cffc ,term))) nil)
          ,@(if gnrt `((,gnrt (gnrt ,term))) nil))
       (declare
          (fixnum ,@(if cffc `(,cffc) nil))
          (type gnrt ,@(if gnrt `(,gnrt) nil)))
       ,@body))

#|
  (macroexpand-1 '(with-term (cffc gnrt) term
                     (statement-1)
                     (statement-2)))
  (macroexpand-1 '(with-term (nil gnrt) term
                     (statement-1)
                     (statement-2)))
|#

(DEFMACRO WITH--TERM ((cffc gnrt) mark . body)
   `(let ((,cffc (-cffc ,mark))
          (,gnrt (-gnrt ,mark)))
       (declare
          (fixnum ,cffc)
          (type gnrt ,gnrt))
       ,@body))

#|
  (macroexpand-1 '(with--term (cffc gnrt) mark
                     (statement-1)
                     (statement-2)))
|#

(DEFMACRO MAKE-CMBN (&key degr list)
   `(cons :cmbn
       (cons ,degr ,list)))

#|
  (make-cmbn :degr 3 :list '((1 . a) (2 . b)))
|#          

(DEFMACRO CMBN-DEGR (cmbn)
   `(cadr ,cmbn))

(DEFMACRO CMBN-LIST (cmbn)
   `(cddr ,cmbn))

#|
  (cmbn-degr (cons :cmbn (cons 5 nil)))
  (cmbn-list (cons :cmbn (cons 5 nil)))
|#

(DEFMACRO WITH-CMBN ((degr list) cmbn . body)
   `(let ((,degr (cmbn-degr ,cmbn))
          (,list (cmbn-list ,cmbn)))
       (declare
          (fixnum ,degr)
          (list ,list))
       ,@body))

#|
  (macroexpand-1 '(with-cmbn (degr list) cmbn
                     (statement-1)
                     (statement-2)))
|#

(DEFMACRO TERM-CMBN (degr cffc gnrt)
   `(cons :cmbn (cons ,degr (cons (cons ,cffc ,gnrt) nil))))

(DEFMACRO CMBN-NON-ZERO-P (cmbn)
   `(cmbn-list ,cmbn)) 

(DEFMACRO CMBN-ZERO-P (cmbn)
  `(null (cmbn-list ,cmbn)))


;;;
;;;  CHAIN COMPLEXES
;;;

(DEFMACRO CMPR (&rest rest)
   (ecase (length rest)
      (1 `(cmpr1 ,@rest))
      (3 `(cmpr3 ,@rest))))

(DEFMACRO CMPR3 (object item1 item2)
   `(funcall (cmpr1 ,object) ,item1 ,item2))

#|
  (macroexpand '(cmpr chcm))
  (macroexpand '(cmpr chcm item1 item2))
|#

(DEFMACRO BASIS (&rest rest)
   (ecase (length rest)
      (1 `(basis1 ,@rest))
      (2 `(basis2 ,@rest))
      (3 `(basis3 ,@rest))))

(DEFUN BASIS2 (object n)         ;;;
   (declare
      (type t object)
      (fixnum n))
   (the list
      (with-slots (basis) object
         (declare (type basis basis))
         (when (eq :locally-effective basis)
            (error "The object ~A is locally-effective." object))
         (funcall basis n))))

(DEFMACRO BASIS3 (smst dmns keyword)
  (declare (ignore keyword))  
  `(a-basis2 (basis ,smst) ,dmns))

(DEFMACRO DFFR (&rest rest)
   (ecase (length rest)
      (1 `(dffr1 ,@rest))
      ((2 3) `(? (dffr1 ,(first rest)) ,@(rest rest)))))

#|
  (macroexpand '(dffr chcm))
  (macroexpand '(dffr chcm cmbn))
  (macroexpand '(dffr chcm degr gnrt))
|#

;;; Result = (:rslt gnrt value clnm rntm . prpr)

(DEFMACRO MAKE-RSLT (&key gnrt value clnm rntm)
   `(list :rslt ,gnrt ,value ,clnm ,rntm))

(DEFMACRO RSLT-GNRT (rslt)
   `(second ,rslt))

(DEFMACRO RSLT-VALUE (rslt)
   `(third ,rslt))

(DEFMACRO RSLT-CLNM (rslt)
   `(fourth ,rslt))

(DEFMACRO RSLT-RNTM (rslt)
   `(fifth ,rslt))

(DEFMACRO ? (&rest rest)
   (ecase (length rest)
      (2 `(?2 ,@rest))
      (3 `(?3 ,@rest))))

(DEFMACRO CONTROL (cmpr cmbn)
   (if *cmbn-control*
      `(do-control ,cmpr ,cmbn)
      cmbn))

(DEFMACRO CONTROLN (cmpr cmbn)
   (if *cmbn-control*
      `(do-control ,cmpr ,cmbn)
      nil))

(DEFVAR *WRONG-CMBN*)


;;;
;;;  CHCM-ELEMENTARY-OP
;;;

(DEFMACRO I-CMPS (mrph1 &rest rest)
   (if rest
      `(cmps ,mrph1 (i-cmps ,@rest))
      mrph1))

#|
  (macroexpand '(i-cmps m1))
  (macroexpand '(i-cmps m1 m2))
  (macroexpand '(i-cmps m1 m2 m3))
|#

(DEFMACRO I-ADD (mrph1 &rest rest)
   (if rest
      `(add ,mrph1 (i-add ,@rest))
      mrph1))

#|
  (macroexpand '(i-add m1))
  (macroexpand '(i-add m1 m2))
  (macroexpand '(i-add m1 m2 m3))
|#

(DEFMACRO I-SBTR (mrph1 mrph2 &rest rest)
   `(sbtr ,mrph1 (i-add ,mrph2 ,@rest)))
   
#|
  (macroexpand '(i-sbtr m1 m2))
  (macroexpand '(i-sbtr m1 m2 m3))
|#

;;;
;;;  EFFECTIVE HOMOLOGY
;;;

(DEFMACRO BCC (&rest rest)
   (case (length rest)
      (1 `(bcc1 ,@rest))
      (otherwise
         `(? (bcc1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO TCC (&rest rest)
   (case (length rest)
      (1 `(tcc1 ,@rest))
      (otherwise
         `(? (tcc1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO F (&rest rest)
   (case (length rest)
      (1 `(f1 ,@rest))
      (otherwise
         `(? (f1 ,(first rest)) ,@(rest rest)))))

#|
  (macroexpand '(f rdct))
  (macroexpand '(f rdct cmbn))
  (macroexpand '(f rdct degr gnrt))
|#

(DEFMACRO G (&rest rest)
   (case (length rest)
      (1 `(g1 ,@rest))
      (otherwise
         `(? (g1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO H (&rest rest)
   (case (length rest)
      (1 `(h1 ,@rest))
      (otherwise
         `(? (h1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO LBCC (&rest rest)
   (case (length rest)
      (1 `(lbcc1 ,@rest))
      (otherwise
         `(? (lbcc1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO RBCC (&rest rest)
   (case (length rest)
      (1 `(rbcc1 ,@rest))
      (otherwise
         `(? (rbcc1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO LF (&rest rest)
   (case (length rest)
      (1 `(lf1 ,@rest))
      (otherwise
         `(? (lf1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO LG (&rest rest)
   (case (length rest)
      (1 `(lg1 ,@rest))
      (otherwise
         `(? (lg1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO LH (&rest rest)
   (case (length rest)
      (1 `(lh1 ,@rest))
      (otherwise
         `(? (lh1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO RF (&rest rest)
   (case (length rest)
      (1 `(rf1 ,@rest))
      (otherwise
         `(? (rf1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO RG (&rest rest)
   (case (length rest)
      (1 `(rg1 ,@rest))
      (otherwise
         `(? (rg1 ,(first rest)) ,@(rest rest)))))

(DEFMACRO RH (&rest rest)
   (case (length rest)
      (1 `(rh1 ,@rest))
      (otherwise
         `(? (rh1 ,(first rest)) ,@(rest rest)))))

;;;
;;;  CONES
;;;

(DEFMACRO BCNX (bicn)
   `(car ,bicn))

(DEFMACRO IBICN (bicn)
   `(cdr ,bicn))

(DEFMACRO BCNB (gnrt)
   `(cons :bcnb ,gnrt))

(DEFMACRO BCNC (gnrt)
   `(cons :bcnc ,gnrt))

(DEFMACRO BCND (gnrt)
   `(cons :bcnd ,gnrt))

(DEFMACRO WITH-BICN ((bcnx ibicn) bicn . body)
   `(let ((,bcnx (bcnx ,bicn))
          (,ibicn (ibicn ,bicn)))
       (declare
          (type (member :bcnb :bcnc :bcnd) ,bcnx)
          (type gnrt ,ibicn))
       ,@body))


;;;
;;;  TENSOR PRODUCTS
;;;

(DEFMACRO TNPR (degr1 gnrt1 degr2 gnrt2)
   `(cons :tnpr (cons (cons ,degr1 ,gnrt1) (cons ,degr2 ,gnrt2))))

(DEFMACRO DEGR1 (tnpr)
   `(caadr ,tnpr))

(DEFMACRO GNRT1 (tnpr)
   `(cdadr ,tnpr))

(DEFMACRO DEGR2 (tnpr)
   `(caddr ,tnpr))

(DEFMACRO GNRT2 (tnpr)
   `(cdddr ,tnpr))

(DEFMACRO WITH-TNPR ((degr1 gnrt1 degr2 gnrt2) tnpr . body)
   `(let (,@(if degr1 `((,degr1 (degr1 ,tnpr))) nil)
          ,@(if gnrt1 `((,gnrt1 (gnrt1 ,tnpr))) nil)
          ,@(if degr2 `((,degr2 (degr2 ,tnpr))) nil)
          ,@(if gnrt2 `((,gnrt2 (gnrt2 ,tnpr))) nil))
       (declare
          (fixnum ,@(if degr1 `(,degr1) nil) ,@(if degr2 `(,degr2) nil))
          (type gnrt ,@(if gnrt1 `(,gnrt1) nil) ,@(if gnrt2 `(,gnrt2) nil)))
       ,@body))

#|
  (macroexpand '(tnpr 1 a 2 b))
            1 (tnpr 2 'a 4 (tnpr 2 'b 2 'c))
  (macroexpand-1 '(with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                    (statement-1)
                    (statement-2)))
  (macroexpand-1 '(with-tnpr (nil gnrt1 degr2 gnrt2) tnpr
                    (statement-1)
                    (statement-2)))
|#

;;;
;;;  COALGEBRAS
;;;

(DEFMACRO CPRD (&rest rest)
   (case (length rest)
      (1 `(cprd1 ,@rest))
      ((2 3) `(? (cprd1 ,(first rest)) ,@(rest rest)))))

;;;
;;;  COBAR
;;;

(DEFMACRO CBGN (degr gnrt)
   `(cons ,degr ,gnrt))

(DEFMACRO CDEGR (cbgn)
   `(car ,cbgn))

(DEFMACRO CGNRT (cbgn)
   `(cdr ,cbgn))

(DEFMACRO -CDEGR (cbgn)
   `(caar ,cbgn))

(DEFMACRO -CGNRT (cbgn)
   `(cdar ,cbgn))

(DEFMACRO WITH-CBGN ((cdegr cgnrt) cbgn . body)
   `(let ((,cdegr (cdegr ,cbgn))
          (,cgnrt (cgnrt ,cbgn)))
       (declare
          (fixnum ,cdegr)
          (type gnrt ,cgnrt))
       ,@body))

(DEFMACRO WITH--CBGN ((cdegr cgnrt) cbgn . body)
   `(let ((,cdegr (-cdegr ,cbgn))
          (,cgnrt (-cgnrt ,cbgn)))
       (declare
          (fixnum ,cdegr)
          (type gnrt ,cgnrt))
       ,@body))

(DEFMACRO MAKE-ALLP (&key list)
   `(cons :allp ,list))

(DEFMACRO ALLP-LIST (allp)
   `(cdr ,allp))

(DEFMACRO WITH-ALLP ((list) allp . body)
   `(let ((,list (allp-list ,allp)))
       (declare (list ,list))
       ,@body))

(DEFMACRO GNRT-ALLP-TNPR (degr gnrt allp)
   `(make-allp :list (cons (cbgn (1- ,degr) ,gnrt) ,allp)))


;;;
;;;  ALGEBRAS
;;;

(DEFMACRO APRD (&rest rest)
   (case (length rest)
      (1 `(aprd1 ,@rest))
      ((2 3) `(? (aprd1 ,(first rest)) ,@(rest rest)))))

;;;
;;;  BAR
;;;

(DEFMACRO BRGN (degr gnrt)
   `(cons ,degr ,gnrt))

(DEFMACRO BDEGR (brgn)
   `(car ,brgn))

(DEFMACRO BGNRT (brgn)
   `(cdr ,brgn))

(DEFMACRO -BDEGR (brgn)
   `(caar ,brgn))

(DEFMACRO -BGNRT (brgn)
   `(cdar ,brgn))

(DEFMACRO WITH-BRGN ((bdegr bgnrt) brgn . body)
   `(let ((,bdegr (bdegr ,brgn))
          (,bgnrt (bgnrt ,brgn)))
       (declare
          (fixnum ,bdegr)
          (type gnrt ,bgnrt))
       ,@body))

(DEFMACRO WITH--BRGN ((bdegr bgnrt) brgn . body)
   `(let ((,bdegr (-bdegr ,brgn))
          (,bgnrt (-bgnrt ,brgn)))
       (declare
          (fixnum ,bdegr)
          (type gnrt ,bgnrt))
       ,@body))

(DEFMACRO MAKE-ABAR (&key list)
   `(cons :abar ,list))

(DEFMACRO ABAR-LIST (abar)
   `(cdr ,abar))

(DEFMACRO WITH-ABAR ((list) abar . body)
   `(let ((,list (abar-list ,abar)))
       (declare (list ,list))
       ,@body))

(DEFMACRO MAKE-GBAR (&key dmns list)
   `(cons :gbar (cons ,dmns ,list)))

(DEFMACRO GBAR-DMNS (gbar)
   `(cadr ,gbar))

(DEFMACRO GBAR-LIST (gbar)
   `(cddr ,gbar))

(DEFMACRO WITH-GBAR ((dmns list) gbar . body)
   `(let ((,dmns (gbar-dmns ,gbar))
	  (,list (gbar-list ,gbar)))
       (declare
	 (fixnum ,dmns)
	 (list ,list))
       ,@body))

(DEFMACRO GNRT-GBAR-TNPR (degr gnrt gbar)
   `(make-gbar :list (cons (brgn (1- ,degr) ,gnrt) ,gbar)))

;;;
;;;  SIMPLICIAL SETS
;;;

(DEFMACRO ABSM (dgop gmsm)
   `(cons :absm (cons ,dgop ,gmsm)))

(DEFMACRO DGOP (absm)
   `(cadr ,absm))

(DEFMACRO GMSM (absm)
   `(cddr ,absm))

(DEFMACRO WITH-ABSM ((dgop-var gmsm-var) absm . body)
   `(let ((,dgop-var (dgop ,absm))
          (,gmsm-var (gmsm ,absm)))
       (declare
          (fixnum ,dgop-var)
          (type gmsm ,gmsm-var))
       ,@body))

(DEFMACRO DEGENERATE-P (absm)
   `(plusp (dgop ,absm)))

(DEFMACRO NON-DEGENERATE-P (absm)
   `(zerop (dgop ,absm)))

(DEFMACRO FACE (&rest rest)
   (ecase (length rest)
      (1 `(face1 ,@rest))
      (4 `(face4 ,@rest))))

(DEFMACRO A-FACE4 (face indx dmns absm)
   `(ia-face4 ,face ,indx ,dmns (cdr ,absm)))

;;;
;;;  DELTA
;;;

(DEFMACRO D (igmsm)
   `(cons :delt ,igmsm))

;;;
;;;  SPECIAL-SMSTS
;;;

;;; GMSM-FACES-INFO = (gmsm (simple-vector absm) . dffr)
;;;                         faces
       
(DEFMACRO MAKE-GMSM-FACES-INFO (&key gmsm faces dffr)
   `(cons ,gmsm (cons ,faces ,dffr)))

(DEFMACRO INFO-GMSM (gmsm-faces-info)
   `(car ,gmsm-faces-info))

(DEFMACRO INFO-FACES (gmsm-faces-info)
   `(cadr ,gmsm-faces-info))

(DEFMACRO INFO-FACE-I (gmsm-faces-info i)
   `(svref (cadr ,gmsm-faces-info) ,i))

(DEFMACRO INFO-DFFR (gmsm-faces-info)
   `(cddr ,gmsm-faces-info))

;;;
;;;  CARTESIAN PRODUCTS
;;;

(DEFMACRO CRPR (&rest rest)
   (ecase (length rest)
      (2 `(cons :crpr (cons (cdr ,(first rest)) (cdr ,(second rest)))))
      (4 `(cons :crpr (cons
                         (cons ,(first rest) ,(second rest))
                         (cons ,(third rest) ,(fourth rest)))))))

#|
  (macroexpand '(crpr absm1 absm2))
  (macroexpand '(crpr dgop1 gmsm1 dgop2 gmsm2))
|#

(DEFMACRO DGOP1 (crpr)
   `(caadr ,crpr))

(DEFMACRO GMSM1 (crpr)
   `(cdadr ,crpr))

(DEFMACRO DGOP2 (crpr)
   `(caddr ,crpr))

(DEFMACRO GMSM2 (crpr)
   `(cdddr ,crpr))

(DEFMACRO ABSM1 (crpr)
   `(cons :absm (cadr ,crpr)))

(DEFMACRO ABSM2 (crpr)
   `(cons :absm (cddr ,crpr)))

#|
  (setf c (crpr 1 'a 2 'b))
  (dgop1 c)
  (gmsm1 c)
  (dgop2 c)
  (gmsm2 c)
  (absm1 c)
  (absm2 c)
|#

(DEFMACRO WITH-CRPR (&rest rest)
   (ecase (length (first rest))
      (2 `(with-crpr-2 ,@rest))
      (4 `(with-crpr-4 ,@rest))))

(DEFMACRO WITH-CRPR-2 ((absm1 absm2) crpr . body)
   `(let ((,absm1 (cons :absm (cadr ,crpr)))
          (,absm2 (cons :absm (cddr ,crpr))))
       (declare (type absm ,absm1 ,absm2))
       ,@body))

(DEFMACRO WITH-CRPR-4 ((dgop1 gmsm1 dgop2 gmsm2) crpr . body)
   `(let ((,dgop1 (dgop1 ,crpr))
          (,gmsm1 (gmsm1 ,crpr))
          (,dgop2 (dgop2 ,crpr))
          (,gmsm2 (gmsm2 ,crpr)))
       (declare
          (fixnum ,dgop1 ,dgop2)
          (type gmsm ,gmsm1 ,gmsm2))
       ,@body))

#|
  (macroexpand-1
  (macroexpand-1 '(with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                     (statement-1)
                     (statement-2))))
  (macroexpand-1
  (macroexpand-1 '(with-crpr (absm1 absm2) crpr
                     (statement-1)
                     (statement-2))))
|#

;;;
;;;  EILENBERG-ZILBER
;;;

(DEFMACRO EILENBERG-ZILBER (&rest rest)
   `(ez ,@rest))

;;;
;;;  LOOP SPACES
;;;

(DEFMACRO POWR (gmsm expn)
   `(cons ,gmsm ,expn))

(DEFMACRO PGMSM (powr)
   `(car ,powr))

(DEFMACRO EXPN (powr)
   `(cdr ,powr))

(DEFMACRO WITH-POWR ((gmsm expn) powr . body)
   `(let ((,gmsm (car ,powr))
          (,expn (cdr ,powr)))
       (declare
          (type gmsm ,gmsm)
          (fixnum expn))
       ,@body))

(DEFMACRO WITH-APOWR ((dgop gmsm expn) apowr . body)
   `(let ((,dgop (apdgop ,apowr))
          (,gmsm (cadr ,apowr))
          (,expn (cddr ,apowr)))
       (declare
          (fixnum ,dgop ,expn)
          (type gmsm ,gmsm))
       ,@body))

(DEFMACRO APOWR (dgop gmsm expn)
   `(cons ,dgop (cons ,gmsm ,expn)))
   
(DEFMACRO APDGOP (apowr)
   `(car ,apowr))

(DEFMACRO APGMSM (apowr)
   `(cadr ,apowr))

(DEFMACRO APEXPN (apowr)
   `(cddr ,apowr))

(DEFMACRO MAKE-LOOP (&key list)
   `(cons :loop ,list))

(DEFMACRO LOOP-LIST (loop)
   `(cdr ,loop))

;; CONSTRUCTOR

(DEFMACRO WHEN-SLOT (slot-name . body)
  `(when (or (eq slot-names t)
             (member ',slot-name slot-names))
     ,@body))

#|
(pprint
 (macroexpand-1
  '(when-slot dfnt (statement-1) (statement-2))))
|#

(DEFMACRO ALREADY (object dfnt)
  (let ((sfound (gensym)))
  `(let ((,sfound (find ,dfnt *k-list* :key #'dfnt :test #'equal)))
     (declare (type (or kenzo-object null) ,sfound))
     (when ,sfound
       (return-from ,object ,sfound)))))

#|
(pprint
 (macroexpand-1
  '(already chain-complex '(z-chcm))))
|#

