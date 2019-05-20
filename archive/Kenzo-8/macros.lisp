;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS
;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS
;;;  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS  MACROS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "macros")

;;; Necessary for SBCL.

(DEFMACRO DEFINE-CONSTANT (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

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
()
(-1-expt-n 5)
(-1-expt-n 6)
(-1-expt-n+1 5)
(-1-expt-n+1 6)
(-1-expt-n-1 5)
(-1-expt-n-1 6)
|#

(DEFMACRO 2-EXP (n)
   `(aref +2-exp+ ,n))

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
()
(let ((*print-level* nil))
  (declare (special
            #+aclpc allegro:*top-print-level*
            #-aclpc *print-level*))
  (pprint (macroexpand
           '(lexico
             comparison1
             comparison2
             comparison3
             comparison4))))
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
  `(make-cmbn :degr ,degr
              :list (list (term ,cffc ,gnrt))))


(DEFMACRO CMBN-NON-ZERO-P (cmbn)
   `(cmbn-list ,cmbn)) 

(DEFMACRO CMBN-ZERO-P (cmbn)
  `(null (cmbn-list ,cmbn)))

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
;;;  CHAIN-COMPLEXES
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

(DEFMACRO ? (&rest rest)
   (ecase (length rest)
      (2 `(?2 ,@rest))
      (3 `(?3 ,@rest))))

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
;;;  HOMOLOGY-GROUPS
;;;

(DEFMACRO BASELIG (mat n)
  `(aref (leftcol ,mat) ,n))

(DEFMACRO BASECOL (mat n)
  `(aref (uplig ,mat) ,n))

;;;
;;;  CONES
;;;

(DEFMACRO CON0 (gnrt)
  `(make-cone :conx 0 :icon ,gnrt))

(DEFMACRO CON1 (gnrt)
  `(make-cone :conx 1 :icon ,gnrt))

(DEFMACRO WITH-CONE ((conx icon) cone . body)
  (let ((scone (gensym)))
    (declare (symbol scone))
    `(let ((,scone ,cone))
       (declare (type cone ,scone))
       (let ((,conx (conx ,scone))
             (,icon (icon ,scone)))
         (declare (type (member 0 1) ,conx) (type gnrt ,icon))
         ,@body))))

(DEFMACRO BCNB (gnrt)
  `(make-bicn :bcnx :bcnb :ibicn ,gnrt))

(DEFMACRO BCNC (gnrt)
  `(make-bicn :bcnx :bcnc :ibicn ,gnrt))

(DEFMACRO BCND (gnrt)
  `(make-bicn :bcnx :bcnd :ibicn ,gnrt))

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

#|
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
|#
(DEFMACRO TNPR (degr1 gnrt1 degr2 gnrt2)
  `(make-tnpr :degr1 ,degr1 :gnrt1 ,gnrt1
              :degr2 ,degr2 :gnrt2 ,gnrt2))


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


#|
(DEFMACRO MAKE-ABAR (&key list)
   `(cons :abar ,list))

(DEFMACRO ABAR-LIST (abar)
   `(cdr ,abar))
|#

(DEFMACRO WITH-ABAR ((list) abar . body)
   `(let ((,list (abar-list ,abar)))
       (declare (list ,list))
       ,@body))


#|
(DEFMACRO MAKE-GBAR (&key dmns list)
   `(cons :gbar (cons ,dmns ,list)))

(DEFMACRO GBAR-DMNS (gbar)
   `(cadr ,gbar))

(DEFMACRO GBAR-LIST (gbar)
   `(cddr ,gbar))
|#


(DEFMACRO WITH-GBAR ((dmns list) gbar . body)
  `(let ((,dmns (gbar-dmns ,gbar))
         (,list (gbar-list ,gbar)))
     (declare
      (fixnum ,dmns)
      (list ,list))
     ,@body))

#|
(DEFMACRO GNRT-GBAR-TNPR (degr gnrt gbar)
   `(make-gbar :list (cons (brgn (1- ,degr) ,gnrt) ,gbar)))
|#


;;;
;;;  SIMPLICIAL SETS
;;;


#|
(DEFMACRO ABSM (dgop gmsm)
   `(cons :absm (cons ,dgop ,gmsm)))

(DEFMACRO DGOP (absm)
   `(cadr ,absm))

(DEFMACRO GMSM (absm)
  `(cddr ,absm))
|#

(DEFMACRO ABSM (dgop gmsm)
  `(make-absm :dgop ,dgop :gmsm ,gmsm))


(DEFMACRO WITH-ABSM ((dgop-var gmsm-var) absm . body)
  (let ((sabsm (gensym)))
    `(let ((,sabsm ,absm))
       (declare (type absm ,sabsm))
       (let ((,dgop-var (dgop ,sabsm))
             (,gmsm-var (gmsm ,sabsm)))
         (declare
          (fixnum ,dgop-var)
          (type gmsm ,gmsm-var))
       ,@body))))

(DEFMACRO DEGENERATE-P (absm)
   `(plusp (dgop ,absm)))

(DEFMACRO NON-DEGENERATE-P (absm)
   `(zerop (dgop ,absm)))

(DEFMACRO BSPN (smst)
   `(bsgn ,smst))

(DEFMACRO BNDR (&rest rest)
   `(dffr ,@rest))

(DEFMACRO DGNL (&rest rest)
   `(cprd ,@rest))

(DEFMACRO FACE (&rest rest)
   (ecase (length rest)
      (1 `(face1 ,@rest))
      (4 `(face4 ,@rest))))

;;;
;;;  DELTA
;;;

(DEFMACRO D (&rest list)
  `(make-delta :cdr (dlop-ext-int (list ,@list))))

#|
(d 4 5 6)
|#

;;;
;;;  SPECIAL-SMSTS
;;;

;;; GMSM-FACES-INFO = (gmsm (simple-vector absm) . bndr)
;;;                         faces
       
(DEFMACRO MAKE-GMSM-FACES-INFO (&key gmsm faces bndr)
   `(cons ,gmsm (cons ,faces ,bndr)))

(DEFMACRO INFO-GMSM (gmsm-faces-info)
   `(car ,gmsm-faces-info))

(DEFMACRO INFO-FACES (gmsm-faces-info)
   `(cadr ,gmsm-faces-info))

(DEFMACRO INFO-FACE-I (gmsm-faces-info i)
   `(svref (cadr ,gmsm-faces-info) ,i))

(DEFMACRO INFO-BNDR (gmsm-faces-info)
   `(cddr ,gmsm-faces-info))

;;;
;;;  CARTESIAN PRODUCTS
;;;

#|
(DEFMACRO CRPR (&rest rest)
   (ecase (length rest)
      (2 `(cons :crpr (cons (cdr ,(first rest)) (cdr ,(second rest)))))
      (4 `(cons :crpr (cons
                         (cons ,(first rest) ,(second rest))
                       (cons ,(third rest) ,(fourth rest)))))))
|#
(DEFMACRO CRPR (&rest rest)
  (ecase (length rest)
    (2 `(crpr2 ,@rest))
    (4 `(crpr4 ,@rest))))

(DEFMACRO CRPR2 (absm1 absm2)
  `(crpr4 (dgop ,absm1) (gmsm ,absm1) (dgop ,absm2) (gmsm ,absm2)))

(DEFMACRO CRPR4 (dgop1 gmsm1 dgop2 gmsm2)
  `(make-crpr :dgop1 ,dgop1 :gmsm1 ,gmsm1
              :dgop2 ,dgop2 :gmsm2 ,gmsm2))

#|
()
(crpr (absm 3 'a) (absm 4 'b))
(crpr 3 'a 4 'b)
|#

#|
()
(macroexpand '(crpr absm1 absm2))
(macroexpand '(crpr dgop1 gmsm1 dgop2 gmsm2))
|#

#|
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
|#
(DEFMACRO ABSM1 (crpr)
  `(absm (dgop1 ,crpr) (gmsm1 ,crpr)))

(DEFMACRO ABSM2 (crpr)
  `(absm (dgop2 ,crpr) (gmsm2 ,crpr)))



#|
()
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


#|
(DEFMACRO WITH-CRPR-2 ((absm1 absm2) crpr . body)
   `(let ((,absm1 (cons :absm (cadr ,crpr)))
          (,absm2 (cons :absm (cddr ,crpr))))
       (declare (type absm ,absm1 ,absm2))
       ,@body))
|#

(DEFMACRO WITH-CRPR-2 ((absm1 absm2) crpr . body)
  `(let ((,absm1 (absm (dgop1 ,crpr) (gmsm1 ,crpr)))
         (,absm2 (absm (dgop2 ,crpr) (gmsm2 ,crpr))))
     (declare (type absm ,absm1 ,absm2))
     ,@body)) 
  
#|
()
(with-crpr (absm1 absm2) (crpr 3 'a 4 'b) (list absm1 absm2))
|#

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
()
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
;;;  KAN
;;;

(DEFMACRO KFLL (&rest rest)
  (ecase (length rest)
     (1 `(kfll1 ,@rest))
     (4 `(kfll4 ,@rest))))

(DEFMACRO KFLL4 (kan indx dmns hat)
  `(funcall (kfll1 ,kan) ,indx ,dmns ,hat))


;;;
;;;  SIMPLICIAL-GROUP
;;;

(DEFMACRO GRML (smgr &rest rest)
   (ecase (length rest)
      (0 `(grml1 ,smgr))
      ((1 2) `(? (grml1 ,smgr) ,@rest))))

(DEFMACRO GRIN (smgr &rest rest)
   (case (length rest)
      (0 `(grin1 ,smgr))
     (otherwise `(? (grin1 ,smgr) ,@rest))))

(DEFMACRO BUILD-AB-SMGR (&rest rest)
  `(change-class (build-smgr ,@rest) 'ab-simplicial-group))

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

;;;
;;;  SMITH
;;;

(DEFMACRO LINE-NUMBER (mtrx)
  `(first (array-dimensions ,mtrx)))

(DEFMACRO COLUMN-NUMBER (mtrx)
  `(second (array-dimensions ,mtrx)))

(DEFMACRO LINE-OP-5 (mtrx-list begin lambda line1 line2)
  (let ((slambda (gensym)))
    `(let ((,slambda ,lambda))
       (column-op (first ,mtrx-list) 0 (- ,slambda) ,line2 ,line1)
       (line-op (second ,mtrx-list) 0 ,slambda ,line1 ,line2)
       (line-op (third ,mtrx-list) ,begin ,slambda ,line1 ,line2))))

(DEFMACRO COLUMN-OP-5 (mtrx-list begin lambda column1 column2)
  (let ((slambda (gensym)))
    `(let ((,slambda ,lambda))
       (column-op (third ,mtrx-list) ,begin ,slambda ,column1 ,column2)
       (column-op (fourth ,mtrx-list) 0 ,slambda ,column1 ,column2)
       (line-op (fifth ,mtrx-list) 0 (- ,slambda) ,column2 ,column1))))

(DEFMACRO LINE-SWAP-5 (mtrx-list begin line1 line2)
  `(progn
     (column-swap (first ,mtrx-list) 0 ,line1 ,line2)
     (line-swap (second ,mtrx-list) 0 ,line1 ,line2)
     (line-swap (third ,mtrx-list) ,begin ,line1 ,line2)))

(DEFMACRO COLUMN-SWAP-5 (mtrx-list begin column1 column2)
  `(progn
     (column-swap (third ,mtrx-list) ,begin ,column1 ,column2)
     (column-swap (fourth ,mtrx-list) 0 ,column1 ,column2)
     (line-swap (fifth ,mtrx-list) 0 ,column1 ,column2)))

(DEFMACRO LINE-MINUS-5 (mtrx-list begin line)
  `(progn
     (column-minus (first ,mtrx-list) 0 ,line)
     (line-minus (second ,mtrx-list) 0 ,line)
     (line-minus (third ,mtrx-list) ,begin ,line)))

(DEFMACRO COLUMN-MINUS-5 (mtrx-list begin column)
  `(progn
     (column-minus (third ,mtrx-list) ,begin ,column)
     (column-minus (fourth ,mtrx-list) 0 ,column)
     (line-minus (fifth ,mtrx-list) 0 ,column)))

(DEFMACRO GNRT-NAME (i)
  `(intern (format nil "GN-~D" ,i) +gnrts-pckg+))
