;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES

(IN-PACKAGE "CAT")

(PROVIDE "classes")

;;;
;;; VARIOUS
;;;

(DEFTYPE ANY () t)

;;; 
;;;  COMBINATIONS
;;;

;; CFFC = CoeFFiCient
(DEFUN CFFC-P (object)
   (declare (type any object))
   (the boolean
      (and (typep object 'fixnum)
           (/= 0 object))))

(DEFTYPE CFFC () '(satisfies cffc-p))

;; GNRT = GeNeRaTor
(DEFTYPE GNRT () 'any)

;; CMPR = CoMPaRison
(DEFTYPE CMPR () '(member :less :equal :greater))

;; CMPRF = CoMPaRison Function
(DEFTYPE CMPRF () 'function)
               ;; (function (gnrt gnrt) cmpr)

(DEFUN TERM-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) 'cffc)
           (typep (cdr object) 'gnrt))))

(DEFTYPE TERM () '(satisfies term-p))

;; ICMBN = Internal-CoMBiNation
(DEFUN ICMBN-P (object)
   ;; ICMBN = Internal CoMBiNation,
   ;;   without the keyword and the degree
   (declare (type any object))
   (the boolean
      (and (listp object)
           (every #'term-p object))))

(DEFTYPE ICMBN () '(satisfies icmbn-p))

;; CMBN = CoMBiNation
(DEFUN CMBN-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eql (car object) :cmbn)
           (consp (cdr object))
           (typep (second object) 'fixnum)
           (typep (cddr object) 'icmbn))))

(DEFTYPE CMBN () '(satisfies cmbn-p))

(DEFTYPE BASIS () '(or function (eql :locally-effective)))
                   ;; (function (degr) (list gnrt))

;; INTR = INTeRNal-morphism
(DEFTYPE INTR () 'function)
               ;; (or (function (degr gnrt) cmbn)  ;; if :gnrt strategy
               ;;     (function (cmbn) (cmbn))     ;; if :cmbn strategy

(DEFVAR REDEFINITION-WARNINGS-SAVED)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (WHEN
  (AND (FIND-PACKAGE "EXCL")
       (FIND-SYMBOL "*REDEFINITION-WARNINGS*" "EXCL"))
  (setf redefinition-warnings-saved
	excl:*redefinition-warnings*
	excl:*redefinition-warnings* nil)))

(DEFCLASS CHAIN-COMPLEX () ())  ;; will be redefined later.

(DEFCLASS MORPHISM () ())  ;; will be redefined later.

(DEFCLASS EQUIVALENCE () ())  ;;  will be redefined later

;;  IDNM = IDentification NuMber
(DEFVAR *IDNM-COUNTER*)
(SETF *IDNM-COUNTER* 0)

(DEFCLASS KENZO-OBJECT ()
  ;; IDentification NuMber
  ((idnm :type fixnum :reader idnm)
   ;; ORiGiN
   (dfnt :type list :reader dfnt)
   (prpr :type list :reader prpr)
   (cmmn :type list :reader cmmn)))

(DEFCLASS CHAIN-COMPLEX (kenzo-object)
  ((cmpr :type cmprf :reader cmpr1)
   (basis :type basis :reader basis1)
   ;; BaSe GeNerator
   (bsgn :type gnrt :reader bsgn)
   ;; DiFFeRential
   (dffr :type morphism :reader dffr1)
   ;; GRound MoDule
   (grmd :type chain-complex :reader grmd)
   ;; EFfective HoMology
   (efhm :type equivalence :accessor efhm)))

;; STRT = STRaTegy
(DEFTYPE STRT () '(member :gnrt :cmbn))

(DEFCLASS MORPHISM (kenzo-object)
     ;; SOuRCe
  ((sorc :type chain-complex :reader sorc)
   ;; TaRGeT
   (trgt :type chain-complex :reader trgt)
   ;; DEGRee
   (degr :type fixnum :reader degr)
   ;; INTeRnal
   (intr :type intr :reader intr)
   ;; STRaTegy
   (strt :type strt :reader strt)
;;     ;; WoRk-TiMe
;;     (wrtm :type integer :initform 0 :accessor wrtm)
   ;; CaLl NuMber
   (???-clnm :type fixnum :accessor ???-clnm)
   (?-clnm :type fixnum :accessor ?-clnm)
   ;; ReSuLTS
   (rslts :type simple-vector :reader rslts)))
            ;; (vector (vector result))

;;;
;;;  EFFECTIVE HOMOLOGY
;;;

(DEFCLASS REDUCTION (kenzo-object)
  ;; Top Chain Complex
  ((tcc :type chain-complex :reader tcc1)
   ;; Bottom Chain Complex
   (bcc :type chain-complex :reader bcc1)
   (f :type morphism :reader f1)
   (g :type morphism :reader g1)
   (h :type morphism :reader h1)))

(DEFCLASS EQUIVALENCE (kenzo-object)
     ;; Left Bottom Chain Complex
    ((lbcc :type chain-complex :reader lbcc1)
     ;; Top Chain Complex
     (tcc  :type chain-complex  :reader tcc1)
     ;; Bottom Right Chain Complex
     (rbcc :type chain-complex :reader rbcc1)
     ;; Left f
     (lf :type morphism        :reader lf1)
     ;; Left g
     (lg :type morphism        :reader lg1)
     ;; Left h
     (lh :type morphism        :reader lh1)
     ;; Right f
     (rf :type morphism        :reader rf1)
     ;; Right g
     (rg :type morphism        :reader rg1)
     ;; Right h
     (rh :type morphism        :reader rh1)
     ;; Left ReDuCTion
     (lrdct :type reduction    :reader lrdct)
     ;; Right ReDuCTion
     (rrdct :type reduction    :reader rrdct)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (WHEN
  (AND (FIND-PACKAGE "EXCL")
       (FIND-SYMBOL "*REDEFINITION-WARNINGS*" "EXCL"))
  (setf excl:*redefinition-warnings*
	redefinition-warnings-saved)))

;;;
;;;  CONES
;;;

;; BICN = bicone
(DEFUN BICN-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) '(member :bcnb :bcnc :bcnd))
           (typep (cdr object) 'gnrt))))

(DEFTYPE BICN () '(satisfies bicn-p))

;;;
;;;  TENSOR PRODUCTS
;;;

(DEFUN TNPR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eql :tnpr (car object))
           (consp (cdr object))
           (consp (cadr object))
           (consp (cddr object))
           (typep (caadr object) 'fixnum)
           (typep (caddr object) 'fixnum))))

(DEFTYPE TNPR () '(satisfies tnpr-p))

;;;
;;;  COALGEBRAS
;;;

(DEFCLASS COALGEBRA (chain-complex)
    ((cprd :type morphism :reader cprd1)))   ;; coproduct

;;;
;;;  COBAR
;;;

;; CBGN CoBar-GeNerator
(DEFUN CBGN-P (object)
   ;; CBGN = CoBar GeNerator
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) 'fixnum)      ;; degree *in* the cobar
           (typep (cdr object) 'gnrt))))

(DEFTYPE CBGN () '(satisfies cbgn-p))

;; IALLP = Internal-ALgebraic LooP
(DEFUN IALLP-P (object)
   (declare (type any object))
   (the boolean
      (and (listp object)
           (every #'cbgn-p object))))

(DEFTYPE IALLP () '(satisfies iallp-p))

;; ALLP = ALgebraic LooP
(DEFUN ALLP-P (object)
   ;; ALLP = ALgebraic LooP
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :allp (car object))
           (typep (cdr object) 'iallp))))

(DEFTYPE ALLP () '(satisfies allp-p))

;;;
;;;  ALGEBRAS
;;;

(DEFCLASS ALGEBRA (chain-complex)
    ((aprd :type morphism :reader aprd1)))   ;; product


;;;
;;;  HOPH-ALGEBRAS
;;;

(DEFCLASS HOPF-ALGEBRA (coalgebra algebra)
  ())

;;;
;;;  BAR
;;;

;; BRGN BaR-GeNerator
(DEFUN BRGN-P (object)
   ;; BRGN = BaR GeNerator
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) 'fixnum)      ;; degree *in* the cobar
           (typep (cdr object) 'gnrt))))

(DEFTYPE BRGN () '(satisfies brgn-p))

;; IABAR = Internal-ALgebraic LooP
(DEFUN IABAR-P (object)
   (declare (type any object))
   (the boolean
      (and (listp object)
           (every #'brgn-p object))))

(DEFTYPE IABAR () '(satisfies iabar-p))

;; ABAR = ALgebraic BAR
(DEFUN ABAR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :abar (car object))
           (typep (cdr object) 'iabar))))

(DEFTYPE ABAR () '(satisfies abar-p))

;;;
;;;  SIMPLICIAL SETS
;;;

;;  DGOP = DeGeneray OPerator
(DEFTYPE DGOP () `(integer 0 ,most-positive-fixnum))

;; GMSM = GeoMetric SiMplex
(DEFTYPE GMSM () 'gnrt)

;;  DLOP = DeL OPerator
(DEFTYPE DLOP () 'dgop) 

;; IABSM = Internal ABstract SiMplex
(DEFUN IABSM-P (object)
  (declare (type any object))
  (the boolean
     (and (consp object)
	  (typep (car object) 'dgop)
	  (typep (cdr object) 'gmsm))))	   

(DEFTYPE IABSM () '(satisfies iabsm-p))

;; ABSM = ABstract SiMplex
(DEFUN ABSM-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :absm (car object))
           (typep (cdr object) 'iabsm))))


(DEFTYPE ABSM () '(satisfies absm-p))

(DEFTYPE FACE () 'function)
            ;;   (function (indx dmns gmsm) absm)

(DEFTYPE FACE* () 'function)
            ;;    (function (indx dmns gmsm) (or gmsm (eql :degenerate)))

(DEFCLASS SIMPLICIAL-SET (coalgebra)
    ((face :type face :reader face1)))

;;;
;;;  SPECIAL SMSTS
;;;

;;  DLOP = DeL OPerator
(DEFUN SOFT-DLOP-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :delt (car object))
           (typep (cdr object) 'dgop))))

(DEFTYPE SOFT-DLOP () '(satisfies soft-dlop-p))

;;;
;;;  CARTESIAN PRODUCTS
;;;

;;  CRPR = CaRtesian PRoduct
(DEFUN CRPR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq (car object) :crpr)
           (consp (cdr object))
           (consp (cadr object))
           (consp (cddr object))
           (typep (caadr object) 'dgop)
           (typep (cdadr object) 'gmsm)
           (typep (caddr object) 'dgop)
           (typep (cdddr object) 'gmsm))))

(DEFTYPE CRPR () '(satisfies crpr-p))

;;;
;;;  LOOP SPACES
;;;

;;  EXPN = EXPoNent
(DEFUN EXPN-P (object)
   (declare (type any object))
   (the boolean
      (typep object '(and fixnum
                          (not (eql 0))))))

(DEFTYPE EXPN () '(satisfies expn-p))

;;  POWR = POWeR
(DEFUN POWR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) 'gmsm)
           (typep (cdr object) 'expn))))

(DEFTYPE POWR () '(satisfies powr-p))

;;  APOWR = Abstract POWeR
(DEFUN APOWR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (typep (car object) 'dgop)
           (typep (cdr object) 'powr))))

(DEFTYPE APOWR () '(satisfies apowr-p))

;;  ILOOP = Internal LOOP
(DEFUN ILOOP-P (object)
   (declare (type any object))
   (the boolean
      (and (listp object)
           (every #'apowr-p object))))

(DEFTYPE ILOOP () '(satisfies iloop-p))

(DEFUN LOOP-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :loop (car object))
           (iloop-p (cdr object)))))

;;;----------------------------
;;; Correction 2000 January 19
;;;------------------------------------
;;;#+(or (and :mswindows :allegro-v5.0)
;;;------------------------------------
#+(or (and :mswindows :allegro-v5.0)
      (and :mswindows :allegro-v5.0.1)
;;;-----------------------------------
      (and :unix :allegro-v4.3)
      (and :unix :allegro-v5.0))
  (eval-when (:compile-toplevel :load-toplevel :execute)
     (setf excl:*enable-package-locked-errors* nil))

(DEFTYPE LOOP () '(satisfies loop-p))

#+(or (and :mswindows :allegro-v5.0)
      (and :unix :allegro-v4.3)
      (and :unix :allegro-v5.0))
  (eval-when (:compile-toplevel :load-toplevel :execute)
     (setf excl:*enable-package-locked-errors* t))

;;;;
;;;;
;;;;

(DEFTYPE KFLL () 'function)
              ;; (function (indx dmns (list absm)) absm)

(DEFCLASS KAN (simplicial-set)
  ((kfll :type kfll :reader kfll1)))

(DEFTYPE SINTR () 'function)
               ;; (function (dmns gmsm) absm)

(DEFCLASS SIMPLICIAL-MRPH (morphism)
  ((sintr :type sintr :reader sintr)))

(DEFUN FIBRATION-P (object)
  (declare (type any object))
  (the boolean
     (and (typep object 'simplicial-mrph)
	  (= -1 (degr object)))))

(DEFTYPE FIBRATION ()
  '(satisfies fibration-p))

(DEFCLASS SIMPLICIAL-GROUP (kan hopf-algebra)
   ((grml :type simplicial-mrph :reader grml1)
    (grin :type simplicial-mrph :reader grin1)))

(DEFCLASS AB-SIMPLICIAL-GROUP (simplicial-group) ())

(DEFMACRO GRML (smgr &rest rest)
   (ecase (length rest)
      (0 `(grml1 ,smgr))
      ((1 2) `(? (grml1 ,smgr) ,@rest))))

(DEFMACRO GRIN (smgr &rest rest)
   (case (length rest)
      (0 `(grin1 ,smgr))
      (otherwise `(? (grin1 ,smgr) ,@rest))))

;;;
;;;  GBAR
;;;

;;  IGBAR Internal Geometrical BAR
;;  (dmns absm_(m-1) ... absm_1 absm_0)
(DEFUN IGBAR-P (object)
   (declare (type any object))
   (the boolean
      (and (listp object)
	   (typep (first object) '(integer 0 *))
	   (= (length object) (1+ (first object)))
	   (every #'absm-p (rest object)))))

(DEFTYPE IGBAR () '(satisfies igbar-p))

;;  GBAR Geometrical BAR
;;  (:gbar dmns absm_(m-1) ... absm_1)
(DEFUN GBAR-P (object)
   (declare (type any object))
   (the boolean
      (and (consp object)
           (eq :gbar (car object))
           (igbar-p (cdr object)))))

(DEFTYPE GBAR () '(satisfies gbar-p))


