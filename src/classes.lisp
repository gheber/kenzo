;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES

(IN-PACKAGE #:cat)

(PROVIDE "classes")

;;;
;;; VARIOUS
;;;

(DEFTYPE ANY ()
  "------------------------------------------------------------------[type-doc]
ANY
A derived type that is equivalent to Lisp's system class T, which includes all
Lisp objects.
------------------------------------------------------------------------------"
  t)

;;;
;;;  COMBINATIONS
;;;

(DEFSTRUCT (CMBN (:print-function cmbn-print))
  "------------------------------------------------------------------[type-doc]
CMBN
Slots: (degr list)
A structure with two slots, DEGR and LIST, for the degree and the list of
terms of a combination, of type FIXNUM and LIST, respectively. This is the
internal representation of combinations.
------------------------------------------------------------------------------"
  (degr -1 :type fixnum)
  (list () :type list))


#+ccl
(DEFMETHOD make-load-form ((c cmbn) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots c))


;; CFFC = CoeFFiCient
(DEFUN CFFC-P (object)
  (declare (type any object))
  (the boolean
       (and (typep object 'fixnum)
	    (/= 0 object))))

(DEFTYPE CFFC ()
  "------------------------------------------------------------------[type-doc]
CFFC
A derived type to represent the coefficients of terms. In Kenzo, the set of
coefficients it the set of objects which satisfy the CFFC-P predicate:
non-zero FIXNUMs.
------------------------------------------------------------------------------"
  '(satisfies cffc-p))


;; GNRT = GeNeRaTor
(DEFTYPE GNRT ()
  "------------------------------------------------------------------[type-doc]
GNRT
A type to represent the generators of terms. In Kenzo, the set of
coefficients it the set of all Lisp objects (T).
------------------------------------------------------------------------------"
  'any)


;; CMPR = CoMPaRison
(DEFTYPE CMPR ()
  "------------------------------------------------------------------[type-doc]
CMPR
An enumerated type to represent the results of comparisons.
------------------------------------------------------------------------------"
  '(member :less :equal :greater))


;; CMPRF = CoMPaRison Function
(DEFTYPE CMPRF ()
  "------------------------------------------------------------------[type-doc]
CMPRF
A derived type to represent comparison functions.
------------------------------------------------------------------------------"
  'function)
;; (function (gnrt gnrt) cmpr)


(DEFUN TERM-P (object)
  (declare (type any object))
  (the boolean
       (and (consp object)
	    (typep (car object) 'cffc)
	    (typep (cdr object) 'gnrt))))

(DEFTYPE TERM ()
  "------------------------------------------------------------------[type-doc]
TERM
A derived type to represent terms. In Kenzo, the set of terms it the set of
objects which satisfy the TERM-P predicate: CONSes whose CAR is of type CFFC
and whose CDR is of type GNRT.
------------------------------------------------------------------------------"
  '(satisfies term-p))


;; ICMBN = Internal-CoMBiNation
(DEFUN ICMBN-P (object)
  ;; ICMBN = Internal CoMBiNation,
  ;;   without the keyword and the degree
  (declare (type any object))
  (the boolean
       (and (listp object)
	    (every #'term-p object))))

(DEFTYPE ICMBN () '(satisfies icmbn-p))


(DEFTYPE BASIS ()
  "------------------------------------------------------------------[type-doc]
BASIS
A derived type to represent the basis of a chain complex. In Kenzo, a basis is
either a (basis-)generating function or, for not finitely-generated modules,
the symbol :LOCALLY-EFFECTIVE.
------------------------------------------------------------------------------"
  '(or function (eql :locally-effective) #+ecl null))
;; (function (degr) (list gnrt))


;; INTR-MRPH = INTeRnal-MoRPHism
(DEFTYPE INTR-MRPH () '(or function morphism #+(or ccl ecl) null))
;; (or (function (degr gnrt) cmbn)  ;; if :gnrt strategy
;;     (function (cmbn) (cmbn))     ;; if :cmbn strategy

;;;
;;;  CHAIN-COMPLEXES
;;;

#+allegro
(DEFVAR REDEFINITION-WARNINGS-SAVED)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf redefinition-warnings-saved
	excl::*redefinition-warnings*
	*redefinition-warnings* nil))

(DEFCLASS CHAIN-COMPLEX ()
  ((cmpr :type cmprf :initarg :cmpr :reader cmpr1)
   (basis :type basis :initarg :basis :reader basis1)
   ;; BaSe GeNerator
   (bsgn :type gnrt :initarg :bsgn :reader bsgn)
   ;; DiFFeRential
   (dffr :type morphism :initarg :dffr :reader dffr1)
   ;; GRound MoDule
   (grmd :type chain-complex :initarg :grmd :reader grmd)
   ;; EFfective HoMology
   (efhm :type homotopy-equivalence :initarg :efhm :accessor efhm)
   ;; IDentification NuMber
   (idnm :type fixnum :initform (incf *idnm-counter*) :reader idnm)
   ;; ORiGiN
   (orgn :type list :initarg :orgn :reader orgn))
  (:documentation
   "-----------------------------------------------------------------[class-doc]
CHAIN-COMPLEX
Slots: (cmps basis bsgn dffr grmd efhm idnm orgn)
Intances of this class represent chain complexes. The class has 8 slots:

1. CMPR, a comparison function or method for generators, which returns :LESS,
   :EQUAL, or :GREATER.

2. BASIS, a Lisp function, which returns the ordered basis of the free
   Z-modules (C_p), or the keyword :LOCALLY-EFFECTIVE.

3. BSGN, a Lisp object representing the base generator in dimension 0.

4. DFFR, the differential morphism, an instance of the class MORPHISM.

5. GRMD, ???

6. EFHM, ???

7. IDNM, an integer, a system generated identifier for this object.

8. ORGN, a list containg a comment indicating the origin of the object. The
   should be unique (per session), since it is used in the implementation for
   caching purposes.
------------------------------------------------------------------------------"))

;; STRT = STRaTegy
(DEFTYPE STRT () '(member :gnrt :cmbn))


(DEFCLASS MORPHISM ()
  ;; SOuRCe
  ((sorc :type chain-complex :initarg :sorc :reader sorc)
   ;; TaRGeT
   (trgt :type chain-complex :initarg :trgt :reader trgt)
   ;; DEGRee
   (degr :type fixnum :initarg :degr :reader degr)
   ;; INTeRnal
   (intr :type intr-mrph :initarg :intr :reader intr)
   ;; STRaTegy
   (strt :type strt :initarg :strt :reader strt)
   ;;     ;; WoRk-TiMe
   ;;     (wrtm :type integer :initform 0 :accessor wrtm)
   ;; CaLl NuMber
   (???-clnm :type fixnum :initform 0 :accessor ???-clnm)
   (?-clnm :type fixnum :initform 0 :accessor ?-clnm)
   ;; ReSuLTS
   (rslts :type simple-vector
	  ;; (vector (vector result))
	  :reader rslts)
   ;; IDentification NuMber
   (idnm :type fixnum :initform (incf *idnm-counter*) :reader idnm)
   ;; ORiGiN
   (orgn :type list :initarg :orgn :reader orgn)))

(DEFSTRUCT (RESULT (:print-function result-print))
  (gnrt nil :type gnrt)
  (value nil :type cmbn)
  (clnm 0 :type fixnum)
  (rntm 0.0 :type single-float))

;;;
;;;  EFFECTIVE HOMOLOGY
;;;

(DEFCLASS REDUCTION ()
  ;; Top Chain Complex
  ((tcc :type chain-complex :initarg :tcc :reader tcc1)
   ;; Bottom Chain Complex
   (bcc :type chain-complex :initarg :bcc :reader bcc1)
   (f :type morphism :initarg :f :reader f1)
   (g :type morphism :initarg :g :reader g1)
   (h :type morphism :initarg :h :reader h1)
   ;; IDentification NuMber
   (idnm :type fixnum :initform (incf *idnm-counter*) :reader idnm)
   ;; ORiGiN
   (orgn :type list :initarg :orgn :reader orgn)))

(DEFCLASS HOMOTOPY-EQUIVALENCE ()
  ;; Left Bottom Chain Complex
  ((lbcc :type chain-complex :initarg :lbcc :reader lbcc1)
   ;; Top Chain Complex
   (tcc  :type chain-complex :initarg :tcc  :reader tcc1)
   ;; Bottom Right Chain Complex
   (rbcc :type chain-complex :initarg :rbcc :reader rbcc1)
   ;; Left f
   (lf   :type morphism      :initarg :lf   :reader lf1)
   ;; Left g
   (lg   :type morphism      :initarg :lg   :reader lg1)
   ;; Left h
   (lh   :type morphism      :initarg :lh   :reader lh1)
   ;; Right f
   (rf   :type morphism      :initarg :rf   :reader rf1)
   ;; Right g
   (rg   :type morphism      :initarg :rg   :reader rg1)
   ;; Right h
   (rh   :type morphism      :initarg :rh   :reader rh1)
   ;; Left ReDuCTion
   (lrdct :type reduction    :initarg :lrdct :reader lrdct)
   ;; Right ReDuCTion
   (rrdct :type reduction    :initarg :rrdct :reader rrdct)
   ;; IDentification NuMber
   (idnm :type fixnum :initform (incf *idnm-counter*) :reader idnm)
   ;; ORiGiN
   (orgn :type list          :initarg :orgn :reader orgn)))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf excl::*redefinition-warnings*
	redefinition-warnings-saved))

;;;
;;;  HOMOLOGY-GROUPS
;;;


(DEFSTRUCT (matrice (:conc-name nil))
  leftcol uplig)

(DEFSTRUCT (t-mat (:conc-name nil))
  val ilig icol left up)


;;;
;;;  CONES
;;;

(DEFSTRUCT (CONE (:conc-name nil) (:print-function cone-print))
  (conx (:type '(member 0 1)))
  (icon (:type 'gnrt)))


(DEFSTRUCT (BICN (:conc-name nil) (:print-function bicn-print))
  (bcnx (:type '(member :bcnb :bcnc :bcnd)))
  (ibicn (:type 'gnrt)))


;;;
;;;  TENSOR-PRODUCTS
;;;


(DEFSTRUCT (TNPR (:conc-name nil) (:print-function tnpr-print))
  degr1 gnrt1 degr2 gnrt2)



;;;
;;;  COALGEBRAS
;;;

(DEFCLASS COALGEBRA (chain-complex)
  ((cprd :type morphism :initarg :cprd :reader cprd1)))   ;; coproduct

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

(DEFSTRUCT (ALLP (:print-function allp-print))
  (list (:type 'iallp)))

#+ccl
(DEFMETHOD make-load-form ((a allp) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots a))

;;;
;;;  ALGEBRAS
;;;

(DEFCLASS ALGEBRA (chain-complex)
  ((aprd :type morphism :initarg :aprd :reader aprd1)))   ;; product

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
	    (typep (car object) 'fixnum)      ;; degree *in* the bar
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


#|
(DEFUN ABAR-P (object)
  (declare (type any object))
  (the boolean
       (and (consp object)
	    (eq :abar (car object))
	    (typep (cdr object) 'iabar))))

(DEFTYPE ABAR () '(satisfies abar-p))
|#

(DEFSTRUCT (ABAR (:print-function abar-print))
  (list (:type 'list)))

#+ccl
(DEFMETHOD make-load-form ((a abar) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots a))

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

(DEFSTRUCT (ABSM (:conc-name nil) (:print-function absm-print))
  (dgop (:type 'dgop))
  (gmsm (:type 'gmsm)))

#|
()
(absm-p 1)
(absm-p '(1))
(absm-p '(:absm nil))  ;; printer-error
(absm-p '(:absm 5 . a))
(absm-p '(:absm -5 . a)) ;; printer-error
|#

(DEFTYPE FACE () 'function)
;;   (function (indx dmns gmsm) absm)

(DEFTYPE FACE* () 'function)
;;    (function (indx dmns gmsm) (or gmsm (eql :degenerate)))

(DEFCLASS SIMPLICIAL-SET (coalgebra)
  ((face :type face :initarg :face :reader face1)))

;;;
;;;  SIMPLICIAL-MRPHS
;;;

(DEFTYPE SINTR () 'function)
;; (function (dmns gmsm) absm)

(DEFCLASS SIMPLICIAL-MRPH (morphism)
  ((sintr :type sintr :initarg :sintr :reader sintr)))


;;;
;;;  SPECIAL-SMSTS
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


(DEFSTRUCT (CRPR (:print-function crpr-print) (:conc-name nil))
  (dgop1 (:type 'dgop))
  (gmsm1 (:type 'gmsm))
  (dgop2 (:type 'dgop))
  (gmsm2 (:type 'gmsm)))

;;;;
;;;;  KAN
;;;;

(DEFTYPE KFLL () 'function)
;; (function (indx dmns (list absm)) absm)

(DEFCLASS KAN (simplicial-set)
  ((kfll :type kfll :initarg :kfll :reader kfll1)))

;;;
;;;  FIBRATIONS
;;;

(DEFUN FIBRATION-P (object)
  (declare (type any object))
  (the boolean
       (and (typep object 'simplicial-mrph)
	    (= -1 (degr object)))))

(DEFTYPE FIBRATION ()
  '(satisfies fibration-p))

(DEFCLASS SIMPLICIAL-GROUP (kan hopf-algebra)
  ((grml :type simplicial-mrph :initarg :grml :reader grml1)
   (grin :type simplicial-mrph :initarg :grin :reader grin1)))

(DEFCLASS AB-SIMPLICIAL-GROUP (simplicial-group) ())


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


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf excl:*enable-package-locked-errors* nil))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :common-lisp) nil))
#+sbcl(eval-when (:compile-toplevel :load-toplevel :execute)
	(sb-ext:unlock-package :common-lisp))

(DEFSTRUCT (LOOP (:print-function loop-print))
  (list (:type 'iloop)))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf excl:*enable-package-locked-errors* t))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :common-lisp) t))
#+sbcl(eval-when (:compile-toplevel :load-toplevel :execute)
	(sb-ext:lock-package :common-lisp))

#+ccl
(DEFMETHOD make-load-form ((l loop) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots l))

;;;
;;;  CLASSIFYING-SPACES
;;;

;;  GBAR Geometrical BAR
;;  (:gbar dmns absm_(m-1) ... absm_1)

(DEFSTRUCT (GBAR (:print-function gbar-print))
  (dmns -1 :type fixnum)
  (list () :type list))

#+ccl
(DEFMETHOD make-load-form ((g gbar) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots g))
