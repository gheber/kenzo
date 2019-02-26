;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES

(IN-PACKAGE #:cat-8)

(PROVIDE "classes")

;;;
;;; VARIOUS
;;;

(DEFTYPE ANY () t)

;;; 
;;;  COMBINATIONS
;;;

(DEFSTRUCT CMBN degr list)

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

#|
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
|#

(DEFTYPE BASIS () '(or function (eql :locally-effective)))
                   ;; (function (degr) (list gnrt))

;; INTR-MRPH = INTeRnal-MoRPHism
(DEFTYPE INTR-MRPH () 'function)
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

(DEFCLASS CHAIN-COMPLEX () ())  ;; will be redefined later.

(DEFCLASS MORPHISM () ())  ;; will be redefined later.

(DEFCLASS HOMOTOPY-EQUIVALENCE () ())  ;;  will be redefined later

;;  IDNM = IDentification NuMber
(DEFVAR *IDNM-COUNTER*)
(SETF *IDNM-COUNTER* 0)

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
     (orgn :type list :initarg :orgn :reader orgn)))

;; STRT = STRaTegy
(DEFTYPE STRT () '(member :gnrt :rcrs :cmbn))


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

(DEFSTRUCT RESULT 
  (gnrt #+allegro :type #+allegro gnrt)
  (value #+allegro :type #+allegro cmbn)
  (clnm 0 :type fixnum)
  (rntm #+allegro :type #+allegro single-float))

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

(DEFSTRUCT (CONE (:conc-name nil))
  (conx #+allegro :type #+allegro (member 0 1))
  (icon #+allegro :type #+allegro gnrt))

(DEFSTRUCT (BICN (:conc-name nil))
  (bcnx #+allegro :type #+allegro (member :bcnb :bcnc :bcnd))
  (ibicn #+allegro :type #+allegro gnrt))


;;;
;;;  TENSOR-PRODUCTS
;;;


(DEFSTRUCT (TNPR (:conc-name nil)) degr1 gnrt1 degr2 gnrt2)

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

(DEFSTRUCT ALLP (list #+allegro :type #+allegro iallp))

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

(DEFSTRUCT ABAR (list #+allegro :type #+allegro list))

;;;
;;;  SIMPLICIAL SETS
;;;

;;  DGOP = DeGeneray OPerator
(DEFTYPE DGOP () `(integer 0 ,most-positive-fixnum))

;; GMSM = GeoMetric SiMplex
(DEFTYPE GMSM () 'gnrt)

;;  DLOP = DeL OPerator
(DEFTYPE DLOP () 'dgop) 

#|
;; IABSM = Internal ABstract SiMplex
(DEFUN IABSM-P (object)
  (declare (type any object))
  (the boolean
     (and (consp object)
	  (typep (car object) 'dgop)
	  (typep (cdr object) 'gmsm))))	   

(DEFTYPE IABSM () '(satisfies iabsm-p))
|#

(DEFSTRUCT (ABSM (:conc-name nil))
  (dgop #+allegro :type #+allegro dgop)
  (gmsm #+allegro :type #+allegro gmsm))


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


(DEFSTRUCT (CRPR (:conc-name nil))
  (dgop1 #+allegro :type #+allegro dgop)
  (gmsm1 #+allegro :type #+allegro gmsm)
  (dgop2 #+allegro :type #+allegro dgop)
  (gmsm2 #+allegro :type #+allegro gmsm))

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


#+ALLEGRO
(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf excl:*enable-package-locked-errors* nil))
#+CLISP
(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :common-lisp) nil))
#+SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package "COMMON-LISP"))

(DEFSTRUCT LOOP
  (list #+allegro :type #+allegro iloop))

#+ALLEGRO
(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf excl:*enable-package-locked-errors* t))
#+CLISP
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (ext:package-lock :common-lisp) t))
#+SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package "COMMON-LISP"))




;;;
;;;  CLASSIFYING-SPACES
;;;

;;  GBAR Geometrical BAR
;;  (:gbar dmns absm_(m-1) ... absm_1)

(DEFSTRUCT GBAR
  (dmns #+allegro :type #+allegro fixnum)
  (list #+allegro :type #+allegro list))

