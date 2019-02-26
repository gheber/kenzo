;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES
;;;  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES  CLASSES

(IN-PACKAGE #:cat-9)

(PROVIDE "classes")

;;;
;;; VARIOUS
;;;

(DEFTYPE ANY () t)

;;; 
;;;  COMBINATIONS
;;;

(DEFSTRUCT CMBN degr list)

(DEFMETHOD PRINT-OBJECT ((cmbn cmbn) stream)
  (declare (type stream stream))
  (the cmbn
    (with-cmbn (degr list) cmbn
      (format stream "~%~70@{-~}{CMBN ~D}" degr)
      (do ((mark list (cdr mark))
           (nth 0 (1+ nth)))
          ((endp mark))
        (declare (list mark) (type fixnum nth))
        (when (and *print-length* (= nth *print-length*))
          (format stream "~%... ...")
          (return))
        (format stream "~%<~D * ~A>" (-cffc mark) (-gnrt mark)))
      (format stream "~%~78@{-~}~%" t)
      cmbn)))

(DEFUN CMBN (degr &rest rest)
   (declare (type fixnum degr))
   (the cmbn
     (make-cmbn :degr degr
                :list (do ((rslt +empty-list+
                                 (cons (term (car mark) (cadr mark)) rslt))
                           (mark rest (cddr mark)))
                          ((endp mark) (nreverse rslt))
                        (declare (list rslt mark))))))


#|
(cmbn 2)
(cmbn 2 2 'a)
(cmbn 2 'a)
(cmbn 2 2 'a -3 'b)
(term-cmbn 3 -5 'a)
|#


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

(DEFCLASS EQUIVALENCE () ())  ;;  will be redefined later

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
     (efhm :type effective-homology :initarg :efhm :accessor efhm)
     ;; IDentification NuMber
     (idnm :type fixnum :initform (incf *idnm-counter*) :reader idnm)
     ;; ORiGiN
     (orgn :type list :initarg :orgn :reader orgn)))

(DEFMETHOD INITIALIZE-INSTANCE :after ((chcm chain-complex) &rest rest)
  (set (intern (format nil "K~D" (idnm chcm))) chcm))

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

(DEFMETHOD INITIALIZE-INSTANCE :after ((mrph morphism) &rest rest)
  (set (intern (format nil "K~D" (idnm mrph))) mrph))

(DEFSTRUCT RESULT gnrt value clnm rntm)

(DEFMETHOD PRINT-OBJECT ((result result) stream)
  (declare (type stream stream))
  (format stream "~%<Rslt>
~4TGNRT-> ~A
~3TVALUE-> ~A
~4TCLNM-> ~6D
~4TRNTM-> ~11,3F"
    (result-gnrt result) (result-value result)
    (result-clnm result) (result-rntm result))
  result)
  
#|
(make-result :gnrt 'a :value '(a a) :clnm 23 :rntm 2.345)
|#



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

(DEFMETHOD INITIALIZE-INSTANCE :after ((rdct reduction) &rest rest)
  (set (intern (format nil "K~D" (idnm rdct))) rdct))

(DEFCLASS EQUIVALENCE ()
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

(DEFMETHOD INITIALIZE-INSTANCE :after ((hmeq equivalence) &rest rest)
  (set (intern (format nil "K~D" (idnm hmeq))) hmeq))

(DEFUN EFFECTIVE-HOMOLOGY-P (object)
  (or (and (typep object 'equivalence) (not (eq (basis (rbcc object)) :locally-effective)))
      (and (typep object 'reduction) (not (eq (basis (bcc object)) :locally-effective)))
      (and (typep object 'chain-complex) (not (eq (basis object) :locally-effective)))))

(DEFTYPE EFFECTIVE-HOMOLOGY () '(satisfies effective-homology-p))


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
  conx icon)

(DEFSTRUCT (BICN (:conc-name nil) (:print-function bicn-print))
  bcnx ibicn)


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
  list)

(DEFUN ALLP-PRINT (allp stream depth)
  (declare
   (type allp allp) (type stream stream)
   (ignore depth))
  (the allp
    (progn
      (format stream "<<AlLp")
      (dolist (cbgn (allp-list allp))
        (declare (type cbgn cbgn))
        (with-cbgn (degr gnrt) cbgn
                   (format stream "[~D ~A]" degr gnrt)))
      (format stream ">>")
      allp)))

;;;
;;;  ALGEBRAS
;;;

(DEFCLASS ALGEBRA (chain-complex)
  ((aprd :type morphism :initarg :aprd :reader aprd1)))   ;; product

(DEFCLASS AB-ALGEBRA (algebra)
  ())

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

(DEFSTRUCT ABAR list)

(DEFMETHOD PRINT-OBJECT ((abar abar) stream)
  (declare
   (type abar abar) (type stream stream))
  (the abar
    (progn
      (format stream "<<Abar")
      (dolist (brgn (abar-list abar))
        (declare (type brgn brgn))
        (with-brgn (degr gnrt) brgn
                   (format stream "[~D ~A]" degr gnrt)))
      (format stream ">>")
      abar)))
  


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

(DEFSTRUCT (ABSM (:conc-name nil) (:print-function absm-print))
  dgop gmsm)


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
  dgop1 gmsm1 dgop2 gmsm2)

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

(DEFCLASS AB-SIMPLICIAL-GROUP (simplicial-group ab-algebra) ())


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
         (setf (ext:package- :common-lisp) nil))
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


(DEFUN DGOP-EXT-INT (ext-dgop)
   (declare (list ext-dgop))
          ;; (list fixnum)
   (when ext-dgop   
      (unless (apply #'> ext-dgop)
         (error "In DGOP-EXT-INT, the external dgop ~A is not decreasing." ext-dgop)))
   (the fixnum
      (apply #'logxor (mapcar
                         #'(lambda (item)
                              (declare (type fixnum item))
                              (the fixnum (2-exp item)))
                         ext-dgop))))

#|
(dgop-ext-int '())
(dgop-ext-int '(2 2))
(dgop-ext-int '(4 0))
|#

(DEFUN DGOP-INT-EXT (dgop)
   (declare (type fixnum dgop))
   (unless (typep dgop 'dgop)
      (error "In DXOP-INT-EXT, ~A is not a dxop." dgop))
   (the list
      (do ((dgop dgop (ash dgop -1))
           (rslt +empty-list+)
           (bmark 0 (1+ bmark)))
          ((zerop dgop) rslt)
         (declare
            (type fixnum dgop bmark)
            (list rslt))
         (when (oddp dgop)
            (push bmark rslt)))))

#|
()
(dgop-int-ext 0)
(dgop-int-ext 4)
(dgop-int-ext 63)
(dotimes (i 33)
  (print (dgop-ext-int (dgop-int-ext i)))))
|#

(DEFUN LOOP-PRINT (loop stream depth)
  (declare
   (type loop loop) (type stream stream)
   (ignore depth))
  (the loop
    (progn
      (format stream "<<Loop")
      (dolist (apowr (loop-list loop))
        (declare (type apowr apowr))
        (with-apowr (dgop gmsm expn) apowr
                    (format stream "[")
                    (unless (zerop dgop)
                      (format stream "~A " (hyphenize-list (dgop-int-ext dgop))))
                    (format stream "~A" gmsm)
                    (unless (= 1 expn)
                      (format stream "\\~D" expn))
                    (format stream "]")))
      (format stream ">>")
      loop)))

(DEFUN HYPHENIZE-LIST (list)
   (declare (list list))
   (if list
      (format nil "~D~{-~D~}" (first list) (rest list))
      (format nil "-")))

#|
(hyphenize-list '())
(hyphenize-list '(3))
(hyphenize-list '(5 3 1)))
|#

#|
  (loop3 nil)
  (loop3 '(0 a 2))
  (loop3 '(1 a 2))
  (loop3 '(3 a 2 5 b -1 6 c 4))
  (loop3 '(7 a 1 11 b 1 13 c 1 14 d 1))
  (loop3 '(7 a 1 11 b 1 13 c 1 7 d 1))
  (loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))
|#

;;;
;;;  CLASSIFYING-SPACES
;;;

;;  GBAR Geometrical BAR
;;  (:gbar dmns absm_(m-1) ... absm_1)

(DEFSTRUCT (GBAR (:print-function gbar-print))
  dmns list)

(DEFUN GBAR-PRINT (gbar stream depth)
  (declare
   (type gbar gbar) (type stream stream)
   (ignore depth))
  (the gbar
    (progn
      (princ "<<GBar" stream)
      (dolist (absm (gbar-list gbar))
        (declare (type absm absm))
        (with-absm (dgop gmsm) absm
                   (format stream "<~A ~A>"
                     (hyphenize-list (dgop-int-ext dgop))
                     gmsm)))
      (princ ">>" stream)
      gbar)))
