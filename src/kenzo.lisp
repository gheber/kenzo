;;;; kenzo.lisp

(IN-PACKAGE #:cat)

(DEFUN KENZO-VERSION ()
  (format t "~%*** Kenzo-Version 1.1.7 ***~2%")
  (values))

;;(PROCLAIM '(OPTIMIZE (speed 3) (safety 1) (space 0) (debug 0)))
(DECLAIM (OPTIMIZE (speed 0) (space 0) (debug 3)))

;; Don't mess with ANSI's definition of DEFCONSTANT!

(DEFMACRO DEFINE-CONSTANT (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

;; globals from cat-init.lisp

(DEFVAR *CMBN-CONTROL*
  "The variable *CMBN-CONTROL* can be used to enable (default) or disable the
validation of combinations.")
(SETF *CMBN-CONTROL* T)

;; globals from various.lisp

(DEFINE-CONSTANT +EMPTY-LIST+ '())

(DEFINE-CONSTANT +F-EMPTY-VECTOR+
   (make-array 0 :element-type 'fixnum))

(DEFINE-CONSTANT +S-EMPTY-VECTOR+ #())

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

;; globals from combinations.lisp

;; globals from chain-complexes.lisp

(DEFVAR *LIST-LIST* +EMPTY-LIST+
  "The variable *LIST-LIST* is bound to a list of variable names, which are
bound to lists of user created objects such as chain complexes, morphisms,
etc.")

(DEFVAR *CHCM-LIST*
  "The variable *CHCM-LIST* is bound to a list of user created chain
complexes.")
(SETF *CHCM-LIST* +EMPTY-LIST+)
(PUSHNEW '*CHCM-LIST* *LIST-LIST*)

(DEFPARAMETER +MAXIMAL-DIMENSION+ 16)

(DEFVAR *MRPH-LIST*
  "This variable is bound to a list of user created morphisms.")
(SETF *MRPH-LIST* +EMPTY-LIST+)
(PUSHNEW '*MRPH-LIST* *LIST-LIST*)

(DEFVAR *START-STACK* +EMPTY-LIST+)

(DEFPARAMETER +TOO-MUCH-TIME+ -1)

(DEFVAR *RESULTS-MAX* (expt 10 5))

(DEFVAR *RESULTS-COEF* 3.0)
(DEFVAR *RESULTS-CMLT-TM* 0.0)
(DEFVAR *RESULTS-N* 0)

(DEFVAR *RESULTS-VERBOSE* t)

(DEFVAR *FUTURE-DISPLAY* nil)

(DEFVAR *TIME-INTERVAL* 60000)

;; globals from classes.lisp

;;  IDNM = IDentification NuMber
(DEFVAR *IDNM-COUNTER*
  "The variable *IDNM-COUNTER* is bound to a counter for Kenzo object numbers.")
(SETF *IDNM-COUNTER* 0)


;; globals from effective homology.lisp

(DEFVAR *RDCT-LIST*)
(SETF *RDCT-LIST* +EMPTY-LIST+)
(PUSHNEW '*RDCT-LIST* *LIST-LIST*)

(DEFVAR *HMEQ-LIST*)
(SETF *HMEQ-LIST* +EMPTY-LIST+)
(PUSHNEW '*HMEQ-LIST* *LIST-LIST*)

(DEFVAR *TDD*)
(DEFVAR *BDD*)
(DEFVAR *ID-FG*)
(DEFVAR *ID-GF-DH-HD*)
(DEFVAR *HH*)
(DEFVAR *FH*)
(DEFVAR *HG*)
(DEFVAR *DF-FD*)
(DEFVAR *DG-GD*)


(DEFVAR *TC*)
(DEFVAR *BC*)

;; globals from homology-groups.lisp

(DEFVAR *HOMOLOGY-VERBOSE* t)

;; globals from tensor-products.lisp

(DEFVAR *TNPR-WITH-DEGREES* nil)

;; globals from coalgebras.lisp

(DEFVAR *CLGB-LIST*)
(SETF *CLGB-LIST* +EMPTY-LIST+)
(PUSHNEW '*CLGB-LIST* *LIST-LIST*)

;; globals from algebras.lisp

(DEFVAR *ALGB-LIST*)
(SETF *ALGB-LIST* +EMPTY-LIST+)
(PUSHNEW '*ALGB-LIST* *LIST-LIST*)

(DEFVAR *HOPF-LIST*)
(SETF *HOPF-LIST* +EMPTY-LIST+)
(PUSHNEW '*HOPF-LIST* *LIST-LIST*)

;; globals from simplicial-sets.lisp

(DEFVAR *SMST-LIST*)
(SETF *SMST-LIST* +empty-list+)
(PUSHNEW '*SMST-LIST* *list-list*)

;; globals from simplicial-mrphs.lisp

(DEFVAR *SMMR-LIST*)
(SETF *SMMR-LIST* +empty-list+)
(PUSHNEW '*SMMR-LIST* *LIST-LIST*)

;; globals from kan.lisp

(DEFVAR *KAN-LIST*)
(SETF *KAN-LIST* +EMPTY-LIST+)
(PUSHNEW '*KAN-LIST* *LIST-LIST*)

;; globals from simplicial-groups.lisp

(DEFVAR *SMGR-LIST*)
(SETF *SMGR-LIST* +empty-list+)
(PUSHNEW '*SMGR-LIST* *list-list*)
