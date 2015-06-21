;; FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES
;; FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES
;; FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES   FILTERED-COMPLEXES

(IN-PACKAGE #:cat)

(provide "filtered-complexes")

(DEFTYPE degr () 'fixnum)

(DEFTYPE CHCM-FLIN () 'function) 
;; '(function (degr gnrt) degr)


(DEFCLASS FILTERED-CHAIN-COMPLEX (chain-complex)
  ((flin :type chcm-flin :initarg :flin :reader flin1)))

(DEFVAR *flcc-list*)
(SETF *flcc-list* +empty-list+)
(PUSHNEW '*flcc-list* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flcm FILTERED-CHAIN-COMPLEX) stream)
  (the FILTERED-CHAIN-COMPLEX
       (progn
	 (format stream "[K~D Filtered-Chain-Complex]" (idnm flcm))
	 flcm)))

(DEFUN FLCC (idnm)
  (declare (type fixnum idnm))
  (the (or FILTERED-CHAIN-COMPLEX null)
       (find idnm *flcc-list* :key #'idnm)))


;; Function that builds a filtered chain complex. 
(DEFUN BUILD-FLCC
    (&key cmpr basis bsgn intr-dffr dffr-strt flin orgn)
  (declare
   (type cmprf cmpr)
   (type intr-mrph intr-dffr)
   (type basis basis)
   (type gnrt bsgn)
   (type strt dffr-strt)
   (type Chcm-Flin flin)
   (list orgn))
  (the FILTERED-CHAIN-COMPLEX
       (progn
         (let ((already (find orgn *flcc-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-CHAIN-COMPLEX) already))
	   (when already
	     (return-from build-flcc already)))
         (let ((rslt (build-chcm :cmpr cmpr :basis basis :bsgn bsgn
				 :intr-dffr intr-dffr :strt dffr-strt
				 :orgn orgn)))
	   (declare (type chain-complex rslt))
	   (change-class rslt 'FILTERED-CHAIN-COMPLEX)
	   (setf (slot-value rslt 'flin) flin)
	   (push rslt *flcc-list*)
	   rslt))))


;; Function that changes a chain complex chcm to a filtered complex, with filtration
;; index of a generator defined by the function flin, and origin orgn. 
#|(DEFUN CHANGE-CHCM-TO-FLCC (chcm &key flin orgn)
(declare
 (type chain-complex chcm)
 (type chcm-flin flin)
 (list orgn))
(the FILTERED-CHAIN-COMPLEX
     (progn
       (change-class chcm 'FILTERED-CHAIN-COMPLEX)
       ;;(setf orgn (list (orgn chcm) 'then orgn))
       (let ((already (find orgn *flcc-list* :key #'orgn :test #'equal)))
	 (declare (type (or null FILTERED-CHAIN-COMPLEX) already))
	 (when already
	   (return-from change-chcm-to-flcc already)))
       (setf (slot-value chcm 'flin) flin)
       ;;(setf (slot-value chcm 'orgn) orgn)
       (push chcm *flcc-list*)
       chcm)))
|#


(DEFMETHOD CHANGE-CHCM-TO-FLCC ((chcm chain-complex) flin flin-orgn)
  (declare
   (type chain-complex chcm)
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-CHAIN-COMPLEX
       (progn
         (change-class chcm 'FILTERED-CHAIN-COMPLEX)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flcc-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-CHAIN-COMPLEX) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flcc-list*)
         chcm)))


(DEFMACRO FLIN (&rest rest)
  (ecase (length rest)
    (1 `(flin1 ,@rest))
    (2 `(flin2 ,@rest))
    (3 `(flin3 ,@rest))))

(DEFUN FLIN3 (object degr gnrt)         
  (declare
   (type FILTERED-CHAIN-COMPLEX object)
   (fixnum degr)
   (type gnrt gnrt))
  (the fixnum
       (with-slots (flin) object
         (declare (type chcm-flin flin))
         (funcall flin degr gnrt))))

(DEFUN FLIN2 (object cmbn)
  (declare
   (type FILTERED-CHAIN-COMPLEX object)
   (type cmbn cmbn))
  (the fixnum
       (with-slots (flin) object
         (declare (type chcm-flin flin))
	 (with-cmbn (degr cmbn-list) cmbn
		    (declare
		     (fixnum degr)
		     (list cmbn-list))
		    (labels ((list-max (list)
			       (declare (type list list))
			       (let* ((max-int (car list)))
				 (do ((mark1 list (cdr mark1)))
				     ((endp mark1))
				   (setq max-int (max max-int (car mark1))))
				 max-int)))
		      (if (endp cmbn-list)
			  0
			  (list-max 
			   (mapcar 
			    #'(lambda (term)
				(flin3 object degr (gnrt term)))
			    cmbn-list))))))))



;; Function that returns the min and max of the filtration index of the elements of the basis
;; for degree degr
(DEFUN FLIN-MIN-MAX (fltrcm degr)
  (declare (type FILTERED-CHAIN-COMPLEX fltrcm)
	   (type fixnum degr))
  (when (eq (basis fltrcm) :locally-effective)
    (error "flin-min-max cannot work with a LOCALLY-EFFECTIVE chain complex."))
  (the list
       (let* ((basis (basis fltrcm degr)))
	 (declare (list basis))
	 (if (endp basis)
	     (list 0 0)
	     (let* ((min (flin fltrcm degr (first basis)))
		    (max min))
	       (declare (type fixnum min max))
	       (dolist (gnrt (cdr basis))
                 (let ((aux (flin fltrcm degr gnrt)))
		   (declare (type fixnum aux))
		   (if (< aux min)
                       (setf min aux)
                       (if (> aux max)
			   (setf max aux)))))
	       (list min max))))))



;; Function that returns the elements of the basis that have bidegree (p,q).
(DEFUN BIGRD-BASIS (fltrcm p q)
  (declare (type FILTERED-CHAIN-COMPLEX fltrcm)
	   (type fixnum p q))
  (when (eq (basis fltrcm) :locally-effective)
    (error "Bigrd-Basis cannot work with a LOCALLY-EFFECTIVE chain complex."))
  (the list
       (let* ((degr (+ p q))
	      (basis (basis fltrcm degr)))
	 (declare 
          (type list basis)
          (fixnum degr))
	 (mapcan 
          #'(lambda (gnrt)
              (declare (type gnrt gnrt))
              (if (= (flin fltrcm degr gnrt) p)
		  (list gnrt)
		  nil))
          basis))))

;; Function that returns the elements of the basis of degree "degr" that have
;; filtration index <= "fltr-index".
(DEFUN FLTRD-BASIS (fltrcm degr fltr-index)
  (declare (type FILTERED-CHAIN-COMPLEX fltrcm)
	   (type fixnum degr fltr-index))
  (when (eq (basis fltrcm) :locally-effective)
    (error "Fltrd-Basis cannot work with a LOCALLY-EFFECTIVE chain complex."))
  (let ((min (first (flin-min-max fltrcm degr))))
    (declare (type fixnum min))
    (the list
	 (mapcan
          #'(lambda (i)
              (bigrd-basis fltrcm i (- degr i)))
          (<a-b> min fltr-index)))))

;; Function that returns the elements of the basis with degree "degr",
;; ordered by the filtration index.
(DEFUN ORDERED-BASIS (fltrcm degr)
  (declare
   (type FILTERED-CHAIN-COMPLEX fltrcm)
   (fixnum degr))
  (when (eq (basis fltrcm) :locally-effective)
    (error "Ordered-Basis cannot work with a LOCALLY-EFFECTIVE chain complex."))
  (the list
       (let ((max (second (flin-min-max fltrcm degr))))
	 (declare (type fixnum max))
	 (fltrd-basis fltrcm degr max))))  

;; Matrix of the differential application of degree "degr" of the subcomplex
;; F_pX, where p=fltr-index and X=fltrcm.
(defun FLCC-DFFR-MTRX (fltrcm degr fltr-index)
  (declare
   (type FILTERED-CHAIN-COMPLEX fltrcm)
   (fixnum degr fltr-index))
  (when (eq (basis fltrcm) :locally-effective)
    (error "flcc-dffr-mtrx cannot work with a LOCALLY-EFFECTIVE chain complex."))
  (the matrix
       (let* ((cmpr (cmpr1 fltrcm))
	      (dffr (dffr fltrcm))
	      (sbasis (fltrd-basis fltrcm degr fltr-index))
	      (tbasis (ordered-basis fltrcm (1- degr)))
	      ;; If the differential application in the filtered complex respects the filtration,
	      ;; i.e., d(F_pA_n) \in (F_pA)_{n-1}, the target basis is (fltrd-basis fltrcm (1- degr) fltr-index)
	      (srank (length sbasis))
	      (trank (length tbasis)))
	 (declare
	  (type cmprf cmpr)
	  (type morphism dffr)
	  (fixnum degr)
	  (list sbasis tbasis)
	  (fixnum srank trank))
	 (let ((rslt 
		#-ACLPC
		 (make-array (list trank srank)
			     :element-type 'fixnum
			     :initial-element 0)
		 #+ACLPC
		 (if (or (zerop srank) (zerop trank))
		     (make-array (list trank srank)
				 :element-type 'fixnum)
		     (make-array (list trank srank)
				 :element-type 'fixnum
				 :initial-element 0))
		 ))               
	   (declare (type matrix rslt))
	   (do ((j 0 (1+ j))
		(mark sbasis (cdr mark)))
	       ((endp mark))
	     (declare
	      (fixnum j)
	      (list mark))
	     (let ((cmbn (gnrt-? dffr degr (car mark))))
	       (declare (type cmbn cmbn))
	       (do ((mark1 (cmbn-list cmbn) (cdr mark1)))
		   ((endp mark1))
		 (declare (list mark1))
		 (with--term (cffc gnrt) mark1
			     (declare 
			      (fixnum cffc)
			      (type gnrt gnrt))
			     (do ((mark2 tbasis (cdr mark2))
				  (i 0 (1+ i))
				  (found nil))
				 ((or (endp mark2) found))
			       (declare 
				(list mark2)
				(fixnum i))
			       (if (eq :equal (funcall cmpr gnrt (car mark2)))
				   (progn
				     (setq found 1)
				     (setf (aref rslt i j) cffc))))))))
	   rslt))))


;; OTHER CLASSES THAT INHERIT FROM FILTERED-CHAIN-COMPLEX


(DEFCLASS FILTERED-COALGEBRA (filtered-chain-complex coalgebra)
  ())

(DEFVAR *FLCLGB-LIST*)
(SETF *FLCLGB-LIST* +empty-list+)
(PUSHNEW '*FLCLGB-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flclgb FILTERED-COALGEBRA) stream)
  (the FILTERED-COALGEBRA
       (progn
	 (format stream "[K~D FILTERED-COALGEBRA]" (idnm flclgb))
	 flclgb)))

(DEFUN FltrClgb (idnm)
  (declare (type fixnum idnm))
  (the (or FILTERED-COALGEBRA null)
       (find idnm *flclgb-list* :key #'idnm)))

;; Function that builds a filtered coalgebra. 
(DEFUN BUILD-FltrClgb
    (&key cmpr basis bsgn intr-dffr dffr-strt intr-cprd cprd-strt flin orgn)
  (declare
   (type cmprf cmpr)
   (type intr-mrph intr-dffr intr-cprd)
   (type basis basis)
   (type gnrt bsgn)
   (type strt dffr-strt cprd-strt)
   (type chcm-flin flin)
   (list orgn))
  (the FILTERED-COALGEBRA
       (progn
         (let ((already (find orgn *flclgb-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-COALGEBRA) already))
	   (when already
	     (return-from build-FltrClgb already)))
         (let ((rslt (build-clgb :cmpr cmpr :basis basis :bsgn bsgn
				 :intr-dffr intr-dffr :dffr-strt dffr-strt
				 :intr-cprd intr-cprf :cprd-strt cprd-strt
				 :orgn orgn)))
	   (declare (type coalgebra rslt))
	   (change-class rslt 'FILTERED-COALGEBRA)
	   (setf (slot-value rslt 'flin) flin)
	   (push rslt *flclgb-list*)
	   rslt))))

(defmethod change-chcm-to-flcc ((chcm coalgebra) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-COALGEBRA
       (progn
         (change-class chcm 'FILTERED-COALGEBRA)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flclgb-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-COALGEBRA) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flCLGB-list*)
         chcm)))


(DEFCLASS FILTERED-ALGEBRA (filtered-chain-complex algebra)
  ())

(DEFVAR *FLALGB-LIST*)
(SETF *FLALGB-LIST* +empty-list+)
(PUSHNEW '*FLALGB-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flalgb FILTERED-ALGEBRA) stream)
  (the FILTERED-ALGEBRA
       (progn
	 (format stream "[K~D FILTERED-ALGEBRA]" (idnm flalgb))
	 flalgb)))

(DEFUN FltrAlgb (idnm)
  (declare (type fixnum idnm))
  (the (or FILTERED-ALGEBRA null)
       (find idnm *flalgb-list* :key #'idnm)))

;; Function that builds a filtered algebra. 
(DEFUN BUILD-FltrAlgb
    (&key cmpr basis bsgn intr-dffr dffr-strt intr-aprd aprd-strt flin orgn)
  (declare
   (type cmprf cmpr)
   (type intr-mrph intr-dffr intr-aprd)
   (type basis basis)
   (type gnrt bsgn)
   (type strt dffr-strt aprd-strt)
   (type chcm-flin flin)
   (list orgn))
  (the FILTERED-ALGEBRA
       (progn
         (let ((already (find orgn *flalgb-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-ALGEBRA) already))
	   (when already
	     (return-from build-FltrAlgb already)))
         (let ((rslt (build-algb :cmpr cmpr :basis basis :bsgn bsgn
				 :intr-dffr intr-dffr :dffr-strt dffr-strt
				 :intr-aprd intr-aprf :aprd-strt aprd-strt
				 :orgn orgn)))
	   (declare (type algebra rslt))
	   (change-class rslt 'FILTERED-ALGEBRA)
	   (setf (slot-value rslt 'flin) flin)
	   (push rslt *flalgb-list*)
	   rslt))))

(defmethod change-chcm-to-flcc ((chcm algebra) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-ALGEBRA
       (progn
         (change-class chcm 'FILTERED-ALGEBRA)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flalgb-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-ALGEBRA) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flaLGB-list*)
         chcm)))


(DEFCLASS FILTERED-HOPF-ALGEBRA (filtered-chain-complex hopf-algebra)
  ())

(DEFVAR *FLHOPF-LIST*)
(SETF *FLHOPF-LIST* +empty-list+)
(PUSHNEW '*FLHOPF-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flhopf FILTERED-HOPF-ALGEBRA) stream)
  (the FILTERED-HOPF-ALGEBRA
       (progn
	 (format stream "[K~D FILTERED-HOPF-ALGEBRA]" (idnm flhopf))
	 flhopf)))

(DEFUN FltrHopf (idnm)
  (declare (type fixnum idnm))
  (the (or FILTERED-HOPF-ALGEBRA null)
       (find idnm *flhopf-list* :key #'idnm)))

(defmethod change-chcm-to-flcc ((chcm hopf-algebra) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-hofp-algebra
       (progn
         (change-class chcm 'FILTERED-hopf-COALGEBRA)
	 (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flhopf-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-HOPF-COALGEBRA) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flhopf-list*)
         chcm)))


(DEFCLASS FILTERED-SIMPLICIAL-SET (FILTERED-CHAIN-COMPLEX simplicial-set )
  ())

(DEFVAR *FLSMST-LIST*)
(SETF *FLSMST-LIST* +empty-list+)
(PUSHNEW '*FLSMST-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flsmst filtered-simplicial-set) stream)
  (the filtered-simplicial-set
       (progn
	 (format stream "[K~D Filtered-Simplicial-Set]" (idnm flsmst))
	 flsmst)))

(DEFUN FltrSmSet (idnm)
  (declare (type fixnum idnm))
  (the (or filtered-simplicial-set null)
       (find idnm *flsmst-list* :key #'idnm)))



;; Function that builds a filtered simplicial set 
(DEFUN BUILD-FltrSmSt
    (&key cmpr basis bspn face face* intr-bndr bndr-strt intr-dgnl dgnl-strt flin orgn)
  (declare
   (type cmprf cmpr)
   (type basis basis)
   (type gmsm bspn)
   (type face face)
   (type (or null face*) face*)
   (type (or intr-mrph null) intr-bndr intr-dgnl)
   (type (or strt null) bndr-strt dgnl-strt)
   (type chcm-flin flin)
   (list orgn))
  (the FILTERED-SIMPLICIAL-SET
       (progn
         (let ((already (find orgn *flsmst-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-SIMPLICIAL-SET) already))
	   (when already
	     (return-from build-FltrSmst already)))
         (let ((rslt (build-smst :cmpr cmpr :basis basis :bspn bspn
				 :face face :face* face* :intr-bndr intr-bndr 
				 :bndr-strt bndr-strt :intr-dgnl intr-dgnl 
				 :dgnl-strt dgnl :orgn orgn)))
	   (declare (type simplicial-set rslt))
	   (change-class rslt 'FILTERED-SIMPLICIAL-SET)
	   (setf (slot-value rslt 'flin) flin)
	   (push rslt *flsmst-list*)
	   rslt))))

(defmethod change-chcm-to-flcc ((chcm simplicial-set) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-SIMPLICIAL-SET
       (progn
         (change-class chcm 'FILTERED-SIMPLICIAL-SET)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flsmst-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-SIMPLICIAL-SET) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flsmst-list*)
         chcm)))


(DEFCLASS FILTERED-KAN (FILTERED-CHAIN-COMPLEX KAN )
  ())

(DEFVAR *FLKAN-LIST*)
(SETF *FLKAN-LIST* +empty-list+)
(PUSHNEW '*FLKAN-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flkan filtered-kan) stream)
  (the filtered-kan
       (progn
	 (format stream "[K~D Filtered-Kan-Simplicial-Set]" (idnm flkan))
	 flkan)))

(DEFUN FltrKan (idnm)
  (declare (type fixnum idnm))
  (the (or filtered-kan null)
       (find idnm *flkan-list* :key #'idnm)))

(defmethod change-chcm-to-flcc ((chcm kan) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-KAN
       (progn
         (change-class chcm 'FILTERED-KAN)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flkan-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-KAN) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flkan-list*)
         chcm)))


(DEFCLASS FILTERED-SIMPLICIAL-GROUP (FILTERED-CHAIN-COMPLEX SIMPLICIAL-GROUP )
  ())

(DEFCLASS FILTERED-AB-SIMPLICIAL-GROUP (FILTERED-CHAIN-COMPLEX AB-SIMPLICIAL-GROUP )
  ())

(DEFVAR *FLSMGR-LIST*)
(SETF *FLSMGR-LIST* +empty-list+)
(PUSHNEW '*FLSMGR-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((flsmgr filtered-simplicial-group) stream)
  (the filtered-simplicial-group
       (progn
	 (format stream "[K~D Filtered-Simplicial-Group]" (idnm flsmgr))
	 flsmgr)))

(DEFMETHOD PRINT-OBJECT ((flsmgr filtered-ab-simplicial-group) stream)
  (the filtered-simplicial-group
       (progn
	 (format stream "[K~D Filtered-Abelian-Simplicial-Group]" (idnm flsmgr))
	 flsmgr)))

(DEFUN FltrSmgr (idnm)
  (declare (type fixnum idnm))
  (the (or filtered-simplicial-group null)
       (find idnm *flsmgr-list* :key #'idnm)))

(DEFUN BUILD-FltrSmGr
    (&key cmpr basis bspn face face* intr-bndr bndr-strt intr-dgnl dgnl-strt 
       sintr-grml sintr-grin flin orgn)
  (declare
   (type cmprf cmpr)
   (type basis basis)
   (type gmsm bspn)
   (type face face)
   (type (or null face*) face*)
   (type (or intr-mrph null) intr-bndr intr-dgnl)
   (type (or strt null) bndr-strt dgnl-strt)
   (type sintr sintr-grml sintr-grin)
   (type chcm-flin flin)
   (list orgn))
  (the FILTERED-SIMPLICIAL-GROUP
       (progn
         (let ((already (find orgn *flsmgr-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-SIMPLICIAL-GROUP) already))
	   (when already
	     (return-from build-FltrSmgr already)))
         (let ((rslt (build-smgr :cmpr cmpr :basis basis :bspn bspn
				 :face face :face* face* :intr-bndr intr-bndr 
				 :bndr-strt bndr-strt :intr-dgnl intr-dgnl 
				 :dgnl-strt dgnl :sintr-grml sintr-grml
				 :sintr-grin sintr-grin :orgn orgn)))
	   (declare (type simplicial-group rslt))
	   (change-class rslt 'FILTERED-SIMPLICIAL-GROUP)
	   (setf (slot-value rslt 'flin) flin)
	   (push rslt *flsmgr-list*)
	   rslt))))

(DEFMACRO BUILD-FlABSMGR (&rest rest)
  `(change-class (build-flsmgr ,@rest) 'filtered-ab-simplicial-group))

(defmethod change-chcm-to-flcc ((chcm simplicial-group) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-SIMPLICIAL-GROUP
       (progn
         (change-class chcm 'FILTERED-SIMPLICIAL-GROUP)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flsmgr-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-SIMPLICIAL-GROUP) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flsmgr-list*)
         chcm)))

(defmethod change-chcm-to-flcc ((chcm ab-simplicial-group) flin flin-orgn )
  (declare
   
   (type chcm-flin flin)
   (list flin-orgn))
  (the FILTERED-AB-SIMPLICIAL-GROUP
       (progn
         (change-class chcm 'AB-FILTERED-SIMPLICIAL-GROUP)
         (setf orgn (append (orgn chcm) (list 'FILTERED-WITH) flin-orgn))
         (let ((already (find orgn *flabsmgr-list* :key #'orgn :test #'equal)))
	   (declare (type (or null FILTERED-AB-SIMPLICIAL-GROUP) already))
	   (when already
	     (return-from change-chcm-to-flcc already)))
         (setf (slot-value chcm 'flin) flin)
         (setf (slot-value chcm 'orgn) orgn)
         (push chcm *flabsmgr-list*)
         chcm)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    EXAMPLES    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; BICOMPLEXES

;; Filtration index function of a Bicomplex (first degree of a generator).
(setf BCflin 
      #'(lambda (degr gnrt)
	  (declare 
	   (type fixnum degr)
	   (type gnrt gnrt))
	  (let* ((degr1 (BcGnrt-Degr1 gnrt))
		 (degr2 (BcGnrt-Degr2 gnrt)))
	    (declare (type fixnum degr1 degr2))
	    (if (=  degr (+ degr1 degr2))
		degr1
		0))))

#|
(funcall BCflin 5 (build-bcgnrt 2 3 'a))
(funcall BCflin 7 (build-bcgnrt 6 1 'c))
|# 

;; Function that changes the bicomplex bc to a filtered complex, with filtration
;; index of a generator defined as its first degree. 
(defun CHANGE-BICM-TO-FLCC (bc)
  (declare 
   (type bicomplex bc))
  (let* ((orgn (orgn bc))
	 (flin BCflin))
    (declare
     (type list orgn)
     (type chcm-flin flin))
    (the FILTERED-CHAIN-COMPLEX
	 (change-chcm-to-flcc bc flin '(Bicomplex-Flin)))))

#|
(defun bas (degr1 degr2)
  (if (and (= degr1 0) (= degr2 1)) (return-from bas '(a)))
  (if (and (= degr1 1) (= degr2 0)) (return-from bas '(b)))
  (if (and (= degr1 1) (= degr2 1)) (return-from bas '(c)))
  (if (and (= degr1 2) (= degr2 0))  (return-from bas '(d)))
  (return-from bas nil))

(defun dif1 (degr1 degr2 gnrt)
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif1 (list (cons 2 'a))))
  (if (and (= degr1 2) (= degr2 0) (eql gnrt 'd)) (return-from dif1 (list (cons 2 'b))))
  (return-from dif1 nil))
(defun dif2 (degr1 degr2 gnrt)
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif2 (list (cons 1 'b))))
  (return-from dif2 nil))

(setf bc (Build-Bicm :bcbasis #'bas :dffr1 #'dif1 :dffr2 #'dif2 :cmpr 's-cmpr 
		     :orgn '(BC-test)))

(change-bicm-to-flcc bc)
(flin bc 1 (build-bcgnrt 0 1 'a))
(flin bc (cmbn 2 4 (build-bcgnrt 1 1 'c)))
(flin bc (cmbn 2 4 (build-bcgnrt 1 1 'c) 2 (build-bcgnrt 2 0 'd)))
(bigrd-basis bc 1 1)
(bigrd-basis bc 2 0)
(fltrd-basis bc 2 1)
(ordered-basis bc 2)
(flcc-dffr-mtrx bc 2 1)
(flcc-dffr-mtrx bc 2 2)
|#


;; BARS

#|
(defun abar-bidegree (abar)
  (declare (type abar abar))
  (the list
       (let ((p (1- (length abar)))
	     (q 0))
	 (declare (type fixnum p q))
	 (setf q (apply #'+ (mapcar #'car (rest abar))))
	 (decf q p)
	 (return-from abar-bidegree (list p q)))))
|#

(setf abar-flin
      #'(lambda (degr abar)
	  (declare (type fixnum degr)
		   (type abar abar))
	  (let* ((abar-bidgr (abar-bidegree abar))
		 (degr1 (first abar-bidgr))
		 (degr2 (second abar-bidgr)))
	    (if (=  degr (+ degr1 degr2))
		degr1
		0))))

#|
(setq kz2 (k-z2-1))
(setq bar2 (bar kz2))

(change-chcm-to-flcc bar2 abar-flin '(abar-flin))


(setf basis7 (basis bar2 7))

(mapcar
 #'(lambda (gnrt)
     (flin bar2 7 gnrt))
 basis7)

(flin bar2 (cmbn 7 (first basis7) (second basis7))) 

(bigrd-basis bar2 2 5)
(bigrd-basis bar2 3 4)
(fltrd-basis bar2 7 2)
(ordered-basis bar2 7)
(flcc-dffr-mtrx bar2 7 2)
(flcc-dffr-mtrx bar2 7 3)
|#

;; COBAR

#|
(defun allp-bidegree (allp)
  (declare (type allp allp))
  (the list
       (let ((p (1- (length allp)))
	     (q 0))
	 (declare (type fixnum p q))
	 (setf q (apply #'+ (mapcar #'car (rest allp))))
	 (incf q p)
	 (return-from allp-bidegree (list (- p) q)))))
|#

(setf cobar-flin 
      #'(lambda (degr allp)
	  (declare (type fixnum degr)
		   (type allp allp))
	  (let* ((allp-bidgr (allp-bidegree allp))
		 (degr1 (first allp-bidgr))
		 (degr2 (second allp-bidgr)))
	    (if (=  degr (+ degr1 degr2))
		degr1
		0))))

;; TENSOR PRODUCTS 

(setf tnpr-flin 
      #'(lambda (degr tnpr)
	  (declare 
	   (type fixnum degr)
	   (type tnpr tnpr))
	  (the fixnum
	       (degr1 tnpr))))


;; FIBRATIONS (CARTESIAN PRODUCTS AND TWISTED PRODUCTS)

#|
(setf crpr-flin 
      #'(lambda (degr crpr)
          (declare
           (type fixnum degr)
           (type crpr crpr))
          (let* ((b (cadr crpr))
                 (dgop (car b)))
	    (declare
	     (type iabsm b)
	     (type fixnum dgop))
	    (the fixnum
		 (- degr (length (dgop-int-ext dgop)))))))
|#

#|
(funcall fbrt-flin 3 '(:crpr (1 . s2) 0 . 3))
(funcall fbrt-flin 4 '(:crpr (5 . s2) 0 . 3))
|# 
 

;; BICONES
 
(defun bicone-flin (flinb flinc flind)
  (declare (type chcm-flin flinb flinc flind))
  (flet ((rslt (degr bicn)
	   (declare 
	    (type bicn bicn)
	    (type degr degr))
	   (let ((bcnx (bcnx bicn)))
	     (declare (type (member :bcnb :bcnc :bcnd) bcnx))
	     (if (eql :bcnb bcnx)
		 (funcall flinb degr (ibicn bicn))
		 (if (eql :bcnc bcnx)
                     (funcall flinc degr (ibicn bicn))
                     (funcall flind degr (ibicn bicn)))))))
    (the chcm-flin #'rslt))) 
