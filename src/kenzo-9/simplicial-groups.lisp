;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS
;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS
;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS

(IN-PACKAGE #:cat-9)

(DEFVAR *SMGR-LIST*)
(SETF *SMGR-LIST* +empty-list+)
(PUSHNEW '*SMGR-LIST* *list-list*)

(DEFUN SMGR (idnm)
  (declare (type fixnum idnm))
  (the (or null simplicial-group)
     (find idnm *smgr-list* :key #'idnm)))

(DEFMETHOD PRINT-OBJECT ((smgr simplicial-group) stream)
  (declare (type stream stream))
  (the simplicial-group
     (progn
       (format stream "[K~D Simplicial-Group]" (idnm smgr))
       smgr)))

(DEFMETHOD PRINT-OBJECT ((smgr ab-simplicial-group) stream)
  (declare (type stream stream))
  (the simplicial-group
     (progn
       (format stream "[K~D Abelian-Simplicial-Group]" (idnm smgr))
       smgr)))

(DEFUN BUILD-SMGR
  (&key cmpr basis bspn
	face face* intr-bndr bndr-strt
	intr-dgnl dgnl-strt
	sintr-grml sintr-grin
	orgn)
   (declare
      (type cmprf cmpr)
      (type basis basis)
      (type gmsm bspn)
      (type face face)
      (type (or null face*) face*)
      (type (or intr-mrph null) intr-bndr intr-dgnl)
      (type (or strt null) bndr-strt dgnl-strt)
      (type sintr sintr-grml sintr-grin)
      (list orgn))
   (let ((already (find orgn *smgr-list* :test #'equal :key #'orgn)))
      (declare (type (or simplicial-group null) already))
      (when already
         (return-from build-smgr already)))
   (let ((rslt (build-smst
		:cmpr cmpr :basis basis :bspn bspn
		:face face :face* face* :intr-bndr intr-bndr
		:bndr-strt bndr-strt :intr-dgnl intr-dgnl
		:dgnl-strt dgnl-strt :orgn orgn)))
     (declare (type simplicial-set rslt))
     (change-class rslt 'simplicial-group)
     (setf (slot-value rslt 'grml)
	   (build-smmr
	      :sorc (crts-prdc rslt rslt) :trgt rslt :degr 0
	      :sintr sintr-grml :orgn `(group-multiplication ,rslt))
	   (slot-value rslt 'grin)
	   (build-smmr
	      :sorc rslt :trgt rslt :degr 0
	      :sintr sintr-grin :orgn `(group-inversion ,rslt)))
     (push rslt *smgr-list*)
     rslt))

(DEFUN A-GRML4 (sintr-grml dmns absm1 absm2)
  (declare
     (type fixnum dmns)
     (type absm absm1 absm2))
  (the absm
     (let ((acrpr (2absm-acrpr absm1 absm2)))
       (declare (type absm acrpr))
       (with-absm (dgop crpr) acrpr
          (ndgnr dgop
		 (funcall sintr-grml
			  (- dmns (logcount dgop))
			  crpr))))))

(DEFUN A-GRIN4 (sintr-grin dmns absm)
  (declare
     (type sintr sintr-grin)
     (type fixnum dmns)
     (type absm absm))
  (the absm
     (with-absm (dgop gmsm) absm
	(absm dgop
	      (gmsm
	       (funcall sintr-grin (- dmns (logcount dgop)) gmsm))))))

(DEFMETHOD SLOT-UNBOUND (class (smgr simplicial-group)
			       (name (eql 'kfll)))
  (declare (ignore class))
  (the kfll
     (progn
       (push smgr *kan-list*)
       (setf (slot-value smgr 'kfll) (smgr-kfll smgr)))))

(DEFUN SMGR-KFLL-INTR (face sintr-grml sintr-grin)
  (flet ((rslt (indx dmns hat
		     &aux (dmns-1 (1- dmns)))
	    (declare
	       (type fixnum indx dmns dmns-1)
	       (list hat))
	    (the absm
	      (let ((rslt (absm 0 nil)))
		(declare (type absm rslt))
		(when (plusp indx)
		   (setf rslt (1dgnr 0 (pop hat)))
		   (do ((i 1 (1+ i)))
		       ((= i indx))
		      (declare (type fixnum i))
		      (setf rslt
			    (a-grml4
			     sintr-grml dmns
			     rslt
			     (1dgnr i
				    (a-grml4
				     sintr-grml dmns-1
				     (a-grin4
				      sintr-grin dmns-1
				      (a-face4 face
					i dmns rslt))
				     (pop hat)))))))
		(when (< indx dmns)
		  (setf hat (reverse hat))
                  (when (zerop indx) 
		  (setf rslt (1dgnr dmns-1 (pop hat))))
		  (do ((i (if (zerop indx)
			      dmns-1
			      dmns)
			  (1- i)))
		      ((endp hat) rslt)
		     (declare
		        (type absm rslt)
			(type fixnum i))
		     (setf rslt
			   (a-grml4
			    sintr-grml dmns
			    rslt
			    (1dgnr (1- i)
				   (a-grml4
				    sintr-grml dmns-1
				    (a-grin4
				     sintr-grin dmns-1
				     (a-face4
				      face
				      i dmns rslt))
				    (pop hat)))))))
		rslt))))
     (the kfll #'rslt)))

(DEFUN SMGR-KFLL (smgr)
  (declare (type simplicial-group smgr))
  (with-slots (face grml grin) smgr
     (declare
        (type face face)
	(type simplicial-mrph grml grin))
     (smgr-kfll-intr face (sintr grml) (sintr grin))))

#|
()
(cat-init)
(setf k (k-z-1))
(setf rslt '(1 10 100))
(setf hat (mapcar #'(lambda (i) (face k i 3 rslt)) (<a-b> 0 3)))
(dotimes (i 4)
  (check-kan k i 3 (remove (nth i hat) hat :test #'equal)))
|#

(DEFMETHOD SLOT-UNBOUND (class (smgr simplicial-group)
			       (name (eql 'aprd)))
  (declare (ignore class))
  (the morphism
     (progn
       (push smgr *hopf-list*)
       (setf (slot-value smgr 'aprd) (smgr-aprd smgr)))))

(DEFUN SMGR-APRD (smgr)
  (declare (type simplicial-group smgr))
  (the morphism
    (let ((eml (eml smgr smgr))
	  (grml (grml smgr)))
      (declare
        (type morphism eml)
	(type simplicial-mrph grml))
      (cmps grml eml))))

