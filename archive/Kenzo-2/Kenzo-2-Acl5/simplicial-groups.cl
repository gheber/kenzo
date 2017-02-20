;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS
;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS
;;;  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS  SIMPLICIAL-GROUPS

(IN-PACKAGE "CAT")

(PROVIDE "simplicial-groups")

(DEFMETHOD PRINT-OBJECT ((smgr simplicial-group) stream)
  (the simplicial-group
     (progn
       (format stream "[K~D Simplicial-Group]" (idnm smgr))
       smgr)))

(DEFMETHOD PRINT-OBJECT ((smgr ab-simplicial-group) stream)
  (the simplicial-group
     (progn
       (format stream "[K~D Abelian-Simplicial-Group]" (idnm smgr))
       smgr)))

(DEFUN SIMPLICIAL-GROUP (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already simplicial-group dfnt)
  (the simplicial-group
    (apply #'make-instance 'simplicial-group rest)))

(DEFMETHOD SHARED-INITIALIZE ((smgr simplicial-group) slot-names
                              &key grml-sintr grin-sintr)
  (declare
   (type (or (eql t) list) slot-names)
   (type sintr grml grin))
  (call-next-method)
  (when-slot grml
             (when grml-sintr
               (setf (slot-value smgr 'grml)
                 (simplicial-mrph
                  :sorc (crts-prdc smgr smgr) :trgt smgr :degr 0
                  :sintr grml-sintr :dfnt `(group-multiplication ,smgr))))
             (when grin-sintr
               (setf (slot-value smgr 'grin)
                 (simplicial-mrph
                  :sorc smgr :trgt smgr :degr 0
                  :sintr grin-sintr :dfnt `(group-inversion ,smgr)))))
  smgr)

(DEFUN AB-SIMPLICIAL-GROUP (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already ab-simplicial-group dfnt)
  (the ab-simplicial-group
    (apply #'make-instance 'ab-simplicial-group rest)))
  
(DEFMETHOD SHARED-INITIALIZE ((asmgr ab-simplicial-group) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (call-next-method))

(DEFMETHOD SLOT-UNBOUND (class (smgr simplicial-group)
			       (name (eql 'kfll)))
  (declare (ignore class))
  (the kfll
     (progn
       (setf (slot-value smgr 'kfll) (smgr-kfll smgr)))))

(DEFMETHOD SLOT-UNBOUND (class (smgr simplicial-group)
			       (name (eql 'aprd)))
  (declare (ignore class))
  (the morphism
     (progn
       (setf (slot-value smgr 'aprd) (smgr-aprd smgr)))))

(DEFUN A-GRML4 (grml-sintr dmns absm1 absm2)
  (declare
     (fixnum dmns)
     (type absm absm1 absm2))
  (the absm
     (let ((acrpr (2absm-acrpr absm1 absm2)))
       (declare (type absm acrpr))
       (with-absm (dgop crpr) acrpr
          (ndgnr dgop
		 (funcall grml-sintr
			  (- dmns (logcount dgop))
			  crpr))))))

(DEFUN A-GRIN4 (grin-sintr dmns absm)
  (declare
     (type sintr grin-sintr)
     (fixnum dmns)
     (type absm absm))
  (the absm
     (with-absm (dgop gmsm) absm
	(absm dgop
	      (gmsm
	       (funcall grin-sintr (- dmns (logcount dgop)) gmsm))))))

(DEFUN SMGR-KFLL-INTR (face grml-sintr grin-sintr)
  (flet ((rslt (indx dmns hat
		     &aux (dmns-1 (1- dmns)))
	    (declare
	       (fixnum indx dmns dmns-1)
	       (list hat))
	    (the absm
	      (let ((rslt (absm 0 nil)))
		(declare (type absm rslt))
		(when (plusp indx)
		   (setf rslt (1dgnr 0 (pop hat)))
		   (do ((i 1 (1+ i)))
		       ((= i indx))
		      (declare (fixnum i))
		      (setf rslt
			    (a-grml4
			     grml-sintr dmns
			     rslt
			     (1dgnr i
				    (a-grml4
				     grml-sintr dmns-1
				     (a-grin4
				      grin-sintr dmns-1
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
			(fixnum i))
		     (setf rslt
			   (a-grml4
			    grml-sintr dmns
			    rslt
			    (1dgnr (1- i)
				   (a-grml4
				    grml-sintr dmns-1
				    (a-grin4
				     grin-sintr dmns-1
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
  (cat-init)
  (setf k (k-z-1))
  (setf rslt '(1 10 100))
  (setf hat (mapcar #'(lambda (i) (face k i 3 rslt)) (<a-b> 0 3)))
  (dotimes (i 4)
    (check-kan k i 3 (remove (nth i hat) hat :test #'equal)))
|#

(DEFUN SMGR-APRD (smgr)
  (declare (type simplicial-group smgr))
  (the morphism
    (let ((eml (eml smgr smgr))
	  (grml (grml smgr)))
      (declare
        (type morphism eml)
	(type simplicial-mrph grml))
      (cmps grml eml))))

       