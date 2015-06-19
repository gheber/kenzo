;;;  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS
;;;  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS
;;;  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS  SIMPLICIAL-HMTPS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "simplicial-hmtps")

(DEFTYPE HINTR () 'function)
;; (function (indx dmns gmsm) absm)
 
(DEFCLASS SIMPLICIAL-HMTP (morphism)
    ((hintr :type hintr :initarg :hintr :reader hintr)))


(DEFVAR *SMHMT-LIST*)
(SETF *SMHMT-LIST* +empty-list+)
(PUSHNEW '*SMHMT-LIST* *LIST-LIST*)

(DEFMETHOD PRINT-OBJECT ((smhmt simplicial-hmtp) stream)
  (the simplicial-hmtp
     (progn
      (format stream "[K~D Simplicial-Homotopy K~D -> K~D]"
        (idnm smhmt) (idnm (sorc smhmt)) (idnm (trgt smhmt)))
      smhmt)))

(DEFUN SMHMT (idnm)
  (declare (fixnum idnm))
  (the (or null simplicial-hmtp)
     (find idnm *smhmt-list* :key #'idnm)))

(DEFUN HINTR-INTR (cmpr hintr)
   (declare (type hintr hintr)
     (type cmprf cmpr))
   (flet ((rslt (dmns gmsm)
            (declare
             (fixnum dmns)
             (type gmsm gmsm))
            (when (minusp dmns) 
               (return-from rslt (zero-cmbn (1+ dmns))))
            (let ((cmbn (zero-cmbn (1+ dmns))))
               (declare (type cmbn cmbn))
               (do ((indx 0 (1+ indx)))
                   ((> indx dmns) cmbn)
                  (declare (fixnum indx))
                  (let ((hmtp-i (funcall hintr indx dmns gmsm)))
                     (declare (type absm hmtp-i))
                     (if (non-degenerate-p hmtp-i)
                        (setf cmbn (2cmbn-add cmpr cmbn (term-cmbn (1+ dmns) (-1-expt-n indx) (gmsm hmtp-i))))))))))
     (the intr-mrph #'rslt)))

(DEFUN BUILD-SMHMT (&key sorc trgt degr hintr intr strt orgn)
   (declare
      (type simplicial-set sorc trgt)
      (fixnum degr)
      (type hintr hint)
      (type (or intr-mrph null) intr)
      (type (or strt null) strt)
      (list orgn))
   (the simplicial-hmtp
      (progn
	 (let ((already (find orgn *smhmt-list* :test #'equal :key #'orgn)))
	    (declare (type (or simplicial-hmtp null) already))
	    (when already
	       (return-from build-smhmt already)))
       (unless (= 1 degr)
          (error "In BUILD-SMHMT, the degree ~D is different to +1."
            degr))
       (if intr
          (unless strt
             (error "In BUILD-SMMR, an intr is given but not its strt"))
          (let ((cmpr (cmpr sorc)))
             (declare (type cmprf cmpr))
             (setf strt :gnrt
                   intr (hintr-intr cmpr hintr))))
          (let ((rslt (build-mrph
		        :sorc sorc :trgt trgt :degr degr
			:intr intr :strt strt
			:orgn orgn)))
          (declare (type morphism rslt))
          (change-class rslt 'simplicial-hmtp)
          (setf (slot-value rslt 'hintr) hintr)
          (push rslt *smhmt-list*)
	   rslt))))


#|
(setf d (delta 3))
(setf h (build-smhmt
         :sorc d :trgt d :degr 1
         :hintr #'(lambda (indx dmns gmsm)
                    (absm (dgop-ext-int (list indx)) gmsm))
         :orgn '(simplicial-homotopy delta-3)))
(dotimes (indx 5)
     (print (? h indx 4 (absm 8 15))))
(dotimes (indx 7)
     (print (? h indx 6 (absm 45 11))))
(? h 2 11)
|#



(DEFMETHOD ?4 ((smhmt simplicial-hmtp) indx dmns absm-or-gmsm)
  (declare
     (fixnum indx dmns)
     (type (or absm gmsm) absm-or-gmsm))
   (the absm
     (typecase absm-or-gmsm
       (absm
        (with-absm (dgop gmsm) absm-or-gmsm
          (declare 
            (fixnum dgop)
          (type gmsm gmsm))
          (if (= 0 dgop) 
             (funcall (hintr smhmt) indx dmns gmsm)
             (let* ((dgop1-ext (first (dgop-int-ext dgop)))
                    (dgop2 (remove-bit dgop dgop1-ext))
                    (absm2 (absm dgop2 gmsm)))
                (declare
                 (fixnum dgop1-ext dgop2)
                 (type absm sbsm2))
                (if (>= dgop1-ext indx)
                   (let ((smhmt-i (?4 smhmt indx (1- dmns)  absm2)))
                      (declare (type absm smhmt-i))
                      (ndgnr (dgop-ext-int (list (1+ dgop1-ext))) smhmt-i))
                   (let ((smhmt-i-1 (?4 smhmt (1- indx) (1- dmns)  absm2)))
                      (declare (type absm smhmt-i-1))
                      (ndgnr (dgop-ext-int (list dgop1-ext)) smhmt-i-1)))))))
       (otherwise
        (funcall (hintr smhmt) indx dmns absm-or-gmsm)))))


;; This macro definition changes the definition of the macro ? in the file macro.lsp
(DEFMACRO ? (&rest rest)
   (ecase (length rest)
     (2 `(?2 ,@rest))
     (3 `(?3 ,@rest))
     (4 `(?4 ,@rest))))







             
      
   
   
    

   

