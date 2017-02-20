;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS
;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS
;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS

(IN-PACKAGE "CAT")

(PROVIDE "simplicial-mrphs")

(DEFMETHOD ?3 ((smmr simplicial-mrph) dmns absm-or-gmsm)
  (declare
     (fixnum dmns)
     (type (or absm gmsm) absm-or-gmsm))
  (typecase absm-or-gmsm
     (absm
      (with-absm (dgop gmsm) absm-or-gmsm
         (let ((gmsm-smmr (funcall (sintr smmr)
			     (- dmns (logcount dgop)) gmsm)))
	   (declare (type absm gmsm-smmr))
	   (ndgnr dgop gmsm-smmr))))
     (otherwise
      (funcall (sintr smmr) dmns absm-or-gmsm))))  

(DEFMETHOD PRINT-OBJECT ((smmr simplicial-mrph) stream)
  (the simplicial-mrph
     (progn
       (if (= -1 (degr smmr))
	   (format stream "[K~D Fibration K~D -> K~D]"
		   (idnm smmr) (idnm (sorc smmr)) (idnm (trgt smmr)))
	 (format stream "[K~D Simplicial-Morphism K~D -> K~D]"
		 (idnm smmr) (idnm (sorc smmr)) (idnm (trgt smmr))))
       smmr)))

(DEFUN SINTR-INTR (sintr)
   (declare (type sintr sintr))
   (flet ((rslt (dmns gmsm)
	     (declare
	        (fixnum dmns)
		(type gmsm gmsm))
	     (when (minusp dmns)
		(return-from rslt (zero-cmbn dmns)))
	     (let ((rslt (funcall sintr dmns gmsm)))
	        (declare (type absm rslt))
		(if (degenerate-p rslt)
		   (zero-cmbn dmns)
		   (term-cmbn dmns 1 (gmsm rslt))))))
       (the intr #'rslt)))

(DEFUN SIMPLICIAL-MRPH (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already simplicial-mrph dfnt)
  (the simplicial-mrph
    (apply #'make-instance 'simplicial-mrph rest)))

(DEFMETHOD SHARED-INITIALIZE ((smmr simplicial-mrph) slot-names &key sintr)
  (declare
   (type (or (eql t) list) slot-names)
   (type sintr sintr))
  (call-next-method)
  (when-slot sintr
             (when sintr (setf (slot-value smmr 'sintr) sintr)))
  smmr)

(DEFMETHOD SLOT-UNBOUND (class (smmr simplicial-mrph) slot-name)
  (declare
   (ignore class)
   (type symbol slot-name))
  (let ((name (symbol-name slot-name)))
    (declare (type string name))
    (unless (member name '("INTR" "STRT") :test #'string=)
      (call-next-method))
    (the (or intr strt)
      (with-slots ((ssintr intr) strt sintr) smmr
        (declare
         (type intr ssintr)
         (type strt strt)
         (type sintr sintr))
        (setf
         ssintr (sintr-intr sintr)
         strt :gnrt)
        (if (string= name "INTR") ssintr strt)))))
        
#|
  (setf d (delta 3))
  (setf m (simplicial-mrph
            :sorc d :trgt d :degr 0
            :sintr #'(lambda (dmns gmsm)
                        (absm 0 gmsm))
            :dfnt '(identity delta-3)))
  (setf m2 (simplicial-mrph
            :sorc d :trgt d :degr 0
            :sintr #'(lambda (dmns gmsm)
                       (absm (mask dmns) 1))
            :dfnt '(null delta-3)))
  (? m2 2 7)
;;  (s? m2 2 7)
|#

(DEFMACRO WITH-IABSM ((dgop gmsm) iabsm . body)
  `(let ((,dgop (car ,iabsm))
	 (,gmsm (cdr ,iabsm)))
     (declare
        (fixnum ,dgop)
	(type gmsm ,gmsm))
     ,@body))

#|
  (macroexpand-1 '(with-iabsm (dgop gmsm) iabsm
                    (statement-1)
                    (statement-2)))
|#
                 
(DEFMACRO A-SINTR3 (sintr dmns absm)
  ;; BE CAREFUL: works only if  degr (sintr) = 0.
  ;; if degr = -1 (fibration), use tw-a-sintr3
  `(ia-sintr3 ,sintr ,dmns (cdr ,absm)))

(DEFUN IA-SINTR3 (sintr dmns iabsm)
  (declare
     (type sintr sintr)
     (fixnum dmns)
     (type iabsm iabsm))
  (the absm
     (with-iabsm (dgop gmsm) iabsm
	(ndgnr dgop (funcall sintr (- dmns (logcount dgop)) gmsm)))))

(DEFMACRO TW-A-SINTR3 (sintr dmns absm bsgn)
  `(tw-ia-sintr3 ,sintr ,dmns (cdr ,absm) ,bsgn))

(DEFUN TW-IA-SINTR3 (sintr dmns iabsm bsgn)
  (declare
     (type sintr sintr)
     (fixnum dmns)
     (type iabsm iabsm)
     (type gmsm bsgn))
  (the absm
     (with-iabsm (dgop gmsm) iabsm
	(let ((dmns-1 (1- dmns)))
	  (declare (fixnum dmns-1))
	  (if (logbitp dmns-1 dgop)
	     (absm (mask dmns-1) bsgn)
	     (ndgnr dgop (funcall sintr
			    (- dmns (logcount dgop))
			    gmsm)))))))