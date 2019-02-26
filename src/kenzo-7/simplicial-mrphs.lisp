;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS
;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS
;;;  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS  SIMPLICIAL-MRPHS

(IN-PACKAGE #:cat)

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


#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((smmr simplicial-mrph) stream)
  (the simplicial-mrph
       (progn
         (if (= -1 (degr smmr))
             (format stream "[K~D Fibration K~D -> K~D]"
                     (idnm smmr) (idnm (sorc smmr)) (idnm (trgt smmr)))
             (format stream "[K~D Simplicial-Morphism K~D -> K~D]"
                     (idnm smmr) (idnm (sorc smmr)) (idnm (trgt smmr))))
         smmr)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))


(DEFUN SMMR (idnm)
  (declare (fixnum idnm))
  (the (or null simplicial-mrph)
       (find idnm *smmr-list* :key #'idnm)))


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
    (the intr-mrph #'rslt)))


(DEFUN BUILD-SMMR (&key sorc trgt degr sintr intr strt orgn)
  (declare
   (type simplicial-set sorc trgt)
   (fixnum degr)
   (type sintr sintr)
   (type (or intr-mrph null) intr)
   (type (or strt null) strt)
   (list orgn))
  (the simplicial-mrph
       (progn
         (let ((already (find orgn *smmr-list* :test #'equal :key #'orgn)))
           (declare (type (or simplicial-mrph null) already))
           (when already
             (return-from build-smmr already)))
         (if (zerop degr)
             (if intr
                 (unless strt
                   (error "In BUILD-SMMR, an intr is given but not its strt"))
                 (setf strt :gnrt
                       intr (sintr-intr sintr)))
             (setf intr nil strt :gnrt))
         (let ((rslt (build-mrph
                      :sorc sorc :trgt trgt :degr degr
                      :intr intr :strt strt
                      :orgn orgn)))
           (declare (type morphism rslt))
           (change-class rslt 'simplicial-mrph)
           (setf (slot-value rslt 'sintr) sintr)
           (push rslt *smmr-list*)
           rslt))))


#|  ????
(DEFUN IA-SINTR3 (sintr dmns iabsm)
  (declare
   (type sintr sintr)
   (fixnum dmns)
   (type iabsm iabsm))
  (the absm
       (with-iabsm (dgop gmsm) iabsm
                   (ndgnr dgop (funcall sintr (- dmns (logcount dgop)) gmsm)))))

(DEFUN A-SINTR3 (sintr dmns absm)
  (declare
   (type sintr sintr)
   (fixnum dmns)
   (type absm absm))
  (the absm
       (with-absm (dgop gmsm) absm
                  (ndgnr dgop (funcall sintr (- dmns (logcount dgop)) gmsm)))))
|# ;;; ????


(DEFUN TW-A-SINTR3 (sintr dmns absm bspn)
  (declare
   (type sintr sintr)
   (fixnum dmns)
   (type absm absm)
   (type gmsm bspn))
  (the absm
       (with-absm (dgop gmsm) absm
                  (let ((dmns-1 (1- dmns)))
                    (declare (fixnum dmns-1))
                    (if (logbitp dmns-1 dgop)
                        (absm (mask dmns-1) bspn)
                        (ndgnr dgop (funcall sintr
                                             (- dmns (logcount dgop))
                                             gmsm)))))))

#|
(DEFUN TW-IA-SINTR3 (sintr dmns iabsm bspn)
  (declare
   (type sintr sintr)
   (fixnum dmns)
   (type iabsm iabsm)
   (type gmsm bspn))
  (the absm
       (with-iabsm (dgop gmsm) iabsm
                   (let ((dmns-1 (1- dmns)))
                     (declare (fixnum dmns-1))
                     (if (logbitp dmns-1 dgop)
                         (absm (mask dmns-1) bspn)
                         (ndgnr dgop (funcall sintr
                                              (- dmns (logcount dgop))
                                              gmsm)))))))
|#
