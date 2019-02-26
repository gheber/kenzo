;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS
;;;  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS  COALGEBRAS

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "coalgebras")

(DEFVAR *CLGB-LIST*)
(SETF *CLGB-LIST* +empty-list+)
(PUSHNEW '*CLGB-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((clgb coalgebra) stream)
 (the coalgebra
   (progn
      (format stream "[K~D Coalgebra]" (idnm clgb))
      clgb)))

(DEFUN CLGB (idnm)
   (declare (type fixnum idnm))
   (the (or coalgebra null)
      (find idnm *clgb-list* :key #'idnm)))

(DEFUN BUILD-CLGB
    (&key cmpr basis bsgn intr-dffr dffr-strt intr-cprd cprd-strt orgn)
   (declare
      (type cmprf cmpr)
      (type intr-mrph intr-dffr intr-cprd)
      (type basis basis)
      (type gnrt bsgn)
      (type strt dffr-strt cprd-strt)
      (list orgn))
   (the coalgebra
      (progn
         (let ((already (find orgn *clgb-list* :key #'orgn :test #'equal)))
            (declare (type (or null coalgebra) already))
            (when already
               (return-from build-clgb already)))
         (let ((rslt (build-chcm :cmpr cmpr :basis basis :bsgn bsgn
                        :intr-dffr intr-dffr :strt dffr-strt
                        :orgn orgn)))
            (declare (type chain-complex rslt))
            (change-class rslt 'coalgebra)
            (setf (slot-value rslt 'cprd)
                  (build-mrph :sorc rslt :degr 0 ;;; trgt non-initialized
                        :intr intr-cprd :strt cprd-strt
                        :orgn `(coalgebra-coproduct ,rslt)))
               (push rslt *clgb-list*)
               rslt))))

(DEFUN CHANGE-CHCM-TO-CLGB (chcm &key intr-cprd cprd-strt orgn)
  (declare
   (type chain-complex chcm)
   (type intr-mrph intr-cprd)
   (type strt cprd-strt)
   (list orgn))
  (the coalgebra
    (progn
      (change-class chcm 'coalgebra)
      (setf orgn (list (orgn chcm) 'then orgn))
      (let ((already (find orgn *clgb-list* :key #'orgn :test #'equal)))
        (declare (type (or null coalgebra) already))
        (when already
          (return-from change-chcm-to-clgb already)))
      (setf (slot-value chcm 'cprd) (build-mrph
                                     :sorc chcm ;;; trgt non-initialized
                                     :degr 0
                                     :intr intr-cprd :strt cprd-strt
                                     :orgn orgn))
         (push chcm *clgb-list*)
         chcm)))

(DEFMETHOD SLOT-UNBOUND (class (mrph morphism) (slot (eql 'trgt)))
  (declare (ignore class))
  (let ((sorc (sorc mrph)))
    (if (and (typep sorc 'coalgebra)
             (eq mrph (cprd sorc)))
        (setf (slot-value mrph 'trgt)
          (tnsr-prdc sorc sorc))
      (call-next-method))))

(DEFMETHOD TNSR-PRDC ((A coalgebra) (B coalgebra))
  (the coalgebra
    (progn
      (let ((orgn `(tnsr-prdc ,A ,B)))
        (declare (type list orgn))
        (let ((already (find orgn *chcm-list* :key #'orgn :test #'equal)))
          (declare (type (or null coalgebra) already))
          (when already
            (return-from tnsr-prdc already)))
        (let ((AB (call-next-method)))
          (CHANGE-CHCM-TO-CLGB AB
                               :intr-cprd 
                               (coalgebra-tnsr-prdc-impl A B)
                               :cprd-strt :gnrt
                               :orgn orgn)
          (push AB *clgb-list*)
          AB)))))


#|
(cat-init)
(setf A (delta 3) B (delta 4))
(tnsr-prdc A B)
(setf AxB (crts-prdc A B))
(setf AtB (tnsr-prdc A B))
(setf AxBtAxB (tnsr-prdc AxB AxB))
(setf AtBtAtB (tnsr-prdc AtB AtB))

(setf f (f (ez A B)))
(setf ff (tnsr-prdc f f))
(setf cdelta (cprd AxB))
(setf tdelta (cprd AtB))
(setf zerof (sbtr (cmps tdelta f) (cmps ff cdelta)))
(setf s1xs1 (crpr 0 3 0 24))
(? f 1 s1xs1)
(? tdelta *)
(? cdelta 1 s1xs1)
(? ff *)
(? zerof 1 s1xs1)

(setf g (g (ez A B)))
(setf gg (tnsr-prdc g g))
(setf zerog (sbtr (cmps cdelta g) (cmps gg tdelta)))
(setf s1ts1 (tnpr 1 3 1 24))
(? zerog 2 s1ts1)
(setf s3ts4 (tnpr 3 15 4 31))
(? zerog 7 s3ts4)

(setf s0xs1 (crpr 0 3 1 16))
(? zerof 1 s0xs1)

(cat-init)
(setf s2 (sphere 2))
(setf ch2 (chml-clss s2 2))
(setf f2 (z-whitehead s2 ch2))
(setf s3 (fibration-total f2))
(setf ezt (twisted-ez s3))
(setf ff (tnsr-prdc (f ezt) (f ezt)))
(setf gg (tnsr-prdc (g ezt) (g ezt)))
(setf s2ts2 (tnpr 2 's2 2 '(3 5)))
(g ezt 4 s2ts2)
(cprd s3 *)
(2cmbn-sbtr (cmpr (tnsr-prdc s3 s3)) (? gg (? ff *)) *)



|#



