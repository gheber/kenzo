;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA
;;;  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA
;;;  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA  DELTA

(IN-PACKAGE #:cat-7)

(PROVIDE "delta")


(DEFGENERIC PRINT-KEYCONS (car cdr stream))


(DEFMETHOD PRINT-KEYCONS ((car (eql :delt)) cdr stream)
  (format stream "~A" (hyphenize-list (dlop-int-ext cdr)))
  (cons car cdr))


(DEFUN SOFT-DELTA-CMPR (gmsm1 gmsm2)
  (f-cmpr (cdr gmsm1) (cdr gmsm2)))


(DEFUN DELTA-FACE (indx dmns gmsm)
  (declare
   (fixnum indx gmsm)
   (ignore dmns))
  (the absm
       (do ((pmark 1 (ash pmark 1))
            (bmark indx)
            (gmsm2 gmsm (ash gmsm2 -1)))
           (nil)
         (declare (fixnum pmark bmark gmsm2))
         (when (oddp gmsm2)
           (when (minusp (decf bmark))
             (return-from delta-face (absm 0 (logxor gmsm pmark))))))))


(DEFUN SOFT-DELTA-FACE (indx dmns gmsm)
  (declare
   (fixnum indx dmns)
   (type soft-dlop gmsm))
  (absm 0 (d (gmsm (delta-face indx dmns (cdr gmsm))))))


(DEFUN DELTA-BNDR (dmns gmsm)
  (declare (fixnum dmns gmsm))
  (the cmbn
       (if (zerop dmns)
           +zero-negative-cmbn+
           (make-cmbn
            :degr (1- dmns)
            :list (do ((rslt +empty-list+)
                       (gmsm2 gmsm (ash gmsm2 -1))
                       (pmark 1 (ash pmark +1))
                       (sign 1))
                      ((zerop gmsm2) rslt)
                    (declare
                     (list rslt)
                     (fixnum gmsm2 pmark sign))
                    (when (oddp gmsm2)
                      (push (term sign (logxor gmsm pmark)) rslt)
                      (setf sign (- sign))))))))


(DEFUN SOFT-DELTA-BNDR (dmns gmsm)
  (declare
   (fixnum dmns)
   (type soft-dlop gmsm))
  (make-cmbn
   :degr (1- dmns)
   :list (mapcar #'(lambda (term)
                     (with-term (cffc gmsm) term
                                (term cffc (d gmsm))))
                 (cmbn-list (delta-bndr dmns (cdr gmsm))))))


(DEFUN DELTA-DGNL (dmns gmsm)
  (declare (fixnum dmns gmsm))
  (the cmbn
       (make-cmbn
        :degr dmns
        :list (do ((rslt +empty-list+)
                   (ldegr dmns)
                   (indx (1- (integer-length gmsm)) (1- indx)))
                  ((minusp indx))
                (declare
                 (list rslt)
                 (fixnum indx))
                (when (logbitp indx gmsm)
                  (push (term 1 (tnpr
                                 ldegr (logand gmsm (mask (1+ indx)))
                                 (- dmns ldegr) (logandc2 gmsm (mask indx))))
                        rslt)
                  (when (minusp (decf ldegr))
                    (return rslt)))))))


(DEFUN SOFT-DELTA-DGNL (dmns gmsm)
  (declare
   (fixnum dmns)
   (type soft-dlop gmsm))
  (make-cmbn :degr dmns
             :list (mapcar
                    #'(lambda (term)
                        (with-term (cffc tnpr) term
                                   (with-tnpr (degr1 gmsm1 degr2 gmsm2) tnpr
                                              (term cffc
                                                    (tnpr degr1 (d gmsm1)
                                                          degr2 (d gmsm2))))))
                    (cmbn-list (delta-dgnl dmns (cdr gmsm))))))


(DEFUN DELTA-INFINITY ()
  (the simplicial-set
       (build-smst
        :cmpr #'f-cmpr
        :basis :locally-effective
        :bspn 1
        :face #'delta-face
        :intr-dgnl #'delta-dgnl
        :dgnl-strt :gnrt
        :intr-bndr #'delta-bndr
        :bndr-strt :gnrt
        :orgn '(delta-infinity))))


(DEFUN SOFT-DELTA-INFINITY ()
  (the simplicial-set
       (build-smst
        :cmpr #'soft-delta-cmpr
        :basis :locally-effective
        :bspn (d 1)
        :face #'soft-delta-face
        :intr-dgnl #'soft-delta-dgnl :dgnl-strt :gnrt
        :intr-bndr #'soft-delta-bndr :bndr-strt :gnrt
        :orgn '(soft-delta-infinity))))


(DEFUN DELTA-N-BASIS (n)
  (declare (fixnum n))
  (flet ((rslt (dmns)
           (declare (fixnum dmns))
           (when (> dmns n)
             (return-from rslt +empty-list+))
           (setf dmns (1+ dmns))
           (do ((rslt +empty-list+)
                (count (binomial-n-p (1+ n) dmns))
                (gmsm (mask (1+ n)) (1- gmsm)))
               ((zerop count) rslt)
             (when (= (logcount gmsm) dmns)
               (push gmsm rslt)
               (decf count)))))
    (the basis #'rslt)))


(DEFUN SOFT-DELTA-N-BASIS (n)
  (declare (fixnum n))
  (flet ((rslt (dmns)
           (when (> dmns n)
             (return-from rslt +empty-list+))
           (setf dmns (1+ dmns))
           (do ((rslt +empty-list+)
                (count (binomial-n-p (1+ n) dmns))
                (gmsm (mask (1+ n)) (1- gmsm)))
               ((zerop count) rslt)
             (when (= (logcount gmsm) dmns)
               (push (d gmsm) rslt)
               (decf count)))))
    (the basis #'rslt)))


(DEFUN DELTA (dmns)
  (declare (fixnum dmns))
  (the simplicial-set
       (build-smst
        :cmpr #'f-cmpr
        :basis (delta-n-basis dmns)
        :bspn 1
        :face #'delta-face
        :intr-dgnl #'delta-dgnl
        :dgnl-strt :gnrt
        :intr-bndr #'delta-bndr
        :bndr-strt :gnrt
        :orgn `(delta ,dmns))))


(DEFUN SOFT-DELTA (dmns)
  (declare (fixnum dmns))
  (the simplicial-set
       (build-smst
        :cmpr #'soft-delta-cmpr
        :basis (soft-delta-n-basis dmns)
        :bspn (d 1)
        :face #'soft-delta-face
        :intr-dgnl #'soft-delta-dgnl :dgnl-strt :gnrt
        :intr-bndr #'soft-delta-bndr :bndr-strt :gnrt
        :orgn `(soft-delta ,dmns))))


(DEFUN DELTAB-CMPR (gmsm1 gmsm2)
  (declare (fixnum gmsm1 gmsm2))
  (if (= 1 (logcount gmsm1))
      :equal
      (f-cmpr gmsm1 gmsm2)))


(DEFUN DELTAB-BNDR (dmns gmsm)
  (declare (fixnum dmns gmsm))
  (the cmbn
       (if (< dmns 2)
           (zero-cmbn (1- dmns))
           (make-cmbn
            :degr (1- dmns)
            :list (do ((rslt +empty-list+)
                       (gmsm2 gmsm (ash gmsm2 -1))
                       (pmark 1 (ash pmark +1))
                       (sign 1))
                      ((zerop gmsm2) rslt)
                    (declare
                     (list rslt)
                     (fixnum gmsm2 pmark sign))
                    (when (oddp gmsm2)
                      (push (term sign (logxor gmsm pmark)) rslt)
                      (setf sign (- sign))))))))


(DEFUN DELTAB2-FACE (indx dmns gmsm)
  (declare
   (fixnum indx dmns gmsm))
  (the absm
       (progn
         (when (= 2 dmns)
           (return-from deltab2-face (absm 1 1)))
         (do ((pmark 1 (ash pmark 1))
              (bmark indx)
              (gmsm2 gmsm (ash gmsm2 -1)))
             (nil)
           (declare (fixnum pmark bmark gmsm2))
           (when (oddp gmsm2)
             (when (minusp (decf bmark))
               (return-from deltab2-face (absm 0 (logxor gmsm pmark)))))))))


(DEFUN DELTAB2-DGNL (dmns gmsm)
  (declare (fixnum dmns gmsm))
  (the cmbn
       (make-cmbn
        :degr dmns
        :list (if (zerop dmns)
                  (list (term 1 (tnpr 0 gmsm 0 gmsm)))
                  (do ((rslt +empty-list+)
                       (ldegr dmns)
                       (indx (1- (integer-length gmsm)) (1- indx)))
                      ((minusp indx))
                    (declare (list rslt) (fixnum indx))
                    (when (logbitp indx gmsm)
                      (push (term 1 (tnpr
                                     ldegr (logand gmsm (mask (1+ indx)))
                                     (- dmns ldegr)
                                     (logandc2 gmsm (mask indx))))
                            rslt)
                      (when (minusp (decf ldegr))
                        (setf rslt (delete 1 rslt
                                           :key #'(lambda (x)
                                                    (degr1 (cdr x)))))
                        (setf (gnrt1 (gnrt (first rslt)))
                              (setf (gnrt2 (gnrt (car (last rslt))))
                                    1))
                        (return (delete 1 rslt
                                        :key #'(lambda (x)
                                                 (gnrt1 (cdr x))))))))))))


(DEFUN DELTAB2-BNDR (dmns gmsm)
  (declare (fixnum dmns gmsm))
  (the cmbn
       (if (< dmns 3)
           (zero-cmbn (1- dmns))
           (make-cmbn
            :degr (1- dmns)
            :list (do ((rslt +empty-list+)
                       (gmsm2 gmsm (ash gmsm2 -1))
                       (pmark 1 (ash pmark +1))
                       (sign 1))
                      ((zerop gmsm2) rslt)
                    (declare
                     (list rslt)
                     (fixnum gmsm2 pmark sign))
                    (when (oddp gmsm2)
                      (push (term sign (logxor gmsm pmark)) rslt)
                      (setf sign (- sign))))))))


(DEFUN DELTAB ()
  (the simplicial-set
       (build-smst
        :cmpr #'deltab-cmpr
        :basis :locally-effective
        :bspn 1
        :face #'delta-face
        :intr-dgnl #'delta-dgnl
        :dgnl-strt :gnrt
        :orgn '(deltab))))


(DEFUN DELTAB2 ()
  (the simplicial-set
       (build-smst
        :cmpr #'f-cmpr
        :basis :locally-effective
        :bspn 1
        :face #'deltab2-face
        :intr-dgnl #'deltab2-dgnl
        :dgnl-strt :gnrt
        :orgn '(deltab2))))
