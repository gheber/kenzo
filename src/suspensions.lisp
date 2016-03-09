;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS
;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS
;;;  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS  SUSPENSIONS

(IN-PACKAGE #:cat)

(PROVIDE "suspensions")

(DEFUN SUSPENSION-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (gnrt1 gnrt2)
           (declare (type gnrt gnrt1 gnrt2))
           (if (eq gnrt1 :s-bsgn)
               :equal
               (funcall cmpr gnrt1 gnrt2))))
    (the cmprf #'rslt)))


(DEFUN SUSPENSION-BASIS (basis)
  (declare (type basis basis))
  (when (eq basis :locally-effective)
    (return-from suspension-basis :locally-effective))
  (flet ((rslt (degr)
           (declare (fixnum degr))
           (case degr
             (0 (list :s-bsgn))
             (1 +empty-list+)
             (otherwise (funcall basis (1- degr))))))
    (the basis #'rslt)))


(DEFUN SUSPENSION-INTR-DFFR (dffr)
  (declare (type morphism dffr))
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (with-cmbn
               (degr list) cmbn
               (decf degr)
               (if (<= degr 0)
                   (zero-cmbn degr)
                   (make-cmbn :degr degr
                              :list (cmbn-list
                                     (cmbn-? dffr
                                             (make-cmbn :degr degr
                                                        :list list))))))))
    (the intr-mrph #'rslt)))


(DEFUN SUSPENSION (obj &optional (n 1))
  (declare (type (or chain-complex morphism reduction
                     homotopy-equivalence) obj)
           (fixnum n))
  (if (= n 0)
      obj
      (suspension-1 (suspension obj (1- n)))))


(DEFGENERIC SUSPENSION-1 (x))


(DEFMETHOD SUSPENSION-1 ((chcm chain-complex))
  (the chain-complex
       (with-slots (cmpr basis dffr) chcm
         (build-chcm
          :cmpr (suspension-cmpr cmpr)
          :basis (suspension-basis basis)
          :bsgn :s-bsgn
          :intr-dffr (suspension-intr-dffr dffr)
          :strt :cmbn
          :orgn `(suspension ,chcm)))))


(DEFUN SUSPENSION-INTR-CPRD (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn
           (degr list) cmbn
           (if (zerop degr)
               (make-cmbn :degr 0
                          :list (if list
                                    (list (term (-cffc list)
                                                (tnpr 0 :s-bsgn
                                                      0 :s-bsgn)))
                                    +empty-list+))
               (make-cmbn :degr degr
                          :list (let ((list1 +empty-list+)
                                      (list2 +empty-list+))
                                  (declare (list list1 list2))
                                  (mapc
                                   #'(lambda (term)
                                       (declare (type term term))
                                       (with-term
                                           (cffc gnrt) term
                                           (push (term cffc
                                                       (tnpr 0 :s-bsgn
                                                             degr gnrt))
                                                 list1)
                                           (push (term cffc
                                                       (tnpr degr gnrt
                                                             0 :s-bsgn))
                                                 list2)))
                                   list)
                                  (nreconc list1 (nreverse list2))))))))


(DEFMETHOD SUSPENSION-1 ((clgb coalgebra))
  (the coalgebra
       (let ((irslt (call-next-method)))
         (declare (type (or chain-complex coalgebra) irslt))
         (when (typep irslt 'coalgebra)
           (return-from suspension-1 irslt))
         (change-chcm-to-clgb irslt :intr-cprd #'suspension-intr-cprd
                              :cprd-strt :cmbn)
         (setf (slot-value irslt 'orgn) `(suspension ,clgb))
         (setf (slot-value (slot-value irslt 'cprd) 'orgn)
               `(coproduct ,irslt))
         irslt)))


(DEFUN SUSPENSION-FACE (face)
  (declare (type face face))
  (flet ((rslt (indx dmns gmsm)
           (declare
            (fixnum indx dmns)
            (type gmsm gmsm))
           (assert (> dmns 1))
           (if (= indx dmns)
               (absm (mask (1- dmns)) :s-bsgn)
               (let ((irslt (funcall face indx (1- dmns) gmsm)))
                 (declare (type absm irslt))
                 (if (= (dgop irslt) (mask (- dmns 2)))
                     (absm (mask (1- dmns)) :s-bsgn)
                     irslt)))))
    (the face #'rslt)))


(DEFMETHOD SUSPENSION-1 ((smst simplicial-set))
  (the simplicial-set
       (let ((irslt (call-next-method)))
         (declare (type coalgebra irslt))
         (when (typep irslt 'simplicial-set)
           (return-from suspension-1 irslt))
         (change-class irslt 'simplicial-set)
         (setf (slot-value irslt 'face)
               (suspension-face (face smst)))
         (setf (slot-value (slot-value irslt 'cprd) 'orgn)
               `(diagonal ,irslt))
         irslt)))


(DEFUN SUSPENSION-INTR (mrph)
  (declare (type morphism mrph))
  (let ((mdegr (degr mrph)))
    (declare (fixnum mdegr))
    (flet ((rslt (cmbn)
             (declare (type cmbn cmbn))
             (with-cmbn
                 (cdegr list) cmbn
                 (when (zerop cdegr)
                   (return-from rslt
                     (if (zerop mdegr)
                         cmbn
                         (zero-cmbn (+ mdegr cdegr)))))
                 (make-cmbn :degr (+ mdegr cdegr)
                            :list (cmbn-list
                                   (cmbn-? mrph (make-cmbn :degr (1- cdegr)
                                                           :list list)))))))
      (the intr-mrph #'rslt))))


(DEFMETHOD SUSPENSION-1 ((mrph morphism))
  (the morphism
       (let ((orgn (orgn mrph)))
         (declare (list orgn))
         (when (eq (first orgn) 'zero-mrph)
           (return-from suspension-1
             (zero-mrph (suspension (sorc mrph))
                        (suspension (trgt mrph))
                        (degr mrph))))
         (when (eq (first orgn) 'idnt-mrph)
           (return-from suspension-1
             (idnt-mrph (suspension (sorc mrph)))))
         (build-mrph
          :sorc (suspension (sorc mrph))
          :trgt (suspension (trgt mrph))
          :degr (degr mrph)
          :intr (suspension-intr mrph)
          :strt :cmbn
          :orgn `(suspension ,mrph)))))


(DEFMETHOD SUSPENSION-1 ((rdct reduction))
  (the reduction (progn
                   (when (eq (first (orgn rdct)) 'trivial-rdct)
                     (return-from suspension-1
                       (trivial-rdct (suspension (second (orgn rdct))))))
                   (build-rdct
                    :f (suspension (f rdct))
                    :g (suspension (g rdct))
                    :h (suspension (h rdct))
                    :orgn `(suspension ,rdct)))))


(DEFMETHOD SUSPENSION-1 ((hmeq homotopy-equivalence))
  (the homotopy-equivalence (progn
                              (when (eq (first (orgn hmeq)) 'trivial-hmeq)
                                (return-from suspension-1
                                  (trivial-hmeq (suspension
                                                 (second (orgn hmeq))))))
                              (build-hmeq
                               :lrdct (suspension (lrdct hmeq))
                               :rrdct (suspension (rrdct hmeq))
                               :orgn `(suspension ,hmeq)))))


(DEFMETHOD SEARCH-EFHM (suspension (orgn (eql 'suspension)))
  (declare (type chain-complex suspension))
  (suspension (efhm (second (orgn suspension)))))
