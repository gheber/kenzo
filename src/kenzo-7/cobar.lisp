;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR
;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR
;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR

(IN-PACKAGE #:cat-7)

(PROVIDE "cobar")

(DEFUN ALLP (&rest list)
  (when (= 1 (length list))
    (setf list (first list)))
  (unless (evenp (length list))
    (error "In ALLP, the length list should be even."))
  (the allp
       (let ((rslt +empty-list+))
         (declare (list rslt))
         (do ((mark list (cddr mark)))
             ((endp mark))
           (declare (list mark))
           (push (cbgn (car mark) (cadr mark)) rslt))
         (make-allp :list (nreverse rslt)))))


#-sbcl (DEFINE-CONSTANT +NULL-ALLP+ (make-allp :list +empty-list+))
#+sbcl (DEFPARAMETER +NULL-ALLP+ (make-allp :list +empty-list+))


(DEFUN ALLP-PRINT (allp stream depth)
  (declare
   (type allp allp) (stream stream)
   (ignore depth))
  (the allp
       (progn
         (format stream "<<AlLp")
         (dolist (cbgn (allp-list allp))
           (declare (type cbgn cbgn))
           (with-cbgn (degr gnrt) cbgn
                      (format stream "[~D ~A]" degr gnrt)))
         (format stream ">>")
         allp)))


(DEFUN COBAR-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (allp1 allp2)
           (declare (type allp allp1 allp2))
           (the cmpr
                (let ((list1 (allp-list allp1))
                      (list2 (allp-list allp2)))
                  (declare (type iallp list1 list2))
                  (lexico
                   (f-cmpr (length list1) (length list2))
                   (maplexico
                    #'(lambda (cbgn1 cbgn2)
                        (lexico
                         (f-cmpr (cdegr cbgn1) (cdegr cbgn2))
                         (funcall cmpr (cgnrt cbgn1) (cgnrt cbgn2))))
                    list1 list2))))))
    (the cmprf #'rslt)))


(DEFUN COBAR-BASIS-LENGTH (basis degr length)
  (declare
   (type basis basis)
   (fixnum degr length))
  (the list
       (progn
         (when (= 1 length)
           (return-from cobar-basis-length
             (mapcar
              #'(lambda (item)
                  (declare (type gnrt item))
                  (list (cbgn degr item)))
              (funcall basis (1+ degr)))))
         (mapcan
          #'(lambda (degr1)
              (declare (fixnum degr1))
              (let ((list1 (funcall basis (1+ degr1)))
                    (list2 (cobar-basis-length
                            basis (- degr degr1) (1- length))))
                (declare (list list1 list2))
                (mapcan
                 #'(lambda (item1)
                     (declare (type gnrt item1))
                     (mapcar
                      #'(lambda (item2)
                          (declare (type iallp item2))
                          (cons (cbgn degr1 item1) item2))
                      list2))
                 list1)))
          (>a-b< 0 degr)))))


(DEFUN COBAR-BASIS (basis)
  (declare (type basis basis))
  (the basis
       (progn
         (when (eq :locally-effective basis)
           (return-from cobar-basis :locally-effective))
         (flet ((rslt (degr)
                  (declare (fixnum degr))
                  (cond ((minusp degr) +empty-list+)
                        ((zerop degr) (list +null-allp+))
                        (t
                         (mapcan
                          #'(lambda (length)
                              (declare (fixnum length))
                              (mapcar
                               #'(lambda (iallp)
                                   (declare (type iallp iallp))
                                   (make-allp :list iallp))
                               (cobar-basis-length basis degr length)))
                          (>a-b> 0 degr))))))
           #'rslt))))

#| The following version of the COBAR-BASIS function is
not correctly compiled with ACL-Unix, probably
because the too complicated (but natural and correct)
structure of closures. This version works on ACL-PC.
(DEFUN COBAR-BASIS (basis)
  (declare
   (type basis basis))
  ;; if the coalgebra is effective, it is assumed simply-connected :
  ;;   (length (funcall basis 0))  ==>  1
  ;;   (length (funcall basis 1))  ==>  0
  (the basis
       (progn
         (when (eq :locally-effective basis)
           (return-from cobar-basis :locally-effecive))
         (labels ((rslt (degr length)
                    (declare (fixnum degr length))
                    (when (= 1 length)
                      (return-from rslt
                        (mapcar
                         #'(lambda (item)
                             (declare (type gnrt item))
                             (list (cbgn degr item)))
                         (funcall basis (1+ degr)))))
                    (mapcan
                     #'(lambda (degr1)
                         (declare (fixnum degr1))
                         (let ((list1 (funcall basis (1+ degr1)))
                               (list2 (rslt (- degr degr1) (1- length))))
                           (declare (list list1 lis2))
                           (mapcan
                            #'(lambda (item1)
                                (declare (type gnrt item1))
                                (mapcar
                                 #'(lambda (item2)
                                     (declare (type iallp item2))
                                     (cons (cbgn degr1 item1) item2))
                                 list2))
                            list1)))
                     (>a-b< 0 degr))))
           #'(lambda (degr)
               (declare (fixnum degr))
               (if (zerop degr)
                   (list (make-allp :list +empty-list+))
                   (mapcan
                    #'(lambda (length)
                        (declare (fixnum length))
                        (mapcar
                         #'(lambda (iallp)
                             (declare (type iallp iallp))
                             (make-allp :list iallp))
                         (rslt degr length)))
                    (>a-b> 0 degr))))))))
|#


(DEFUN COBAR-INTR-VRTC-DFFR (dffr)
  (declare (type morphism dffr))
  (labels ((rslt (degr iallp)
             ;; the argument iallp is an internal algebraic loop,
             ;;     without the keyword :allp
             ;; rslt returns an internal combination
             ;;     without the keyword :cmbn, without degree
             (declare
              (fixnum degr)
              (type iallp iallp))
             (the icmbn
                  (progn
                    (unless iallp
                      (return-from rslt +empty-list+))
                    (let ((cbgn1 (first iallp))
                          (rest (rest iallp)))
                      (declare
                       (type cbgn cbgn1)
                       (type iallp rest))
                      (with-cbgn
                          (degr1 gnrt1) cbgn1
                          (let ((d-gnrt1 (cmbn-list (gnrt-? dffr (1+ degr1)
                                                            gnrt1)))
                                (d-rest (rslt (- degr degr1) rest))
                                (degr1-1 (1- degr1))
                                (first-sign (-1-expt-n-1 (length rest)))
                                (rest-sign (-1-expt-n degr1)))
                            (declare
                             (type icmbn d-gnrt1 d-rest)
                             (fixnum degr1-1 rest-sign))
                            (nconc
                             (mapcar
                              #'(lambda (term)
                                  (with-term
                                      (cffc gnrt) term
                                      (term (* first-sign cffc)
                                            (cons (cbgn degr1-1 gnrt) rest))))
                              d-gnrt1)
                             (mapcar
                              #'(lambda (term)
                                  (with-term
                                      (cffc gnrt) term
                                      (term (* rest-sign cffc)
                                            (cons cbgn1 gnrt))))
                              d-rest)))))))))
    (the intr-mrph
         #'(lambda (degr allp)
             (make-cmbn
              :degr (1- degr)
              :list (mapcar
                     #'(lambda (term)
                         (with-term (cffc iallp) term
                                    (term cffc (make-allp :list iallp))))
                     (rslt degr (allp-list allp))))))))


(DEFGENERIC VRTC-COBAR (chcm-or-hmeq-or-rdct))


(DEFMETHOD VRTC-COBAR ((chcm chain-complex))
  (the chain-complex
       (with-slots (cmpr basis dffr) chcm
         (declare
          (type cmprf cmpr)
          (type basis basis)
          (type morphism dffr))
         (build-chcm
          :cmpr (cobar-cmpr cmpr)
          :basis (cobar-basis basis)
          :bsgn +null-allp+
          :intr-dffr (cobar-intr-vrtc-dffr dffr)
          :strt :gnrt
          :orgn `(vrtc-cobar ,chcm)))))


(DEFUN COBAR-INTR-HRZN-DFFR (cprd)
  (declare (type morphism cprd))
  (labels ((rslt (degr iallp)
             (declare
              (fixnum degr)
              (type iallp iallp))
             (the icmbn
                  (progn
                    (unless iallp
                      (return-from rslt +empty-list+))
                    (let ((cbgn1 (first iallp))
                          (rest (rest iallp)))
                      (declare
                       (type cbgn cbgn1)
                       (type iallp rest))
                      (with-cbgn
                          (degr1 gnrt1) cbgn1
                          (let ((cprd-gnrt1 (cmbn-list (gnrt-? cprd (1+ degr1) gnrt1)))
                                (cprd-rest (rslt (- degr degr1) rest))
                                (sign (-1-expt-n-1 (length rest))))
                            (declare
                             (type icmbn cprd-gnrt1 cprd-rest)
                             (fixnum sign))
			    ;;; because \bar{A}
                            (setf cprd-gnrt1 (rest (butlast cprd-gnrt1)))
                            (nconc
                             (mapcar
                              #'(lambda (term1)
                                  (declare (type term term1))
                                  (with-term
                                      (nil tnpr) term1  ;;; cffc is in fact 1
                                      (with-tnpr
                                          (degr1 gnrt1 degr2 gnrt2) tnpr
                                          (term sign ;;; (* cffc sign)
                                                (cons (cbgn (1- degr1) gnrt1)
                                                      (cons (cbgn (1- degr2) gnrt2)
                                                            rest))))))
                              cprd-gnrt1)
                             (mapcar
                              #'(lambda (term2)
                                  (declare (type term term2))
                                  (with-term (cffc allp2) term2
                                             (term cffc
                                                   (cons cbgn1 allp2))))
                              cprd-rest)))))))))
    (the intr-mrph
         #'(lambda (degr allp)
             (declare
              (fixnum degr)
              (type allp allp))
             (the cmbn
                  (make-cmbn :degr (1- degr)
                             :list (mapcar #'(lambda (term)
                                               (declare (type term term))
                                               (with-term
                                                   (cffc iallp) term
                                                   (term cffc (make-allp
                                                               :list iallp))))
                                           (rslt degr (allp-list allp)))))))))


(DEFUN COBAR-HRZN-DFFR (clgb)
  (declare (type coalgebra clgb))
  (the morphism
       (with-slots (cprd) clgb
         (declare (type morphism cprd))
         (build-mrph
          :sorc (vrtc-cobar clgb) :trgt (vrtc-cobar clgb) :degr -1
          :intr (cobar-intr-hrzn-dffr cprd) :strt :gnrt
          :orgn `(cobar-hrzn-dffr ,clgb)))))


(DEFUN COBAR-INTR-DFFR (vrtc-dffr hrzn-dffr)
  (declare (type morphism vrtc-dffr hrzn-dffr))
  (flet ((rslt (degr allp)
           (declare
            (fixnum degr)
            (type allp allp))
           (make-cmbn :degr (1- degr)
                      :list (append ;;; and not nconc, otherwise a terrible
			            ;;; bug, when the
                                    ;;; first result is stored in memory...
                             (cmbn-list (gnrt-? vrtc-dffr degr allp))
                             (cmbn-list (gnrt-? hrzn-dffr degr allp))))))
    (the intr-mrph #'rslt)))


(DEFGENERIC COBAR (clgbr-or-hmeq))


(DEFMETHOD COBAR ((coalgebra coalgebra))
  (let ((vrtc-cobar (vrtc-cobar coalgebra))
        (cobar-hrzn-dffr (cobar-hrzn-dffr coalgebra)))
    (declare (type chain-complex vrtc-cobar)
             (type morphism cobar-hrzn-dffr))
    (the chain-complex
         (let ((rslt (build-chcm
                      :cmpr (cmpr vrtc-cobar)
                      :basis (basis vrtc-cobar)
                      :bsgn +null-allp+
                      :intr-dffr (cobar-intr-dffr (dffr vrtc-cobar)
                                                  cobar-hrzn-dffr)
                      :strt :gnrt
                      :orgn `(add ,vrtc-cobar ,cobar-hrzn-dffr))))
           (declare (type chain-complex rslt))
           (setf (slot-value rslt 'grmd) (grmd vrtc-cobar))
           rslt))))


(DEFUN CMBN-ALLP-CMBN-TNPR (cmbn allp-cmbn)
  (declare (type cmbn cmbn allp-cmbn))
  (the cmbn
       (with-cmbn
           (degr1 list1) cmbn
           (decf degr1)     ;; because allp organization
           (with-cmbn
               (degrr listr) allp-cmbn
               (make-cmbn
                :degr (+ degr1 degrr)
                :list
                (mapcan
                 #'(lambda (term1)
                     (declare (type term term1))
                     (with-term
                         (cffc1 gnrt1) term1
                         (let ((cbgn1 (cbgn degr1 gnrt1)))
                           (declare (type cbgn cbgn1))
                           (mapcar
                            #'(lambda (termr)
                                (declare (type term termr))
                                (with-term
                                    (cffcr allpr) termr
                                    (term (* cffc1 cffcr)
                                          (make-allp
                                           :list (cons cbgn1
                                                       (allp-list allpr))))))
                            listr))))
                 list1))))))


(DEFUN NCMBN-COBAR (cmbn-list)
  (declare (list cmbn-list))
  (the cmbn
       (progn
         (unless cmbn-list
           (return-from ncmbn-cobar (cmbn 0 1 +null-allp+)))
         (cmbn-allp-cmbn-tnpr
          (first cmbn-list)
          (ncmbn-cobar (rest cmbn-list))))))


(DEFUN MRPH-VRTC-COBAR-INTR (mrph)
  (declare (type morphism mrph))
  (flet ((rslt (degr allp)
           (declare
            (ignore degr)
            (type allp allp))
           (the cmbn
                (ncmbn-cobar
                 (mapcar #'(lambda (cbgn)
                             (declare (type cbgn cbgn))
                             (with-cbgn (degr gnrt) cbgn
                                        (gnrt-? mrph (1+ degr) gnrt)))
                         (allp-list allp))))))
    (the intr-mrph #'rslt)))


(DEFMETHOD VRTC-COBAR ((mrph morphism))
  (the morphism
       (if (eq (first (orgn mrph)) 'idnt-mrph)
           (idnt-mrph (vrtc-cobar (sorc mrph)))
           (build-mrph
            :sorc (vrtc-cobar (sorc mrph))
            :trgt (vrtc-cobar (trgt mrph))
            :degr 0
            :intr (mrph-vrtc-cobar-intr mrph)
            :strt :gnrt
            :orgn `(vrtc-cobar ,mrph)))))


(DEFUN HMTP-VRTC-COBAR-INTR (h gf)
  (declare (type morphism h gf))
  (flet ((rslt (degr allp)
           (declare
            (fixnum degr)
            (type allp allp))
           (let* ((cbgn-list (reverse (allp-list allp)))
                  (rslt (list nil))
                  (sign (oddp (apply #'+ (mapcar #'car cbgn-list))))
                  ;; nil = -1 ;; t = +1
                  (gfgf-tail (term-cmbn 0 1 +null-allp+)))
             (declare
              (list cbgn-list rslt)
              (boolean sign)
              (type cmbn gfgf-tail))
             (unless cbgn-list
               (return-from rslt (zero-cmbn 1)))
             (loop
                (let ((cbgn (car cbgn-list)))
                  (declare (type cbgn cbgn))
                  (with-cbgn
                      (degr gnrt) cbgn
                      (when (evenp degr)
                        (setf sign (not sign)))
                      (incf degr)
                      (let ((h-gnrt (gnrt-? h degr gnrt)))
                        (declare (type cmbn h-gnrt))
                        (let ((hgfgf (cmbn-list (cmbn-allp-cmbn-tnpr
                                                 h-gnrt
                                                 gfgf-tail))))
                          (declare (type list hgfgf))
                          (when sign
                            (mapc #'(lambda (term)
                                      (declare (type term term))
                                      (setf (cffc term) (- (cffc term))))
                                  hgfgf))
                          (setf cbgn-list (rest cbgn-list))
                          (let ((head (reverse cbgn-list)))
                            (declare (list head))
                            (mapc #'(lambda (term)
                                      (setf (allp-list (gnrt term))
                                            (append head (allp-list (gnrt term)))))
                                  hgfgf))
                          (nconc rslt hgfgf)))
                      (unless cbgn-list (return))
                      (setf gfgf-tail
                            (cmbn-allp-cmbn-tnpr
                             (gnrt-? gf degr gnrt) gfgf-tail)))))
             (make-cmbn
              :degr (1+ degr)
              :list (rest rslt)))))
    (the intr-mrph #'rslt)))


(DEFUN HMTP-VRTC-COBAR (h gf)
  (declare (type morphism h gf))
  (unless (and (= +1 (degr h))
               (= +0 (degr gf)))
    (error "In HMTP-VRTC-COBAR, the morphism degrees are not the right ones."))
  (unless (and (eq (sorc h) (trgt h))
               (eq (trgt h) (sorc gf))
               (eq (sorc gf) (trgt gf)))
    (error "In HMTP-VRTC-COBAR, fg-h sources and targets are not the same."))
  (the morphism
       (if (eq (first (orgn h)) 'zero-mrph)
           (zero-mrph (vrtc-cobar (sorc h)))
           (build-mrph
            :sorc (vrtc-cobar (sorc h)) :trgt (vrtc-cobar (sorc h)) :degr +1
            :intr (hmtp-vrtc-cobar-intr h gf)
            :strt :gnrt
            :orgn `(hmtp-vrtc-cobar ,h ,gf)))))


(DEFMETHOD VRTC-COBAR ((rdct reduction))
  (the reduction
       (if (eq (first (orgn rdct)) 'trivial-rdct)
           (trivial-rdct (vrtc-cobar (bcc rdct)))
           (with-slots (f g h) rdct
             (build-rdct
              :f (vrtc-cobar f)
              :g (vrtc-cobar g)
              :h (hmtp-vrtc-cobar h (cmps g f))
              :orgn `(vrtc-cobar ,rdct))))))


(DEFMETHOD VRTC-COBAR ((hmeq homotopy-equivalence))
  (the homotopy-equivalence
       (if (eq (first (orgn hmeq)) 'trivial-hmeq)
           (trivial-hmeq (vrtc-cobar (lbcc hmeq)))
           (with-slots (lrdct rrdct) hmeq
             (build-hmeq
              :lrdct (vrtc-cobar lrdct)
              :rrdct (vrtc-cobar rrdct)
              :orgn `(vrtc-cobar ,hmeq))))))


(DEFMETHOD COBAR ((hmeq homotopy-equivalence))
  (unless (typep (lbcc hmeq) 'coalgebra)
    (error "In (COBAR HMEQ), the LBCC should be a coalgebra."))
  (the homotopy-equivalence
       (if (eq (first (orgn hmeq)) 'trivial-hmeq)
           (trivial-hmeq (cobar (lbcc hmeq)))
           (add (vrtc-cobar hmeq) (cobar-hrzn-dffr (lbcc hmeq))))))
