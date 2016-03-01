;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR
;;;  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR
;;;  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR  BAR

;;;
;;;  MAC-LANE signs (p. 306)
;;;

(IN-PACKAGE #:cat)

(PROVIDE "bar")

(DEFUN ABAR (&rest list)
  (when (= 1 (length list))
    (setf list (first list)))
  (unless (evenp (length list))
    (error "In ABAR, the length list should be even."))
  (the abar
       (let ((rslt +empty-list+))
         (declare (list rslt))
         (do ((mark list (cddr mark)))
             ((endp mark))
           (declare (list mark))
           (push (brgn (car mark) (cadr mark)) rslt))
         (make-abar :list (nreverse rslt)))))


#-sbcl (DEFINE-CONSTANT +NULL-ABAR+ (make-abar :list +empty-list+))
#+sbcl (DEFPARAMETER +NULL-ABAR+ (make-abar :list +empty-list+))

;;; -[CLISP
#|
(DEFMETHOD PRINT-KEYCONS ((car (eql :abar)) cdr stream)
  (declare
   (list cdr)
   (stream stream))
  (the abar
       (progn
         (format stream "<<Abar")
         (dolist (brgn cdr)
           (declare (type brgn brgn))
           (with-brgn (degr gnrt) brgn
                      (format stream "[~D ~A]" degr gnrt)))
         (format stream ">>")
         (cons car cdr))))
|#
;;;    CLISP]-
;;; +[CLISP
(DEFUN ABAR-PRINT (abar stream depth)
  (declare
   (type abar abar) (stream stream)
   (ignore depth))
  (the abar
       (progn
         (format stream "<<Abar")
         (dolist (brgn (abar-list abar))
           (declare (type brgn brgn))
           (with-brgn (degr gnrt) brgn
                      (format stream "[~D ~A]" degr gnrt)))
         (format stream ">>")
         abar)))
;;; CLISP]+


(DEFUN BAR-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (abar1 abar2)
           (declare (type abar abar1 abar2))
           (the cmpr
                (let ((list1 (abar-list abar1))
                      (list2 (abar-list abar2)))
                  (declare (list list1 list2))
                  (lexico
                   (f-cmpr (length list1) (length list2))
                   (maplexico
                    #'(lambda (brgn1 brgn2)
                        (lexico
                         (f-cmpr (bdegr brgn1) (bdegr brgn2))
                         (funcall cmpr (bgnrt brgn1) (bgnrt brgn2))))
                    list1 list2))))))
    (the cmprf #'rslt)))


(DEFUN BAR-BASIS-LENGTH (basis degr length)
  (declare
   (type basis basis)
   (fixnum degr length))
  (the list
       (progn
         (when (= 1 length)
           (return-from bar-basis-length
             (mapcar
              #'(lambda (item)
                  (declare (type gnrt item))
                  (list (brgn degr item)))
              (funcall basis (1- degr)))))
         (when (< degr 4)
           (return-from bar-basis-length +empty-list+))
         (mapcan
          #'(lambda (degr1)
              (declare (fixnum degr1))
              (let ((list1 (funcall basis (1- degr1)))
                    (list2 (bar-basis-length
                            basis (- degr degr1) (1- length))))
                (declare (list list1 list2))
                (mapcan
                 #'(lambda (item1)
                     (declare (type gnrt item1))
                     (mapcar
                      #'(lambda (item2)
                          (declare (type iabar item2))
                          (cons (brgn degr1 item1) item2))
                      list2))
                 list1)))
          (>a-b< 1 (1- degr))))))


(DEFUN BAR-BASIS (basis)
  (declare (type basis basis))
  (the basis
       (progn
         (when (eq :locally-effective basis)
           (return-from bar-basis :locally-effective))
         (flet
             ((rslt (degr)
                (declare (fixnum degr))
                (cond ((zerop degr) (list +null-abar+))
                      ((< degr 2) +empty-list+)
                      (t
                       (mapcan
                        #'(lambda (length)
                            (declare (fixnum length))
                            (mapcar
                             #'(lambda (iabar)
                                 (declare (type iabar iabar))
                                 (make-abar :list iabar))
                             (bar-basis-length basis degr length)))
                        (>a-b> 0 (floor degr 2)))))))
           #'rslt))))


(DEFUN BAR-INTR-VRTC-DFFR (dffr)
  (declare (type morphism dffr))
  (labels ((rslt (degr iabar)
             ;; the argument iabar is an internal algebraic bar,
             ;;     without the keyword :abar
             ;; rslt returns an internal combination
             ;;     without the keyword :cmbn, without degree
             (declare
              (fixnum degr)
              (type iabar iabar))
             (progn
               (unless iabar
                 (return-from rslt +empty-list+))
               (let ((brgn1 (first iabar))
                     (rest2 (rest iabar)))
                 (declare
                  (type brgn brgn1)
                  (type iabar rest2))
                 (with-brgn (degr1 gnrt1) brgn1
                            (let ((d-gnrt1 (cmbn-list (gnrt-? dffr (1- degr1)
                                                              gnrt1)))
                                  (d-rest2 (rslt (- degr degr1) rest2))
                                  (degr1-1 (1- degr1))
                                  (rest-sign (-1-expt-n degr1)))
                              (declare
                               (type icmbn d-gnrt1 d-rest2)
                               (fixnum degr1-1 rest-sign))
                              (nconc
                               (mapcar
                                #'(lambda (term)
                                    (declare (type term term))
                                    (with-term (cffc gnrt) term
                                               (term (- cffc)
                                                     (cons (brgn degr1-1 gnrt)
                                                           rest2))))
                                d-gnrt1)
                               (mapcar
                                #'(lambda (term)
                                    (with-term (cffc gnrt) term
                                               (term (* rest-sign cffc)
                                                     (cons brgn1 gnrt))))
                                d-rest2))))))))
    (the intr-mrph
         #'(lambda (degr abar)
             (declare
              (fixnum degr)
              (type abar abar))
             (make-cmbn
              :degr (1- degr)
              :list (mapcar
                     #'(lambda (term)
                         (with-term (cffc iabar) term
                                    (term cffc (make-abar :list iabar))))
                     (rslt degr (abar-list abar))))))))


(DEFGENERIC VRTC-BAR (chcm-hmeq-mrph))


(DEFMETHOD VRTC-BAR ((chcm chain-complex))
  (the chain-complex
       (with-slots (cmpr basis dffr) chcm
         (declare
          (type cmprf cmpr)
          (type basis basis)
          (type morphism dffr))
         (build-chcm
          :cmpr (bar-cmpr cmpr)
          :basis (bar-basis basis)
          :bsgn +null-abar+
          :intr-dffr (bar-intr-vrtc-dffr dffr)
          :strt :gnrt
          :orgn `(vrtc-bar ,chcm)))))


(DEFUN BAR-INTR-HRZN-DFFR (aprd)
  (declare (type morphism aprd))
  (labels ((rslt (degr iabar)
             (declare
              (fixnum degr)
              (type iabar iabar))
             (the icmbn
                  (progn
                    (unless (cdr iabar)
                      (return-from rslt +empty-list+))
                    (let ((brgn1 (first iabar))
                          (brgn2 (second iabar))
                          (rest1 (rest iabar))
                          (rest2 (cddr iabar)))
                      (declare
                       (type brgn brgn1 brgn2)
                       (type iabar rest1 rest2))
                      (with-brgn (degr1 gnrt1) brgn1
                                 (with-brgn (degr2 gnrt2) brgn2
                                            (let ((sign (-1-expt-n degr1))
                                                  (aprd-brgn1-brgn2
                                                   (gnrt-? aprd
                                                           (+ degr1 degr2 -2)
                                                           (tnpr (1- degr1)
                                                                 gnrt1
                                                                 (1- degr2)
                                                                 gnrt2)))
                                                  (aprd-rest1 (rslt
                                                               (- degr degr1)
                                                               rest1)))
                                              (declare
                                               (type cmbn aprd-brgn1-brgn2)
                                               (type icmbn aprd-rest1))
                                              (with-cmbn (degr12 icmbn12)
                                                aprd-brgn1-brgn2
                                                (incf degr12)
                                                (nconc
                                                 (mapcar
                                                  #'(lambda (term2)
                                                      (declare (type term
                                                                     term2))
                                                      (with-term (cffc2 iabar2)
                                                        term2
                                                        (term (* sign cffc2)
                                                              (cons brgn1
                                                                    iabar2))))
                                                  aprd-rest1)
                                                 (mapcar
                                                  #'(lambda (term1)
                                                      (declare (type term
                                                                     term1))
                                                      (with-term (cffc1 gnrt12)
                                                        term1
                                                        (term (* sign cffc1)
                                                              (cons (brgn
                                                                     degr12
                                                                     gnrt12)
                                                                    rest2))))
                                                  icmbn12)))))))))))
    (the intr-mrph
         #'(lambda (degr abar)
             (declare
              (fixnum degr)
              (type abar abar))
             (the cmbn
                  (make-cmbn :degr (1- degr)
                             :list (mapcar #'(lambda (term)
                                               (declare (type term term))
                                               (with-term (cffc iabar) term
                                                          (term cffc
                                                                (make-abar
                                                                 :list iabar))))
                                           (rslt degr (abar-list abar)))))))))


(DEFUN BAR-HRZN-DFFR (algb)
  (declare (type algebra algb))
  (the morphism
       (with-slots (aprd) algb
         (declare (type morphism aprd))
         (build-mrph
          :sorc (vrtc-bar algb) :trgt (vrtc-bar algb) :degr -1
          :intr (bar-intr-hrzn-dffr aprd) :strt :gnrt
          :orgn `(bar-hrzn-dffr ,algb)))))


(DEFUN BAR-INTR-DFFR (vrtc-dffr hrzn-dffr)
  (declare (type morphism vrtc-dffr hrzn-dffr))
  (flet ((rslt (degr abar)
           (declare
            (fixnum degr)
            (type abar abar))
           (make-cmbn :degr (1- degr)
                      :list (append ;;; and not nconc, otherwise a terrible bug,
                        ;;; when the first result is stored in memory
                             (cmbn-list (gnrt-? hrzn-dffr degr abar))
                             (cmbn-list (gnrt-? vrtc-dffr degr abar))))))
    (the intr-mrph #'rslt)))


(DEFGENERIC BAR (algebra-or-hmeq))


(DEFMETHOD BAR ((algebra algebra))
  (let ((vrtc-bar (vrtc-bar algebra))
        (bar-hrzn-dffr (bar-hrzn-dffr algebra)))
    (declare (type chain-complex vrtc-bar)
             (type morphism bar-hrzn-dffr))
    (the chain-complex
         (let ((rslt (build-chcm
                      :cmpr (cmpr vrtc-bar)
                      :basis (basis vrtc-bar)
                      :bsgn +null-abar+
                      :intr-dffr (bar-intr-dffr (dffr vrtc-bar) bar-hrzn-dffr)
                      :strt :gnrt
                      :orgn `(add ,vrtc-bar ,bar-hrzn-dffr))))
           (declare (type chain-complex rslt))
           (setf (slot-value rslt 'grmd) (grmd vrtc-bar))
           rslt))))


(DEFUN CMBN-ABAR-CMBN-TNPR (cmbn abar-cmbn)
  (declare (type cmbn cmbn abar-cmbn))
  (the cmbn
       (with-cmbn (degr1 list1) cmbn
                  (incf degr1)     ;; because abar organization
                  (with-cmbn (degrr listr) abar-cmbn
                             (make-cmbn
                              :degr (+ degr1 degrr)
                              :list
                              (mapcan
                               #'(lambda (term1)
                                   (declare (type term term1))
                                   (with-term (cffc1 gnrt1) term1
                                              (let ((brgn1 (brgn degr1 gnrt1)))
                                                (declare (type brgn brgn1))
                                                (mapcar
                                                 #'(lambda (termr)
                                                     (declare (type term
                                                                    termr))
                                                     (with-term (cffcr abarr)
                                                       termr
                                                       (term
                                                        (* cffc1 cffcr)
                                                        (make-abar
                                                         :list (cons
                                                                brgn1
                                                                (abar-list
                                                                 abarr))))))
                                                 listr))))
                               list1))))))


(DEFUN NCMBN-BAR (cmbn-list)
  (declare (list cmbn-list))
  (the cmbn
       (progn
         (unless cmbn-list
           (return-from ncmbn-bar (cmbn 0 1 +null-abar+)))
         (cmbn-abar-cmbn-tnpr
          (first cmbn-list)
          (ncmbn-bar (rest cmbn-list))))))


(DEFUN MRPH-VRTC-BAR-INTR (mrph)
  (declare (type morphism mrph))
  (flet ((rslt (degr abar)
           (declare
            (ignore degr)
            (type abar abar))
           (the cmbn
                (ncmbn-bar
                 (mapcar #'(lambda (brgn)
                             (declare (type brgn brgn))
                             (with-brgn (degr gnrt) brgn
                                        (gnrt-? mrph (1- degr) gnrt)))
                         (abar-list abar))))))
    (the intr-mrph #'rslt)))


(DEFMETHOD VRTC-BAR ((mrph morphism))
  (the morphism
       (if (eq (first (orgn mrph)) 'idnt-mrph)
           (idnt-mrph (vrtc-bar (sorc mrph)))
           (build-mrph
            :sorc (vrtc-bar (sorc mrph))
            :trgt (vrtc-bar (trgt mrph))
            :degr 0
            :intr (mrph-vrtc-bar-intr mrph)
            :strt :gnrt
            :orgn `(vrtc-bar ,mrph)))))


(DEFUN HMTP-VRTC-BAR-INTR (h gf)
  (declare (type morphism h gf))
  (labels ((rslt (degr iabar)
             (declare
              (fixnum degr)
              (type iabar iabar))
             (unless iabar
               (return-from rslt +empty-list+))
             (let ((brgn1 (first iabar))
                   (rest2 (rest iabar)))
               (declare
                (type brgn brgn1)
                (list rest2))
               (with-brgn (degr1 gnrt1) brgn1
                          (let ((h-gnrt1 (cmbn-list (gnrt-? h (1- degr1)
                                                            gnrt1)))
                                (h-rest2 (rslt (- degr degr1) rest2))
                                (degr1+1 (1+ degr1))
                                (rest-sign (-1-expt-n degr1))
                                (gf-gnrt1 (cmbn-list (gnrt-? gf (1- degr1)
                                                             gnrt1))))
                            (declare
                             (fixnum degr1+1 rest-sign)
                             (type icmbn h-gnrt1 h-rest2 gf-gnrt1))
                            (nconc
                             (mapcan
                              #'(lambda (term1)
                                  (declare (type term term1))
                                  (with-term (cffc1 gnrt11) term1
                                             (mapcar
                                              #'(lambda (term2)
                                                  (declare (type term term2))
                                                  (with-term (cffc2 iabar2)
                                                    term2
                                                    (term (* rest-sign cffc1
                                                             cffc2)
                                                          (cons
                                                           (brgn degr1 gnrt11)
                                                           iabar2))))
                                              h-rest2)))
                              gf-gnrt1)
                             (mapcar
                              #'(lambda (term1)
                                  (declare (type term term1))
                                  (with-term (cffc1 gnrt11) term1
                                             (term (- cffc1)
                                                   (cons
                                                    (brgn degr1+1 gnrt11)
                                                    rest2))))
                              h-gnrt1)))))))
    (the intr-mrph
         #'(lambda (degr abar)
             (declare
              (fixnum degr)
              (type abar abar))
             (make-cmbn
              :degr (1+ degr)
              :list (mapcar
                     #'(lambda (term)
                         (declare (type term term))
                         (with-term (cffc iabar) term
                                    (declare
                                     (fixnum cffc)
                                     (type iabar iabar))
                                    (term cffc (make-abar :list iabar))))
                     (rslt degr (abar-list abar))))))))


(DEFUN HMTP-VRTC-BAR (h gf)
  (declare (type morphism h gf))
  (unless (and (= +1 (degr h))
               (= +0 (degr gf)))
    (error "In HMTP-VRTC-BAR, the morphism degrees are not the right ones."))
  (unless (and (eq (sorc h) (trgt h))
               (eq (trgt h) (sorc gf))
               (eq (sorc gf) (trgt gf)))
    (error "In HMTP-VRTC-BAR, fg-h sources and targets are not the same."))
  (the morphism
       (if (eq (first (orgn h)) 'zero-mrph)
           (zero-mrph (vrtc-bar (sorc h)))
           (build-mrph
            :sorc (vrtc-bar (sorc h)) :trgt (vrtc-bar (sorc h)) :degr +1
            :intr (hmtp-vrtc-bar-intr h gf)
            :strt :gnrt
            :orgn `(hmtp-vrtc-bar ,h ,gf)))))

(DEFMETHOD VRTC-BAR ((rdct reduction))
  (the reduction
       (if (eq (first (orgn rdct)) 'trivial-rdct)
           (trivial-rdct (vrtc-bar (bcc rdct)))
           (with-slots (f g h) rdct
             (build-rdct
              :f (vrtc-bar f)
              :g (vrtc-bar g)
              :h (hmtp-vrtc-bar h (cmps g f))
              :orgn `(vrtc-bar ,rdct))))))


(DEFMETHOD VRTC-BAR ((hmeq homotopy-equivalence))
  (the homotopy-equivalence
       (if (eq (first (orgn hmeq)) 'trivial-hmeq)
           (trivial-hmeq (vrtc-bar (lbcc hmeq)))
           (with-slots (lrdct rrdct) hmeq
             (build-hmeq
              :lrdct (vrtc-bar lrdct)
              :rrdct (vrtc-bar rrdct)
              :orgn `(vrtc-bar ,hmeq))))))


(DEFMETHOD BAR ((hmeq homotopy-equivalence))
  (unless (typep (lbcc hmeq) 'algebra)
    (error "In (BAR HMEQ), the LBCC should be a algebra."))
  (the homotopy-equivalence
       (if (eq (first (orgn hmeq)) 'trivial-hmeq)
           (trivial-hmeq (bar (lbcc hmeq)))
           (add (vrtc-bar hmeq) (bar-hrzn-dffr (lbcc hmeq))))))
