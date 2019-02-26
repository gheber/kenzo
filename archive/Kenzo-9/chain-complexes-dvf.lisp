;;;  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF
;;;  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF
;;;  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF  CHAIN-COMPLEX-VF

(IN-PACKAGE "COMMON-LISP-USER")

(DEFTYPE STTS () '(member :sorc :trgt :crtc))

;; A vctr (= vector) is a triple stts (status) valu (value) incd (incidence number)
;; If a vector is source, the value is the corresponding *target* and
;; conversely.
;; If the status is critical, no value, no incidence number.

(DEFSTRUCT (VCTR (:conc-name nil))
  stts valu incd)

(DEFMETHOD PRINT-OBJECT ((vctr vctr) stream)
  (declare
   (type vctr vctr)
   (type stream stream))
  (the vctr (progn
              (ecase (stts vctr)
                (:sorc (format stream "<VF SS ~A ~D>" (valu vctr) (incd vctr)))
                (:trgt (format stream "<VF TT ~A ~D>" (valu vctr) (incd vctr)))
                (:crtc (format stream "<VF CC>")))
              vctr)))

(DEFUN VCTR (stts &optional valu incd)
  (declare (type stts stts)
           (type (or null gnrt) valu)
           (type (or null fixnum) incd))
  (ecase stts
    (:sorc (make-vctr :stts :sorc :valu valu :incd incd))
    (:trgt (make-vctr :stts :trgt :valu valu :incd incd))
    (:crtc (make-vctr :stts :crtc))))

#|
(vctr :sorc 'trgt 1)
(vctr :trgt 'sorc -1)
(vctr :crtc)
|#

;; A vector field is implemented as a function cell -> vctr.

(DEFTYPE VECTOR-FIELD () 'function)
         ;; (function (degr gnrt) vctr)


;; Exemple for the canonical contraction of a standard simplex.

(DEFUN DELTA-VF (dmns gmsm)
  (declare (type fixnum dmns gmsm))
  (the vctr
    (if (evenp gmsm)
        (vctr :sorc (1+ gmsm) 1)
      (if (zerop dmns)
          (vctr :crtc)
        (vctr :trgt (1- gmsm) 1)))))

#|
(delta-vf 0 1)
(delta-vf 0 16)
(setf d (delta 3))
(dotimes (i 4)
  (dolist (gmsm (basis d i))
    (print (list i gmsm (delta-vf i gmsm)))))
|#

;; To obtain the sub-combination made of the cells with a given status.

(DEFUN CMBN-SELECT-STTS (vf cmbn stts)
  (declare (type cmbn cmbn) (type stts stts))
  (the cmbn
    (with-slots (degr list) cmbn
      (declare (type fixnum degr) (type list list))
      (make-cmbn :degr degr
                 :list (remove-if (complement
                                   #'(lambda (term)
                                       (declare (type term term))
                                       (the boolean
                                         (eq stts 
                                             (stts (funcall vf degr (gnrt term)))))))
                                  list)))))

#|
(setf vf #'ez-vf) ;;
(setf cmbn (cmbn 2 1 (crpr 1 'gmsm1 2 'gmsm2)
                  10 (crpr 0 'gmsm1 0 'gmsm2)
                 100 (crpr 2 'gmsm1 1 'gmsm2)))
(cmbn-select-stts vf cmbn :sorc)
(cmbn-select-stts vf cmbn :trgt)
(cmbn-select-stts vf cmbn :crtc)
|#

;; Computing the homotopy component of the reduction
;;  from a vector field.
;; Very sophisticated implementation for optimization.
;; A new type of :strt is defined = :rcrs,
;; Necessary to process this type of recursivity.

(DEFUN CHCM-VF-REDUCTION-H (chcm vf &aux
                                 (dffr (dffr chcm)) (cmpr (cmpr chcm)))
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type morphism dffr)
           (type cmprf cmpr))
  (the morphism
    (let (morphism)
      (declare (type (or null morphism) morphism))
      (flet
          ((rslt
            (degr gnrt &aux (1+degr (1+ degr)))
            (declare (type fixnum degr 1+degr) (type gnrt gnrt))
            (the cmbn
              (with-slots (stts valu incd) (funcall vf degr gnrt)
                (declare (type stts stts)
                         (type gnrt valu)
                         (type fixnum incd))
                (unless (eq stts :sorc)
                  (return-from rslt (zero-cmbn 1+degr)))
                (let ((cmbn (? dffr 1+degr valu)))
                  (declare (type cmbn cmbn))
                  (with-slots (list) (cmbn-select-stts vf cmbn :sorc)
                    (declare (type list list))
                    (setf list (delete gnrt list :key #'cdr :test #'equalp))
                    (unless list
                      (return-from rslt (cmbn 1+degr incd valu)))
                    (setf list
                      (mapcar #'(lambda (term)
                                  (declare (type term))
                                  (the term
                                    (with-term (cffc gnrt) term
                                      (term cffc (? morphism degr gnrt)))))
                        list))
                    (let ((rslt (cmbn-cmbn cmpr list)))
                      (declare (type cmbn rslt))
                      ;;; (dstr-add-term-to-cmbn cmpr -1 valu rslt)
                      ;;; = Terrible bug !!!
                      (setf rslt (2cmbn-add cmpr rslt (cmbn 1+degr -1 valu)))
                      (return-from rslt
                        (ecase incd
                          (-1 rslt)
                          (+1 (cmbn-opps rslt)))))))))))
        (setf morphism
          (build-mrph :sorc chcm :trgt chcm :degr +1
                      :intr #'rslt :strt :rcrs
                      :orgn `(chcm-vf-reduction-h ,chcm ,vf)))))))

#| ;; Debugging (*was* erroneous!)
(cat-init)
(setf dd (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf h (chcm-vf-reduction-h dd vf))
(? h 3 (crpr 6 3 0 15))
(? h 3 (crpr 6 3 0 15))
(inspect h)
|#

#|
(cat-init)
(setf x (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf hh (chcm-vf-reduction-h x vf))
(? hh 5 (crpr 0 63 0 63))
(? hh *)  ;; (previous ?) error
(every #'(lambda (term)
           (eq :trgt (stts (funcall vf 6 (gnrt term)))))
       (cmbn-list *))
|#

;; The reduction induced by a vector field is CC => CC^c, but this
;; is not convenient because usually instead of the critical complex
;; another more convenient complex is preferred.
;; Isof is this isomorphism *from* the critical complex
;; in the Eilenberg-Zilber case.

(DEFUN EZ-ISOF (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
    (with-slots (degr list) cmbn
      (declare (type fixnum degr) (type list list))
      (unless list
        (return-from ez-isof cmbn))
      (flet
       ((convert-rslt2
         (list)
         (declare (type list list))
         (assert list)
         (the list
           (let* ((degr1 (logcount (dgop2 (gnrt (first list)))))
                  (degr2 (- degr degr1)))
             (declare (type fixnum degr1 degr2))
             (mapcar #'(lambda (term)
                         (declare (type term term))
                         (the term
                           (with-term (cffc crpr) term
                             (declare (type crpr crpr))
                             (with-slots (gmsm1 gmsm2) crpr
                               (declare (type gmsm gmsm1 gmsm2))
                               (term cffc (tnpr degr1 gmsm1 degr2 gmsm2))))))
               (nreverse list))))))
        (declare (ftype (function (list) list) convert-rslt2))
        (do ((mark (cdr list) (cdr mark))
             (rslt1 nil)
             (rslt2 (list (first list)))
             (last-dgop (dgop1 (gnrt (first list)))))
            ((endp mark)
             (push (convert-rslt2 rslt2) rslt1)
             (setf rslt1 (apply #'nconc rslt1))
             (make-cmbn :degr degr
                        :list rslt1))
          (declare (type list mark rslt1 rslt2) (type fixnum last-dgop))
          (let* ((term (car mark))
                 (dgop1 (dgop1 (gnrt term))))
            (declare (type term term) (type fixnum dgop1))
            (cond ((eql last-dgop dgop1)
                   (push term rslt2))
                  (t
                   (setf last-dgop dgop1)
                   (push (convert-rslt2 rslt2) rslt1)
                   (setf rslt2 (list term))))))))))

#|
(cat-init)
(setf cp (crts-prdc (delta 2) (delta 2)))
(setf tp (tnsr-prdc (delta 2) (delta 2)))
(setf cb (basis cp 2))
(length *)
(setf tb (basis tp 2))
(length *)
(setf cb (remove-if-not #'(lambda (crpr)
                            (eq :crtc (stts (ez-vf 2 crpr))))
                        cb))
(length *)
(setf cmbn (make-cmbn :degr 2
                      :list (mapcar #'(lambda (crpr)
                                        (term 1 crpr))
                              cb)))
(control (cmpr cp) cmbn)
(control (cmpr cp) (make-cmbn :degr 2
                              :list (reverse (cmbn-list cmbn))))  ;; error
(ez-isof cmbn)
(2cmbn-sbtr (cmpr tp)
            (make-cmbn :degr 2
                       :list (mapcar #'(lambda (tnpr)
                                        (term 1 tnpr))
                               tb))
            *)
(control (cmpr tp) **)
(control (cmpr tp) (make-cmbn :degr 2
                              :list (reverse (cmbn-list ***))))
|#

(DEFUN CHCM-VF-REDUCTION-F-IMPL (chcm vf isof &aux
                                      (cmpr (cmpr chcm))
                                      (dffr (dffr chcm))
                                      (hh (chcm-vf-reduction-h chcm vf)))
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type function isof)
           ;;   (function (cmbn) cmbn)
           (type cmprf cmpr)
           (type morphism isof dffr hh))
  (the intr-mrph
    (flet
        ((rslt
          (cmbn)
          (declare (type cmbn cmbn))
          (the cmbn
            (let ((cmbn-sorc (cmbn-select-stts vf cmbn :sorc))
                  (cmbn-crtc (cmbn-select-stts vf cmbn :crtc)))
              (declare (type cmbn cmbn-sorc cmbn-crtc))
              (setf cmbn-sorc (? hh cmbn-sorc))
              (setf cmbn-sorc (? dffr cmbn-sorc))
              (setf cmbn-sorc (cmbn-select-stts vf cmbn-sorc :crtc))
              (setf cmbn-crtc (2cmbn-sbtr cmpr cmbn-crtc cmbn-sorc))
              (setf cmbn-crtc (funcall isof cmbn-crtc))
              cmbn-crtc))))
      (declare (ftype (function (cmbn) cmbn) rslt))
      #'rslt)))

#|
(cat-init)
(setf x (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf isof #'ez-isof)
(setf f (chcm-vf-reduction-f-impl x vf isof))
(funcall f (cmbn 5 1 (crpr 0 63 0 63)))
(funcall f (cmbn 2 1 (crpr 1 3 2 3)))
(funcall f (cmbn 4 1 (crpr 3 7 (* 3 4) 7)))
(funcall f (cmbn 6 1 (crpr 7 15 (* 7 8) 15)))
(funcall f (cmbn 8 1 (crpr 15 31 (* 15 16) 31)))
(funcall f (cmbn 10 1 (crpr 31 63 (* 31 32) 63)))
(funcall f (cmbn 10 1 (crpr (* 31 32) 63 31 63)))
|#

(DEFUN CHCM-VF-REDUCTION-F (chcm vf bcc isof)
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type chain-complex bcc)
           (type function isof))
  ;;            (function (cmbn) cmbn)
  (the morphism
    (build-mrph
     :sorc chcm :trgt bcc :degr 0
     :intr (chcm-vf-reduction-f-impl chcm vf isof)
     :strt :cmbn
     :orgn `(chcm-vf-reduction-f ,chcm ,vf ,bcc))))

#|
(cat-init)
(setf x (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf isof #'ez-isof)
(setf bcc (tnsr-prdc (delta 5) (delta 5)))
(setf f (chcm-vf-reduction-f x vf bcc isof))
(? f 5 (crpr 0 63 0 63))
(? f 2 (crpr 1 3 2 3))
(? f 4 (crpr 3 7 (* 3 4) 7))
(? f 6 (crpr 7 15 (* 7 8) 15))
(? f 8 (crpr 15 31 (* 15 16) 31))
(? f 10 (crpr 31 63 (* 31 32) 63))
(? f 10 (crpr (* 31 32) 63 31 63))
|#

(DEFUN CHCM-VF-REDUCTION-G-IMPL (chcm vf isog &aux
                                      (dffr (dffr chcm))
                                      (hh (chcm-vf-reduction-h chcm vf))
                                      (cmpr (cmpr chcm)))
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type morphism dffr hh)
           (type function isog)
           ;;   (function (cmbn) cmbn)
           (type cmprf cmpr))
  (the intr-mrph
    (flet
        ((rslt
          (cmbn)
          (declare (type cmbn cmbn))
          (setf cmbn (funcall isog cmbn))
          (the cmbn
            (with-slots (degr list) cmbn
              (declare (type fixnum degr) (type list list))
              (2cmbn-sbtr
               cmpr
               cmbn
               (? hh
                  (cmbn-select-stts vf
                                    (? dffr cmbn)
                                    :sorc)))))))
      (declare (ftype (function (cmbn) cmbn) rslt))
      #'rslt)))

#| ;; Debugging
(cat-init)
(setf dd (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf h (chcm-vf-reduction-h dd vf))
(? h 3 (crpr 6 3 0 15))
(setf isog #'ez-isog)
(setf g (chcm-vf-reduction-g-impl dd vf isog))
(funcall g (cmbn 4 1 (tnpr 1 3 3 15)))
(funcall g (cmbn 4 1 (tnpr 1 3 3 15)))
|#

(DEFUN CHCM-VF-REDUCTION-G (chcm vf bcc isog)
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type function isog))
  ;;            (function (cmbn) cmbn)
  (the morphism
    (build-mrph :sorc bcc :trgt chcm :degr 0
                :intr (chcm-vf-reduction-g-impl chcm vf isog)
                :strt :cmbn
                :orgn `(chcm-vf-reduction-g ,chcm ,vf ,bcc))))

#|
(cat-init)
(setf dd (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf h (chcm-vf-reduction-h dd vf))
(? h 3 (crpr 6 3 0 15))
(setf isog #'ez-isog)
(setf g (chcm-vf-reduction-g dd vf (tnsr-prdc (delta 5) (delta 5)) isog))
(? g (cmbn 4 1 (tnpr 1 3 3 15)))
(? g (cmbn 4 1 (tnpr 1 3 3 15)))
|#

(DEFUN EZ-ISOG (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
    (with-slots (degr list) cmbn
      (declare (type fixnum degr) (type list list))
      (unless list
        (return-from ez-isog cmbn))
      (flet
          ((convert-rslt2
            (list)
            (declare (type list list))
            (assert list)
            (the list
              (with-slots (degr1 degr2) (gnrt (first list))
                (declare (type fixnum degr1 degr2))
                (let ((dgop1 (ash (mask degr2) degr1))
                      (dgop2 (mask degr1)))
                  (declare (type fixnum dgop1 dgop2))
                  (mapcar #'(lambda (term)
                              (declare (type term term))
                              (the term
                                (with-term (cffc tnpr) term
                                  (with-slots (gnrt1 gnrt2) tnpr
                                    (declare (type gmsm gnrt1 gnrt2))
                                    (term cffc (crpr dgop1 gnrt1 dgop2 gnrt2))))))
                    (nreverse list)))))))
        (declare (ftype (function (list) list) convert-rslt2))
        (do ((mark (cdr list) (cdr mark))
             (rslt1 nil)
             (rslt2 (list (first list)))
             (last-degr (degr1 (gnrt (first list)))))
            ((endp mark)
             (push (convert-rslt2 rslt2) rslt1)
             (setf rslt1 (apply #'nconc rslt1))
             (make-cmbn :degr degr
                        :list rslt1))
          (declare (type list mark rslt1 rslt2) (type fixnum last-degr))
          (let* ((term (car mark))
                 (degr1 (degr1 (gnrt term))))
            (declare (type term term) (type fixnum degr1))
            (cond ((eql last-degr degr1)
                   (push term rslt2))
                  (t
                   (setf last-degr degr1)
                   (push (convert-rslt2 rslt2) rslt1)
                   (setf rslt2 (list term))))))))))

#|
(cat-init)
(setf x (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf isog #'ez-isog)
(setf g (chcm-vf-reduction-g-impl x vf isog))
(funcall g (cmbn 4 1 (tnpr 2 7 2 7)))
(setf f (chcm-vf-reduction-f x vf (trgt (cprd x)) #'ez-isof))
(? f **)
(funcall g (cmbn 5 1 (tnpr 2 7 3 15)))
(? f *)
(funcall g (cmbn 6 1 (tnpr 3 15 3 15)))
(? f *)
(dotimes (i 6)
  (dotimes (j 6)
    (assert (cmbn-zero-p
              (2cmbn-sbtr (cmpr (tnsr-prdc (delta 5) (delta 5)))
                          (setf cmbn (cmbn (+ i j) 1 (tnpr i (mask (1+ i)) j (mask (1+ j)))))
                          (print (? f (funcall g cmbn))))))))
(funcall g (cmbn 6 1 (tnpr 3 15 3 15)))
(funcall g (cmbn 5 1 (tnpr 2 7 3 15)))
(funcall g (cmbn 4 1 (tnpr 1 3 3 15)))
(funcall g (cmbn 4 1 (tnpr 3 15 1 3)))


(funcall g (cmbn 5 1 (tnpr 3 15 2 7)))
(funcall g (cmbn 4 1 (tnpr 2 7 2 7)))
(funcall g (cmbn 2 1 (tnpr 1 3 1 3)))
|#


(DEFUN CHCM-VF-REDUCTION-DP-IMPL (chcm vf bcc isof isog &aux
                                       (dffr (dffr chcm))
                                       (rdct-f (chcm-vf-reduction-f chcm vf bcc isof)))
  (declare (type chain-complex chcm)
           (type vector-field vf)
           (type function isof isog)
           ;;   (function (cmbn) cmbn)
           (type morphism dffr rdct-f)
           (type morphism rdct-f))
  (the intr-mrph
    (flet
        ((rslt
          (cmbn)
          (declare (type cmbn cmbn))
          (the cmbn
            (progn
              (setf cmbn (funcall isog cmbn))
              (setf cmbn (? dffr cmbn))
              (setf cmbn (? rdct-f cmbn))
              cmbn))))
      (declare (ftype (function (cmbn) cmbn) rslt))
      #'rslt)))

#|
(cat-init)
(setf dd (crts-prdc (delta 5) (delta 5)))
(setf vf #'ez-vf)
(setf h (chcm-vf-reduction-h dd vf))
(setf bcc (tnsr-prdc (delta 5) (delta 5)))
(setf isof #'ez-isof isog #'ez-isog)
(setf dp (chcm-vf-reduction-dp-impl dd vf bcc isof isog))
(funcall dp (cmbn 5 1 (tnpr 2 7 3 15)))
(funcall dp (cmbn 10 1 (tnpr 5 63 5 63)))
|#

#|
(cat-init)
(setf real-line
  (build-chcm
   :cmpr #'f-cmpr
   :basis :locally-effective
   :bsgn 0
   :intr-dffr #'(lambda (dmns n)
                  (ecase dmns
                    (0 (zero-cmbn -1))
                    (1 (cmbn 0 -1 (1- n) 1 n))))
   :strt :gnrt
   :orgn '(real-line)))
(defun real-line-vf (degr gnrt)
  (ecase degr
    (1 (if (plusp gnrt)
           (vctr :trgt gnrt 1)
         (vctr :trgt (1- gnrt) -1)))
    (0 (cond ((plusp gnrt) (vctr :sorc gnrt 1))
             ((minusp gnrt) (vctr :sorc (1+ gnrt) -1))
             (t (vctr :crtc))))))
(setf new-basis
  #'(lambda (dmns)
      (if (zerop dmns) '(1) nil)))
(setf r (chcm-vf-reduction real-line #'real-line-vf new-basis))
(pre-check-rdct r)
(setf *tc* (cmbn 0 -100 -2 -10 -1 1 0 10 1 100 2))
(setf *bc* (cmbn 0 1 0))
(check-rdct)
(setf *tc* (cmbn 1 -100 -2 -10 -1 1 0 10 1 100 2))
(setf *bc* (cmbn 0 1 0))
(check-rdct)
(homology (bcc r) 0 3)
|#

;;; The key vector field for Eilenberg-Zilber
;;; very carefully implemented.

(DEFUN EZ-VF (dmns crpr)
  (declare (type crpr crpr))
  (the (values vctr fixnum)
    (with-slots (dgop1 gmsm1 dgop2 gmsm2) crpr
      (declare (type fixnum dgop1 dgop2) (type gmsm gmsm1 gmsm2))
      (let ((last-diag (integer-length (- (mask dmns) dgop1 dgop2)))
            (last-bend (integer-length (logand dgop1 (ash dgop2 -1)))))
        (declare (type fixnum last-diag last-bend))
        (cond ((< last-diag last-bend)
               (values
                (vctr :trgt
                      (let ((mask-1 (mask (1- last-bend)))
                            (mask (mask last-bend))
                            (mask+1 (mask (1+ last-bend))))
                        (declare (type fixnum mask-1 mask mask+1))
                        (crpr (logior (ash (logandc2 dgop1 mask) -1) (logand dgop1 mask-1))
                              gmsm1
                              (logior (ash (logandc2 dgop2 mask+1) -1) (logand dgop2 mask))
                              gmsm2))
                      (-1-expt-n last-bend))
                last-bend))
              ((< last-bend last-diag)
               (values
                (vctr :sorc
                      (let ((mask (mask (1- last-diag))))
                        (declare (type fixnum mask))
                        (crpr (logior (the fixnum (ash (logandc2 dgop1 mask) +1))
                                      (2-exp (1- last-diag))
                                      (logand dgop1 mask))
                              gmsm1
                              (logior (the fixnum (ash (logandc2 dgop2 mask) +1))
                                      (2-exp last-diag)
                                      (logand dgop2 mask))
                              gmsm2))
                      (-1-expt-n last-diag))
                last-diag))
              (t (values (vctr :crtc) -1)))))))

#|
(dotimes (i 16)
  (dotimes (j 16)
    (when (zerop (logand i j))
      (format t "~%Dgop1 = ~A   Dgop2 = ~A   Vctr = ~A"
        (dgop-int-ext i) (dgop-int-ext j)
        (multiple-value-list (ez-vf 4 (crpr i 'gmsm1 j 'gmsm2)))))))
|#

#|
(cat-init)
(setf d3d3 (crts-prdc (delta 3) (delta 3)))
(setf r (chcm-vf-reduction d3d3 #'ez-vf))
(pre-check-rdct r)
(setf *tc* (cmbn 3 1 (crpr 0 15 0 15)))
(setf *bc* (cmbn 6 1 (crpr 56 15 7 15)))
(check-rdct)
|#

#|
(cat-init)
(setf d (deltab))

(setf r0 (chcm-vf-reduction (crts-prdc d (loop-space d)) #'ez-vf))
(setf r1 (szczarba d))
(setf r2 (chcm-vf-reduction (twisted-crts-prdc d) #'ez-vf))


(setf twtn1 (bcc r1))
(setf twtn2 (bcc r2))

(setf gnrt1 (tnpr 5 63 5 (loop3 0 127 1)))
(time (? twtn1 10 gnrt1))
(length (cmbn-list *))
(setf gnrt2 (crpr 992 63 31 (loop3 0 127 1)))
(time (? twtn2 10 gnrt2))
(length (cmbn-list *))

(setf gmsm (crpr 0 127 0 (loop3 0 255 1)))
(time (h r1 6 gmsm))
(length (cmbn-list *))
(time (h r2 6 gmsm))
(length (cmbn-list *))

;;; Identification de l'erreur d'isomorphisme
;;;   tordu \cong non-tordu.

(setf gmsm (crpr 0 7 0 (loop3 0 15 1)))
(h r0 2 gmsm)
(h r2 2 gmsm)
(face (tcc r0) 3 3 (crpr 2 7 4 (loop3 0 15 1)))
(ez-vf 2 (gmsm *))
|#

(DEFUN CHCM-VF-REDUCTION (chcm vf bcc isof isog)
  (declare (type chain-complex chcm bcc)
           (type vector-field vf)
           (type function isof isog))
  ;;            (function (cmbn) cmbn)
  (the reduction
    (let ((new-bcc (build-chcm :cmpr (cmpr bcc)
                               :basis (basis bcc)
                               :intr-dffr (chcm-vf-reduction-dp-impl
                                           chcm vf bcc isof isog)
                               :strt :cmbn
                               :orgn `(chcm-vf-reduction-new-bcc ,chcm ,vf ,bcc ,isof ,isog))))
      (declare (type chain-complex new-bcc))
      (when (slot-boundp bcc 'bsgn)
        (setf (slot-value new-bcc 'bsgn) (bsgn bcc)))
      ;; ?? (slot-makunbound new-chcm 'bsgn))
      (setf (slot-value new-bcc 'grmd) (grmd bcc))                   
      (build-rdct
       :f (chcm-vf-reduction-f chcm vf new-bcc isof)
       :g (chcm-vf-reduction-g chcm vf new-bcc isog)
       :h (chcm-vf-reduction-h chcm vf)
       :orgn `(chcm-vf-reduction ,chcm ,vf)))))

#|
(cat-init)
(setf rdct (chcm-vf-reduction (crts-prdc (delta-infinity) (delta-infinity))
                              #'ez-vf
                              (tnsr-prdc (delta-infinity) (delta-infinity))
                              #'ez-isof #'ez-isog))
(setf *tc* (cmbn 2 4 (crpr 0 7 0 7)))
(setf *bc* (cmbn 4 44 (tnpr 2 7 2 7)))
(pre-check-rdct rdct)
(check-rdct)
|#
           

;; The next functions are used to *deduce* a vector field
;; from a *given* reduction. See Section 2.7 in the paper.

(DEFUN COMMON-FACE (rdct dmns gmsm &aux
                         (smst (tcc rdct))
                         (hmtp (h rdct))                                    
                         (face (face smst))
                         (cmpr (cmpr smst)))
  (declare (type reduction rdct)
           (type simplicial-set smst)
           (type morphism hmtp)
           (type fixnum dmns)
           (type gmsm gmsm)
           (type face face)
           (type cmprf cmpr))
  (the list
    (with-slots (degr list) (? hmtp dmns gmsm)
      (declare (type fixnum degr) (type list list))
      (let ((count 0)
            (rslt +empty-list+))
        (declare (type fixnum count) (type list rslt))
        (dolist (term list)
          (declare (type term term))
          (let ((gmsm2 (gnrt term)))
            (declare (type gmsm gmsm2))
            (dotimes (i (1+ degr))
              (declare (type fixnum i))
              (let ((iface (funcall face i degr gmsm2)))
                (declare (type absm iface))
                (when (and (non-degenerate-p iface)
                           (eq :equal (funcall cmpr gmsm (gmsm iface))))
                  (push (list i gmsm2) rslt)
                  (incf count))))))
        (return-from common-face (cons count rslt))))))

#|
(cat-init)
(setf x (crts-prdc (deltab) (deltab)))
(setf r (chcm-vf-reduction x #'ez-vf))
(setf h (h r))
(? h 2 (crpr 0 7 0 7))
(common-face r 2 (crpr 0 7 0 7))
|#

;;; EXPERIENCE REDUCTION (LOOP-SPACE (DELTAB)) => (COBAR (DELTAB))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(? h 2 (loop3 0 15 1))
(common-face r 2 (loop3 0 15 1))
|#

(DEFUN SOURCE-P (rdct dmns gmsm)
  (declare (type reduction rdct)
           (type fixnum dmns)
           (type gmsm gmsm))
  (the (or null vctr)
    (let ((common-face (common-face rdct dmns gmsm)))
      (declare (type list common-face))
      (case (first common-face)
        (0 nil)
        (1 (vctr :sorc (cadadr common-face) (caadr common-face)))
        ;;; the returned "incidence number" is in fact the face index.
        (t (error "Abnormal situation."))))))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(source-p r 1 (loop3 0 7 1))
(source-p r 2 (loop3 0 15 1))
(source-p r 3 (loop3 0 31 1))
(source-p r 4 (loop3 0 63 1))
(source-p r 2 (loop3 0 15 1 0 240 1))
|#

(DEFUN TARGET-P (rdct dmns gmsm &aux
                      (smst (tcc rdct)))
  (declare (type reduction rdct)
           (type simplicial-set smst)
           (type fixnum dmns)
           (type gmsm gmsm))
  (the (or null vctr)
    (progn
      (unless (plusp dmns)
        (return-from target-p nil))
      (let ((rslt nil))
        (declare (type (or null vctr) rslt))
        (dotimes (i (1+ dmns))
          (declare (type fixnum i))
          (let ((iface (face smst i dmns gmsm))
                (flag nil))
            (declare (type (or absm gmsm) iface) (type boolean flag))
            (unless (degenerate-p iface)
              (setf iface (gmsm iface))
              (let ((common-face (common-face rdct (1- dmns) iface)))
                (declare (type list common-face))
                (case (first common-face)
                  (0)
                  (1
                   (when (eq :equal (cmpr smst gmsm (cadadr common-face)))
                     (when flag (error "Abnormal-situation."))
                     (setf flag t)
                     (setf rslt (vctr :trgt iface i))))
                  ;; the returned "incidence number" is in fact the face index.
                  (t (error "Abnormal-situation.")))))))
        rslt))))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(target-p r 0 (loop3))
(target-p r 2 (loop3 1 7 1 2 13 1))
(source-p r 1 (loop3 0 7 1 0 13 1))
(target-p r 2 (loop3 2 7 1 0 15 1))
(source-p r 2 (loop3 2 7 1 0 15 1))
(target-p r 2 (loop3 1 7 1 2 14 1))
(source-p r 1 (loop3 0 7 1 0 14 1))
(target-p r 2 (loop3 2 7 1 1 14 1))
(source-p r 2 (loop3 2 7 1 1 14 1))
|#

(DEFUN CRITICAL-P (rdct dmns gmsm)
  (declare (type reduction rdct)
           (type fixnum dmns)
           (type gmsm gmsm))
  (not (or (source-p rdct dmns gmsm)
           (target-p rdct dmns gmsm))))

(DEFUN STATUS (rdct dmns gmsm)
  (declare (type reduction rdct)
           (type fixnum dmns)
           (type gmsm gmsm))
  (the vctr
    (or (source-p rdct dmns gmsm)
        (target-p rdct dmns gmsm)
        (vctr :crtc))))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(target-p r 0 (loop3))
(status r 0 (loop3))
(status r 1 (loop3 0 7 1))
(status r 1 (loop3 0 7 2))
(status r 1 (loop3 0 7 10))
(status r 1 (loop3 0 7 -1))
(status r 1 (loop3 0 7 -10))

(status r 2 (loop3 0 15 1))
(status r 2 (loop3 0 15 2))
(status r 2 (loop3 0 15 3))
(status r 2 (loop3 0 15 10))
(status r 2 (loop3 0 15 -1))
(status r 2 (loop3 0 15 -2))
(status r 2 (loop3 0 15 -3))
(status r 2 (loop3 0 15 -10))

(status r 2 (loop3 0 15 1))
|#

(DEFUN PRINT-TREE (tree)
  (declare (type list tree))
  (the (values)
    (let ((ind 0))
      (labels ((main
                (tree)
                (declare (type list tree))
                (the (values)
                  (progn
                    (dotimes (i ind) (format t " "))
                    (format t "(")
                    (dolist (item tree)
                      (cond ((listp item)
                             (incf ind 3)
                             (terpri)
                             (main item)
                             (decf ind 3))
                            (t (format t "~A " item))))))))
        (terpri)
        (main tree)))))

#|
(print-tree (v-path r 2 (loop3 0 15 1)))
(print-tree (v-path r 3 (loop3 0 31 1)))
|#


(DEFUN V-PATH (rdct dmns gmsm &aux (smst (tcc rdct)))
  (declare (type reduction rdct)
           (type simplicial-set smst)
           (type fixnum dmns)
           (type gmsm gmsm))
  (the list
    (let ((status (status rdct dmns gmsm)))
      (declare (type vctr status))
      (ecase (stts status)
        (:sorc
         (let ((target (valu status))
               (indx (incd status)))
           (declare (type gmsm target) (type fixnum indx))
           (let ((others +empty-list+))
             (declare (type list others))
             (dotimes (i (+ 2 dmns))
               (declare (type fixnum i))
               (unless (eql i indx)
                 (let ((iface (face smst i (1+ dmns) target)))
                   (declare (type (or absm gmsm) iface))
                   (unless (degenerate-p iface)
                     (setf iface (gmsm iface))
                     (let ((source-p (source-p rdct dmns iface)))
                       (when source-p
                         (push (cons i (v-path rdct dmns iface)) others)))))))
             (return-from v-path
               (list* gmsm indx target (nreverse others))))))
        ((:trgt :crtc) nil)))))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(v-path r 1 (loop3 0 7 1))
(v-path r 2 (loop3 0 15 1))
(v-path r 3 (loop3 0 31 1))
|#

(DEFUN PRINT-VPATH (rdct dmns gmsm)
  (declare (type reduction rdct)
           (type fixnum dmns)
           (type gmsm gmsm))
  (print-tree (v-path rdct dmns gmsm)))

;; Finding the critical cell associated to some
;; cell of the small complex, valid only if the
;; reduction actually comes from a vector field.

(DEFUN FIND-CRTC (rdct degr gnrt)
  (declare (type reduction rdct)
           (type fixnum degr)
           (type gnrt gnrt))
  (the gmsm
    (let ((ggnrt (g rdct degr gnrt))
          (rslt nil)
          (flag nil))      
      (declare (type cmbn ggnrt)
               (type list rslt)
               (type boolean flag))
      (with-slots (list) ggnrt
        (declare (type list list))
        (dolist (term list)
          (declare (type term term))
          (let ((vctr (status rdct degr (gnrt term))))
            (ecase (stts vctr)
              (:crtc (when flag
                       (error "Abnormal Situation."))
                     (setf flag t)
                     (setf rslt (list vctr (gnrt term))))
              (:trgt)
              (:sorc (error "Abnormal Situation."))))))
      (return-from find-crtc rslt))))

#|
(cat-init)
(setf d (deltab2))
(setf x (loop-space d))
(setf hmeq (ls-left-hmeq d))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf r (build-rdct :f f :g g :h h :orgn '(essai)))
(find-crtc r 0 (allp))
(find-crtc r 1 (allp 1 7))
(find-crtc r 2 (allp 2 15))
(find-crtc r 3 (allp 3 31))
(find-crtc r 4 (allp 4 63))

(find-crtc r 1 (allp 1 7))
(find-crtc r 2 (allp 1 7 1 7))
(find-crtc r 3 (allp 1 7 1 7 1 7))
(find-crtc r 3 (allp 1 7 2 15))
(find-crtc r 3 (allp 2 15 1 7))
|#

