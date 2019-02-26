;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM
;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM
;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM

(IN-PACKAGE #:cat)

(PROVIDE "lp-space-efhm")

(DEFUN LS-HAT-U-U (space
                   &aux (cobar (cobar space))
                     (loop-space (loop-space space))
                     (spac-tnsr-lpsp (tnsr-prdc space loop-space)))
  (declare
   (type simplicial-set space)
   (type chain-complex cobar spac-tnsr-lpsp)
   (type simplicial-group loop-space))
  (the chain-complex
       (tnsr-prdc cobar spac-tnsr-lpsp)))


(DEFUN LS-HAT-LEFT-PERTURBATION-INTR (space)
  (declare (type simplicial-set space))
  (let ((cprd (dgnl space)))
    (flet ((rslt (degr tnpr)
             (declare
              (fixnum degr)
              (type tnpr tnpr))
             (with-tnpr
                 (degr1 allp1 nil tnpr2) tnpr
                 (with-tnpr
                     (degr21 gmsm21 degr22 loop22) tnpr2
                     (let ((cprd (cmbn-list (gnrt-? cprd degr21 gmsm21))))
                       (declare (list cprd))
                       (setf cprd (rest cprd))    ;;; because \bar{A}
                       (make-cmbn
                        :degr (1- degr)
                        :list
                        (mapcar
                         #'(lambda (term)
                             (declare (type term term))
                             (with-term
                                 (cffc tnpr21) term
                                 (with-tnpr
                                     (degr211 gmsm211 degr212 gmsm212) tnpr21
                                     (decf degr211)
                                     (term cffc (tnpr (+ degr1 degr211)
                                                      (make-allp
                                                       :list
                                                       (append
                                                        (allp-list allp1)
                                                        (list (cbgn degr211
                                                                    gmsm211))))
                                                      (+ degr212 degr22)
                                                      (tnpr degr212 gmsm212
                                                            degr22 loop22))))))
                         cprd)))))))
      (the intr-mrph #'rslt))))


(DEFUN LS-HAT-LEFT-PERTURBATION (space)
  (declare (type simplicial-set space))
  (the morphism
       (let ((ls-hat-u-u (ls-hat-u-u space)))
         (build-mrph
          :sorc ls-hat-u-u
          :trgt ls-hat-u-u
          :degr -1
          :intr (ls-hat-left-perturbation-intr space)
          :strt :gnrt
          :orgn `(ls-hat-left-perturbation ,space)))))

(DEFUN LS-HAT-T-U (space
                   &aux (ls-hat-u-u (ls-hat-u-u space))
                     (ls-hat-left-perturbation
                      (ls-hat-left-perturbation space)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-u-u)
   (type morphism ls-hat-left-perturbation))
  (the chain-complex
       (progn
         (setf (slot-value ls-hat-left-perturbation 'sorc) ls-hat-u-u
               (slot-value ls-hat-left-perturbation 'trgt) ls-hat-u-u)
         ;; because maybe these slots have been modified when constructing
         ;;   the ls-left-hmeq-right-reduction
         (add ls-hat-u-u ls-hat-left-perturbation))))


(DEFUN LS-HAT-RIGHT-PERTURBATION (space
                                  &aux (cobar (cobar space))
                                    (ls-hat-t-u (ls-hat-t-u space)))
  (declare
   (type simplicial-set space)
   (type chain-complex cobar ls-hat-t-u))
  (the morphism
       (multiple-value-bind (szczarba bottom-perturbation)
           (szczarba space)
         (declare
          (ignore szczarba)
          (type morphism bottom-perturbation))
         (let ((rslt (tnsr-prdc (idnt-mrph cobar) bottom-perturbation)))
           (declare (type morphism rslt))
           (setf (slot-value rslt 'sorc) ls-hat-t-u
                 (slot-value rslt 'trgt) ls-hat-t-u)
           rslt))))

(DEFUN LS-HAT-U-T (space
                   &aux (cobar (cobar space))
                     (twisted-tnsr-prdc (twisted-tnsr-prdc space))
                     (ls-hat-u-u (ls-hat-u-u space)))
  (declare
   (type simplicial-set space)
   (type chain-complex cobar twisted-tnsr-prdc ls-hat-u-u))
  (the chain-complex
       (let ((rslt (tnsr-prdc cobar twisted-tnsr-prdc)))
         (declare (type chain-complex rslt))
         (setf (slot-value rslt 'grmd) ls-hat-u-u)
         rslt)))


(DEFUN LS-LEFT-HMEQ-HAT (space
                         &aux
                           (ls-hat-u-t (ls-hat-u-t space))
                           (ls-hat-left-perturbation
                            (ls-hat-left-perturbation space)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-u-t)
   (type morphism ls-hat-left-perturbation))
  (the chain-complex
       (add ls-hat-u-t ls-hat-left-perturbation)))


(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-F (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (let ((rslt +empty-list+))
         (declare (list rslt))
         (dolist (term (cmbn-list cmbn))
           (declare (type term term))
           (with-term
               (cffc tnpr) term
               (with-tnpr (degr1 nil nil tnpr2) tnpr
                          (if (zerop degr1)
                              (with-tnpr
                                  (degr21 nil nil loop22) tnpr2
                                  (if (zerop degr21)
                                      (push (term cffc loop22) rslt)
                                      (return)))
                              (return)))))
         (make-cmbn :degr (cmbn-degr cmbn)
                    :list (nreverse rslt)))))


(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-F (space
                                          &aux (ls-hat-t-u (ls-hat-t-u space))
                                            (loop-space (loop-space space)))
  (declare
   (type simplicial-set space loop-space)
   (type chain-complex ls-hat-t-u))
  (the morphism
       (build-mrph
        :sorc ls-hat-t-u
        :trgt loop-space
        :degr 0
        :intr #'ls-pre-left-hmeq-left-reduction-intr-f
        :strt :cmbn
        :orgn `(ls-pre-left-hmeq-left-reduction-f ,space))))

(DEFUN LS-LEFT-HMEQ-LEFT-REDUCTION-G-INTR (bspn)
  (declare (type gmsm bspn))
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (the cmbn
                (with-cmbn
                    (degr list) cmbn
                    (make-cmbn :degr degr
                               :list (mapcar
                                      #'(lambda (term)
                                          (declare (type term term))
                                          (with-term
                                              (cffc loop) term
                                              (term cffc
                                                    (tnpr
                                                     0 +null-allp+
                                                     degr
                                                     (tnpr 0 bspn degr
                                                           loop)))))
                                      list))))))
    (the intr-mrph #'rslt)))


(DEFUN LS-LEFT-HMEQ-LEFT-REDUCTION-G (space
                                      &aux (bspn (bspn space))
                                        (ls-hat-t-u (ls-hat-t-u space))
                                        (loop-space (loop-space space)))
  (declare
   (type simplicial-set space loop-space)
   (type gmsm bspn)
   (type chain-complex ls-hat-t-u))
  (the morphism
       (build-mrph
        :sorc loop-space
        :trgt ls-hat-t-u
        :degr 0
        :intr (ls-left-hmeq-left-reduction-g-intr bspn)
        :strt :cmbn
        :orgn `(ls-left-hmeq-left-reduction-g ,space))))


(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-H-INTR
    (space
     &aux (ls-hat-t-u (ls-hat-t-u space))
       (cmpr (cmpr ls-hat-t-u)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-t-u)
   (type cmprf cmpr))
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (the cmbn
                (with-cmbn
                    (degr list) cmbn
                    (let ((rslt (zero-cmbn (1+ degr))))
                      (declare (type cmbn rslt))
                      (dolist (term list)
                        (declare (type term term))
                        (with-term
                            (cffc tnpr) term
                            (with-tnpr
                                (degr1 allp1 degr2 tnpr2) tnpr
                                (unless (zerop degr1)
                                  (with-tnpr
                                      (degr21 nil degr22 loop22) tnpr2
                                      (when (zerop degr21)
                                        (setf allp1 (allp-list allp1)) ;;;+++
                                        (let ((last-cbgn (car (last allp1))))
                                          (declare (type cbgn last-cbgn))
                                          (with-cbgn
                                              (degrl gmsml) last-cbgn
                                              (dstr-add-term-to-cmbn
                                               cmpr cffc
                                               (tnpr (- degr1 degrl)
                                                     (make-allp
                                                      :list (butlast allp1))
                                                     (+ degr2 degrl 1)
                                                     (tnpr (1+ degrl) gmsml
                                                           degr22 loop22))
                                               rslt)))))))))
                      rslt)))))
    (the intr-mrph #'rslt)))


(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-H (space
                                          &aux (ls-hat-t-u (ls-hat-t-u space)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-t-u))
  (the morphism
       (build-mrph
        :sorc ls-hat-t-u
        :trgt ls-hat-t-u
        :degr +1
        :intr (ls-pre-left-hmeq-left-reduction-h-intr space)
        :strt :cmbn
        :orgn `(ls-pre-left-hmeq-left-reduction-h ,space))))


(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION (space)
  (declare (type simplicial-set space))
  (the reduction
       (build-rdct
        :f (ls-pre-left-hmeq-left-reduction-f space)
        :g (ls-left-hmeq-left-reduction-g space)
        :h (ls-pre-left-hmeq-left-reduction-h space)
        :orgn `(ls-pre-left-hmeq-left-reduction ,space))))


(DEFUN LS-LEFT-HMEQ-LEFT-REDUCTION (space
                                    &aux (ls-pre-left-hmeq-left-reduction
                                          (ls-pre-left-hmeq-left-reduction
                                           space))
                                      (ls-hat-right-perturbation
                                       (ls-hat-right-perturbation space)))
  (declare
   (type simplicial-set space)
   (type reduction ls-pre-left-hmeq-left-reduction)
   (type morphism ls-hat-right-perturbation))
  (the reduction
       (progn
         (dstr-change-sorc-trgt ls-hat-right-perturbation
                                :new-sorc (tcc ls-pre-left-hmeq-left-reduction)
                                :new-trgt (tcc ls-pre-left-hmeq-left-reduction))
         (let ((rslt (special-bpl ls-pre-left-hmeq-left-reduction
                                  ls-hat-right-perturbation)))
           (declare (type reduction rslt))
           (with-slots (tcc f g h) rslt
             (setf tcc (ls-left-hmeq-hat space)
                   (slot-value f 'sorc) tcc
                   (slot-value g 'trgt) tcc
                   (slot-value h 'sorc) tcc
                   (slot-value h 'trgt) tcc)
             rslt)))))


;;; -[CLISP
#|
(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-F (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (let ((tail (member 0 list :test #'= :key #'cadddr)))
                    (make-cmbn :degr degr
                               :list (mapcar
                                      #'(lambda (term)
                                          (declare (type term term))
                                          (with-term (cffc tnpr) term
                                                     (term cffc (gnrt1 tnpr))))
                                      tail))))))
|#
;;;   LISP]-
;;; +[CLISP
(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-F (cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (let ((tail (member 0 list
                                      :key #'(lambda (term)
                                               (declare (type term term))
                                               (degr2 (gnrt term))))))
                    (make-cmbn :degr degr
                               :list (mapcar
                                      #'(lambda (term)
                                          (declare (type term term))
                                          (with-term (cffc tnpr) term
                                                     (term cffc (gnrt1 tnpr))))
                                      tail))))))
;;;   CLISP]+


(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-F (space
                                           &aux (ls-hat-u-t (ls-hat-u-t space))
                                             (cobar (cobar space)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-u-t cobar))
  (the morphism
       (build-mrph
        :sorc ls-hat-u-t :trgt cobar :degr 0
        :intr #'ls-pre-left-hmeq-right-reduction-intr-f
        :strt :cmbn
        :orgn `(ls-pre-left-hmeq-right-reduction-f ,space))))


(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-INTR-G (bspn)
  (declare (type gmsm bspn))
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (the cmbn
                (with-cmbn
                    (degr list) cmbn
                    (let ((bsgn (tnpr 0 bspn 0 +null-loop+)))
                      (declare (type tnpr bsgn))
                      (make-cmbn :degr degr
                                 :list (mapcar
                                        #'(lambda (term)
                                            (declare (type term term))
                                            (with-term
                                                (cffc gnrt) term
                                                (term cffc (tnpr degr gnrt 0
                                                                 bsgn))))
                                        list)))))))
    (the intr-mrph #'rslt)))


(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-G (space
                                           &aux (ls-hat-u-t (ls-hat-u-t space))
                                             (cobar (cobar space))
                                             (bspn (bspn space)))
  (declare
   (type simplicial-set space)
   (type chain-complex ls-hat-u-t cobar)
   (type gmsm bspn))
  (the morphism
       (build-mrph
        :sorc cobar :trgt ls-hat-u-t :degr 0
        :intr (ls-pre-left-hmeq-right-reduction-intr-g bspn)
        :strt :cmbn
        :orgn `(ls-pre-left-hmeq-right-reduction-g ,space))))

(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION-H (space
                                           &aux (cobar (cobar space))
                                             (tnpr-contraction
                                              (tnpr-contraction space)))
  (declare
   (type simplicial-set space)
   (type chain-complex cobar)
   (type morphism tnpr-contraction))
  (the morphism
       (tnsr-prdc (idnt-mrph cobar) tnpr-contraction)))


(DEFUN LS-PRE-LEFT-HMEQ-RIGHT-REDUCTION (space)
  (declare (type simplicial-set space))
  (the reduction
       (build-rdct
        :f (ls-pre-left-hmeq-right-reduction-f space)
        :g (ls-pre-left-hmeq-right-reduction-g space)
        :h (ls-pre-left-hmeq-right-reduction-h space)
        :orgn `(ls-pre-left-hmeq-right-reduction ,space))))


(DEFUN LS-LEFT-HMEQ-RIGHT-REDUCTION (space
                                     &aux (pre-reduction
                                           (ls-pre-left-hmeq-right-reduction
                                            space))
                                       (perturbation
                                        (ls-hat-left-perturbation space)))
  (declare
   (type simplicial-set space)
   (type reduction pre-reduction)
   (type morphism perturbation))
  (the reduction
       (progn
         (setf (slot-value perturbation 'sorc) (tcc pre-reduction)
               (slot-value perturbation 'trgt) (tcc pre-reduction))
         (special-bpl pre-reduction perturbation))))


(DEFUN LS-LEFT-HMEQ (space)
  (declare (type simplicial-set space))
  (the homotopy-equivalence
       (build-hmeq
        :lrdct (ls-left-hmeq-left-reduction space)
        :rrdct (ls-left-hmeq-right-reduction space)
        :orgn `(ls-left-hmeq ,space))))


(DEFUN LOOP-SPACE-EFHM (space)
  (declare (type simplicial-set space))
  (let ((ls-left-hmeq (ls-left-hmeq space))
        (right-hmeq (cobar (efhm space))))
    (declare (type homotopy-equivalence ls-left-hmeq right-hmeq))
    (cmps ls-left-hmeq right-hmeq)))


(DEFMETHOD SEARCH-EFHM (loop-space (orgn (eql 'loop-space)))
  (declare (type simplicial-set loop-space))
  (loop-space-efhm (second (orgn loop-space))))
