;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM
;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM
;;;  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM  LP-SPACE-EFHM

(IN-PACKAGE "COMMON-LISP-USER")

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
                   (type fixnum degr)
                   (type tnpr tnpr))
                (with-tnpr (degr1 allp1 nil tnpr2) tnpr
                (with-tnpr (degr21 gmsm21 degr22 loop22) tnpr2
                   (let ((cprd (cmbn-list (gnrt-? cprd degr21 gmsm21))))
                      (declare (list cprd))
                      (setf cprd (rest cprd))    ;;; because \bar{A}
                      (make-cmbn :degr (1- degr)
                         :list
                         (mapcar
                            #'(lambda (term)
                                 (declare (type term term))
                                 (with-term (cffc tnpr21) term
                                 (with-tnpr (degr211 gmsm211 degr212 gmsm212) tnpr21
                                    (decf degr211)
                                    (term cffc
                                       (tnpr
                                          (+ degr1 degr211)
                                          (make-allp
                                             :list (append
                                                      (allp-list allp1)
                                                      (list (cbgn degr211 gmsm211))))
                                          (+ degr212 degr22)
                                          (tnpr degr212 gmsm212 degr22 loop22))))))
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

#|
  (cat-init)
  (setf c (ls-hat-t-u (deltab)))
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) gmsm) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (setf allp (random-allp 4))  ;; the degree could be too big
                               ;; then redo
  (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
  (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
  (? c (+ 4 allp-degr) gnrt)
  (? c *)
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
        (unless (>= allp-degr 11)
           (print (? c (+ 4 allp-degr) gnrt))
           (print (? c (? c (+ 4 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 3 15 2 (loop3 0 15 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 2 7 3 (loop3 0 31 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
|#

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

#|
  (cat-init)
  (setf c (ls-hat-u-t (deltab)))
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) gmsm) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (setf allp (random-allp 3))
  (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
  (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
  (? c (+ 4 allp-degr) gnrt)
  (? c *)
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
        (unless (>= allp-degr 11)
           (print (? c (+ 4 allp-degr) gnrt))
           (print (? c (? c (+ 4 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 3 15 2 (loop3 0 15 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 2 7 3 (loop3 0 31 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
|#

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

#|
  (cat-init)
  (setf c (ls-left-hmeq-hat (deltab)))
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) gmsm) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (setf allp (random-allp 3))
  (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
  (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
  (? c (+ 4 allp-degr) gnrt)
  (? c *)
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
        (unless (>= allp-degr 11)
           (print (? c (+ 4 allp-degr) gnrt))
           (print (? c (? c (+ 4 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 3 15 2 (loop3 0 15 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
  (dotimes (i 20)
     (let ((allp (random-allp 3)))
        (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
        (setf gnrt (tnpr allp-degr allp 5 (tnpr 2 7 3 (loop3 0 31 2))))
        (unless (>= allp-degr 10)
           (print (? c (+ 5 allp-degr) gnrt))
           (print (? c (? c (+ 5 allp-degr) gnrt))))))
|#

(DEFUN LS-PRE-LEFT-HMEQ-LEFT-REDUCTION-INTR-F (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (let ((rslt +empty-list+))
         (declare (list rslt))
         (dolist (term (cmbn-list cmbn))
            (declare (type term term))
            (with-term (cffc tnpr) term
            (with-tnpr (degr1 nil nil tnpr2) tnpr
               (if (zerop degr1)
                  (with-tnpr (degr21 nil nil loop22) tnpr2
                     (if (zerop degr21)
                        (push (term cffc loop22) rslt)
                        (return)))
                  (return)))))
         (make-cmbn :degr (cmbn-degr cmbn)
            :list (nreverse rslt)))))

#|
  (ls-pre-left-hmeq-left-reduction-intr-f
    (cmbn 6 100 (tnpr 0 'a 6 (tnpr 0 'b 6 'c))
            50 (tnpr 0 'a 6 (tnpr 0 'b 6 'cc))
            10 (tnpr 0 'a 6 (tnpr 2 'b 4 'c))
            1 (tnpr 2 'a 4 (tnpr 2 'b 2 'cc))))            
|#

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
                (with-cmbn (degr list) cmbn
                   (make-cmbn :degr degr
                      :list (mapcar
                               #'(lambda (term)
                                    (declare (type term term))
                                    (with-term (cffc loop) term
                                       (term cffc
                                          (tnpr
                                             0 +null-allp+
                                             degr (tnpr 0 bspn degr loop)))))
                               list))))))
      (the intr-mrph #'rslt)))

#|
  (setf r (ls-left-hmeq-left-reduction-g-intr '*))
  (funcall r (cmbn 3 4 'loop))
|#

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
             (with-cmbn (degr list) cmbn
               (let ((rslt (zero-cmbn (1+ degr))))
                 (declare (type cmbn rslt))
                 (dolist (term list)
                   (declare (type term term))
                   (with-term (cffc tnpr) term
                   (with-tnpr (degr1 allp1 degr2 tnpr2) tnpr
                     (unless (zerop degr1)
                       (with-tnpr (degr21 nil degr22 loop22) tnpr2
                         (when (zerop degr21)
                           (setf allp1 (allp-list allp1)) ;;;+++
                           (let ((last-cbgn (car (last allp1))))
                             (declare (type cbgn last-cbgn))
                             (with-cbgn (degrl gmsml) last-cbgn
                               (dstr-add-term-to-cmbn cmpr
                                 cffc (tnpr (- degr1 degrl)
                                            (make-allp :list (butlast allp1))
                                            (+ degr2 degrl 1)
                                            (tnpr (1+ degrl) gmsml
                                                  degr22 loop22))
                                 rslt)))))))))
                 rslt)))))
      (the intr-mrph #'rslt)))

#|
  (setf r (ls-pre-left-hmeq-left-reduction-h-intr (deltab)))
  (setf *tnpr-with-degrees* t)
  (funcall r (cmbn 10 1 (tnpr 3 (allp 2 'a 1 'b) 7 (tnpr 5 'c 2 'd)) 
                      1 (tnpr 7 (allp 2 'a 5 'b) 3 (tnpr 0 '* 3 'c))
                      10 (tnpr 8 (allp 4 'aa 4 'bb) 2 (tnpr 0 '* 2 'cc))))
  (funcall r (cmbn 10 1 (tnpr 3 (allp 2 'a 1 'b) 7 (tnpr 5 'c 2 'd)) 
                      1 (tnpr 7 (allp 4 'a 3 'b) 3 (tnpr 0 '* 3 'c))
                      10 (tnpr 8 (allp 3 'aa 5 'bb) 2 (tnpr 0 '* 2 'cc))))
  (setf *tnpr-with-degrees* nil)
|#

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

#|
  (cat-init)
  (setf rdct (ls-pre-left-hmeq-left-reduction (deltab)))
  (pre-check-rdct rdct)
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) gmsm) rslt))))
        (make-allp :list rslt)))
  (setf allp (random-allp 3))
  (setf allp-degr (apply #'+ (mapcar #'car (allp-list allp))))
  (setf gnrt (tnpr allp-degr allp 4 (tnpr 2 7 2 (loop3 0 15 2))))
  (setf *tc* (cmbn (+ 4 allp-degr) 1 gnrt))
  (setf *bc* (cmbn 2 1 (loop3 0 15 2)))
  (check-rdct)
  (setf gnrt (tnpr allp-degr allp 4 (tnpr 0 1 4 (loop3 0 (mask 6) 2))))
  (setf *tc* (cmbn (+ 4 allp-degr) 1 gnrt))
  (check-rdct)
  (setf *bc* (cmbn 0 1 (bspn (bcc rdct)))
  (setf *tc* (cmbn 0 1 (bsgn (tcc rdct)))
  (check-rdct)
|#

(DEFUN LS-LEFT-HMEQ-LEFT-REDUCTION (space
                                   &aux (ls-pre-left-hmeq-left-reduction
                                           (ls-pre-left-hmeq-left-reduction space))
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
         (let ((rslt (special-bpl-1 ls-pre-left-hmeq-left-reduction
                        ls-hat-right-perturbation)))
            (declare (type reduction rslt))
            (with-slots (tcc f g h) rslt
               (setf tcc (ls-left-hmeq-hat space)
                     (slot-value f 'sorc) tcc
                     (slot-value g 'trgt) tcc
                     (slot-value h 'sorc) tcc
                     (slot-value h 'trgt) tcc)
               rslt)))))

#|
  (cat-init)
  (setf rdct (ls-left-hmeq-left-reduction (deltab2)))
  (pre-check-rdct rdct)
  (defun a (d1 d2 d3)
     (setf *tc* (cmbn (+ d1 d2 d3)
                 1 (tnpr d1 (allp d1 (mask (+ d1 2)))
                         (+ d2 d3)
                      (tnpr d2 (mask (1+ d2))
                            d3 (loop3 0 (mask (+ d3 2)) 1))))
           *bc* (cmbn d3 1
                  (loop3 0 (mask (+ d3 2)) -1)))
      (check-rdct))
  (a 1 0 1)
  (a 1 1 1)  ;; error because 3 does not exist in deltab2
  (a 1 2 1)
  (a 2 2 1)
|#

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

#|
()
(ls-pre-left-hmeq-right-reduction-intr-f
 (cmbn 3 12 (tnpr 2 'a 1 'b) 14 (tnpr 3 'aa 0 '*))))
|#

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
                (with-cmbn (degr list) cmbn
                   (let ((bsgn (tnpr 0 bspn 0 +null-loop+)))
                      (declare (type tnpr bsgn))
                      (make-cmbn :degr degr
                         :list (mapcar
                                  #'(lambda (term)
                                       (declare (type term term))
                                       (with-term (cffc gnrt) term
                                          (term cffc (tnpr degr gnrt 0 bsgn))))
                                  list)))))))
      (the intr-mrph #'rslt)))

#|
  (setf r (ls-pre-left-hmeq-right-reduction-intr-g '*))
  (funcall r (cmbn 3 4 'a))
|#

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
                                               (tnpr-contraction (tnpr-contraction space)))
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

#|
  (cat-init)
  (setf r (ls-pre-left-hmeq-right-reduction (deltab2)))
  (pre-check-rdct r)
  (setf *tc* (cmbn 0 1 (bsgn (tcc r))))
  (setf *bc* (cmbn 0 1 (bsgn (bcc r))))
  (check-rdct)
  (setf *tc* (cmbn 3 1 (tnpr 3 (allp 3 (mask 5)) 0 (tnpr 0 1 0 +null-loop+))))
  (setf *bc* (cmbn 3 1 (allp 1 7 2 15)))
  (check-rdct)
  (setf *tc* (cmbn 6 1 (tnpr 3 (allp 1 7 2 15) 3 (tnpr 2 7 1 (loop3 0 7 2)))))
  (check-rdct)
  (setf *tc* (cmbn 6 1 (tnpr 3 (allp 1 7 2 15) 3 (tnpr 0 1 3 (loop3 0 (mask 5) 2)))))
  (check-rdct)
|#

(DEFUN LS-LEFT-HMEQ-RIGHT-REDUCTION (space
                                    &aux (pre-reduction
                                            (ls-pre-left-hmeq-right-reduction space))
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
         (special-bpl-1 pre-reduction perturbation))))

#|
  (cat-init)
  (setf r (ls-left-hmeq-right-reduction (deltab2)))
  (pre-check-rdct r)
  (setf *tc* (cmbn 0 1 (bsgn (tcc r))))
  (setf *bc* (cmbn 0 1 (bsgn (bcc r))))
  (check-rdct)
  (setf *tc* (cmbn 3 1 (tnpr 3 (allp 3 (mask 5)) 0 (tnpr 0 1 0 +null-loop+))))
  (setf *bc* (cmbn 3 1 (allp 1 7 2 15)))
  (check-rdct)
  (setf *tc* (cmbn 6 1 (tnpr 3 (allp 1 7 2 15) 3 (tnpr 2 7 1 (loop3 0 7 2)))))
  (check-rdct)
  (setf *tc* (cmbn 6 1 (tnpr 3 (allp 1 7 2 15) 3 (tnpr 0 1 3 (loop3 0 (mask 5) 2)))))
  (check-rdct)
|#

(DEFUN LS-LEFT-HMEQ (space)
   (declare (type simplicial-set space))
   (the equivalence
      (build-hmeq 
         :lrdct (ls-left-hmeq-left-reduction space)
         :rrdct (ls-left-hmeq-right-reduction space)
         :orgn `(ls-left-hmeq ,space))))

#|
(cat-init)
(setf h (ls-left-hmeq (deltab2)))
(setf loop (loop3 0 (mask 5) 2))
(lg h 3 loop)
(rf h *)
(rg h *)
(lf h *)
(setf allp (allp 2 (mask 4) 3 (mask 5)))
(rg h 5 allp)
(lf h *)
(lg h *)
(rf h *)  ;; = allp, but why ?
|#

(DEFUN LS-LEFT-RDCT (space)
  (declare (type simplicial-set space))
  (the reduction
    (let ((hmeq (ls-left-hmeq space)))
      (declare (type equivalence hmeq))
      (with-slots (lf lg lh rf rg rh) hmeq
        (build-rdct :f (cmps rf lg)
                    :g (cmps lf rg)
                    :h (cmps lf (cmps rh lg))
                    :orgn `(ls-left-rdct ,space))))))

(DEFUN LOOP-SPACE-EFHM (space)
  (declare (type simplicial-set space))
  (the effective-homology
    (let ((ls-left-rdct (ls-left-rdct space))
          (right-efhm (cobar (efhm space))))
      (declare (type reduction ls-left-rdct) (type effective-homology right-efhm))
      (etypecase right-efhm
        (chain-complex ls-left-rdct)
        (reduction (cmps right-efhm ls-left-rdct))
        (equivalence (error "EQUIVALENCE NOT YET IMPLEMENTED in LOOP-SPACE-EFHM"))))))

#|
(cat-init)
(setf s4 (sphere 4))
(setf os4 (loop-space s4))
(homology os4 0 10)
(setf oos4 (loop-space os4))
(homology oos4 0 10)
(setf ooos4 (loop-space oos4))
(homology ooos4 0 6)
|#

(DEFMETHOD SEARCH-EFHM (loop-space (orgn (eql 'loop-space)))
  (declare (type simplicial-set loop-space))
  (loop-space-efhm (second (orgn loop-space))))

#|
  (cat-init)
  (setf l (loop-space (sphere 2)))
  (homology l 6)
  (setf oos3 (loop-space (loop-space (sphere 3))))
  (homology oos3 3)
  (setf ooos4 (loop-space (loop-space (loop-space (sphere 4)))))
  (homology ooos4 2)
|#
