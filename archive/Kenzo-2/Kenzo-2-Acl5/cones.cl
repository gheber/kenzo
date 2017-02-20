;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES
;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES
;;;  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES  CONES

(IN-PACKAGE "CAT")

(PROVIDE "cones")

(DEFUN DISPATCH-BICN-CMBN (cmbn)
   (declare (type cmbn cmbn))
   (the (values cmbn cmbn cmbn)
      (with-cmbn (degr list) cmbn
         (let ((listb +empty-list+)
               (listc +empty-list+)
               (listd +empty-list+))
            (declare (list listb listc listd))
            (dolist (term list)
               (declare (type term term))
               (with-term (cffc bicn) term
                  (with-bicn (bcnx ibicn) bicn
                     (ecase bcnx
                        (:bcnb (push (term cffc ibicn) listb))
                        (:bcnc (push (term cffc ibicn) listc))
                        (:bcnd (push (term cffc ibicn) listd))))))
            (values
               (make-cmbn :degr degr :list (nreverse listb))
               (make-cmbn :degr (1+ degr) :list (nreverse listc))
               (make-cmbn :degr degr :list (nreverse listd)))))))

#|
  (dispatch-bicn-cmbn (cmbn 3 3 (bcnb 'b1) 4 (bcnb 'b2) 33 (bcnc 'c) 333 (bcnd 'd)))
|#

(DEFUN BICN-CMBN-CMBNB (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr degr
            :list (do ((rslt +empty-list+)
                       (mark list (cdr mark)))
                      ((endp mark) (nreverse rslt))
                     (declare (list rslt mark))
                     (with--term (cffc bicn) mark
                     (with-bicn (bcnx gnrtB) bicn
                        (if (eq bcnx :bcnB)
                           (push (term cffc gnrtB) rslt)
                           (return (nreverse rslt))))))))))

(DEFUN BICN-CMBN-CMBNC (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr (1+ degr)
            :list (do ((rslt +empty-list+)
                       (mark (member :bcnc list :key #'cadr) (cdr mark)))
                      ((endp mark) (nreverse rslt))
                     (declare (list rslt mark))
                     (with--term (cffc bicn) mark
                     (with-bicn (bcnx gnrtC) bicn
                        (if (eq bcnx :bcnC)
                           (push (term cffc gnrtC) rslt)
                           (return (nreverse rslt))))))))))

(DEFUN BICN-CMBN-CMBND (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr degr
            :list (mapcar
                     #'(lambda (term)
                          (declare (type term term))
                          (with-term (cffc bicn) term
                             (term cffc (ibicn bicn))))
                     (member :bcnd list :key #'cadr))))))

#|
  (bicn-cmbn-cmbnb (cmbn 3 1 (bcnb 'a) 2 (bcnc 'b)))
  (bicn-cmbn-cmbnb (cmbn 3 2 (bcnc 'b)))
  (bicn-cmbn-cmbnc (cmbn 3 1 (bcnb 'a) 2 (bcnc 'b) 3 (bcnd 'c)))
  (bicn-cmbn-cmbnc (cmbn 3 1 (bcnb 'a) 3 (bcnd 'c)))
  (bicn-cmbn-cmbnd (cmbn 3 1 (bcnb 'a) 2 (bcnc 'b) 3 (bcnd 'c)))
  (bicn-cmbn-cmbnd (cmbn 3 2 (bcnb 'b)))
|#

(DEFUN MAKE-BICN-CMBN (cmbnb cmbnc cmbnd)
   (declare (type cmbn cmbnb cmbnc cmbnd))
   (the cmbn
      (with-cmbn (degrb listb) cmbnb
      (with-cmbn (degrc listc) cmbnc
      (with-cmbn (degrd listd) cmbnd
         (unless (= degrb (1- degrc) degrd)
            (error "In MAKE-BICN-CMBN, the degrees are not coherent."))
         (make-cmbn :degr degrb
            :list (nconc
                     (mapcar #'(lambda (termb)
                                  (declare (type term termb))
                                  (with-term (cffc gnrtb) termb
                                     (term cffc (bcnb gnrtb))))
                        listb)
                     (mapcar #'(lambda (termc)
                                  (declare (type term termc))
                                  (with-term (cffc gnrtc) termc
                                     (term cffc (bcnc gnrtc))))
                        listc)
                     (mapcar #'(lambda (termd)
                                  (declare (type term termd))
                                  (with-term (cffc gnrtd) termd
                                     (term cffc (bcnd gnrtd))))
                        listd))))))))

#|
  (multiple-value-call #'make-bicn-cmbn 
     (dispatch-bicn-cmbn (cmbn 3 3 (bcnb 'b1) 4 (bcnb 'b2) 33 (bcnc 'c) 333 (bcnd 'd))))
|#

(DEFMETHOD PRINT-KEYCONS ((car (eql :bcnb)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<BcnB ~A>" cdr)
         t)))

(DEFMETHOD PRINT-KEYCONS ((car (eql :bcnc)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<BcnC ~A>" cdr)
         t)))

(DEFMETHOD PRINT-KEYCONS ((car (eql :bcnd)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<BcnD ~A>" cdr)
         t)))

#|
  (bcnb 'a)
  (bcnc 'b)
  (bcnd 'c)
|#

(DEFUN BICONE-CMPR (cmprb cmprc cmprd)
   (declare (type cmprf cmpra cmprb cmprd))
   (flet ((rslt (bicn1 bicn2)
             (declare (type bicn bicn1 bicn2))
             (let ((bcnx1 (bcnx bicn1)))
                (declare (type (member :bcnb :bcnc :bcnd) bcnx))
                (lexico
                   (s-cmpr bcnx1 (bcnx bicn2))
                   (case bcnx1
                      (:bcnb (funcall cmprb (ibicn bicn1) (ibicn bicn2)))
                      (:bcnc (funcall cmprc (ibicn bicn1) (ibicn bicn2)))
                      (:bcnd (funcall cmprd (ibicn bicn1) (ibicn bicn2))))))))
      (the cmprf #'rslt)))

#|
  (setf r (bicone-cmpr #'f-cmpr #'f-cmpr #'f-cmpr))
  (funcall r (bcnb 1) (bcnc 0))
  (funcall r (bcnb 1) (bcnb 2))
|#

(DEFUN BICONE-BASIS (basisb basisc basisd)
   (declare (type basis basisb basisc basisd))
   (the basis
      (if (or (eq :locally-effective basisb)
              (eq :locally-effective basisc)
              (eq :locally-effective basisd))
         :locally-effective
         (flet ((rslt (degr)
                   (declare (fixnum degr))
                   (append
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (bcnb item))
                         (funcall basisb degr))
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (bcnc item))
                         (funcall basisc (1+ degr)))
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (bcnd item))
                         (funcall basisd degr)))))
            (the basis #'rslt)))))

#|
  (bicone-basis :locally-effective :locally-effective :locally-effective)
  (setf b #'(lambda (degr) (mapcar #'(lambda (item) (cons degr item)) (<a-b> 0 degr))))
  (setf r (bicone-basis b b b))
  (funcall r 3)
|#

(DEFUN BICONE-DFFR-INTR (cmprc dffrb dffrc dffrd f1 f2)
   (declare
      (type cmprf cmprc)
      (type morphism dffrb dffrc dffrd f1 f2))
   (flet ((rslt (cmbn)
             (declare (type cmbn cmbn))
             (the cmbn
                (multiple-value-bind (cmbnb cmbnc cmbnd) (dispatch-bicn-cmbn cmbn)
                   (declare (type cmbn cmbnb cmbnc cmbnd))
                   (let ((dffrb (cmbn-? dffrb cmbnb))
                         (dffrc (cmbn-? dffrc cmbnc))
                         (dffrd (cmbn-? dffrd cmbnd))
                         (f1 (cmbn-? f1 cmbnb))
                         (f2 (cmbn-? f2 cmbnd)))
                      (declare (type cmbn dffrb dffrc dffrd f1 f2))
                      (let ((cmbnc (2cmbn-sbtr cmprc (2cmbn-add cmprc f1 f2) dffrc)))
                         (declare (type cmbn cmbnc))
                         (make-bicn-cmbn dffrb cmbnc dffrd)))))))
      (the intr #'rslt)))
                      
(DEFUN BICONE (rdct1 rdct2)
   (declare (type reduction rdct1 rdct2))
   (the chain-complex
      (with-slots ((tcc1 tcc) (bcc1 bcc) (f1 f)) rdct1
         (declare
            (type chain-complex tcc1 bcc1)
            (type morphism f1 g1 h1))
      (with-slots ((tcc2 tcc) (bcc2 bcc) (f2 f)) rdct2
         (declare
            (type chain-complex tcc2 bcc2)
            (type morphism f2 g2 h2))
         (unless (eq bcc1 bcc2)
            (error "In BICONE, both reductions have different BCC's."))
         (let ((rslt (chain-complex
                        :cmpr (bicone-cmpr (cmpr tcc1) (cmpr bcc1) (cmpr tcc2))
                        :basis (bicone-basis (basis tcc1) (basis bcc1) (basis tcc2))
                        :bsgn :undefined
                        :dffr-intr (bicone-dffr-intr (cmpr bcc1)
                                      (dffr tcc1) (dffr bcc1) (dffr tcc2)
                                      f1 f2)
                        :dffr-strt :cmbn
                        :dfnt `(bicone ,rdct1 ,rdct2))))
            (declare (type chain-complex rslt))
            (slot-makunbound rslt 'bsgn)
            rslt)))))

#|
  (cat-init)
  (progn
   (defun cdelta (dmns)
     (chain-complex
        :cmpr #'l-cmpr
        :basis #'(lambda (n)
                    (mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
        :bsgn '(0)
        :dffr-intr #'(lambda (degr gmsm)
                        (make-cmbn
                           :degr (1- degr)
                           :list (do ((rslt +empty-list+
                                           (cons (cons sign (append
                                                               (subseq gmsm 0 nark)
                                                               (subseq gmsm (1+ nark))))
                                                 rslt))
                                     (sign 1 (- sign))
                                     (nark 0 (1+ nark)))
                                    ((> nark degr) rslt))))                                           
        :dffr-strt :gnrt
        :dfnt `(locally effective version of C_* delta ,dmns)))
   (defun make-f (tdmns bdmns)
     (morphism
        :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
        :intr #'(lambda (degr gmsm)
                   (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns)) gmsm)))
                      (if pos
                         (if (< pos degr)
                            (zero-cmbn degr)
                            (cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                         (cmbn degr 1 gmsm))))
        :strt :gnrt
        :dfnt `(projection delta ,tdmns => delta ,bdmns)))
   (defun make-g (tdmns bdmns)
     (morphism
        :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
        :intr #'identity
        :strt :cmbn
        :dfnt `(injection delta ,bdmns => delta ,tdmns)))
   (defun make-h (tdmns bdmns)
     (morphism
        :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
        :intr #'(lambda (degr gmsm)
                   (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns)) gmsm)))
                      (if pos
                         (if (member bdmns gmsm)
                            (zero-cmbn (1+ degr))
                            (cmbn (1+ degr) (-1-expt-n pos)
                               (append (subseq gmsm 0 pos) (list bdmns) (subseq gmsm pos))))
                         (zero-cmbn (1+ degr)))))
        :strt :gnrt
        :dfnt `(homotopy for delta ,tdmns => ,bdmns)))
   (defun make-rdct (tdmns bdmns)
       (setf rdct (reduction
                    :f (make-f tdmns bdmns)
                    :g (make-g tdmns bdmns)
                    :h (make-h tdmns bdmns)
                    :dfnt `(reduction delta ,tdmns ,bdmns)))))
  (setf b (bicone (make-rdct 3 2) (make-rdct 4 2)))
  (basis b 1)
  (? b (cmbn 1 3 (bcnb '(0 3)) 4 (bcnc '(0 1 2)) 5 (bcnd '(0 4))))
|#

(DEFMETHOD CMPS ((eqvl1 equivalence) (eqvl2 equivalence) &optional dummy)
   (declare (ignore dummy))
   (the equivalence
      (progn
         (when (eq 'trivial-eqvl (dfnt eqvl2))
            (return-from cmps eqvl1))
         (with-slots ((cc-A lbcc) (cc-B tcc) (cc-C rbcc)
                      (f1 lf) (g1 lg) (h1 lh) (f2 rf) (g2 rg) (h2 rh)) eqvl1
            (declare
               (type chain-complex cc-A cc-B cc-C)
               (type morphism f1 g1 h1 f2 g2 h2))
         (with-slots ((cc-C2 lbcc) (cc-D tcc) (cc-E rbcc)
                      (f3 lf) (g3 lg) (h3 lh) (f4 rf) (g4 rg) (h4 rh)) eqvl2
            (declare
               (type chain-complex cc-C2 cc-D cc-E)
               (type morphism f3 g3 h3 f4 g4 h4))
            (unless (eq (grmd cc-C) (grmd cc-C2))
               (error "In (CMPS eqvl eqvl), both eqvl's may not be composed."))
            (let ((bicone (bicone (rrdct eqvl1) (lrdct eqvl2)))
                  (cmpr-B (cmpr cc-B))
                  (cmpr-D (cmpr cc-D)))
               (declare (type chain-complex bicone))
               (let ((lf (morphism
                            :sorc bicone :trgt cc-A :degr 0
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (cmbn-? f1 (bicn-cmbn-cmbnB cmbn)))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 lf)))
                     (lg (morphism
                            :sorc cc-A :trgt bicone :degr 0
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (let* ((cmbnB (cmbn-? g1 cmbn))
                                              (cmbnC (zero-cmbn (1+ (cmbn-degr cmbn))))
                                              (cmbnD (cmbn-? g3 (cmbn-? f2 (cmbn-opps cmbnB)))))
                                          (declare (type cmbn cmbnB cmbnC cmbnD))
                                          (make-bicn-cmbn cmbnB cmbnC cmbnD)))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 lg)))
                     (lh (morphism
                            :sorc bicone :trgt bicone :degr +1
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (multiple-value-bind (cmbnB cmbnC cmbnD) 
                                           (dispatch-bicn-cmbn cmbn)
                                          (declare (type cmbn cmbnB cmbnC cmbnD))
                                          (setf cmbnB (cmbn-? h1 cmbnB))
                                          (let ((cmbnC (zero-cmbn (1+ (cmbn-degr cmbnC))))
                                                (cmbnD1 (cmbn-? g3 (cmbn-? f2 cmbnB)))
                                                (cmbnD2 (cmbn-? g3 cmbnC))  ;; let, not let*
                                                (cmbnD3 (cmbn-? h3 cmbnD)))
                                             (declare (type cmbn cmbnC cmbnD1 cmbnD2 cmbnD3))
                                             (make-bicn-cmbn
                                                cmbnB
                                                cmbnC
                                                (2cmbn-sbtr cmpr-D
                                                   (2cmbn-add cmpr-D cmbnD2 cmbnD3)
                                                   cmbnD1)))))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 lh)))
                     (rf (morphism
                            :sorc bicone :trgt cc-E :degr 0
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (cmbn-? f4 (bicn-cmbn-cmbnD cmbn)))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 rf)))
                     (rg (morphism
                            :sorc cc-E :trgt bicone :degr 0
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (let* ((cmbnD (cmbn-? g4 cmbn))
                                              (cmbnC (zero-cmbn (1+ (cmbn-degr cmbn))))
                                              (cmbnB (cmbn-? g2 (cmbn-? f3 (cmbn-opps cmbnD)))))
                                          (declare (type cmbn cmbnD cmbnC cmbnB))
                                          (make-bicn-cmbn cmbnB cmbnC cmbnD)))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 rg)))
                     (rh (morphism
                            :sorc bicone :trgt bicone :degr +1
                            :intr #'(lambda (cmbn)
                                       (declare (type cmbn cmbn))
                                       (multiple-value-bind (cmbnB cmbnC cmbnD) 
                                           (dispatch-bicn-cmbn cmbn)
                                          (declare (type cmbn cmbnB cmbnC cmbnD))
                                          (setf cmbnD (cmbn-? h4 cmbnD))
                                          (let ((cmbnC (zero-cmbn (1+ (cmbn-degr cmbnC))))
                                                (cmbnB1 (cmbn-? g2 (cmbn-? f3 cmbnD)))
                                                (cmbnB2 (cmbn-? g2 cmbnC))  ;; let, not let*
                                                (cmbnB3 (cmbn-? h2 cmbnB)))
                                             (declare (type cmbn cmbnC cmbnB1 cmbnB2 cmbnB3))
                                             (make-bicn-cmbn
                                                (2cmbn-sbtr cmpr-B
                                                   (2cmbn-add cmpr-B cmbnB2 cmbnB3)
                                                   cmbnB1)
                                                cmbnC
                                                cmbnD))))
                            :strt :cmbn
                            :dfnt `(cmps ,eqvl1 ,eqvl2 rh))))
                  (declare
                     (type chain-complex lbcc tcc rbcc)
                     (type morphism lf lg lh rf rg rh))
                  (equivalence
                   :lrdct (reduction :f lf :g lg :h lh :dfnt `(cmps ,eqvl1 ,eqvl2 :lrdct))
                   :rrdct (reduction :f rf :g rg :h rh :dfnt `(cmps ,eqvl1 ,eqvl2 :rrdct))
                   :dfnt `(cmps ,eqvl1 ,eqvl2)))))))))

#|
  (cat-init)
  (setf c (chain-complex
             :cmpr #'s-cmpr
             :basis #'(lambda (dmns) '(a))
             :bsgn 'a
             :dffr-intr #'zero-dffr-intr
             :dffr-strt :cmbn
             :dfnt '(c)))
  (setf h1 (trivial-eqvl c))
  (setf h2 (cmps h1 h1))
  (pre-check-rdct (lrdct h2))
  (setf *tc* (cmbn 3 1 (bcnB 'a) 10 (bcnC 'a) 100 (bcnD 'a)))
  (setf *bc* (cmbn 3 1 'a))
  (check-rdct)
  (pre-check-rdct (rrdct h2))
  (check-rdct)
  (setf h3 (cmps h2 h2))
  (setf *tc* (cmbn 3 1 (bcnB (bcnB 'a)) 10 (bcnB (bcnC 'a)) 100 (bcnB (bcnD 'a))
                     1000 (bcnC 'a)
                    10000 (bcnD (bcnB 'a)) 5234 (bcnD (bcnC 'a)) 223 (bcnD (bcnD 'a))))
  (pre-check-rdct (lrdct h3))
  (check-rdct)
  (pre-check-rdct (rrdct h3))
  (check-rdct)
|#
