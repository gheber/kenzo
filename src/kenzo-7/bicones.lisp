;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES
;;;  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES
;;;  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES  BICONES

(IN-PACKAGE #:cat)

(PROVIDE "bicones")

(DEFUN DISPATCH-BICN-CMBN (cmbn)
  "(DISPATCH-BICN-CMBN CMBN) splits the bicone combination CMBN into three
combinations and returns 3 values."
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
                                              (:bcnb (push (term cffc ibicn)
                                                           listb))
                                              (:bcnc (push (term cffc ibicn)
                                                           listc))
                                              (:bcnd (push (term cffc ibicn)
                                                           listd))))))
                    (values
                     (make-cmbn :degr degr :list (nreverse listb))
                     (make-cmbn :degr (1+ degr) :list (nreverse listc))
                     (make-cmbn :degr degr :list (nreverse listd)))))))


(DEFUN BICN-CMBN-CMBNB (cmbn)
  (declare (type cmbn cmbn))
  "(BICN-CMBN-CMBNB CMBN) extracts from the bicone combination CMBN of degree
n the B-component as a legal combination of degree n in B. If the B-component
is empty, the null combination of degree n in B is returned."
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (make-cmbn
                   :degr degr
                   :list (do ((rslt +empty-list+)
                              (mark list (cdr mark)))
                             ((endp mark) (nreverse rslt))
                           (declare (list rslt mark))
                           (with--term (cffc bicn) mark
                                       (with-bicn (bcnx gnrtB) bicn
                                                  (if (eq bcnx :bcnB)
                                                      (push (term cffc gnrtB)
                                                            rslt)
                                                      (return
                                                        (nreverse rslt))))))))))

(DEFUN BICN-CMBN-CMBNC (cmbn)
  (declare (type cmbn cmbn))
  "(BICN-CMBN-CMBNC CMBN) extracts from the bicone combination CMBN of degree
n the C-component as a legal combination of degree n+1 in C. If the C-component
is empty, the null combination of degree n+1 in C is returned."
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (make-cmbn
                   :degr (1+ degr)
                   :list (do ((rslt +empty-list+)
                              (mark (member :bcnc list
                                            ;; Correction 1-1-2
                                            :key #'(lambda (term)
                                                     (declare (type term term))
                                                     (bcnx (gnrt term))))
                                    (cdr mark)))
                             ((endp mark) (nreverse rslt))
                           (declare (list rslt mark))
                           (with--term (cffc bicn) mark
                                       (with-bicn (bcnx gnrtC) bicn
                                                  (if (eq bcnx :bcnC)
                                                      (push (term cffc gnrtC)
                                                            rslt)
                                                      (return
                                                        (nreverse rslt))))))))))


(DEFUN BICN-CMBN-CMBND (cmbn)
  (declare (type cmbn cmbn))
  "(BICN-CMBN-CMBND CMBN) extracts from the bicone combination CMBN of degree
n the D-component as a legal combination of degree n in D. If the D-component
is empty, the null combination of degree n in D is returned."
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (make-cmbn
                   :degr degr
                   :list (mapcar
                          #'(lambda (term)
                              (declare (type term term))
                              (with-term (cffc bicn) term
                                         (term cffc (ibicn bicn))))
                          (member
                           :bcnd list
                           :key #'(lambda (term)
                                    (declare (type term term))
                                    (the keyword (bcnx (gnrt term))))))))))


(DEFUN MAKE-BICN-CMBN (cmbnb cmbnc cmbnd)
  (declare (type cmbn cmbnb cmbnc cmbnd))
  "(MAKE-BICN-CMBN CMBNB CMBNC CMBND) constructs a bicone combination from
CMBNB, CMBNC and CMBND, which belong to chain complexes B, C, D, respectively.
Combinations CMBNB and CMBND must be of the same degree n and combination
CMBNC must be of degree n+1, yiedling a bicone combination of degree n."
  (the cmbn
       (with-cmbn
           (degrb listb) cmbnb
           (with-cmbn
               (degrc listc) cmbnc
               (with-cmbn
                   (degrd listd) cmbnd
                   (unless (= degrb (1- degrc) degrd)
                     (error "In MAKE-BICN-CMBN, the degrees are not coherent."))
                   (make-cmbn
                    :degr degrb
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


(DEFUN BICN-PRINT (bicn stream depth)
  (declare
   (type bicn bicn) (stream stream)
   (ignore depth))
  (the bicn
       (with-bicn (bcnx ibicn) bicn
                  ;; Correction 1-1-2
                  (format stream "<Bcn~A ~A>"
                          ;; Correction 1-1-3
                          (ecase bcnx (:bcnb #\B) (:bcnc #\C) (:bcnd #\D))
                          ibicn)
                  bicn)))


(DEFUN BICONE-CMPR (cmprb cmprc cmprd)
  (declare (type cmprf cmprb cmprc cmprd))
  "(BICONE-CMPR CMPRB CMPRC CMPRD) returns a comparison function for bicone
generators, which is contructed from the comparison functions CMPRB, CMPRC
and CMPRD."
  (flet ((rslt (bicn1 bicn2)
           (declare (type bicn bicn1 bicn2))
           (let ((bcnx1 (bcnx bicn1)))
             (declare (type (member :bcnb :bcnc :bcnd) bcnx1))
             (lexico
              (s-cmpr bcnx1 (bcnx bicn2))
              (case bcnx1
                (:bcnb (funcall cmprb (ibicn bicn1) (ibicn bicn2)))
                (:bcnc (funcall cmprc (ibicn bicn1) (ibicn bicn2)))
                (:bcnd (funcall cmprd (ibicn bicn1) (ibicn bicn2))))))))
    (the cmprf #'rslt)))


(DEFUN BICONE-BASIS (basisb basisc basisd)
  (declare (type basis basisb basisc basisd))
  "(BICONE-BASIS BASISB BASISC BASISD) returns a bicone basis function built
from the basis functions BASISB, BASISC and BASISD. If at least one is
locally effective, the function returns the symbol :locally-effective."
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


(DEFUN BICONE-INTR-DFFR (cmprc dffrb dffrc dffrd f1 f2)
  (declare
   (type cmprf cmprc)
   (type morphism dffrb dffrc dffrd f1 f2))
  "(BICONE-INTR-DFFR CMPRC DFFRB DFFRC DFFRD F1 F2) "
  (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (the cmbn
                (multiple-value-bind (cmbnb cmbnc cmbnd)
                    (dispatch-bicn-cmbn cmbn)
                  (declare (type cmbn cmbnb cmbnc cmbnd))
                  (let ((dffrb (cmbn-? dffrb cmbnb))
                        (dffrc (cmbn-? dffrc cmbnc))
                        (dffrd (cmbn-? dffrd cmbnd))
                        (f1 (cmbn-? f1 cmbnb))
                        (f2 (cmbn-? f2 cmbnd)))
                    (declare (type cmbn dffrb dffrc dffrd f1 f2))
                    (let ((cmbnc (2cmbn-sbtr cmprc
                                             (2cmbn-add cmprc f1 f2)
                                             dffrc)))
                      (declare (type cmbn cmbnc))
                      (make-bicn-cmbn dffrb cmbnc dffrd)))))))
    (the intr-mrph #'rslt)))



(DEFUN BICONE (rdct1 rdct2)
  (declare (type reduction rdct1 rdct2))
  "(BICONE RDCT1 RDCT2) returns the bicone chain complex from the reductions
RDCT1 and RDCT2. B is the top chain complex of RDCT1 and D is the top chain
complex of RDCT2. The bottom chain complexes of RDCT1 and RDCT2 must be the
same, and C is that chain complex."
  (the chain-complex
       (with-slots ((tcc1 tcc) (bcc1 bcc) (f1 f)) rdct1
         (declare
          (type chain-complex tcc1 bcc1)
          (type morphism f1))
         (with-slots ((tcc2 tcc) (bcc2 bcc) (f2 f)) rdct2
           (declare
            (type chain-complex tcc2 bcc2)
            (type morphism f2))
           (unless (eq bcc1 bcc2)
             (error "In BICONE, both reductions have different BCC's."))
           (let ((rslt (build-chcm
                        :cmpr (bicone-cmpr (cmpr tcc1) (cmpr bcc1) (cmpr tcc2))
                        :basis (bicone-basis (basis tcc1) (basis bcc1)
                                             (basis tcc2))
                        :bsgn :undefined
                        :intr-dffr (bicone-intr-dffr (cmpr bcc1)
                                                     (dffr tcc1) (dffr bcc1)
                                                     (dffr tcc2)
                                                     f1 f2)
                        :strt :cmbn
                        :orgn `(bicone ,rdct1 ,rdct2))))
             (declare (type chain-complex rslt))
                                        ; (slot-makunbound rslt 'bsgn)
             rslt)))))


(DEFMETHOD CMPS ((hmeq1 homotopy-equivalence) (hmeq2 homotopy-equivalence)
                 &optional dummy)
  (declare (ignore dummy))
  "(CMPS HMEQ1 HMEQ2 &OPTIONAL DUMMY) constructs a homotopy equivalence
by composition of the homotopy equivalences HMEQ1 and HMEQ2."
  (the homotopy-equivalence
       (progn
         (when (eq 'trivial-hmeq (orgn hmeq2))
           (return-from cmps hmeq1))
         (with-slots ((cc-A lbcc) (cc-B tcc) (cc-C rbcc)
                      (f1 lf) (g1 lg) (h1 lh) (f2 rf) (g2 rg) (h2 rh)) hmeq1
           (declare
            (type chain-complex cc-A cc-B cc-C)
            (type morphism f1 g1 h1 f2 g2 h2))
           (with-slots ((cc-C2 lbcc) (cc-D tcc) (cc-E rbcc)
                        (f3 lf) (g3 lg) (h3 lh) (f4 rf) (g4 rg) (h4 rh)) hmeq2
             (declare
              (type chain-complex cc-C2 cc-D cc-E)
              (type morphism f3 g3 h3 f4 g4 h4))
             (unless (eq (grmd cc-C) (grmd cc-C2))
               (error "In (CMPS HMEQ HMEQ), both hmeq's may not be composed."))
             (let ((bicone (bicone (rrdct hmeq1) (lrdct hmeq2)))
                   (cmpr-B (cmpr cc-B))
                   (cmpr-D (cmpr cc-D)))
               (declare (type chain-complex bicone))
               (let ((lf (build-mrph
                          :sorc bicone :trgt cc-A :degr 0
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (cmbn-? f1 (bicn-cmbn-cmbnB cmbn)))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 lf)))
                     (lg (build-mrph
                          :sorc cc-A :trgt bicone :degr 0
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (let* ((cmbnB (cmbn-? g1 cmbn))
                                           (cmbnC (zero-cmbn
                                                   (1+ (cmbn-degr cmbn))))
                                           (cmbnD (cmbn-? g3
                                                          (cmbn-? f2
                                                                  (cmbn-opps
                                                                   cmbnB)))))
                                      (declare (type cmbn cmbnB cmbnC cmbnD))
                                      (make-bicn-cmbn cmbnB cmbnC cmbnD)))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 lg)))
                     (lh (build-mrph
                          :sorc bicone :trgt bicone :degr +1
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (multiple-value-bind (cmbnB cmbnC cmbnD)
                                        (dispatch-bicn-cmbn cmbn)
                                      (declare (type cmbn cmbnB cmbnC cmbnD))
                                      (setf cmbnB (cmbn-? h1 cmbnB))
                                      (let ((cmbnC (zero-cmbn
                                                    (1+ (cmbn-degr cmbnC))))
                                            (cmbnD1 (cmbn-? g3
                                                            (cmbn-? f2 cmbnB)))
                                            (cmbnD2 (cmbn-? g3 cmbnC))  ;; let, not let*
                                            (cmbnD3 (cmbn-? h3 cmbnD)))
                                        (declare (type cmbn cmbnC cmbnD1 cmbnD2
                                                       cmbnD3))
                                        (make-bicn-cmbn
                                         cmbnB
                                         cmbnC
                                         (2cmbn-sbtr cmpr-D
                                                     (2cmbn-add cmpr-D cmbnD2
                                                                cmbnD3)
                                                     cmbnD1)))))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 lh)))
                     (rf (build-mrph
                          :sorc bicone :trgt cc-E :degr 0
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (cmbn-? f4 (bicn-cmbn-cmbnD cmbn)))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 rf)))
                     (rg (build-mrph
                          :sorc cc-E :trgt bicone :degr 0
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (let* ((cmbnD (cmbn-? g4 cmbn))
                                           (cmbnC (zero-cmbn
                                                   (1+ (cmbn-degr cmbn))))
                                           (cmbnB
                                            (cmbn-?
                                             g2 (cmbn-? f3 (cmbn-opps cmbnD)))))
                                      (declare (type cmbn cmbnD cmbnC cmbnB))
                                      (make-bicn-cmbn cmbnB cmbnC cmbnD)))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 rg)))
                     (rh (build-mrph
                          :sorc bicone :trgt bicone :degr +1
                          :intr #'(lambda (cmbn)
                                    (declare (type cmbn cmbn))
                                    (multiple-value-bind (cmbnB cmbnC cmbnD)
                                        (dispatch-bicn-cmbn cmbn)
                                      (declare (type cmbn cmbnB cmbnC cmbnD))
                                      (setf cmbnD (cmbn-? h4 cmbnD))
                                      (let ((cmbnC (zero-cmbn
                                                    (1+ (cmbn-degr cmbnC))))
                                            (cmbnB1 (cmbn-? g2
                                                            (cmbn-? f3 cmbnD)))
                                            (cmbnB2 (cmbn-? g2 cmbnC))  ;; let, not let*
                                            (cmbnB3 (cmbn-? h2 cmbnB)))
                                        (declare (type cmbn cmbnC cmbnB1 cmbnB2
                                                       cmbnB3))
                                        (make-bicn-cmbn
                                         (2cmbn-sbtr cmpr-B
                                                     (2cmbn-add cmpr-B cmbnB2
                                                                cmbnB3)
                                                     cmbnB1)
                                         cmbnC
                                         cmbnD))))
                          :strt :cmbn
                          :orgn `(cmps ,hmeq1 ,hmeq2 rh))))
                 (declare
                  (type morphism lf lg lh rf rg rh))
                 (build-hmeq :lf lf :lg lg :lh lh :rf rf :rg rg :rh rh
                             :orgn `(cmps ,hmeq1 ,hmeq2)))))))))
