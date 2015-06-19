;;;  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  
;;;  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS 
;;;  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS  CYLIDERS 

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "CYCLINDERS")


;; MACROS

(DEFMACRO CYLNX (CYLN)
   `(car ,CYLN))

(DEFMACRO ICYLN (CYLN)
   `(cdr ,CYLN))

(DEFMACRO CYLNA1 (gnrt)
   `(cons :CYLNA1 ,gnrt))

(DEFMACRO CYLNB (gnrt)
   `(cons :CYLNB ,gnrt))

(DEFMACRO CYLNA2 (gnrt)
   `(cons :CYLNA2 ,gnrt))

(DEFMACRO WITH-CYLN ((CYLNx iCYLN) CYLN . body)
   `(let ((,CYLNx (CYLNx ,CYLN))
          (,iCYLN (iCYLN ,CYLN)))
       (declare
          (type (member :CYLNA1 :CYLNB :CYLNA2) ,CYLNx)
          (type gnrt ,iCYLN))
       ,@body))


(DEFUN DISPATCH-CYLN-CMBN (cmbn)
   (declare (type cmbn cmbn))
   (the (values cmbn cmbn cmbn)
      (with-cmbn (degr list) cmbn
         (let ((listA1 +empty-list+)
               (listB +empty-list+)
               (listA2 +empty-list+))
            (declare (list listA1 listB listA2))
            (dolist (term list)
               (declare (type term term))
               (with-term (cffc cyln) term
                  (with-cyln (cylnx icyln) cyln
                     (ecase cylnx
                        (:cylnA1 (push (term cffc icyln) listA1))
                        (:cylnB (push (term cffc icyln) listB))
                        (:cylnA2 (push (term cffc icyln) listA2))))))
            (values
               (make-cmbn :degr (1- degr) :list (nreverse listA1))
               (make-cmbn :degr degr :list (nreverse listB))
               (make-cmbn :degr degr :list (nreverse listA2)))))))

#|
  (dispatch-CYLN-cmbn (cmbn 3 3 (cylnA1 'a11) 4 (cylnA1 'a12) 33 (cylnB 'b) 333 (cylnA2 'a2)))
|#

(DEFUN CYLN-CMBN-CMBNA1 (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr (1- degr)
            :list (do ((rslt +empty-list+)
                       (mark list (cdr mark)))
                      ((endp mark) (nreverse rslt))
                     (declare (list rslt mark))
                     (with--term (cffc CYLN) mark
                     (with-CYLN (CYLNx gnrtA1) CYLN
                        (if (eq CYLNx :CYLNA1)
                           (push (term cffc gnrtA1) rslt)
                           (return (nreverse rslt))))))))))

(DEFUN CYLN-CMBN-CMBNB (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr degr
            :list (do ((rslt +empty-list+)
                       (mark (member :CYLNb list :key #'cadr) (cdr mark)))
                      ((endp mark) (nreverse rslt))
                     (declare (list rslt mark))
                     (with--term (cffc CYLN) mark
                     (with-CYLN (CYLNx gnrtB) CYLN
                        (if (eq CYLNx :CYLNB)
                           (push (term cffc gnrtB) rslt)
                           (return (nreverse rslt))))))))))

(DEFUN CYLN-CMBN-CMBNA2 (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn :degr degr
            :list (mapcar
                     #'(lambda (term)
                          (declare (type term term))
                          (with-term (cffc CYLN) term
                             (term cffc (iCYLN CYLN))))
                     (member :CYLNA2 list :key #'cadr))))))

#|
 
  (CYLN-cmbn-cmbnA1 (cmbn 3 1 (CYLNA1 'a) 2 (CYLNB 'b)))
  (CYLN-cmbn-cmbnA1 (cmbn 3 2 (CYLNB 'b)))
  (CYLN-cmbn-cmbnB (cmbn 3 1 (CYLNA1 'a) 2 (CYLNB 'b) 3 (CYLNA2 'c)))
  (CYLN-cmbn-cmbnB (cmbn 3 1 (CYLNA1 'a) 3 (CYLNA2 'c)))
  (CYLN-cmbn-cmbnA2 (cmbn 3 1 (CYLNA1 'a) 2 (CYLNB 'b) 3 (CYLNA2 'c)))
  (CYLN-cmbn-cmbnA2 (cmbn 3 2 (CYLNA1 'b)))
|#

(DEFUN MAKE-CYLN-CMBN (cmbnA1 cmbnB cmbnA2)
   (declare (type cmbn cmbnA1 cmbnB cmbnA2))
   (the cmbn
      (with-cmbn (degrA1 listA1) cmbnA1
      (with-cmbn (degrB listB) cmbnB
      (with-cmbn (degrA2 listA2) cmbnA2
         (unless (= (1+ degrA1) degrB degrA2)
            (error "In MAKE-CYLN-CMBN, the degrees are not coherent."))
         (make-cmbn :degr degrB
            :list (nconc
                     (mapcar #'(lambda (termA1)
                                  (declare (type term termA1))
                                  (with-term (cffc gnrtA1) termA1
                                     (term cffc (CYLNA1 gnrtA1))))
                        listA1)
                     (mapcar #'(lambda (termB)
                                  (declare (type term termB))
                                  (with-term (cffc gnrtB) termB
                                     (term cffc (CYLNB gnrtB))))
                        listB)
                     (mapcar #'(lambda (termA2)
                                  (declare (type term termA2))
                                  (with-term (cffc gnrtA2) termA2
                                     (term cffc (CYLNA2 gnrtA2))))
                        listA2))))))))

#|
  (multiple-value-call #'make-CYLN-cmbn 
     (dispatch-CYLN-cmbn (cmbn 3 3 (CYLNA1 'A11) 4 (CYLNA1 'A12) 33 (CYLNB 'B) 333 (CYLNA2 'A2))))
|#

(DEFMETHOD PRINT-KEYCONS ((car (eql :CYLNA1)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<CylnA1 ~A>" cdr)
         t)))

(DEFMETHOD PRINT-KEYCONS ((car (eql :CYLNB)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<CylnB ~A>" cdr)
         t)))

(DEFMETHOD PRINT-KEYCONS ((car (eql :CYLNA2)) cdr stream)
   (declare
      (type gnrt cdr)
      (stream stream))
   (the (eql t)
      (progn
         (format stream "<CylnA2 ~A>" cdr)
         t)))

#|
  (CYLNa1 'a)
  (CYLNB 'b)
  (CYLNA2 'c)
|#

(DEFUN cylnX-CMPR (s1 s2)
   (declare (symbol s1 s2))
   (the cmpr
      (if (string= s1 s2) :equal
         (if (string= s1 :cylnA1) :less
            (if (string= s2 :cylnA1) :greater
               (if (string= s1 :cylnB) :less
                  :greater))))))
         

(DEFUN CYLINDER-CMPR (cmprA cmprB)
   (declare (type cmprf cmpra cmprb))
   (flet ((rslt (cyln1 cyln2)
             (declare (type cyln cyln1 cyln2))
             (let ((cylnx1 (cylnx cyln1)))
                (declare (type (member :cylnA1 :cylnB :cylnA2) cylnx))
                (lexico
                   (cylnX-CMPR cylnx1 (cylnx cyln2))
                   (case cylnx1
                      (:cylnA1 (funcall cmprA (icyln cyln1) (icyln cyln2)))
                      (:cylnB (funcall cmprB (icyln cyln1) (icyln cyln2)))
                      (:cylnA2 (funcall cmprA (icyln cyln1) (icyln cyln2))))))))
      (the cmprf #'rslt)))

#|
  (setf r (cylinder-cmpr #'f-cmpr #'f-cmpr ))
  (funcall r (cylnA1 1) (cylnB 0))
  (funcall r (cylnA1 1) (cylnA1 2))
  (funcall r (cylnB 0) (cylnA1 1))
  (funcall r (cylnA1 2) (cylnA1 1)) 
|#

(DEFUN CYLINDER-BASIS (basisA basisB)
   (declare (type basis basisA basisB))
   (the basis
      (if (or (eq :locally-effective basisA)
              (eq :locally-effective basisB)
              )
         :locally-effective
         (flet ((rslt (degr)
                   (declare (fixnum degr))
                   (append
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (cylnA1 item))
                         (funcall basisA (1- degr)))
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (cylnB item))
                         (funcall basisB degr))
                      (mapcar #'(lambda (item)
                                   (declare (type gnrt item))
                                   (cylnA2 item))
                         (funcall basisA degr)))))
            (the basis #'rslt)))))

#|
  (CYLINDER-basis :locally-effective :locally-effective )
  (setf b #'(lambda (degr) (mapcar #'(lambda (item) (cons degr item)) (<a-b> 0 degr))))
  (setf r (CYLINDER-basis b b))
  (funcall r 3)
|#

(DEFUN CYLINDER-INTR-DFFR (cmprA cmprB dffrA dffrB f)
   (declare
      (type cmprf cmprA cmprB)
      (type morphism dffrA dffrB f))
   (flet ((rslt (cmbn)
             (declare (type cmbn cmbn))
             (the cmbn
                (multiple-value-bind (cmbnA1 cmbnB cmbnA2) (dispatch-cyln-cmbn cmbn)
                   (declare (type cmbn cmbnA1 cmbnB cmbnA2))
                   (let ((dffrA1-cmbn (cmbn-? dffrA cmbnA1))
                         (dffrB-cmbn (cmbn-? dffrB cmbnB))
                         (dffrA2-cmbn (cmbn-? dffrA cmbnA2))
                         (f-cmbn (cmbn-? f cmbnA1))
                         )
                      (declare (type cmbn dffrA1-cmbn dffrB-cmbn dffrA2-cmbn f-cmbn))
                      (let ((cmbnB (2cmbn-add cmprB f-cmbn dffrB-cmbn))
                            (cmbnA2 (2cmbn-sbtr cmprA dffrA2-cmbn cmbnA1)))
                         (declare (type cmbn cmbnB cmbnA2))
                         (make-cyln-cmbn (cmbn-opps dffrA1-cmbn) cmbnB cmbnA2)))))))
      (the intr-mrph #'rslt)))


(DEFUN CYLINDER (f)
   (declare (type morphism f))
   (the chain-complex
     (with-slots (sorc trgt) f
       (declare
        (type chain-complex sorc trgt ))
       (let ((rslt (build-chcm
                    :cmpr (cylinder-cmpr (cmpr sorc) (cmpr trgt))
                    :basis (cylinder-basis (basis sorc) (basis trgt))
                    :bsgn :undefined
                    :intr-dffr (cylinder-intr-dffr (cmpr sorc) (cmpr trgt)
                                     (dffr sorc) (dffr trgt) f)
                    :strt :cmbn
                    :orgn `(cylinder ,f))))
          (declare (type chain-complex rslt))
          (slot-makunbound rslt 'bsgn)
          rslt))))





(defun cylinder-rrdct (f)
   (declare (type morphism f))
   (the reduction
     (with-slots (sorc (Bcc trgt)) f
       (with-slots ((cmprb cmpr)) Bcc
         (let ((cylinder (cylinder f)))
            (let ((rf (build-mrph
                       :sorc cylinder :trgt Bcc :degr 0
                       :intr #'(lambda (cmbn)
                                 (declare (type cmbn cmbn))
                                 (2cmbn-add cmprB (cyln-cmbn-cmbnB cmbn) (cmbn-? f (cyln-cmbn-cmbnA2 cmbn))))
                       :strt :cmbn
                       :orgn `(cylinder ,f rf)))
                  (rg (build-mrph
                       :sorc Bcc :trgt cylinder :degr 0
                       :intr #'(lambda (cmbn)
                                 (declare (type cmbn cmbn))
                                 (let* ((cmbnA1 (zero-cmbn (1- (cmbn-degr cmbn))))
                                        (cmbnA2 (zero-cmbn (cmbn-degr cmbn))))
                                    (declare (type cmbn cmbnA1 cmbnA2))
                                    (make-cyln-cmbn cmbnA1 cmbn cmbnA2)))
                       :strt :cmbn
                       :orgn `(cylinder ,f rg)))
                  (rh (build-mrph
                       :sorc cylinder :trgt cylinder :degr +1
                       :intr #'(lambda (cmbn)
                                 (declare (type cmbn cmbn))
                                 (let* ((cmbnA1 (cmbn-opps (cyln-cmbn-cmbnA2 cmbn)))
                                        (cmbnB (zero-cmbn (1+ (cmbn-degr cmbn))))
                                        (cmbnA2 (zero-cmbn (1+ (cmbn-degr cmbn)))))
                                    (declare (type cmbn cmbnA1 cmbnB cmbnA2))
                                    (make-cyln-cmbn cmbnA1 cmbnB cmbnA2)))
                       :strt :cmbn
                       :orgn `(cylinder ,f rh))))
               (declare
                (type chain-complex cylinder)
                (type morphism rf rg rh))
               (build-rdct :f rf :g rg :h rh :orgn `(cylinder right reduction ,f))))))))


(defun cylinder-lrdct (f g h k)
   (declare (type morphism f g h k))
   (the reduction
     (with-slots ((Acc sorc) (Bcc trgt)) f
       (with-slots ((cmprA cmpr)) Acc
         (with-slots ((cmprb cmpr)) Bcc
           (let ((cylinder (cylinder f)))
              (let ((lf (build-mrph
                         :sorc cylinder :trgt Acc :degr 0
                         :intr #'(lambda (cmbn)
                                   (declare (type cmbn cmbn))
                                   (2cmbn-sbtr cmprA 
                                     (2cmbn-add cmpra (cyln-cmbn-cmbnA2 cmbn) (cmbn-? g (cyln-cmbn-cmbnB cmbn)))
                                     (cmbn-? h (cyln-cmbn-cmbnA1 cmbn))))
                         :strt :cmbn
                         :orgn `(cylinder hmtpeq ,f lf)))
                    (lg (build-mrph
                         :sorc Acc :trgt cylinder :degr 0
                         :intr #'(lambda (cmbn)
                                   (declare (type cmbn cmbn))
                                   (let* ((cmbnA1 (zero-cmbn (1- (cmbn-degr cmbn))))
                                          (cmbnB (zero-cmbn (cmbn-degr cmbn)))
                                          )
                                      (declare (type cmbn cmbnA1 cmbnB ))
                                      (make-cyln-cmbn cmbnA1 cmbnB cmbn)))
                         :strt :cmbn
                         :orgn `(cylinder hmtpeq ,f lg)))
                    (lh (build-mrph
                         :sorc cylinder :trgt cylinder :degr +1
                         :intr #'(lambda (cmbn)
                                   (declare (type cmbn cmbn))
                                   (multiple-value-bind (cmbnA1 cmbnB cmbnA2) 
                                       (dispatch-cyln-cmbn cmbn)
                                      (let* ((cmbnA11 (cmbn-opps (cmbn-? h cmbnA1)))
                                             (cmbnA12 (cmbn-opps (cmbn-? g (cmbn-? k (cmbn-? f cmbnA1)))))
                                             (cmbnA13 (cmbn-? g (cmbn-? f (cmbn-? h cmbnA1))))
                                             (cmbnA14 (cmbn-? g cmbnB))
                                             (cmbnB1 (cmbn-opps (cmbn-? k (cmbn-? k (cmbn-? f cmbnA1)))))
                                             (cmbnB2 (cmbn-? k (cmbn-? f (cmbn-? h cmbnA1))))
                                             (cmbnB3 (cmbn-? k cmbnB))
                                             (cmbnA21 (cmbn-opps (cmbn-? h (cmbn-? h cmbnA1))))
                                             (cmbnA22 (cmbn-opps (cmbn-? h (cmbn-? g (cmbn-? k (cmbn-? f cmbnA1))))))
                                             (cmbnA23 (cmbn-? h (cmbn-? g (cmbn-? f (cmbn-? h cmbnA1)))))
                                             (cmbnA24 (cmbn-? g (cmbn-? k (cmbn-? k (cmbn-? f cmbnA1)))))
                                             (cmbnA25 (cmbn-opps (cmbn-? g (cmbn-? k (cmbn-? f (cmbn-? h cmbnA1))))))
                                             (cmbnA26 (cmbn-? h (cmbn-? g cmbnB)))
                                             (cmbnA27 (cmbn-opps (cmbn-? g (cmbn-? k cmbnB)))))
                                         (declare (type cmbn cmbnA11 cmbnA12 cmbnA13 cmbnA14
                                                    cmbnB1 cmbnB2 cmbnB3 
                                                    cmbnA21 cmbnA22 cmbnA23 cmbnA24 cmbnA25 cmbnA26 cmbnA27))
                                         (make-cyln-cmbn (ncmbn-add cmprA cmbnA11 cmbnA12 cmbnA13 cmbnA14)
                                     (ncmbn-add cmprB cmbnB1 cmbnB2 cmbnB3)
                                     (ncmbn-add cmprA cmbnA21 cmbnA22 cmbnA23 cmbnA24 cmbnA25 cmbnA26 cmbnA27)))))
                                     
                         :strt :cmbn
                         :orgn `(cylinder hmtpeq ,f lh))))
            (declare
                     (type chain-complex cylinder)
                     (type morphism lf lg lh))
            (build-rdct :f lf :g lg :h lh :orgn `(cylinder left reduction ,f)))))))))


#|
 (cat-init) 
 (setf z3 (cyclicgroup 3))
 
 (setf rsltn1 (cyclicgroup-rsltn 3))
 (setf rsltn2 (bar-rsltn z3))
 (setf f (zgmrph-twi (2rsltn-zgmrph rsltn1 rsltn2)))
 (setf g (zgmrph-twi (2rsltn-zgmrph rsltn2 rsltn1)))
 (setf h (zgmrph-twi (2rsltn-hmtpop rsltn1 rsltn2)))
 (setf k (zgmrph-twi (2rsltn-hmtpop rsltn2 rsltn1)))
 (setf Acc (sorc f) Bcc (trgt f))


 (setf rrdct (cylinder-rrdct f))
 (setf lrdct (cylinder-lrdct f g h k))

 (setf lf (f lrdct) lg (g lrdct) lh (h lrdct))
 (setf rf (f rrdct) rg (g rrdct) rh (h rrdct))

 (pre-check-rdct rrdct)
 (setf *tc* (cmbn 3 1 (cylnA1 2) 10 (cylnB '(1 2 1)) 100 (cylnA2 3)))
 (setf *bc* (cmbn 3 1 '(1 1 2)))
 (check-rdct)
 (setf *tc* (cmbn 4 10 (cylnB '(1 2 1 1)) 5 (cylnB '(1 2 2 1)) 100 (cylnA2 4)))
 (setf *bc* (cmbn 2 5 '(1 1) 7 '(2 1) -4 '(2 2)))
 (check-rdct)


 (pre-check-rdct lrdct)

 (setf *tc* (cmbn 3 1 (cylnA1 2) 10 (cylnB '(1 2 1)) 100 (cylnA2 3)))
 (setf *bc* (cmbn 3 1 3))
 (check-rdct)

 (setf *tc* (cmbn 4 10 (cylnB '(1 2 1 1)) 5 (cylnB '(1 2 2 1)) 100 (cylnA2 4)))
 (setf *bc* (cmbn 2 1 2))
 (check-rdct)
|#


 (DEFMETHOD SEARCH-EFHM (smst (orgn (eql 'k-g-1)))
   (declare (type simplicial-group smst))
   (the homotopy-equivalence
    (let* ((group (second (orgn smst)))
           (rsltn1 (bar-rsltn group))
           (rsltn2 (resolution group))
           (f (zgmrph-twi (2rsltn-zgmrph rsltn1 rsltn2)))
           (g (zgmrph-twi (2rsltn-zgmrph rsltn2 rsltn1)))
           (h (zgmrph-twi (2rsltn-hmtpop rsltn1 rsltn2)))
           (k (zgmrph-twi (2rsltn-hmtpop rsltn2 rsltn1)))
           (rrdct (cylinder-rrdct f))
           (lrdct (cylinder-lrdct f g h k)))
       (build-hmeq :lrdct lrdct :rrdct rrdct :orgn `(efhm of ,smst)))))

 #|
 (setf z3 (cyclicgroup 3))
 (setf k-z3-1 (k-g-1 z3))

 (cat-init)
 (setf z4 (cyclicgroup 4))
 (setf k-z4-1 (k-g-1 z4))
 (setf k-z4-2 (classifying-space k-z4-1))

 (cat-init)
 (setf z2 (cyclicgroup 2))
 (setf k-z2-1 (k-g-1 z2))
 (setf k-z2-2 (classifying-space k-z2-1))
 |#
        
