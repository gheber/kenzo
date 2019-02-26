;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS
;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS
;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS

(IN-PACKAGE "COMMON-LISP-USER")

(IN-PACKAGE #:cat-8)

;;; GMSM-FACES-INFO = (gmsm (simple-vector absm) . bndr)
;;;                         faces

(DEFUN FINITE-SS-PRE-TABLE (list)
   (declare (list list))
   (the list
      (let ((pre-rslt +empty-list+)
            (dmns-mark nil)
            (gmsm-mark nil))
         (declare (list pre-rslt dmns-mark gmsm-mark))
         (dolist (item list)
            (declare (type (or fixnum symbol list) item))
            (cond ((typep item 'fixnum)
                   (let ((found (assoc item pre-rslt)))
                      (declare (list found))
                      (setf dmns-mark 
                            (or found
                                (car (push (list item) pre-rslt)))
                            gmsm-mark nil)))
                  ((symbolp item)
                   (when (assoc item (cdr dmns-mark))
                      (error "In BUILD-FINITE-SS, the symbol ~A is present two times."
                         item))
                   (setf gmsm-mark (car (push (list item) (cdr dmns-mark)))))
                  ((listp item)
                   (unless gmsm-mark
                      (error "In BUILD-FINITE-SS, the face list ~A~@
                              is not after a symbol." item))
                   (nconc gmsm-mark item)
                   (setf gmsm-mark nil))
                  (t
                     (error "In BUILD-FINITE-SS, the argument ~A does not make sense." item))))         
         (do ((mark1 pre-rslt (cdr mark1)))
             ((endp mark1))
             (declare (list mark1))
            (do ((mark2 (rest mark1) (cdr mark2)))
                ((endp mark2))
               (declare (list mark2))
               (let ((inter (intersection (cdar mark1) (cdar mark2) :key #'first)))
                  (declare (list inter))
                  (when inter
                     (error "In BUILD-FINITE-SS, the symbol ~A is present two times."
                        (caar inter))))))
         pre-rslt)))

#|
  (setf p (finite-ss-pre-table '(0 v0 v1 v2)))
  (finite-ss-pre-table '(0 v0 v0 v2))
  (setf p (finite-ss-pre-table '(0 v0 v1 1 e0 e1 e2)))
  (finite-ss-pre-table '(0 v0 v1 1 e0 e1 v1))
  (setf p (finite-ss-pre-table '(0 v0 (v0 v0))))
  (setf p (finite-ss-pre-table '(0 v0 v1 v2 0 v3)))
  (finite-ss-pre-table '(0 v0 (v0 v0) (v1 v1)))
  (finite-ss-pre-table '(0 (v0 v0) (v1 v1)))
  (finite-ss-pre-table '(0 v0 (v0 v0) #(1 2))))
|#

(DEFUN FINITE-SS-PRE-TABLE-TABLE (pre-table)
   (declare (list pre-table))
   (let* ((maxdim (1+ (apply #'max (mapcar #'first pre-table))))
          (table (make-array maxdim :initial-element +empty-list+)))
      (declare
         (fixnum maxdim)
         (simple-vector table))
      (dolist (item pre-table)
         (declare (list item))
         (setf (svref table (first item))
               (sort (rest item) #'string< :key #'first)))
      table))

#|
  (setf p (finite-ss-pre-table-table
             (finite-ss-pre-table '(2 v0 (e1 e2) v1 1 e0 e1 e2)))))
|#

(DEFUN FINITE-SS-FIND-GMSM (table gmsm dmns &optional (max-dmns (1+ dmns)))
   (declare
      (type gmsm gmsm)
      (fixnum dmns max-dmns))
   (do ((dmns dmns (1+ dmns)))
       ((>= dmns max-dmns) nil)
      (declare (fixnum dmns))
      (let ((found (find gmsm (svref table dmns) :test #'eq :key #'car)))
         (declare (type (or cons null) found))
         (when found
            (return-from finite-ss-find-gmsm dmns)))))

(DECLAIM (FTYPE function finite-ss-finish-line))

(DEFUN FINITE-SS-FINISH-TABLE (table bspn)
   (declare (simple-vector table))
   (dotimes (dmns (length table))
      (declare (fixnum dmns))
      (finite-ss-finish-line table dmns bspn))
   table)

(DECLAIM (FTYPE function finite-ss-finish-entry))

(DEFUN FINITE-SS-FINISH-LINE (table dmns bspn)
   (declare
      (simple-vector table)
      (fixnum dmns))
   (setf (svref table dmns)
         (mapcar
            #'(lambda (entry)
                 (finite-ss-finish-entry table entry dmns bspn))
            (svref table dmns))))

(DEFUN FINITE-SS-FINISH-ENTRY (table entry dmns bspn)
   (declare
      (simple-vector table)
      (list entry)
      (fixnum dmns)
      (symbol bspn))
   (let ((simplex (first entry))
         (faces (rest entry)))
      (declare
         (symbol simplex)
         (list faces))
      (when (zerop dmns)
         (return-from finite-ss-finish-entry
            (make-gmsm-faces-info
               :gmsm simplex :faces +s-empty-vector+
               :bndr +zero-negative-cmbn+)))
      (setf faces (nconc faces (make-list (1+ (- dmns (length faces)))
                                  :initial-element bspn)))
      (let ((rslt (make-gmsm-faces-info :gmsm simplex)))
         (declare (cons rslt))
         (flet ((process-face (face)
                 (declare (type (or symbol list) face))
                 (when (symbolp face)
                    (setf face (list face)))
                 (let* ((gmsm2 (car (last face)))
                        (dgop-ext (nbutlast face))
                        (dmns2 (finite-ss-find-gmsm table gmsm2 0 dmns)))
                    (declare
                       (symbol gmsm2)
                       (list dgop-ext)
                       (type (or fixnum null) dmns2))
                    (unless dmns2
                       (error "In BUILD-FINITE-SS, the face ~A is absent." gmsm2))
                    (when (zerop (length dgop-ext))
                       (setf dgop-ext (nreverse (<a-b< dmns2 (1- dmns)))))
                    (unless (= (+ (length dgop-ext) dmns2 1) dmns)
                       (error "In BUILD-FINITE-SS, the face ~A has a wrong dimension."
                          (append dgop-ext (list gmsm2))))
                    (absm (dgop-ext-int dgop-ext) gmsm2))))
            (setf (info-faces rslt)
                  (map 'simple-vector #'process-face faces))
            (setf (info-bndr rslt)
                  (apply #'nterm-add #'s-cmpr (1- dmns)
                     (do ((rslt +empty-list+)
                          (faces (info-faces rslt))
                          (indx dmns (1- indx)))
                         ((minusp indx) rslt)
                        (declare
                           (list rslt)
                           (simple-vector faces)
                           (fixnum indx))
                        (let ((face (svref faces indx)))
                           (declare (type absm face))
                           (unless (degenerate-p face)
                              (push (term (-1-expt-n indx) (gmsm face))
                                 rslt))))))
            rslt))))

(DEFUN FINITE-SS-TABLE (list)
   (declare (list list))
   (setf list (cons 0 list))
   (let* ((bspn (second list))
          (pre-table (finite-ss-pre-table list))
          (table (finite-ss-pre-table-table pre-table)))
      (declare
         (symbol bspn)
         (list pre-table)
         (simple-vector table))
      ;; (vector (vector gmsm-faces-info))
      (finite-ss-finish-table table bspn)))

#|
  (finite-ss-table '(*))
  (finite-ss-table '(a b))
  (finite-ss-table '(a b 1 c (b a)))
  (finite-ss-table '(* 2 s2 3 s3))
  (finite-ss-table '(s0 s1 s2 1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1) 2 s012 (s12 s02 s01)))
  (finite-ss-table '(* 4 s4 ((2 1 0 *))))
  (finite-ss-table '(* 4 s4 ((1 1 0 *))))
  (finite-ss-table '(* 1 s (t)))
  (finite-ss-table '(* 4 s4 ((1 0 *)))))
|#

(DEFUN FINITE-SS-BASIS (table)
   (declare (simple-vector table))
   (flet ((rslt (dmns)
             (declare (fixnum dmns))
             (the list
                (if (< -1 dmns (length table))
                   (mapcar #'car (svref table dmns))
                   +empty-list+))))
      #'rslt))

(DEFUN FINITE-SS-FACE (ind-smst table)
   (declare
      (symbol ind-smst)
      (simple-vector table))
   (flet ((rslt (indx dmns gmsm)
           (declare
              (fixnum indx dmns)
              (symbol gmsm))
           (let ((found (find gmsm (svref table dmns) :key #'car)))
              (unless found
                 (error "In the finite simplicial set ~A,~@
                         the simplex ~A is absent in dimension ~D." (eval ind-smst) gmsm dmns))
              (svref (info-faces found) indx))))
      #'rslt))

(DEFUN FINITE-SS-INTR-BNDR (ind-smst table)
   (declare
      (symbol ind-smst)
      (simple-vector table))
   (flet ((rslt (dmns gmsm)
           (declare
              (fixnum dmns)
              (symbol gmsm))
           (let ((found (find gmsm (svref table dmns) :key #'car)))
              (unless found
                 (error "In the finite simplicial set ~A,~@
                         the simplex ~A is absent in dimension ~D." (eval ind-smst) gmsm dmns))
              (info-bndr found))))
      #'rslt))

(DEFUN BUILD-FINITE-SS (list)
   (declare (list list))
   (let ((bspn (first list))
         (table (finite-ss-table list))
         (ind-smst (gensym)))
      (declare
         (symbol bspn ind-smst)
         (simple-vector table))
      ;;  (vector (vector gmsm-faces-info))
      (let ((rslt (build-smst
                     :cmpr #'s-cmpr
                     :basis (finite-ss-basis table)
                     :bspn bspn
                     :face (finite-ss-face ind-smst table)
                     :intr-bndr (finite-ss-intr-bndr ind-smst table)
                     :bndr-strt :gnrt
                     :orgn `(build-finite-ss ,list))))
         (setf (symbol-value ind-smst) rslt)
         (unless (check-smst rslt 0 (length table))
            (pop *smst-list*)
            (pop *chcm-list*)
            (pop *mrph-list*)
            (return-from build-finite-ss nil))
         rslt)))

#|
()
(cat-init)
(setf tr '(s0 s1 s2
              1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
              2 s012 (s12 s02 s01)))
(setf tr-sseh (build-finite-ss tr))
(cmpr tr-sseh 's01 's02)
(basis tr-sseh 2)
(bspn tr-sseh)
(face tr-sseh 1 2 's012)
(? tr-sseh 2 's012)
(? tr-sseh *)
(inspect tr-sseh)
(mapcar #'(lambda (s) (length (eval s))) *list-list*)
(setf tr (build-finite-ss '(s0 s1 s2
                               1 s01 (s1 s1) s02 (s2 s0) s12 (s2 s1)
                               2 s012 (s12 s02 s01))))
(mapcar #'(lambda (s) (length (eval s))) *list-list*))
|#

(DEFUN SPHERE-CMPR (gmsm1 gmsm2)
   (declare (ignore gmsm1 gmsm2))
   (the cmpr :equal))

(DEFUN SPHERE-BASIS (dmns)
   (declare (fixnum dmns))
   (let ((fund-gmsm (intern (format nil "S~D" dmns))))
      (declare (symbol fund-gmsm))
      (flet ((rslt (dmns2)
                (declare (fixnum dmns2))
                (cond ((zerop dmns2)
                       (list '*))
                      ((= dmns2 dmns)
                       (list fund-gmsm))
                      (t
                         +empty-list+))))
         (the basis #'rslt))))

(DEFUN SPHERE-FACE (dmns)
   (declare (fixnum dmns))
   (let ((face (absm (mask (1- dmns)) '*)))
      (declare (type absm face))
      (flet ((rslt (indx dmns2 gmsm)
                (declare (ignore indx dmns2 gmsm))
                (the absm face)))
         #'rslt)))

(DEFUN SPHERE (dmns)
   (declare (fixnum dmns))
   (unless (plusp dmns)
      (error "In SPHERE, the dimension ~D should be positive." dmns))
   (unless (< dmns +maximal-dimension+)
      (error "In SPHERE, the dimension ~D should be < ~D."
         dmns +maximal-dimension+))
   (the simplicial-set
      (let ((rslt (build-smst
                     :cmpr #'sphere-cmpr
                     :basis (sphere-basis dmns)
                     :bspn '*
                     :face (sphere-face dmns)
                     :intr-bndr #'zero-intr-dffr
                     :bndr-strt :cmbn
                     :orgn `(sphere ,dmns))))
         (declare (type simplicial-set rslt))
         (setf (slot-value (bndr rslt) 'orgn)
               `(zero-mrph ,rslt ,rslt -1))
         rslt)))

#|
  (cat-init)
  (setf s3 (sphere 3))
  (funcall (cmpr s3) 's3 's3)
  (dotimes (i 5)
     (print (funcall (basis s3) i)))
  (mapcar #'(lambda (i) (funcall (face s3) i 3 's3)) (<a-b> 0 3))
  (? s3 3 's3)
  (smst (idnm s3))
  (chcm (idnm s3))
  (setf d (bndr s3))
  (add d d))
|#

(DEFUN SPHERE-WEDGE-BASIS (dmns-list)
   (declare (list dmns-list))
   (flet ((rslt (dmns)
             (declare (fixnum dmns))
             (when (zerop dmns)
                (return-from rslt '(*)))
             (do ((i (count dmns dmns-list) (1- i))
                  (basis +empty-list+
                         (cons (intern (format nil "S~D-~D" dmns i))
                               basis)))
                 ((zerop i) basis)
                (declare
                   (fixnum i)
                   (list basis)))))
      (the basis #'rslt)))

(DEFUN SPHERE-WEDGE-FACE (indx dmns gmsm)
   (declare
      (ignore indx gmsm)
      (fixnum dmns))
   (the absm
      (absm (mask (1- dmns)) '*)))

(DEFUN SPHERE-WEDGE (&rest dmns-list)
   (declare (list dmns-list))
   (the simplicial-set
      (let ((rslt (build-smst
                     :cmpr #'s-cmpr
                     :basis (sphere-wedge-basis dmns-list)
                     :face #'sphere-wedge-face
                     :intr-bndr #'zero-intr-dffr
                     :bndr-strt :cmbn
                     :orgn `(sphere-wedge ,@dmns-list))))
         (declare (type simplicial-set rslt))
         (setf (slot-value (bndr rslt) 'orgn)
               `(zero-mrph ,rslt ,rslt -1))
         rslt)))

#|
  (cat-init)
  (setf w (sphere-wedge 3 2 3))
  (funcall (cmpr w) 's3-1 's3-2)
  (dotimes (i 5) (print (funcall (basis w) i)))
  (funcall (face w) 2 3 's3-1)
  (gnrt-? (bndr w) 3 's3-2))
|#

(DEFUN MOORE-CMPR (gmsm1 gmsm2)
   (declare (ignore gmsm1 gmsm2))
   (the cmpr :equal))

(DEFUN MOORE-BASIS (dmns)
   (declare (fixnum dmns))
   (let ((lgmsm1 (list (intern (format nil "M~D" dmns))))
         (lgmsm2 (list (intern (format nil "N~D" (1+ dmns))))))
      (declare (list lgmsm1 lgmsm2))
      (flet ((rslt (dmns2)
                (declare (fixnum dmns2))
                (cond ((zerop dmns2) '(*))
                      ((= dmns dmns2) lgmsm1)
                      ((= (1+ dmns) dmns2) lgmsm2)
                      (t +empty-list+))))
         (the basis #'rslt))))

(DEFUN MOORE-FACE (pii dmns)
   (declare (fixnum pii dmns))
   (let ((face (absm 0 (intern (format nil "M~D" dmns))))
         (bspn1 (absm (mask (1- dmns)) '*))
         (bspn2 (absm (mask dmns) '*))
         (2pii (ash pii 1)))
      (declare
         (type absm face)
         (type absm bspn1 bspn2)
         (fixnum 2pii))
      (flet ((rslt (indx dmns2 gmsm)
                (declare
                   (fixnum indx dmns2)
                   (ignore gmsm))
                (the absm
                   (if (= dmns dmns2)
                      bspn1
                      (if (oddp indx)
                         bspn2
                         (if (< indx 2pii)
                            face
                            bspn2))))))
         (the face #'rslt))))

(DEFUN MOORE-INTR-BNDR (pii dmns)
   (declare (fixnum pii dmns))
   (let ((1+dmns (1+ dmns))
         (gmsm1 (intern (format nil "M~D" dmns))))
      (declare
         (fixnum 1+dmns)
         (type symbol gmsm1))
      (flet ((rslt (cmbn)
                (declare
                   (type cmbn cmbn))
                (with-cmbn (degr list) cmbn
                   (unless list
                      (return-from rslt (zero-cmbn (1- (cmbn-degr cmbn)))))
                   (if (= degr 1+dmns)
                      (term-cmbn dmns (* (cffc (first list)) pii) gmsm1)
                      (zero-cmbn (1- (cmbn-degr cmbn)))))))
         (the intr-mrph #'rslt))))

(DEFUN MOORE (pii dmns)
   (declare (fixnum pii dmns))
   (the simplicial-set
      (build-smst
         :cmpr #'moore-cmpr
         :basis (moore-basis dmns)
         :face (moore-face pii dmns)
         :intr-bndr (moore-intr-bndr pii dmns)
         :bndr-strt :cmbn
         :orgn `(moore ,pii ,dmns))))

#|
  (cat-init)
  (setf m4 (moore 2 4))
  (cmpr m4 'n5 'n5)
  (dotimes (i 7)
     (print (basis m4 i)))
  (mapcar #'(lambda (i) (face m4 i 5 'n5)) (<a-b> 0 5))
  (? m4 4 'm4)
  (? m4 5 'n5))
|#

(DEFUN R-PROJ-SPACE-CMPR (gmsm1 gmsm2)
   (declare (ignore gmsm1 gmsm2))
   (the cmpr :equal))

(DEFUN R-PROJ-SPACE-BASIS (k l)
  (declare (fixnum k) (type (or (eql :infinity) fixnum) l))
  (the basis
    (progn
      (when (eq l :infinity)
        (setf l most-positive-fixnum))
      (flet
          ((rslt (dmns)
                 (declare (fixnum dmns))
                 (the list
                   (if (or (minusp dmns)
                           (< 0 dmns k)
                           (>= dmns l))
                       +empty-list+
                     (list dmns)))))
        (the basis #'rslt)))))

#|
()
(setf b (r-proj-space-basis 1 :infinity))
(dotimes (i 5) (print (funcall b i)))
(setf b (r-proj-space-basis 1 5))
(dotimes (i 8) (print (funcall b i)))
(setf b (r-proj-space-basis 3 :infinity))
(dotimes (i 5) (print (funcall b i)))
(setf b (r-proj-space-basis 3 6))
(dotimes (i 10) (print (funcall b i)))
|#

(DEFUN R-PROJ-SPACE-FACE (k)
   (declare (fixnum k))
   (flet ((rslt (indx dmns gmsm)
           (declare
	      (fixnum indx dmns)
	      (ignore gmsm))
           (if (<= dmns k)
              (absm (mask (1- dmns)) 0)
              (if (or (zerop indx)
                      (= indx dmns))
                 (absm 0 (1- dmns))
                 (if (= dmns (1+ k))
                    (absm (mask (1- dmns)) 0)
                    (absm (2-exp (1- indx)) (- dmns 2)))))))
      (the face #'rslt)))

(DEFUN R-PROJ-SPACE-INTR-BNDR (k)
   (declare (fixnum k))
   (flet ((rslt (cmbn)
           (declare (type cmbn cmbn))
           (with-cmbn (degr list) cmbn
              (unless list
                 (return-from rslt (zero-cmbn (1- degr))))
              (if (<= degr k)
                 (zero-cmbn (1- degr))
                 (if (evenp degr)
                    (make-cmbn
                       :degr (1- degr)
                       :list (list (term (* 2 (-cffc list)) (1- degr))))
                    (zero-cmbn (1- degr)))))))
      (the intr-mrph #'rslt)))

(DEFUN R-PROJ-SPACE (&optional (l :infinity) (k 1))
  (declare (fixnum k) (type (or fixnum (eql :infinity)) l))
  (assert
   (and (typep k 'fixnum)
        (plusp k)
        (or (eq l :infinity)
            (and (typep l 'fixnum)
                 (<= k l)))))    
  (the simplicial-set
    (build-smst
     :cmpr #'R-proj-space-cmpr
     :basis (R-proj-space-basis k l)
     :bspn 0
     :face (R-proj-space-face k)
     :intr-bndr (R-proj-space-intr-bndr k)
     :bndr-strt :cmbn
     :orgn `(R-proj-space ,k ,l))))

#|
()
(cat-init)
(setf p (R-proj-space))
(basis p 4)
(dotimes (i 5)
  (print (face p i 4 4)))
(dotimes (i 5)
  (print (? p i i)))
(setf dd (cmps p p))
(dotimes (i 6)
  (print (? dd i i)))
(setf p (R-proj-space 3))
(dotimes (i 7)
  (print (basis p i)))
(dotimes (i 5)
  (print (face p i 4 4)))
(dotimes (i 7)
  (print (? p i i)))
(setf dd (cmps p p))
(dotimes (i 7)
  (print (? dd i i)))
|#

;;; Constructing a small simplicial subset
;;; from a combination of simplices.

(DEFUN CMBN-GMSMS (cmbn)
  (declare (type cmbn cmbn))
  (the list
    ;; Returns a list made of:
    ;;   the degree of the combination;
    ;;   the list of simplex generators of the combinations.
    (cons (cmbn-degr cmbn) (mapcar #'cdr (cmbn-list cmbn)))))

#|
()
|#

(DEFUN GMSMS-SUBSMST (smst gmsms
                           &aux
                           (dmns (typecase gmsms
                                   (cmbn (cmbn-degr gmsms))
                                   (otherwise (first gmsms))))
                           (cmpr= #'(lambda (gmsm1 gmsm2)
                                      (declare (type gmsm gmsm1 gmsm2))
                                      (the boolean
                                        (eq (cmpr smst gmsm1 gmsm2) :equal)))))                           
  (declare
   (type simplicial-set smst)
   (type (or cmbn list) gmsms)
   (fixnum dmns)
   (type (function (gmsm gmsm) boolean) cmpr=))
  (when (typep gmsms 'cmbn)
    (setf gmsms (cmbn-gmsms gmsms)))
  (the (values simplicial-set morphism)
    (let ((prslt (make-array (1+ dmns) :initial-element +empty-list+)))
      (declare (type simple-vector prslt))
      (setf (svref prslt dmns) (rest gmsms))
      (do ((d dmns (1- d)))
          ((zerop d))
        (declare (fixnum d))
        (dolist (gmsm (svref prslt d))
          (declare (type gmsm gmsm))
          (dotimes (iface (1+ d))
            (declare (fixnum iface))
            (with-absm (dgop gmsm) (face smst iface d gmsm)
                       (declare (fixnum dgop) (type gmsm gmsm))
              (pushnew gmsm (svref prslt (- d 1 (logcount dgop)))
                       :test cmpr=)))))
      (dotimes (i (1+ dmns))
        (declare (type fixnum i))
        (setf (svref prslt i)
          (sort (svref prslt i)
                #'(lambda (gmsm1 gmsm2)
                    (eq :less
                        (cmpr smst gmsm1 gmsm2))))))
      (let ((rslt-basis #'(lambda (k)
                            (declare (fixnum dmns))
                            (the list
                              (if (<= 0 k dmns) 
                                  (svref prslt k)
                                +empty-list+)))))
        (declare (type (function (fixnum) list) rslt-basis))
        (let ((subsmst (build-smst
                        :cmpr (cmpr smst)
                        :basis rslt-basis
                        :bspn (let ((bspn (bspn smst)))
                                (declare (type gmsm bspn))
                                (pushnew bspn (svref prslt 0) :test cmpr=)
                                bspn)
                        :face (face smst)
                        :orgn `(gmsms-subsmst ,gmsms))))
          (declare (type simplicial-set subsmst))
          (values subsmst
                  (build-mrph :sorc subsmst
                              :trgt smst
                              :degr 0
                              :intr #'(lambda (cmbn)
                                        (declare (type cmbn cmbn))
                                        (the cmbn cmbn))
                              :strt :cmbn
                              :orgn `(gmsms-subsmst-incl ,gmsms))))))))

#|
()
(cat-init)
(setf k (k-z 2))
(setf efhm (efhm k))
(setf *homology-verbose* nil)
(chcm-homology-gen (rbcc efhm) 4)
(setf g (first *))
(setf cmbn (lf efhm (rg efhm g)))
(multiple-value-setq (kf incl) (gmsms-subsmst k cmbn))
(dotimes (i 5) (print (length (basis kf i))))
(setf cone (cone incl))
(efhm cone)
(homology kf 0 5)
(homology (cone incl) 0 6)
(display-finite-ss kf 5)

(chcm-homology-gen (rbcc efhm) 8)
(setf g (first *))
(setf cmbn (lf efhm (rg efhm g)))
(multiple-value-setq (kf incl) (gmsms-subsmst k cmbn))
(setf cone (cone incl))
(efhm cone)
(homology kf 0 9)
(homology (cone incl) 0 10)
(dotimes (i 9) (print (length (basis kf i))))

(chcm-homology-gen (rbcc efhm) 10)
(setf g (first *))
(setf cmbn (lf efhm (rg efhm g)))
(multiple-value-setq (kf incl) (gmsms-subsmst k cmbn))
(setf cone (cone incl))
(efhm cone)
(homology kf 0 11)
(homology (cone incl) 0 12)
(dotimes (i 11) (print (length (basis kf i))))

()
(setf s2 (sphere 2))
(setf cl2 (chml-clss s2 2))
(setf f2 (z-whitehead s2 cl2))
(setf s3 (fibration-total f2))
(setf efhm (efhm s3))
(basis (rbcc efhm) 3)
(setf cmbn (lf efhm (rg efhm 3 (first (basis (rbcc efhm) 3)))))
(setf kf (gmsms-subsmst s3 cmbn))
(homology kf 0 4)
|#

(DEFUN DISPLAY-FINITE-SS (ss n &aux (cmpr (cmpr ss)))
  (declare (type simplicial-set ss) (type fixnum n))
  (the (values)
    (let ((ss-table (make-array n)))
      (declare (type simple-vector ss-table))
      (dotimes (i n)
        (declare (type fixnum i))
        (let ((basis-i (basis ss i)))
          (declare (type list basis-i))
          (setf (svref ss-table i)
            (make-array (length basis-i)
                        :element-type 'absm
                        :initial-contents basis-i))))
      (flet
          ((absm-name
            (dmns absm)
            (declare (type absm absm))
            (the string
              (with-slots (dgop gmsm) absm
                (declare (type fixnum dgop) (type gmsm gmsm))
                (let* ((ext-dgop (dgop-int-ext dgop))
                       (gmsm-dmns (- dmns (length ext-dgop))))
                  (declare (type list ext-dgop) (type fixnum gmsm-dmns))
                  (format nil
                      "<Absm ~A S~A~A>"
                    (hyphenize-list ext-dgop)
                    gmsm-dmns
                    (position-if #'(lambda (item)
                                     (declare (type gmsm item))
                                     (eq :equal (funcall cmpr gmsm item)))
                                 (svref ss-table gmsm-dmns))))))))
        (declare (ftype (function (fixnum absm) string) absm-name))
        (dotimes (i n)
          (declare (type fixnum i))
          (format t "Dimension ~D:~%" i)
          (let ((ss-table-i (svref ss-table i)))
            (declare (type vector ss-table-i))
            (do ((j 0 (1+ j))
                 (jend (length ss-table-i)))
                ((eql j jend))
              (declare (type fixnum j jend))
              (format t "  S~D~D~%" i j)
              (unless (zerop i)
                (let ((ss-table-i-j (svref ss-table-i j)))
                  (declare (type gmsm ss-table-i-j))
                  (dotimes (k (1+ i))
                    (declare (type fixnum k))
                    (format t "    Face ~D = ~A~%"
                      k (absm-name (1- i) (face ss k i ss-table-i-j)))))))))))))

#|
(cat-init)
(setf m (moore 2 3))
(display-finite-ss m 5)
(setf s2 (sphere 2))
(display-finite-ss s2 3)
(setf d3 (delta 3))
(display-finite-ss d3 4)
|#
            
                                 
                
                      
        
      

































  
