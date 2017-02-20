;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS
;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS
;;;  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS  SPECIAL-SMSTS

(IN-PACKAGE "CAT")

(PROVIDE "special-smsts")

;;; GMSM-FACES-INFO = (gmsm (simple-vector absm) . dffr)
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
 
(DEFUN FINITE-SS-FINISH-TABLE (table bsgn)
   (declare (simple-vector table))
   (dotimes (dmns (length table))
      (declare (fixnum dmns))
      (finite-ss-finish-line table dmns bsgn))
   table)

(DEFUN FINITE-SS-FINISH-LINE (table dmns bsgn)
   (declare
      (simple-vector table)
      (fixnum dmns))
   (setf (svref table dmns)
         (mapcar
            #'(lambda (entry)
                 (finite-ss-finish-entry table entry dmns bsgn))
            (svref table dmns))))

(DEFUN FINITE-SS-FINISH-ENTRY (table entry dmns bsgn)
   (declare
      (simple-vector table)
      (list entry)
      (fixnum dmns)
      (symbol bsgn))
   (let ((simplex (first entry))
         (faces (rest entry)))
      (declare
         (symbol simplex)
         (list faces))
      (when (zerop dmns)
         (return-from finite-ss-finish-entry
            (make-gmsm-faces-info
               :gmsm simplex :faces +s-empty-vector+
               :dffr +zero-negative-cmbn+)))
      (setf faces (nconc faces (make-list (1+ (- dmns (length faces)))
                                  :initial-element bsgn)))
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
            (setf (info-dffr rslt)
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
   (let* ((bsgn (second list))
          (pre-table (finite-ss-pre-table list))
          (table (finite-ss-pre-table-table pre-table)))
      (declare
         (symbol bsgn)
         (list pre-table)
         (simple-vector table))
      ;; (vector (vector gmsm-faces-info))
      (finite-ss-finish-table table bsgn)))

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

(DEFUN FINITE-SS-DFFR-INTR (ind-smst table)
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
              (info-dffr found))))
      #'rslt))

(DEFUN BUILD-FINITE-SS (list)
  (declare (list list))
  (let ((bsgn (first list))
        (table (finite-ss-table list))
        (ind-smst (gensym)))
    (declare
     (symbol bsgn ind-smst)
     (simple-vector table))
    ;;  (vector (vector gmsm-faces-info))
    (let ((rslt (simplicial-set
                 :cmpr #'s-cmpr
                 :basis (finite-ss-basis table)
                 :bsgn bsgn
                 :face (finite-ss-face ind-smst table)
                 :dffr-intr (finite-ss-dffr-intr ind-smst table)
                 :dffr-strt :gnrt
                 :dfnt `(build-finite-ss ,list))))
      (set ind-smst rslt)
      (unless (check-smst rslt 0 (length table))
        ;; ???            (pop *k-list*)
        ;; ???            (pop *k-list*)
        (return-from build-finite-ss nil))
      rslt)))

#|
  (cat-init)
  (setf tr (build-finite-ss '(s0 s1 s2
                            1 s01 (s1 s0) s02 (s2 s0) s12 (s2 s1)
                            2 s012 (s12 s02 s01))))
  (cmpr tr 's01 's02)
  (basis tr 2)
  (bsgn tr)
  (face tr 1 2 's012)
  (? tr 2 's012)
  (? tr *)
  (inspect tr)
  (mapcar #'(lambda (s) (length (eval s))) *k-list*)
  (setf tr (build-finite-ss '(s0 s1 s2
                            1 s01 (s1 s1) s02 (s2 s0) s12 (s2 s1)
                            2 s012 (s12 s02 s01))))
  (mapcar #'(lambda (s) (length (eval s))) *k-list*))
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
      (declare (type absm absm))
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
    (let ((rslt (simplicial-set
                 :cmpr #'sphere-cmpr
                 :basis (sphere-basis dmns)
                 :bsgn '*
                 :face (sphere-face dmns)
                 :dffr-intr #'zero-dffr-intr
                 :dffr-strt :cmbn
                 :dfnt `(sphere ,dmns))))
      (declare (type simplicial-set rslt))
      (setf (slot-value (dffr rslt) 'dfnt)
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
  (setf d (dffr s3))
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
    (let ((rslt (simplicial-set
                 :cmpr #'s-cmpr
                 :basis (sphere-wedge-basis dmns-list)
                 :face #'sphere-wedge-face
                 :dffr-intr #'zero-dffr-intr
                 :dffr-strt :cmbn
                 :dfnt `(sphere-wedge ,@dmns-list))))
      (declare (type simplicial-set rslt))
      (setf (slot-value (dffr rslt) 'dfnt)
        `(zero-mrph ,rslt ,rslt -1))
      rslt)))

#|
  (cat-init)
  (setf w (sphere-wedge 3 2 3))
  (funcall (cmpr w) 's3-1 's3-2)
  (dotimes (i 5) (print (funcall (basis w) i)))
  (funcall (face w) 2 3 's3-1)
  (gnrt-? (dffr w) 3 's3-2))
|#

(DEFUN MOORE-CMPR (gmsm1 gmsm2)
   (declare (ignore gmsm1 gmsm2))
   (the cmpr :equal))

(DEFUN MOORE-BASIS (dmns)
   (declare (fixnum dmns))
   (let ((lgmsm1 (list (intern (format nil "M~D" dmns))))
         (lgmsm2 (list (intern (format nil "N~D" (1+ dmns))))))
      (declare (symbol gmsm1 gmsm2))
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
         (bsgn1 (absm (mask (1- dmns)) '*))
         (bsgn2 (absm (mask dmns) '*))
         (2pii (ash pii 1)))
      (declare
         (type absm face)
         (type absm bsgn1 bsgn2)
         (fixnum 2pii))
      (flet ((rslt (indx dmns2 gmsm)
                (declare
                   (fixnum indx dmns2)
                   (ignore gmsm))
                (the absm
                   (if (= dmns dmns2)
                      bsgn1
                      (if (oddp indx)
                         bsgn2
                         (if (< indx 2pii)
                            face
                            bsgn2))))))
         (the face #'rslt))))

(DEFUN MOORE-DFFR-INTR (pii dmns)
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
         (the intr #'rslt))))

(DEFUN MOORE (pii dmns)
  (declare (fixnum pii dmns))
  (the simplicial-set
    (simplicial-set
     :bsgn '*
     :cmpr #'moore-cmpr
     :basis (moore-basis dmns)
     :face (moore-face pii dmns)
     :dffr-intr (moore-dffr-intr pii dmns)
     :dffr-strt :cmbn
     :dfnt `(moore ,pii ,dmns))))

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

(DEFUN R-PROJ-SPACE-BASIS (k &optional (l 15))
   (declare (fixnum k))
   (flet ((rslt (dmns)
             (declare (fixnum dmns))
             (the list
                (if (or (minusp dmns)
                        (< 0 dmns k)
		        (>= dmns l))
                   +empty-list+
                   (list dmns)))))
      (the basis #'rslt)))

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

(DEFUN R-PROJ-SPACE-DFFR-INTR (k)
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
      (the intr #'rslt)))

(DEFUN R-PROJ-SPACE (&optional (k 1) (l 15))
  (declare (fixnum k))
  (the simplicial-set
    (simplicial-set
     :cmpr #'R-proj-space-cmpr
     :basis (R-proj-space-basis k l)
     :bsgn 0
     :face (R-proj-space-face k)
     :dffr-intr (R-proj-space-dffr-intr k)
     :dffr-strt :cmbn
     :dfnt `(R-proj-space ,k))))

#|
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
