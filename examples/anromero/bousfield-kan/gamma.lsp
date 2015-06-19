;; GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA
;; GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA
;; GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA GAMMA

(IN-PACKAGE "COMMON-LISP-USER")

(provide "gamma")


;; TYPES

(DEFUN SGM-P (object)
   ;; SGM = SiGMa   
   (declare (type any object))
   (the boolean
     (and (consp object)
          (typep (car object) 'fixnum)
          (typep (cdr object) 'cmbn))))

(DEFTYPE SGM () '(satisfies sgm-p))

(DEFUN IGAMMA-P (object)
   ;; IGAMMA = Internal GAMMA,
   ;;   without the keyword and the degree
   (declare (type any object))
   (the boolean
     (and (listp object)
          (every #'sgm-p object)))) 

(DEFTYPE IGAMMA () '(satisfies igamma-p))

(DEFUN GAMMA-P (object)
   (declare (type any object))
   (the boolean
     (and (consp object)
          (eql (car object) :gamma)
          (consp (cdr object))
          (typep (second object) 'fixnum)
          (typep (cddr object) 'igamma))))

(DEFTYPE GAMMA () '(satisfies gamma-p))



;; MACROS

(DEFMACRO SGM (sgop cmbn)
   `(cons ,sgop ,cmbn))

(DEFMACRO -SGM (mark)
   `(car ,mark))

(DEFMACRO SGOP (sgm)
   `(car ,sgm))

(DEFMACRO -SGOP (mark)
   `(caar ,mark))

(DEFMACRO SGCM (sgm)
   `(cdr ,sgm))

(DEFMACRO -SGCM (mark)
   `(cdar ,mark))

(DEFMACRO WITH-SGM ((sgop-var sgcm-var) sgm . body)
   `(let ((,sgop-var (sgop ,sgm))
          (,sgcm-var (sgcm ,sgm)))
       (declare
          (fixnum ,sgop-var)
          (type cmbn ,sgcm-var))
       ,@body))

(DEFMACRO WITH--SGM ((sgop-var sgcm-var) mark . body)
   `(let ((,sgop-var (-sgop ,mark))
          (,sgcm-var (-sgcm ,mark)))
       (declare
          (fixnum ,sgop-var)
          (type cmbn ,sgcm-var))
       ,@body))

(DEFMACRO SGM-NON-ZERO-P (sgm)
   `(cmbn-non-zero-p (sgcm ,sgm)))

(DEFMACRO SGM-ZERO-P (sgm)
  `(cmbn-zero-p (sgcm ,sgm)))

(DEFMACRO MAKE-GAMMA (&key degr list)
   `(cons :gamma
       (cons ,degr ,list)))

(DEFMACRO GAMMA-DEGR (gmm)
   `(cadr ,gmm))

(DEFMACRO GAMMA-LIST (gmm)
   `(cddr ,gmm))

(DEFMACRO WITH-GAMMA ((degr list) gmm . body)
   `(let ((,degr (gamma-degr ,gmm))
          (,list (gamma-list ,gmm)))
       (declare
          (fixnum ,degr)
          (list ,list))
       ,@body))

(DEFMACRO SGM-GAMMA (degr sgop sgcm)
   `(cons :gamma (cons ,degr (cons (cons ,sgop ,sgcm) nil))))

(DEFMACRO GAMMA-NON-ZERO-P (gmm)
   `(gamma-list ,gmm)) 

(DEFMACRO GAMMA-ZERO-P (gmm)
  `(null (gamma-list ,gmm)))


#|(DEFMETHOD PRINT-KEYCONS ((key (eql :sgm)) cdr stream)
   (setf cdr (cons :sgm cdr))
   (format stream
      "<Sgm ~A ~A>"
      (hyphenize-list (dgop-int-ext (sgop cdr)))
      (sgcm cdr))
   cdr)
|#


;; METHODS

(DEFMETHOD PRINT-KEYCONS ((key (eql :gamma)) cdr stream)
   (declare
    (list cdr)
    (stream stream))
   (the (eql t)
     (let ((degr (first cdr))
           (list (rest cdr)))
        (declare
         (fixnum degr)
         (list list))
        (format stream "<GAMMA ~D" degr)
        (do ((mark list (cdr mark))
             (nth 0 (1+ nth)))
            ((endp mark))
           (declare
            (list mark)
            (fixnum nth))
           (when (and *print-length* (= nth *print-length*))
              (format stream "~%... ...")
              (return))
           (format stream "~%  <~D  ~A  >" (hyphenize-list (dgop-int-ext (sgop (car mark)))) (sgcm (car mark))))
        (format stream "~%>" )
        t)))


;; ELEMENTARY OPERATIONS WITH GAMMA ELEMENTS
 
(DEFUN GAMMA-ELT (degr &rest rest)
   (declare (fixnum degr))
   (the gamma
     (let ((rslt (list degr :gamma)))
        (do ((mark rest (cddr mark)))
            ((endp mark) (nreverse rslt))
           (declare (list mark))
           (push (sgm (car mark) (cadr mark)) rslt)))))

(DEFUN CHECK-GAMMA (gmm)
  (declare
   (type gamma gmm))
  (the boolean
    (with-gamma (degr glist) gmm
      (declare
       (fixnum degr)
       (list glist))
      (do ((mark2 glist mark1)
           (mark1 (rest glist) (cdr mark1)))
          ((endp mark1) t)
         (declare
          (list mark1 mark2))
         (unless (= degr (+ (length (dgop-int-ext (-sgop mark2))) (cmbn-degr (-sgcm mark2))))
            (error "In CHECK-GAMMA, bad degre; the element:~@
                ~A~@
                has a degree different to:~@
                ~A."
	       (-sgm mark2) degr))
         (unless (< (-sgop mark2) (-sgop mark1))
            (error "In CHECK-GAMMA, bad order; the degeneracy operator:~@
                ~A~@
                is not less than the degeneracy operator:~@
                ~A."
              (dgop-int-ext (-sgop mark2)) (dgop-int-ext (-sgop mark1))))))))

#|
(setf g1 (gamma-elt 5 3 (cmbn 3 8 'a) 1 (cmbn 4 3 'b)))
(check-gamma g1)
(setf g2 (gamma-elt 5 3 (cmbn 4 3 'b) 1 (cmbn 3 8 'a)))
(check-gamma g2)
(setf g3 (gamma-elt 5 1 (cmbn 4 3 'b) 3 (cmbn 3 8 'a)))
(check-gamma g3)
(setf g4 (gamma-elt 5))
(check-gamma g4)
|#

(DEFUN ZERO-GAMMA (degr)
   (declare (fixnum degr))
   (the gamma
     (make-gamma
      :degr degr
      :list +empty-list+)))

#|
(setf g0 (zero-gamma 3))
(check-gamma g0) 
|#

(DEFUN GAMMA-OPPS (gmm)
   (declare (type gamma gmm))
   (the gamma
      (with-gamma (degr glist) gmm
         (make-gamma
            :degr degr
            :list (mapcar #'(lambda (sgm)
                               (declare (type sgm sgm))
                               (with-sgm (sgop cmbn) sgm
                                  (sgm sgop (cmbn-opps cmbn))))
                     glist)))))

#|
(setf g (gamma-elt 5 1 (cmbn 4 3 'b 8 'c) 3 (cmbn 3 8 'a -9 'b)))
(gamma-opps g)
|# 

(DEFUN 2GAMMA-ADD (cmpr gmm1 gmm2)
   (declare
    (type cmprf cmpr)
    (type gamma gmm1 gmm2))
   (the gamma
     (with-gamma (degr1 list1) gmm1
       (with-gamma (degr2 list2) gmm2
         (declare
          (fixnum degr1 degr2)
          (list list1 list2))
         (unless (= degr1 degr2)
            (error "In 2GAMMA-ADD, the gamma elements have different degrees ~D and ~D."
              degr1 degr2))
         (unless list1
            (return-from 2gamma-add gmm2))
         (unless list2
            (return-from 2gamma-add gmm1))
         (let ((pre-rslt +empty-list+))
            (declare (list pre-rslt))
            (do ((mark1 list1)
                 (mark2 list2))
                ((or (endp mark1) (endp mark2))
                 (if mark1
                    (setf pre-rslt (nreconc pre-rslt mark1))
                    (if mark2
                       (setf pre-rslt (nreconc pre-rslt mark2))
                       (setf pre-rslt (nreverse pre-rslt)))))
               (declare (list mark1 mark2))
               (with--sgm (sgop1 cmbn1) mark1
                 (with--sgm (sgop2 cmbn2) mark2
                   (declare 
                     (fixnum sgop1 sgop2)
                     (type cmbn cmbn1 cmbn2))
                   (if (= sgop1 sgop2)
                      (let ((cmbn (2cmbn-add cmpr cmbn1 cmbn2)))
                         (declare (type cmbn cmbn))
                         (unless (cmbn-zero-p cmbn)
                            (push (sgm sgop1 cmbn) pre-rslt))
                         (setf mark1 (cdr mark1)
                               mark2 (cdr mark2)))
                      (if (< sgop1 sgop2)
                         (progn
                          (push (-sgm mark1) pre-rslt)
                          (setf mark1 (cdr mark1)))
                         (progn
                          (push (-sgm mark2) pre-rslt)
                          (setf mark2 (cdr mark2))))))))
            (make-gamma :degr degr1 :list pre-rslt))))))

#|
(setf g1 (gamma-elt 5 1 (cmbn 4 3 'b -5 'd) 3 (cmbn 3 8 'a -7 'c)))
(setf g2 (gamma-elt 5 0 (cmbn 5 3 'a 8 'c) 1 (cmbn 4 2 'b 9 'c) 
           2 (cmbn 4 -5 'a) 3 (cmbn 3 -4 'a 8 'b)))
(setf g3 (2GAMMA-ADD #'s-cmpr g1 g2))
(setf g4 (gamma-elt 5 0 (cmbn 5 -3 'a -8 'c) 1 (cmbn 4 -2 'b 8 'c)
           3 (cmbn 3 5 'a -8 'b 7 'c)))
(setf g5 (2GAMMA-ADD #'s-cmpr g3 g4))
(setf g6 (gamma-elt 5 1 (cmbn 4 -3 'b -17 'c 5 'd) 2 (cmbn 4 5 'a) 3 (cmbn 3 -9 'a)))
(2GAMMA-ADD #'s-cmpr g5 g6)
|#  

(defun ADD-SGM-TO-GAMMA (cmpr sgm gmm)
   (declare
    (type cmprf cmpr)
    (type sgm sgm)
    (type gamma gmm))
   (the gamma
     (with-sgm (sgop sgcm) sgm
       (declare 
         (fixnum sgop)
         (type cmbn sgcm))
       (with-gamma (degr glist) gmm
         (declare 
           (fixnum degr)
           (list glist))
         (let ((pre-rslt +empty-list+))
            (declare (list pre-rslt))
            (do ((mark glist (cdr mark)))
                ((or (endp mark) (equal nil sgop))
                 (if sgop
                  (progn
                   (push sgm pre-rslt)
                   (setf pre-rslt (nreverse pre-rslt)))
                    (setf pre-rslt (nreconc pre-rslt mark))))
               (declare (list mark))
               (with--sgm (sgop2 sgcm2) mark
                 (declare 
                   (fixnum sgop2)
                   (type cmbn sgcm2))
                 (if (= sgop sgop2)
                    (let ((cmbn (2cmbn-add cmpr sgcm sgcm2)))
                       (declare (type cmbn cmbn))
                       (unless (cmbn-zero-p cmbn)
                          (push (sgm sgop cmbn) pre-rslt))
                       (setf sgop nil))
                    (if (< sgop sgop2)
                       (progn 
                         (push sgm pre-rslt)
                         (setf sgop nil)
                         (push (-sgm mark) pre-rslt))
                       (push (-sgm mark) pre-rslt)))))
            (make-gamma :degr degr :list pre-rslt))))))

 #|
(setf g1 (gamma-elt 5 1 (cmbn 4 3 'b -5 'd) 3 (cmbn 3 8 'a -7 'c)))
(setf g2 (add-sgm-to-gamma cmpr (sgm 7 (cmbn 2 -8 'd)) g1))
(setf g3 (add-sgm-to-gamma cmpr (sgm 7 (cmbn 2 8 'd)) g2))
(setf g4 (add-sgm-to-gamma cmpr (sgm 3 (cmbn 3 -8 'a 7 'c 8 'd)) g2))
(setf g5 (add-sgm-to-gamma cmpr (sgm 11 (cmbn 2 9 'a )) g4))
(setf g6 (add-sgm-to-gamma cmpr (sgm 9 (cmbn 3 8 'b)) g5))
(setf g7 (add-sgm-to-gamma cmpr (sgm 9 (cmbn 3 -8 'b 8 'c)) g6))
(setf g8 (add-sgm-to-gamma cmpr (sgm 9 (cmbn 3 -8 'b )) g6))
|# 

(DEFUN NSGM-ADD (cmpr degr &rest rest)
   (declare
      (type cmprf cmpr)
      (fixnum degr)
      (list rest))
   (unless rest
      (return-from nsgm-add (zero-gamma degr)))
   (the gamma
      (let ((rslt (list (first rest))))
         (declare (list rslt))
         (dolist (sgm (rest rest))
            (declare (type sgm sgm))
            (with-sgm  (sgop sgcm) sgm
               (do ((mark1 rslt (cdr mark1))
                    (mark2 nil mark1))
                   ((endp mark1) 
                    (setf (cdr mark2) (list sgm)))
                  (ecase (funcall #'f-cmpr sgop (-sgop mark1))
                     (:less
                      (if mark2
                         (push sgm (cdr mark2))
                         (push sgm rslt))
                      (return))                     
                     (:equal
                      (let ((cmbn2 (2cmbn-add cmpr (-sgcm mark1) sgcm)))
                         (setf (-sgcm mark1) cmbn2))
                      (return))                     
                     (:greater )))))
         (make-gamma
          :degr degr
          :list (flet ((sgm-zero-p (sgm)
                        (let ((sgcm (sgcm sgm)))
                           (cmbn-zero-p sgcm))))
                  (remove-if #'sgm-zero-p rslt))))))

 #|
(setf cmpr #'s-cmpr)
(setf degr 5)
(progn 
  (setf sgm1 (sgm 7 (cmbn 2 3 'a 5 'b)))
  (setf sgm2 (sgm 4 (cmbn 4 7 'a 8 'c)))
  (setf sgm3 (sgm 12 (cmbn 3 -5 'a 8 'b)))
  (setf sgm4 (sgm 4 (cmbn 4 -7 'a 9 'c 12 'd)))
  (setf sgm5 (sgm 6 (cmbn 3 8 'a)))
  (setf sgm6 (sgm 12 (cmbn 3 6 'a)))
  (nsgm-add cmpr degr sgm1 sgm2 sgm3 sgm4 sgm5 sgm6))

(progn 
  (setf sgm1 (sgm 7 (cmbn 2 3 'a 5 'b)))
  (setf sgm2 (sgm 4 (cmbn 4 7 'a 8 'c)))
  (setf sgm3 (sgm 12 (cmbn 3 -5 'a 8 'b)))
  (setf sgm4 (sgm 4 (cmbn 4 -7 'a 9 'c 12 'd)))
  (setf sgm5 (sgm 6 (cmbn 3 8 'a)))
  (setf sgm6 (sgm 12 (cmbn 3 6 'a)))
  (setf sgm7 (sgm 12 (cmbn 3 -6 'a)))
  (nsgm-add cmpr degr sgm1 sgm2 sgm3 sgm4 sgm5 sgm6 sgm7))

(progn 
  (setf sgm1 (sgm 7 (cmbn 2 3 'a 5 'b)))
  (setf sgm2 (sgm 4 (cmbn 4 7 'a 8 'c)))
  (setf sgm3 (sgm 12 (cmbn 3 -5 'a 8 'b)))
  (setf sgm4 (sgm 4 (cmbn 4 -7 'a 9 'c 12 'd)))
  (setf sgm5 (sgm 6 (cmbn 3 8 'a)))
  (setf sgm6 (sgm 12 (cmbn 3 6 'a)))
  (setf sgm7 (sgm 7 (cmbn 2 -3 'a -5 'b)))
  (nsgm-add cmpr degr sgm1 sgm2 sgm3 sgm4 sgm5 sgm6 sgm7))

(progn 
  (setf sgm1 (sgm 7 (cmbn 2 3 'a 5 'b)))
  (setf sgm2 (sgm 7 (cmbn 2 -3 'a -5 'b)))
  (nsgm-add cmpr degr sgm1 sgm2))

|#  


(DEFUN SGM-NDGNR (dgop sgm)
   (declare
      (fixnum dgop)
      (type sgm sgm))
   (the sgm
      (with-sgm (sgop sgcm) sgm
         (sgm (dgop*dgop dgop sgop) sgcm)))) 

#|
(setf sgm (sgm 5 (cmbn 2 5 'a)))
(sgm-ndgnr 2 sgm)
(sgm-ndgnr 1 sgm)
(sgm-ndgnr 5 sgm)
|# 

(DEFUN GAMMA-NDGNR (dgop gmm)
   (declare
      (fixnum dgop)
      (type gamma gmm))
   (the gamma
      (with-gamma (degr glist) gmm
        (let ((pre-rslt (mapcar #'(lambda (sgm)
                          (sgm-ndgnr dgop sgm))
                          glist))
              (degr2 (+ degr (length (dgop-int-ext dgop)))))
           (make-gamma :degr degr2 :list pre-rslt)))))

#|
(setf g (gamma-elt 5 3 (cmbn 3 7 'a) 11 (cmbn 2 -4 'b)))
(gamma-ndgnr 2 g)
(gamma-ndgnr 8 g)
(gamma-ndgnr 3 g)

|# 
        


 
;; FUNCTIONS FOR THE CONSTRUCTION OF THE SIMPLICIAL ABELIAN GROUP
;; \GAMMA(C) 
 
(DEFUN GAMMA-TO-ABSM (gmm)
   (declare (type gamma gmm))
   (the absm
     (with-gamma (degr glist) gmm
       (declare (fixnum degr)
         (list glist))
       (if (endp glist)
          (let ((dgop (dgop-ext-int (reverse (<a-b< 0 degr)))))
             (declare (fixnum dgop))
             (absm dgop (gamma-elt 0)))
          (let* ((sgop-list (mapcar #'(lambda (sgm)
                                        (sgop sgm))
                              glist))
                 (dgop (first sgop-list)))
             (declare
              (type list sgop-list)
              (fixnum dgop))
             (progn
              (do ((mark1 (cdr sgop-list) (cdr mark1)))
                  ((endp mark1))
                 (declare (list mark1))
                 (setf dgop (logand dgop (car mark1))))
              (let* ((degr2 (- degr (length (dgop-int-ext dgop))))
                     (pre-rslt +empty-list+))
                 (declare (fixnum degr2)
                   (type list pre-rslt))
                 (do ((mark2 glist (cdr mark2)))
                     ((endp mark2))
                    (declare (list mark2))
                    (with--sgm (sgop sgcm) mark2
                      (declare (fixnum sgop)
                        (type cmbn sgcm))
                      (let ((sgop2 sgop))
                          (declare (fixnum sgop2))
                         (progn
                          (do ((indx (1- (integer-length dgop)) (1- indx)))
                              ((minusp indx))
                             (declare (fixnum indx))
                              (when (logbitp indx dgop)
                                 (setf sgop2 (remove-bit sgop2 indx))))
                          (push (sgm sgop2 sgcm) pre-rslt)))))
                 (absm dgop (make-gamma :degr degr2
                              :list (nreverse pre-rslt))))))))))



#|
 (setf gmm (gamma-elt 5 3 (cmbn 3 6 'a) 7 (cmbn 2 -8 'c)))
(gamma-to-absm gmm)

(setf gmm2 (gamma-elt 5 3 (cmbn 3 6 'a) 7 (cmbn 2 -8 'c) 11 (cmbn 2 6 'a)))
(gamma-to-absm gmm2)
|#
     

(DEFUN CMBN-CMPR (cmpr)
   (declare (type cmprf cmpr))
   (flet ((rslt (cmbn1 cmbn2)
            (declare (type cmbn cmbn1 cmbn2))
            (let ((degr1 (cmbn-degr cmbn1))
                  (degr2 (cmbn-degr cmbn2)))
               (declare (fixnum degr1 degr2))
               (unless (= degr1 degr2)
                  (error "In CMBN-CMPR, the two combinations have different degrees ~D and ~D."
                    degr1 degr2))
               (flet ((term-cmpr (term1 term2)
                        (with-term (cffc1 gnrt1) term1
                          (with-term (cffc2 gnrt2) term2
                            (declare (fixnum cffc1 cffc2)
                              (type gnrt gnrt1 gnrt2))
                            (lexico
                             (f-cmpr cffc1 cffc2)
                             (funcall cmpr gnrt1 gnrt2))))))
                 (maplexico #'term-cmpr (cmbn-list cmbn1) (cmbn-list cmbn2))))))
     (the cmprf #'rslt)))
 
(DEFUN GAMMA-CMPR (cmpr)
   (declare (type cmprf cmpr))
   (let ((c-cmpr (cmbn-cmpr cmpr)))
      (declare (type cmprf c-cmpr))
      (flet ((rslt (gmm1 gmm2)
               (declare (type gamma gmm1 gmm2))
               (flet ((sgm-cmpr (sgm1 sgm2)
                       (declare (type sgm sgm1 sgm2))
                       (with-sgm (sgop1 cmbn1) sgm1
                         (with-sgm (sgop2 cmbn2) sgm2
                           (declare
                            (fixnum sgop1 sgop2)
                            (type cmbn cmbn1 cmbn2))
                           (lexico
                            (f-cmpr sgop1 sgop2)
                            (funcall c-cmpr cmbn1 cmbn2))))))
                 (maplexico #'sgm-cmpr (gamma-list gmm1) (gamma-list gmm2)))))
     (the cmprf #'rslt))))   

#|
(setf g-cmpr (gamma-cmpr #'s-cmpr))
(setf g1 (gamma-elt 3 1 (cmbn 2 3 'a) 3 (cmbn 1 5 'b)))
(setf g2 (gamma-elt 3 1 (cmbn 2 3 'a) 3 (cmbn 1 8 'b)))
(funcall g-cmpr g1 g2)
(funcall g-cmpr g2 g1)
(setf g3 (gamma-elt 3 2 (cmbn 2 3 'a) 3 (cmbn 1 5 'b)))
(funcall g-cmpr g1 g3)
(funcall g-cmpr g2 g3)
(funcall g-cmpr g2 g2)
(setf g4 (gamma-elt 3 2 (cmbn 2 3 'b) 3 (cmbn 1 5 'b)))
(funcall g-cmpr g4 g3)
(setf g5 (gamma-elt 3 2 (cmbn 2 3 'b)))
(funcall g-cmpr g5 g3)
(funcall g-cmpr g5 g4)
|# 



(defun sgm-face (dffr)
   (declare (type morphism dffr))
   (flet ((rslt (indx dmns sgm)
            (declare 
              (fixnum indx dmns)
              (type sgm sgm))
            (the sgm 
              (with-sgm (sgop sgcm) sgm
                (declare 
                  (fixnum sgop)
                  (type cmbn sgcm))
                (let* ((ext-sgop (dgop-int-ext sgop))
                       (k (length ext-sgop))
                       (r (- dmns k))
                       (c-degr (cmbn-degr sgcm)))
                   (declare
                    (type list ext-sgop)
                    (fixnum k r c-degr))
                   (unless (= r c-degr)
                      (error "In SGM-FACE, the degree of the Sigma element ~D is different to the degree introduced ~D."
                        (+ c-degr k) dmns))
                   (multiple-value-bind (dgop 1dlop) (1dlop-dgop indx sgop)
                      (declare (fixnum dgop 1dlop))                     
                      (if (not 1dlop)
                         (SGM dgop sgcm)
                         (if (= r 1dlop)
                          (let ((cmbn (? dffr sgcm)))
                             (declare (type cmbn cmbn))
                             (SGM dgop cmbn))
                            (sgm dgop (cmbn (1- r)))
                            ))))))))
     (the face #'rslt)))


 
(DEFUN GAMMA-FACE (cmpr dffr)
   (declare 
     (type cmprf cmpr)
     (type morphism dffr))
   (let ((sgm-f (sgm-face dffr)))
      (flet ((rslt (indx dmns gmsm)
               (declare
                (fixnum indx dmns)
                (type gamma gmsm))
               (the absm
                 (let ((pre-rslt nil))
                    (do ((mark (gamma-list gmsm) (cdr mark)))
                        ((endp mark) (setf pre-rslt (nreverse pre-rslt)))
                       (declare 
                         (type list mark))
                       (let ((sgm (-sgm mark)))
                          (let ((sface (funcall sgm-f indx dmns sgm)))
                             (if (sgm-non-zero-p sface)
                                (push sface pre-rslt)))))
                    (gamma-to-absm (apply #'nsgm-add cmpr (1- dmns) pre-rslt))))))
        (the face #'rslt))))


#|
(setf d3 (delta 3))
(setf cmpr (cmpr d3))
(setf dffr (dffr d3))

(setf sface (sgm-face dffr))
(setf s1 (sgm 3 (cmbn 2 5 7 -8 13)))
(dotimes (i 5)
   (print (funcall sface i 4 s1)))

(setf s2 (sgm 23 (cmbn 3 -7 15)))
(dotimes (i 8)
   (print (funcall sface i 7 s2)))

(setf gface (gamma-face cmpr dffr))
(setf g1 (sgm-gamma 4 3 (cmbn 2 5 7 -8 13)))
(dotimes (i 5)
   (print i)
   (print (funcall gface i 4 g1)))

(setf g2 (sgm-gamma 7 23 (cmbn 3 -7 15)))
(dotimes (i 8)
   (print i)
   (print (funcall gface i 7 g2)))

(setf g3 (sgm-gamma 7 59 (cmbn 2 1 7 -5 13 6 14)))
(setf g4 (2gamma-add cmpr g2 g3))

(dotimes (i 8)
   (print i)
   (print (funcall gface i 7 g3)))

(setf g5 (sgm-gamma 7 27 (cmbn 3 1 15)))
(setf g6 (2gamma-add cmpr g2 g5))
(dotimes (i 8)
   (print i)
   (print (funcall gface i 7 g6)))
|# 

(defun gamma-grml (cmpr)
   (flet ((rslt (dmns crpr)
            (the absm
              (with-crpr (dgop1 gmm1 dgop2 gmm2) crpr
                (let ((sgm-list1 +empty-list+)
                      (sgm-list2 +empty-list+)
                      (k1 (length (dgop-int-ext dgop1)))
                      (k2 (length (dgop-int-ext dgop2)))
                      (g1 (gamma-elt 0))
                      (g2 (gamma-elt 0)))
                   (with-gamma (degr1 glist1) gmm1
                     (do ((mark1 glist1 (cdr mark1)))
                         ((endp mark1))
                        (with--sgm (sgop1 sgcm1) mark1 
                          (push (sgm (dgop*dgop dgop1 sgop1) sgcm1) sgm-list1)))
                     (setf g1 (make-gamma :degr (+ degr1 k1)
                                :list (nreverse sgm-list1))))
                   (with-gamma (degr2 glist2) gmm2
                     (do ((mark2 glist2 (cdr mark2)))
                         ((endp mark2))
                        (with--sgm (sgop2 sgcm2) mark2 
                          (push (sgm (dgop*dgop dgop2 sgop2) sgcm2) sgm-list2)))
                     (setf g2 (make-gamma :degr (+ degr2 k2)
                                :list (nreverse sgm-list2))))
                   (gamma-to-absm (2gamma-add cmpr g1 g2)))))))
     #'rslt))

#|
(setf ggrml (gamma-grml cmpr))
(funcall ggrml 9 (crpr 136 g3 9 g4))

(funcall ggrml 7 (crpr 0 g3 0 g4))
(funcall ggrml 7 (crpr 0 g3 0 (gamma-opps g4)))
|# 

(defun gamma-grin (dmns gmm)
   (the absm
     (absm 0 (gamma-opps gmm))))


#|
(gamma-grin 7 g3)
(gamma-grin 7 g4)
|# 
 
  
(defun gamma-chcm (x)
   (declare (type simplicial-set x))
   (the ab-simplicial-group
     (let* ((x-cmpr (cmpr x))
            (x-dffr (dffr x))
            (x-orgn (orgn x))
            (cmpr (gamma-cmpr x-cmpr))
            (bspn (gamma-elt 0))
            (face (gamma-face x-cmpr x-dffr))
            (sintr-grml (gamma-grml x-cmpr)))
        (build-ab-smgr
         :cmpr cmpr
         :basis :locally-effective
         :bspn bspn
         :face face
         :bndr-strt :gnrt
         :dgnl-strt :gnrt
         :sintr-grml sintr-grml
         :sintr-grin #'gamma-grin
         :orgn `(GAMMA ,x)
         ))))


#|
(setf d2 (delta 2)) 
(setf gamma-d2 (gamma-chcm d2))
(basis gamma-d2 2)
(orgn gamma-d2)
(setf g1 (gamma-elt 5 7 (cmbn 2 3 7) 11 (cmbn 2 -9 13)))
(setf g2 (gamma-elt 5 7 (cmbn 2 -3 7) 9 (cmbn 3 -5 15)))
(cmpr gamma-d2 g1 g2)
(cmpr gamma-d2 g2 g1)
(setf g3 (2gamma-add (cmpr d2) g1 g2))
(cmpr gamma-d2 g1 g3)
(cmpr gamma-d2 g2 g3)
(bspn gamma-d2)
(dotimes (i 6)
   (print i)
   (print (face gamma-d2 i 5 g3)))
(grml gamma-d2 5 (crpr 0 g2 0 (gamma-opps g3)))
(grin gamma-d2 5 g1)

|# 



;; GAMMA APPLIED TO A MORPHISM

(defun gamma-sintr (f)
   (declare (type morphism f))
   (flet ((rslt (dmns gmm)
            (declare 
              (fixnum dmns)
              (type gamma gmm))
            (the absm
              (with-gamma (degr glist) gmm
                (declare 
                  (fixnum degr)
                  (list glist))
                (let ((pre-rslt +empty-list+))
                   (declare (list pre-rslt))
                   (do ((mark glist (cdr mark)))
                       ((endp mark))
                      (declare (list mark))
                      (with--sgm (sgop sgcm) mark
                        (declare
                         (fixnum sgop)
                         (type cmbn sgcm))
                        (let ((sgm2 (sgm sgop (? f sgcm))))
                           (declare (type sgm sgm2))
                           (if (sgm-non-zero-p sgm2)
                              (push sgm2 pre-rslt)))))
                   (gamma-to-absm (make-gamma :degr degr :list (nreverse pre-rslt))))))))
     #'rslt))
                    
#|
(setf d3 (delta 3))
(setf f (idnt-mrph d3))
(setf gamma-f (gamma-sintr f))
(setf gmm1 (gamma-elt 5 5 (cmbn 3 7 15) 26 (cmbn 2 -3 11)))
(funcall gamma-f 5 gmm1)

(setf g (opps f))
(setf gamma-g (gamma-sintr g))
(funcall gamma-g 5 g1)

(setf f+g (add f g))
(setf gamma-fg (gamma-sintr f+g))
(funcall gamma-fg 5 g1)

|#  

(defun gamma-mrph (f)
   (declare (type morphism f))
   (the simplicial-mrph
     (let ((sorc (sorc f))
           (trgt (trgt f))
           (degr (degr f)))
        (unless (= degr 0)
           (error "In GAMMA-MRPH, the morphism has degree ~D different to zero."
             degr))
        (build-smmr
         :sorc (gamma-chcm sorc)
         :trgt (gamma-chcm trgt)
         :degr degr
         :sintr (gamma-sintr f)
         :orgn `(GAMMA ,f)))))

#|
(setf gamma-f (gamma-mrph f))
(? gamma-f 5 g1)
(setf g2 (gamma-elt 5 5 (cmbn 3 -7 15) 10 (cmbn 3 7 15)))
(? gamma-f 5 g2)
(? gamma-f 5 (2gamma-add (cmpr d3) g1 g2))
(? gamma-f (cmbn 5 1 g2 1 g1))
|#


(defun sgm-hintr (cmpr h d)
   (declare 
     (type cmprf cmpr)
     (type morphism h d))
   (labels ((rslt (indx dmns sgm)
            (declare 
              (fixnum indx dmns)
              (type sgm sgm))
            (the gamma
              (with-sgm (sgop sgcm) sgm
                (declare 
                  (fixnum sgop)
                  (type cmbn sgcm))
                (if (= 0 sgop)
                   (let ((degr (cmbn-degr sgcm)))
                      (declare (fixnum degr))
                      (if (= degr indx)
                         (let* ((sgm1 (sgm 0 (cmbn-opps (? h sgcm))))
                                (sgm2 (sgm (dgop-ext-int (list indx)) (2cmbn-add cmpr sgcm (cmbn-opps (? h (? d sgcm))))))
                                (glist +empty-list+))
                            (declare 
                              (type sgm sgm1 sgm2)
                              (list glist))
                            (if (sgm-non-zero-p sgm2)
                               (push sgm2 glist))
                            (if (sgm-non-zero-p sgm1)
                               (push sgm1 glist))
                            (make-gamma :degr (1+ dmns)
                              :list glist))
                         (if (= (1- degr) indx)
                            (let* ((sgm1 (sgm (dgop-ext-int (list indx)) sgcm))
                                   (sgm2 (sgm (dgop-ext-int (list degr)) (cmbn-opps (? h (? d sgcm)))))
                                   (glist +empty-list+))
                               (declare 
                                 (type sgm sgm1 sgm2)
                                 (list glist))
                               (if (sgm-non-zero-p sgm2)
                                  (push sgm2 glist))
                               (if (sgm-non-zero-p sgm1)
                                  (push sgm1 glist))
                               (make-gamma :degr (1+ dmns)
                                 :list glist))
                            (sgm-gamma (1+ dmns) (dgop-ext-int (list indx)) sgcm))))
                   (let* ((dgop1-ext (first (dgop-int-ext sgop)))
                          (dgop2 (remove-bit sgop dgop1-ext))
                          (sgm2 (sgm dgop2 sgcm)))
                      (declare
                       (fixnum dgop1-ext dgop2)
                       (type sgm sgm2))
                      (if (>= dgop1-ext indx)
                         (let ((smhmt-i (rslt indx (1- dmns)  (sgm dgop2 sgcm))))
                            (declare (type gamma smhmt-i))
                            (gamma-ndgnr (dgop-ext-int (list (1+ dgop1-ext))) smhmt-i))
                         (let ((smhmt-i-1 (rslt (1- indx) (1- dmns)  sgm2)))
                            (declare (type gamma smhmt-i-1))
                            (gamma-ndgnr (dgop-ext-int (list dgop1-ext)) smhmt-i-1)))))))))
     (the hintr #'rslt)))
     
#|
(setf d3 (delta 3))
(setf d (dffr d3))
(setf h (lh (efhm d3)))
(setf cmpr #'f-cmpr)

(setf sgm-smhmt (sgm-hintr cmpr h d))
(setf sgm (sgm 5 (cmbn 3 -7 15)))
(dotimes (indx 6)
   (print (funcall sgm-smhmt indx 5 sgm)))

(setf sgm2 (sgm 11 (cmbn 2 7 7)))
(dotimes (indx 6)
   (print (funcall sgm-smhmt indx 5 sgm2)))


|# 


(defun gamma-hintr (cmpr h d)
   (declare 
     (type morphism h d)
     (type cmprf cmpr))
   (let ((s-hintr (sgm-hintr cmpr h d)))
      (flet ((rslt (indx dmns gmsm)
               (declare 
                 (type fixnum indx dmns)
                 (type gamma gmsm))
               (let ((rslt (gamma-elt (1+ dmns))))
                  (do ((mark (gamma-list gmsm) (cdr mark)))
                      ((endp mark) (gamma-to-absm rslt))
                     (setf rslt (2gamma-add cmpr rslt (funcall s-hintr indx dmns (-sgm mark))))))))
      (the hintr #'rslt))))
             

#|
(setf d3 (delta 3))
(setf d (dffr d3))
(setf h (lh (efhm d3)))

(setf gmm (gamma-elt 5 5 (cmbn 3 -7 15) 11 (cmbn 2 7 7)))
(setf cmpr #'f-cmpr)

(setf g-hintr (gamma-hintr cmpr h d))
(dotimes (indx 6)
   (print (funcall g-hintr indx 5 gmm)))

|# 
           

(defun gamma-hmtp (h)
   (declare (type morphism h))
   (the simplicial-hmtp
     (let* ((cc (sorc h))
            (gamma-cc (gamma-chcm cc))
            (degr (degr h))
            (cmpr (cmpr cc))
            (dffr (dffr cc))
            (orgn (orgn cc)))
        (declare
         (type chain-complex cc)
         (type ab-simplicial-group gamma-cc)
         (fixnum degr)
         (type cmprf cmpr)
         (type morphism dffr))
        (unless (= degr 1)
           (error "In GAMMA-HMTP, the morphism has degree ~D different to +1."
             degr))
        (build-smhmt
         :sorc gamma-cc
         :trgt gamma-cc
         :degr 1
         :hintr (gamma-hintr cmpr h dffr)
         :orgn `(GAMMA ,orgn)))))

#|
(setf d3 (delta 3))
(setf h (lh (efhm d3)))
(setf gamma-h (gamma-hmtp h))
(setf gmm (gamma-elt 5 5 (cmbn 3 -7 15) 11 (cmbn 2 7 7)))
(dotimes (i 6)
  (print (? gamma-h i 5 gmm)) )

(setf i 0)
(? gamma-h i 5 gmm)

(? gamma-h 5 gmm)

|# 
 
#|
(defun deltan-h (n)
   (the morphism
     (let ((delta-n (delta n)))
        (build-mrph
         :sorc delta-n
         :trgt delta-n
         :degr +1
         :intr #'(lambda (degr gmsm)
                   (if (oddp gmsm)
                      (zero-cmbn (1+ degr))
                      (term-cmbn (1+ degr) 1 (1+ gmsm))))
                   :strt :gnrt
                   :orgn `(homotopy for delta ,n)))))

(setf h5 (deltan-h 5))
(? h5 3 15)
(? h5 3 30)
(? h5 3 39)

(setf gmm-d5-h (gamma-hmtp h5))

(setf g1 (gamma-elt 3 0 (cmbn 3 1 15)))
(dotimes (indx 4)
   (print (? gmm-d5-h indx 3 g1)))

(setf g2 (gamma-elt 3 0 (cmbn 3 1 46)))
(dotimes (indx 4)
   (print (? gmm-d5-h indx 3 g2)))

(setf g3 (gamma-elt 5 5 (cmbn 3 -7 15)))
(dotimes (indx 6)
   (print (? gmm-d5-h indx 5 g3)))

(dotimes (i 6)
   (dotimes (j 7)
      (format t "i= ~D, j= ~D" i j)
      (print (? gmm-d5-h j 6 (? gmm-d3-h i 5 g3)))))

(setf g4 (gamma-elt 5 11 (cmbn 2 1 14)))
(dotimes (indx 6)
   (print (? gmm-d5-h indx 5 g4)))

(setf g5 (2gamma-add #'f-cmpr g3 g4))
(dotimes (indx 6)
   (print (? gmm-d5-h indx 5 g5)))

(setf g6 (2gamma-add #'f-cmpr g5 (gamma-elt 5 0 (cmbn 5 2 63))))
(dotimes (indx 6)
   (print (? gmm-d5-h indx 5 g6)))
|#


(defun gamma-rdct (rdct)
   (declare (type reduction rdct))
   (the reduction
     (let ((f (f rdct))
           (g (g rdct))
           (h (h rdct))
           (orgn (orgn rdct)))
        (declare
         (type morphism f g h)
         (list orgn))
        (build-rdct
         :f (gamma-mrph f)
         :g (gamma-mrph g)
         :h (gamma-hmtp h)
         :orgn `(gamma ,orgn)))))

#|
(setf d3 (delta 3))
(setf rdct (lrdct (efhm d3)))
(setf grdct (gamma-rdct rdct))

(setf d5 (delta 5))
(setf z (z-chcm))
(setf f (build-mrph
         :sorc d5
         :trgt z
         :degr 0
         :intr #'(lambda (dmns gnrt)
                   (if (and (= dmns 0) (= gnrt 1))
                      (cmbn 0 1 :zgnrt)
                      (cmbn dmns)))
         :strt :gnrt
         :orgn `(trivial-morphism from ,d5 to ,z)))
      
(setf g (build-mrph
         :sorc z
         :trgt d5
         :degr 0
         :intr #'(lambda (dmns gnrt)
                   (if (equal gnrt :z-gnrt)
                      (cmbn 0 1 1)
                      (cmbn dmns)))
         :strt :gnrt
         :orgn `(trivial-morphism from ,z to ,d5)))

(setf h (deltan-h 5))
(setf rdct5 (build-rdct :f f :g g :h h :orgn `(contraction-homotopy ,d5)))
(setf grdct5 (gamma-rdct rdct5))

(pre-check-rdct grdct5)
(setf *tc* (cmbn 5 1 (gamma-elt 5 0 (cmbn 5 2 63) 5 (cmbn 3 -7 15) 11 (cmbn 2 1 14)))) 
(setf *bc* (cmbn 0 1 (gamma-elt 0 0 (cmbn 0 1 :z-gnrt))))
(check-rdct)

(setf *tc* (cmbn 3 1 (gamma-elt 3 0 (cmbn 3 -6 27 2 43) 5 (cmbn 1 8 9) 7 (cmbn 0 7 8 1 32)) 6 
             (gamma-elt 3 0 (cmbn 3 1 15) 5 (cmbn 1 -7 5 6 18) 6 (cmbn 1 6 12 -3 18 4 20) 7 (cmbn 0 8 16 -3 32))
))

(check-rdct)




|# 
   

(DEFMETHOD GAMMA ((chcm chain-complex))
   (gamma-chcm chcm))

(DEFMETHOD GAMMA ((rdct reduction))
   (gamma-rdct rdct))
 
   