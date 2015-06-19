;; BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES  
;; BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES
;; BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES   BICOMPLEXES


(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "bicomplexes")

(DEFMETHOD PRINT-KEYCONS ((car (eql :BcGnrt)) cdr stream)
   (declare
      (cons cdr)
      (stream stream))
   (the (eql t)
       (let* 
            ((cdr (cons car cdr))
             (degr1 (BcGnrt-Degr1 cdr))
             (degr2 (BcGnrt-Degr2 cdr))
             (gnrt (BcGnrt-gnrt cdr)))
          (progn
            (format stream "<BcGnrt ")
            (format stream "[~D ~D] ~A" degr1 degr2 gnrt)
            (format stream ">"))
         t)))



        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; TYPES FOR BICOMPLEXES ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TYPE BcGnrt (a generator in a Bicomplex)
(DEFUN BcGnrt-p (object)
   (declare (type any object))
   (the boolean
     (and (consp object)
          (eql :BcGnrt (car object))
          (consp (cdr object))
          (consp (cadr object))
          (typep (caadr object) 'fixnum)
          (typep (cdadr object) 'fixnum))))
(DEFTYPE BcGnrt () '(satisfies BcGnrt-p))

#|
(typep  `(:BcGnrt ,(cons 5 6) u) 'BcGnrt)
(typep `(:BcGnrt ,(cons 3 2) '(f g)) 'BcGnrt)
(typep `(:BcGnrt ,(cons 4 5.4) u) 'BcGnrt)
|#

 
;; TYPE BcBasis (basis of a Bicomplex, function of the variables p and q or
;; ":locally-effective")
(DEFTYPE BcBASIS () '(or function (eql :locally-effective))) 
                     ;; (function (degr degr) (list gnrt)) 


;; TYPE Bicomplex
(DEFUN OrgnBiCmpl-p (object)
   (declare (type chain-complex object))
     (let* ((orgn (orgn object)))
        (the boolean
          (and (consp orgn)
             (eql (car orgn) 'BiCmpl)))))
(DEFTYPE Bicomplex () '(satisfies OrgnBiCmpl-p))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; FUCTIONS FOR THE REPRESENTATION OF (GENERAL) BICOMPLEXES ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Function that builds a generator of a Bicomplex from two degrees and 
;; a simple generator.
(DEFUN build-BcGnrt (degr1 degr2 gnrt)
   (declare 
     (type fixnum degr1 degr2)
     (type any gnrt))
   (list :BcGnrt (cons degr1 degr2) gnrt))

#|
(build-BcGnrt 2 3 'c)
(typep  (build-BcGnrt 2 2 'd) 'BcGnrt)
|#
 

;; Function that returns the first degree from a bicomplex' generator.
(DEFUN BcGnrt-Degr1 (BcGnrt)
   (declare (type BcGnrt BcGnrt))
   (caadr BcGnrt))

;; Function that returns the second degree from a bicomplex' generator.
(DEFUN BcGnrt-Degr2 (BcGnrt)
   (declare (type BcGnrt BcGnrt))
   (cdadr BcGnrt))

;; Function that returns the original generator from a bicomplex' generator.
(DEFUN BcGnrt-Gnrt (BcGnrt)
   (declare (type BcGnrt BcGnrt))
   (caddr BcGnrt))

#|
(build-BcGnrt 5 6 8)
(BcGnrt-Degr1 (build-BcGnrt 5 6 8))
(BcGnrt-Degr2 (build-BcGnrt 6 4 '(4 6)))
(BcGnrt-Gnrt (build-BcGnrt 6 4 '(4 6)))
(BcGnrt-Gnrt (build-BcGnrt 2 8 'u))
|# 

 
;; Comparison function of a bicomplex. 
(defun BC-CMPR (cmpr)
   (declare (type cmprf cmpr))
   (flet ((comp (bc1 bc2)
            (declare (type BcGnrt bc1 bc2))
            (let ((degr11 (BcGnrt-Degr1 bc1))
                  (degr21 (BcGnrt-Degr2 bc1))
                  (degr12 (BcGnrt-Degr1 bc2))
                  (degr22 (BcGnrt-Degr2 bc2))
                  (gnrt1 (BcGnrt-Gnrt bc1))
                  (gnrt2 (BcGnrt-Gnrt bc2)))
               (declare 
                 (type fixnum degr11 degr21 degr12 degr22)
                 (type any gnrt1 gnrt2))
               (lexico
                (f-cmpr degr11 degr12)
                (f-cmpr degr21 degr22)
                (funcall cmpr gnrt1 gnrt2)))))
     (the cmprf #'comp)))

#|
(setf cmpr (BC-CMPR #'s-cmpr)) 
(funcall cmpr (build-BcGnrt 5 6 'a) (build-BcGnrt 7 8 'b))
(funcall cmpr (build-BcGnrt 8 6 'a) (build-BcGnrt 7 8 'b))
(funcall cmpr (build-BcGnrt 8 5 'c) (build-BcGnrt 8 6 'b))
(funcall cmpr (build-BcGnrt 8 5 'c) (build-BcGnrt 8 5 'b))
|# 
      
;; Basis of a bicomplex (from a function of type BcBasis).
(DEFUN BC-BASIS (bcbasis)
   (declare (type BcBasis bcbasis))
   (when (eq bcbasis :locally-effective)
      (return-from bc-basis :locally-effective))
   (flet ((bas (degr)
            (declare (type fixnum degr))
            (the list
              (progn
               (when (minusp degr)
                  (return-from bas +empty-list+))
               (mapcan
                #'(lambda (degr1)
                    (declare (type fixnum degr1))
                    (let* ((degr2 (- degr degr1))
                           (basis (funcall bcbasis degr1 degr2)))
                       (declare
                        (fixnum degr2)
                        (list basis))
                       (the list
                         (mapcar
                          #'(lambda (gnrt)
                              (declare (type gnrt gnrt))
                              (the BcGnrt
                                (build-BcGnrt degr1 degr2 gnrt)))
                          basis)))
                    )
                (<a-b> 0 degr))))))
     (the basis #'bas)))

#|
(defun bas (degr1 degr2)
   (if (and (= degr1 0) (= degr2 1)) (return-from bas '(a)))
   (if (and (= degr1 1) (= degr2 0)) (return-from bas '(b)))
   (if (and (= degr1 1) (= degr2 1)) (return-from bas '(c)))
   (if (and (= degr1 2) (= degr2 0))  (return-from bas '(d)))
   (return-from bas nil))
(setf basis (BC-BASIS #'bas))
(dotimes (i 5)
   (print (funcall basis i)))
|#

;; Differential function of a Bicomplex (from the differential functions,
;; dffr1=horizontal dffr, dffr2=vertical dffr). 
(DEFUN BC-INTR-DFFR (dffr1 dffr2)
   (declare (type function dffr1 dffr2))
   (flet ((dif (degr bc)
            (declare
             (fixnum degr)
             (type BcGnrt bc))
            (the cmbn
              (let* ((degr1 (BcGnrt-Degr1 bc))
                     (degr2 (BcGnrt-Degr2 bc))
                     (gnrt (BcGnrt-Gnrt bc))
                     (degr1-1 (1- degr1))
                     (degr2-1 (1- degr2))
                     (list1 (funcall dffr1 degr1 degr2 gnrt))
                     (list2 (funcall dffr2 degr1 degr2 gnrt)))
                 (declare (type fixnum degr1 degr2 degr1-1 gegr2-1)
                   (list list1 list2))
                 (make-cmbn
                  :degr (1- degr)
                  :list (nconc
                         (mapcar
                          #'(lambda (term1)
                              (declare (type term term1))
                              (let* ((cffc1 (cffc term1))
                                     (gnrt1 (gnrt term1)))
                                 (declare (type fixnum cffc1)
                                   (type gnrt gnrt1))
                                 (term cffc1 (build-BcGnrt degr1-1 degr2 gnrt1))))
                          list1)
                         (mapcar 
                           #'(lambda (term2)
                               (declare (type term term2))
                               (let* ((cffc2 (cffc term2))
                                      (gnrt2 (gnrt term2)))
                                  (declare (type fixnum cffc2)
                                    (type gnrt gnrt2))
                                  (term cffc2 (build-BcGnrt degr1 degr2-1 gnrt2))))
                           list2)))))))
     (the intr-mrph #'dif)))

#|
(defun dif1 (degr1 degr2 gnrt)
   (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif1 (list (cons 2 'a))))
   (if (and (= degr1 2) (= degr2 0) (eql gnrt 'd)) (return-from dif1 (list (cons 2 'b))))
   (return-from dif1 nil))
(defun dif2 (degr1 degr2 gnrt)
   (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif2 (list (cons 1 'b))))
   (return-from dif2 nil))
(setf dif (BC-INTR-DFFR #'dif1 #'dif2))
(funcall dif 2 (build-BcGnrt 1 1 'c))
|#

;; Function that builds a bicomplex from a basis function, the two differential
;; functions and the original comparison. 
(DEFUN BUILD-BICM (&key bcbasis dffr1 dffr2 cmpr orgn)
   (declare 
     (type BCbasis bcbasis)
     (type function dffr1 dffr2)
     (type cmprf cmpr)
     (list (orgn)))
   (the chain-complex
     (let ((chcm
            (build-chcm
             :cmpr (BC-CMPR cmpr)
             :basis (BC-BASIS bcbasis)
             :intr-dffr (BC-INTR-DFFR dffr1 dffr2)
             :strt :gnrt
             :orgn orgn)))
        (declare (type chain-complex chcm))
        (slot-makunbound chcm 'bsgn)
        chcm)))

#|
(setf bc (Build-Bicm :bcbasis #'bas :dffr1 #'dif1 :dffr2 #'dif2 :cmpr 's-cmpr 
          :orgn '(BC-test)))
(dotimes (i 5)
   (print (basis bc i)))
(cmpr bc (build-BcGnrt 0 1 'a) (build-BcGnrt 1 1 'c))

(? bc (? bc 2 (build-BcGnrt 1 1 'c))) 
(? bc 2 (build-BcGnrt 2 0 'd))
(? bc 2 '(:BcGnrt (1 . 1) c))
(? bc (cmbn 2 3 '(:BcGnrt (1 . 1) c)))
(? bc 1 (build-bicm 0  1 'a))
|#





