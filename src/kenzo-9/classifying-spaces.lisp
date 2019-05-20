;;;  CLASSIFYING-SPACES  CLASSIFYING-SPACES  CLASSIFYING-SPACES
;;;  CLASSIFYING-SPACES  CLASSIFYING-SPACES  CLASSIFYING-SPACES
;;;  CLASSIFYING-SPACES  CLASSIFYING-SPACES  CLASSIFYING-SPACES

(IN-PACKAGE #:cat-9)

(provide "classifying-spaces")

(DEFINE-CONSTANT +NULL-GBAR+ (make-gbar :dmns 0 :list +empty-list+))

;;; NIGBAR = non-normalized igbar (with dimension)

(DEFUN NORMALIZE-GBAR (nigbar)
  (declare (list nigbar))
  (the absm
    (let ((dmns (first nigbar))
          (igbar (rest nigbar)))
      (declare
       (type fixnum dmns)
       (list igbar))
      (when (zerop dmns)
        (return-from normalize-gbar (absm 0 +null-gbar+)))
      (do ((mark igbar (cdr mark))
           (indx (1- dmns) (1- indx))
           (2-indx (2-exp (1- dmns)) (ash 2-indx -1))
           (mask (mask (1- dmns)) (ash mask -1))
           (and-dgops -1 (logand and-dgops (dgop (car mark))))
           ;; r = result
           (r-dgop 0)
           (r-absm-list +empty-list+))
          ((endp mark)
           (absm r-dgop
                 (make-gbar
                  :dmns (- dmns (logcount r-dgop))
                  :list (nreverse r-absm-list))))	   
        (declare
         (list mark r-absm-list)
         (type fixnum indx 2-indx mask and-dgops r-dgop))
        (when (zerop and-dgops)
          (return-from normalize-gbar
            (absm r-dgop
                  (make-gbar
                   :dmns (- dmns (logcount r-dgop))
                   :list (nreconc r-absm-list mark)))))
        (let ((absm (car mark)))
          (with-absm (dgop gmsm) absm
            (declare (ignore gmsm))
            (if (and (= dgop mask)
                     (logbitp indx and-dgops))
                (progn
                  (incf r-dgop 2-indx)
                  (mapl
                      #'(lambda (sublist)
                          (declare (list sublist))
                          (setf (car sublist)
                            (let ((absm (car sublist)))
                              (declare (type absm absm))
                              (with-absm (dgop gmsm) absm
                                (absm (multiple-value-bind (q r)
                                          (floor dgop 2-indx)
                                        (declare (type fixnum q r))
                                        (+ (ash (1- q) (1- indx)) r))
                                      gmsm)))))
                    r-absm-list))
              (push absm r-absm-list))))))))

#|
()
(normalize-gbar (list 0))
(normalize-gbar (list 1 (absm 0 'i)))
(normalize-gbar (list 2 (absm 1 'i) (absm 0 'i)))
(normalize-gbar (list 2 (absm 0 'a) (absm 0 'i)))
(normalize-gbar (list 4 (absm 0 'a) (absm 0 'b) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 7 'i) (absm 3 'i) (absm 1 'i) (absm 0 'i)))
(normalize-gbar (list 4 (absm 0 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 1 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 2 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 4 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 3 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 5 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 6 'a) (absm 3 'i) (absm 0 'c) (absm 0 'i)))
(normalize-gbar (list 4 (absm 1 'a) (absm 3 'i) (absm 1 'i) (absm 0 'i)))
|#

(DEFUN UNNORMALIZE-GBAR (absm idnt)
  (declare
     (type absm absm)
     (type gmsm idnt))
  (the list ;; igbar
    (with-absm (dgop gbar) absm
    (with-gbar (dmns list) gbar
      (setf list (reverse list))
      (do ((dgop2 dgop (ash dgop2 -1))
	   (indx 0 (1+ indx))
	   (2-indx 1 (ash 2-indx 1))
	   (dgop3 0)
	   (rslt +empty-list+))
	  ((zerop dgop2)
	   (cons (+ dmns (logcount dgop))
		 (nreconc
		  (mapcar
		   #'(lambda (absm)
		       (declare (type absm absm))
		       (ndgnr dgop3 absm))
		   list)
		  rslt)))
	  (declare
	    (list rslt)
	    (type fixnum dgop2 2-indx dgop3))
	 (if (oddp dgop2)
	    (progn
	      (push (absm (mask indx) idnt) rslt)
	      (incf dgop3 2-indx))
	    (push (ndgnr dgop3 (pop list)) rslt)))))))

#|
()
(unnormalize-gbar (absm 1 +null-gbar+) 'i)
(unnormalize-gbar (absm 15 +null-gbar+) 'i)
(normalize-gbar (unnormalize-gbar (absm 15 +null-gbar+) 'i))
(unnormalize-gbar
 (absm 0 (make-gbar :dmns 4
                    :list (list (absm 0 'a) (absm 0 'b)
                                (absm 0 'c) (absm 0 'i)))) 'i)
(normalize-gbar
 (unnormalize-gbar
  (absm 0 (make-gbar :dmns 4
                     :list (list (absm 0 'a) (absm 0 'b)
                                 (absm 0 'c) (absm 0 'i)))) 'i))
(unnormalize-gbar
 (absm 5 (make-gbar :dmns 4
                    :list (list (absm 0 'a) (absm 0 'b)
                                (absm 0 'c) (absm 0 'i)))) 'i)
(normalize-gbar
 (unnormalize-gbar
  (absm 5 (make-gbar :dmns 4
                     :list (list (absm 0 'a) (absm 0 'b)
                                 (absm 0 'c) (absm 0 'i)))) 'i))
(unnormalize-gbar
 (absm 9 (make-gbar :dmns 4
                    :list (list (absm 0 'a) (absm 0 'b)
                                (absm 0 'c) (absm 0 'i)))) 'i)
(normalize-gbar
 (unnormalize-gbar
  (absm 9 (make-gbar :dmns 4
                     :list (list (absm 0 'a) (absm 0 'b)
                                 (absm 0 'c) (absm 0 'i)))) 'i))
|#

(DEFUN GBAR (dmns &rest rest)
  (the gbar
    (progn
      (unless (= (+ dmns dmns) (length rest))
        (error "In GBAR, the argument list has a wrong length."))
      (make-gbar :dmns dmns
                 :list (do ((mark rest (cddr mark))
                            (rslt +empty-list+
                                  (cons (absm (car mark) (cadr mark))
                                        rslt)))
                           ((endp mark) (nreverse rslt))
                         (declare (list mark rslt)))))))

#|
()
(gbar 0)
(gbar 1)
(gbar 2 1 'a 2 'b)
|#

(DEFUN CLASSIFYING-SPACE-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet
      ((rslt
        (gbar1 gbar2)
        (declare (type gbar gbar1 gbar2))
        (maplexico
         #'(lambda (absm1 absm2)
             (declare (type absm absm1 absm2))
             (a-cmpr3 cmpr absm1 absm2))
         (gbar-list gbar1) (gbar-list gbar2))))
    (the cmprf #'rslt)))

#|
  (setf cmpr (classifying-space-cmpr 's-cmpr))
  (funcall cmpr (gbar 2 0 'a 0 'a) (gbar 2 1 'a 0 'a))
  (funcall cmpr (gbar 2 0 'a 0 'a) (gbar 2 0 'b 0 'a))
  (funcall cmpr (gbar 2 0 'a 0 'a) (gbar 2 0 'a 0 'a))
|#

(DEFUN CLASSIFYING-SPACE-BASIS (basis)
  (declare (type basis basis))
  (when (eq basis :locally-effective)
    (return-from classifying-space-basis :locally-effective))
  (let ((crts-basis :locally-effective)  ;; to be redefined
        (idnt (first (funcall basis 0))))
    (declare
     (type basis crts-basis)
     (type gmsm idnt))
    (labels
        ((rslt (dmns)
               (declare (type fixnum dmns))
               (when (minusp dmns)
                 (return-from rslt +empty-list+))
               (when (zerop dmns)
                 (return-from rslt (list +null-gbar+)))
               (let ((basis-1 (funcall crts-basis (1- dmns))))
                 (declare (list basis-1))
                 (nreverse
                  (mapcar  ;; xx bug
                      #'(lambda (crpr)
                          (declare (type crpr crpr))
                          (with-crpr
                           (absm1 absm2) crpr
                           (make-gbar
                            :dmns dmns
                            :list (cons absm1
                                        (rest (unnormalize-gbar absm2 idnt))))))
                    (member-if  ;; xx bug
                     #'(lambda (dgop)
                         (declare (type fixnum dgop))
                         (< dgop (mask (1- dmns))))
                     (reverse basis-1)
                     :key #'dgop1))))))
;;;                  :key #'caadr))))))
      (setf crts-basis (crts-prdc-basis basis #'rslt))
      (the basis #'rslt))))

#|
()
(cat-init)
(setf k (k-z2-1))
(setf b (classifying-space-basis (basis k)))
(funcall b 0)
(funcall b 1)
(dotimes (i 5) (print (funcall b i)))
|#

(DEFUN CLASSIFYING-SPACE-FACE (face sintr-grml)
  (declare
   (type face face)
   (type sintr sintr-grml))
  (flet
      ((rslt
        (indx dmns gbar)
        (declare
         (type fixnum indx dmns)
         (type gbar gbar))
        (when (= indx dmns)
          (return-from rslt
            (normalize-gbar (cons (1- dmns) (rest (gbar-list gbar))))))
        (do ((mark (gbar-list gbar) (cdr mark))
             (dmns2 (1- dmns) (1- dmns2))
             (rslt +empty-list+
                   (cons (a-face4 face indx dmns2 (car mark))
                         rslt)))
            ((= dmns2 indx)
             (if (zerop indx)
                 (normalize-gbar (cons (1- dmns) (nreverse rslt)))
               (normalize-gbar
                (cons (1- dmns)
                      (nreconc
                       rslt
                       (cons (a-grml4 sintr-grml (1- indx)
                                      (second mark)
                                      (a-face4 face indx indx (first mark)))
                             (nthcdr 2 mark))))))))))
    (the face #'rslt)))

#|
  (setf om (loop-space (moore 2 2)))
  (setf face (classifying-space-face (face om) (sintr (grml om))))
  (setf gbar (gbar 4 0 (loop3 3 'm2 1 4 'n3 1)
                     0 (loop3 0 'n3 1)
                     0 (loop3 0 'm2 1)
                     0 +null-loop+))
  (dotimes (i 5)
    (print (funcall face i 4 gbar)))
|#

(DEFGENERIC CLASSIFYING-SPACE (arg1))

(DEFMETHOD CLASSIFYING-SPACE ((smgr simplicial-group))
  (the simplicial-set
     (build-smst :cmpr (classifying-space-cmpr (cmpr smgr))
		 :basis (classifying-space-basis (basis smgr))
		 :bspn +null-gbar+
		 :face (classifying-space-face (face smgr) (sintr (grml smgr)))
		 :orgn `(classifying-space ,smgr))))

#|
  (cat-init)
  (setf c (classifying-space (k-z2-1)))
  (orgn c)
  (first (basis c 4))
  (? c 4 (first (basis c 4)))
  (? c *)
  (cprd c 4 ***)
  (dotimes (i 5)
    (print (face c i 4 (first (basis c 4)))))
|#

(DEFUN CLASSIFYING-SPACE-GRML-SINTR (idnt sintr-grml)
  (declare
    (type gmsm idnt)
    (type sintr sintr-grml))
  (flet ((rslt (dmns crpr)
	   (declare
	     (type fixnum dmns)
	     (type crpr crpr))
	   (with-crpr (absm1 absm2) crpr
	   (let ((absm-list-1 (rest (unnormalize-gbar absm1 idnt)))
		 (absm-list-2 (rest (unnormalize-gbar absm2 idnt))))
	     (declare (list absm-list-1 absm-list-2))
	     (normalize-gbar
	       (cons dmns
		 (mapcar
		   #'(lambda (absm1 absm2)
		       (declare (type absm absm1 absm2))
		       (decf dmns) ;; !!
		       (a-grml4 sintr-grml dmns absm1 absm2))
		   absm-list-1 absm-list-2)))))))
    (the sintr #'rslt)))

#|
  (setf grml (classifying-space-grml-sintr '() (sintr (grml (k-z-1)))))
  (funcall grml 3 (crpr 0 (gbar 3 0 '(1 2) 0 '(3) 0 '())
                        0 (gbar 3 0 '(-1 -2) 0 '(-3) 0 '())))
  (funcall grml 3 (crpr 0 (gbar 3 0 '(1 2) 0 '(3) 0 '())
                        4 (gbar 2 0 '(-3) 0 '())))
  (funcall grml 3 (crpr 0 (gbar 3 0 '(1 2) 0 '(3) 0 '())
                        1 (gbar 2 0 '(-3) 0 '())))
|#


(DEFUN CLASSIFYING-SPACE-GRIN-SINTR (sintr-grin)
  (declare (type sintr sintr-grin))
  (flet ((rslt (dmns gbar)
	   (declare
	     (type fixnum dmns)
	     (type gbar gbar))
	   (absm 0
	     (make-gbar
	       :dmns dmns
	       :list (mapcar
		       #'(lambda (absm)
			   (declare (type absm absm))
			   (a-grin4 sintr-grin dmns absm))
		       (gbar-list gbar))))))
    (the sintr #'rslt)))

#|
  (setf grin (classifying-space-grin-sintr (sintr (grin (k-z-1)))))
  (funcall grin 3 (gbar 3 0 '(1 2) 1 '() 0 '()))
|#

(DEFMETHOD CLASSIFYING-SPACE ((smgr ab-simplicial-group))
  (the ab-simplicial-group
     (change-class 
       (build-smgr :cmpr (classifying-space-cmpr (cmpr smgr))
		   :basis (classifying-space-basis (basis smgr))
		   :bspn +null-gbar+
		   :face (classifying-space-face (face smgr) (sintr (grml smgr)))
		   :sintr-grml (classifying-space-grml-sintr
				 (bspn smgr)
				 (sintr (grml smgr)))
		   :sintr-grin (classifying-space-grin-sintr
				 (sintr (grin smgr)))
		   :orgn `(classifying-space ,smgr))
       'ab-simplicial-group)))

#|
  (setf k-z-1 (k-z-1))
  (setf k-z-2 (classifying-space k-z-1))
  (setf k-z-3 (classifying-space k-z-2))
  (homology k-z-3 0 10)
  (setf k-z2-1 (k-z2-1))
  (setf k-z2-2 (classifying-space k-z2-1))
  (setf k-z2-3 (classifying-space k-z2-2))
  (setf k-z2-4 (classifying-space k-z2-3))
  (setf k-z2-5 (classifying-space k-z2-4))
  (homology k-z2-5 0 7)
|#
