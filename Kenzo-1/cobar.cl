;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR
;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR
;;;  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR  COBAR

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "cobar")

(DEFUN ALLP (&rest list)
  (when (= 1 (length list))
    (setf list (first list)))
  (unless (evenp (length list))
    (error "In ALLP, the length list should be even."))
  (the allp
    (let ((rslt +empty-list+))
      (declare (list rslt))
      (do ((mark list (cddr mark)))
          ((endp mark))
        (declare (list mark))
        (push (cbgn (car mark) (cadr mark)) rslt))
      (make-allp :list (nreverse rslt)))))

#|
()
(allp )
(allp '(2 a 3 b))
(allp 2 'a 3 'b)
(allp 2 'a 3))  ;; error
|#

(DEFCONSTANT +NULL-ALLP+ (make-allp :list +empty-list+))

(DEFUN ALLP-PRINT (allp stream depth)
  (declare
   (type allp allp) (stream stream)
   (ignore depth))
  (the allp
    (progn
      (format stream "<<AlLp")
      (dolist (cbgn (allp-list allp))
        (declare (type cbgn cbgn))
        (with-cbgn (degr gnrt) cbgn
                   (format stream "[~D ~A]" degr gnrt)))
      (format stream ">>")
      allp)))
            
(DEFUN COBAR-CMPR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (allp1 allp2)
               (declare (type allp allp1 allp2))
               (the cmpr
                 (let ((list1 (allp-list allp1))
                       (list2 (allp-list allp2)))
                   (declare (type iallp list1 list2))
                   (lexico
                    (f-cmpr (length list1) (length list2))
                    (maplexico
                     #'(lambda (cbgn1 cbgn2)
                         (lexico
                          (f-cmpr (cdegr cbgn1) (cdegr cbgn2))
                          (funcall cmpr (cgnrt cbgn1) (cgnrt cbgn2))))
                     list1 list2))))))
    (the cmprf #'rslt)))

#|
  (setf r (cobar-cmpr #'s-cmpr))
  (funcall r (allp) (allp))
  (funcall r (allp 3 'a) (allp))
  (funcall r (allp 3 'a) (allp 2 'a 1 'b))
  (funcall r (allp 3 'a) (allp 3 'b))
  (funcall r (allp 3 'a) (allp 3 'a)))
|#

(DEFUN COBAR-BASIS-LENGTH (basis degr length)
   (declare
      (type basis basis)
      (fixnum degr length))
   (the list
      (progn
	(when (= 1 length)
	      (return-from cobar-basis-length
			   (mapcar
			    #'(lambda (item)
				(declare (type gnrt item))
				(list (cbgn degr item)))
			    (funcall basis (1+ degr)))))                                         
	(mapcan
	 #'(lambda (degr1)
	     (declare (fixnum degr1))
	     (let ((list1 (funcall basis (1+ degr1)))
		   (list2 (cobar-basis-length
			    basis (- degr degr1) (1- length))))
	       (declare (list list1 list2))
	       (mapcan
		#'(lambda (item1)
		    (declare (type gnrt item1))
		    (mapcar
		     #'(lambda (item2)
			 (declare (type iallp item2))
			 (cons (cbgn degr1 item1) item2))
		     list2))
		list1)))
	 (>a-b< 0 degr)))))

#|
  (setf basis #'(lambda (degr)
                   (list degr)))
  (cobar-basis-length basis 1 1)
  (cobar-basis-length basis 2 1)
  (cobar-basis-length basis 2 2)
  (cobar-basis-length basis 3 1)
  (cobar-basis-length basis 3 2)
  (cobar-basis-length basis 3 3)
|#

(DEFUN COBAR-BASIS (basis)
   (declare (type basis basis))
   (the basis
      (progn
	(when (eq :locally-effective basis)
	  (return-from cobar-basis :locally-effective))
	(flet ((rslt (degr)
		     (declare (fixnum degr))
		     (cond ((minusp degr) +empty-list+)
			   ((zerop degr) (list +null-allp+))
			   (t
			    (mapcan
			     #'(lambda (length)
				 (declare (fixnum length))
				 (mapcar
				  #'(lambda (iallp)
                                      (declare (type iallp iallp))
                                      (make-allp :list iallp))
				  (cobar-basis-length basis degr length)))
			     (>a-b> 0 degr))))))
	      #'rslt))))
   
#| The following version of the COBAR-BASIS function is
   not correctly compiled with ACL-Unix, probably
   because the too complicated (but natural and correct)
   structure of closures. This version works on ACL-PC.
(DEFUN COBAR-BASIS (basis)
   (declare
      (type basis basis))
   ;; if the coalgebra is effective, it is assumed simply-connected :
   ;;   (length (funcall basis 0))  ==>  1
   ;;   (length (funcall basis 1))  ==>  0
   (the basis
      (progn
         (when (eq :locally-effective basis)
            (return-from cobar-basis :locally-effecive))
         (labels ((rslt (degr length)
                     (declare (fixnum degr length))
                     (when (= 1 length)
                        (return-from rslt
                           (mapcar
                              #'(lambda (item)
                                   (declare (type gnrt item))
                                   (list (cbgn degr item)))
                              (funcall basis (1+ degr)))))                                         
                     (mapcan
                        #'(lambda (degr1)
                             (declare (fixnum degr1))
                             (let ((list1 (funcall basis (1+ degr1)))
                                   (list2 (rslt (- degr degr1) (1- length))))
                                (declare (list list1 lis2))
                                (mapcan
                                   #'(lambda (item1)
                                        (declare (type gnrt item1))
                                        (mapcar
                                           #'(lambda (item2)
                                                (declare (type iallp item2))
                                                (cons (cbgn degr1 item1) item2))
                                           list2))
                                   list1)))
                        (>a-b< 0 degr))))
            #'(lambda (degr)
                 (declare (fixnum degr))
                 (if (zerop degr)
                    (list (make-allp :list +empty-list+))
                    (mapcan
                       #'(lambda (length)
                            (declare (fixnum length))
                            (mapcar
                               #'(lambda (iallp)
                                    (declare (type iallp iallp))
                                    (make-allp :list iallp))
                               (rslt degr length)))
                       (>a-b> 0 degr))))))))
|#

#|
  (setf basis #'(lambda (degr)
                   (list degr)))
  (setf r (cobar-basis basis))
  (funcall r 0)
  (funcall r 1)
  (funcall r 2)
  (dotimes (i 7)
     (print (funcall r i)))
  (cobar-basis :locally-effective)
|#

(DEFUN COBAR-INTR-VRTC-DFFR (dffr)
   (declare (type morphism dffr))
   (labels ((rslt (degr iallp)
               ;; the argument iallp is an internal algebraic loop,
               ;;     without the keyword :allp
               ;; rslt returns an internal combination
               ;;     without the keyword :cmbn, without degree
               (declare
                  (fixnum degr)
                  (type iallp iallp))
               (the icmbn
                  (progn
                     (unless iallp
                        (return-from rslt +empty-list+))
                     (let ((cbgn1 (first iallp))
                           (rest (rest iallp)))
                        (declare
                           (type cbgn cbgn1)
                           (type iallp rest))
                        (with-cbgn (degr1 gnrt1) cbgn1
                           (let ((d-gnrt1 (cmbn-list (gnrt-? dffr (1+ degr1) gnrt1)))
                                 (d-rest (rslt (- degr degr1) rest))
                                 (degr1-1 (1- degr1))
                                 (first-sign (-1-expt-n-1 (length rest)))
                                 (rest-sign (-1-expt-n degr1)))
                              (declare
                                 (type icmbn d-gnrt1 d-rest)
                                 (fixnum degr1-1 rest-sign))
                              (nconc
                                 (mapcar
                                    #'(lambda (term)
                                         (with-term (cffc gnrt) term
                                            (term (* first-sign cffc)
                                               (cons (cbgn degr1-1 gnrt) rest))))
                                    d-gnrt1)
                                 (mapcar
                                    #'(lambda (term)
                                         (with-term (cffc gnrt) term
                                            (term (* rest-sign cffc)
                                               (cons cbgn1 gnrt))))
                                    d-rest)))))))))
      (the intr-mrph
         #'(lambda (degr allp)
              (make-cmbn
                 :degr (1- degr)
                 :list (mapcar
                          #'(lambda (term)
                               (with-term (cffc iallp) term
                                  (term cffc (make-allp :list iallp))))
                          (rslt degr (allp-list allp))))))))
         

#|
(setf d (soft-delta-infinity))
(setf r (cobar-intr-vrtc-dffr (dffr d)))
(funcall r 0 (allp))
(funcall r 3 (allp 3 (d 15)))
(funcall r 5 (allp 3 (d (mask 5)) 2 (d (mask 4))))
(funcall r 5 (allp 2 (d (mask 4)) 3 (d (mask 5)))))
|#

(DEFMETHOD VRTC-COBAR ((chcm chain-complex))
   (the chain-complex
      (with-slots (cmpr basis dffr) chcm
         (declare
            (type cmprf cmpr)
            (type basis basis)
            (type morphism dffr))
         (build-chcm
            :cmpr (cobar-cmpr cmpr)
            :basis (cobar-basis basis)
            :bsgn +null-allp+
            :intr-dffr (cobar-intr-vrtc-dffr dffr)
            :strt :gnrt
            :orgn `(vrtc-cobar ,chcm)))))

#|
  (cat-init)
  (setf v (vrtc-cobar (soft-delta-infinity)))
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) (d gmsm)) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (print allp)
        (print (? v (apply #'+ (mapcar #'car (allp-list allp))) allp))
        (print (? v (? v (apply #'+ (mapcar #'car (allp-list allp))) allp)))))
|#

(DEFUN COBAR-INTR-HRZN-DFFR (cprd)
   (declare (type morphism cprd))
   (labels ((rslt (degr iallp)
               (declare
                  (fixnum degr)
                  (type iallp iallp))
               (the icmbn
                  (progn
                     (unless iallp
                        (return-from rslt +empty-list+))
                     (let ((cbgn1 (first iallp))
                           (rest (rest iallp)))
                        (declare
                           (type cbgn cbgn1)
                           (type iallp rest))
                        (with-cbgn (degr1 gnrt1) cbgn1
                           (let ((cprd-gnrt1 (cmbn-list (gnrt-? cprd (1+ degr1) gnrt1)))
                                 (cprd-rest (rslt (- degr degr1) rest))
                                 (sign (-1-expt-n-1 (length rest))))
                              (declare
                                 (type icmbn cprd-gnrt1 cprd-rest)
                                 (fixnum sign))
                              (setf cprd-gnrt1 (rest (butlast cprd-gnrt1)))  ;;; because \bar{A}
                              (nconc
                                 (mapcar
                                    #'(lambda (term1)
                                         (declare (type term term1))
                                         (with-term (nil tnpr) term1  ;;; cffc is in fact 1
                                            (with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                                               (term sign              ;;; (* cffc sign)
                                                  (cons (cbgn (1- degr1) gnrt1)
                                                     (cons (cbgn (1- degr2) gnrt2)
                                                        rest))))))
                                    cprd-gnrt1)
                                 (mapcar
                                    #'(lambda (term2)
                                         (declare (type term term2))
                                         (with-term (cffc allp2) term2
                                            (term cffc
                                               (cons cbgn1 allp2))))
                                    cprd-rest)))))))))
      (the intr-mrph
         #'(lambda (degr allp)
              (declare
                 (fixnum degr)
                 (type allp allp))
              (the cmbn
                 (make-cmbn :degr (1- degr)
                    :list (mapcar #'(lambda (term)
                                       (declare (type term term))
                                       (with-term (cffc iallp) term
                                          (term cffc (make-allp :list iallp))))
                             (rslt degr (allp-list allp)))))))))

#|
  (setf d (soft-delta-infinity))
  (setf r (cobar-intr-hrzn-dffr (cprd d)))
  (funcall r 0 (allp))
  (funcall r 3 (allp 3 (d (mask 5))))
  (funcall r 5 (allp 3 (d (mask 5)) 2 (d (mask 4))))
  (funcall r 5 (allp 2 (d (mask 4)) 3 (d (mask 5))))
|#

(DEFUN COBAR-HRZN-DFFR (clgb)
   (declare (type coalgebra clgb))
   (the morphism
      (with-slots (cprd) clgb
         (declare (type morphism cprd))
         (build-mrph
            :sorc (vrtc-cobar clgb) :trgt (vrtc-cobar clgb) :degr -1
            :intr (cobar-intr-hrzn-dffr cprd) :strt :gnrt
            :orgn `(cobar-hrzn-dffr ,clgb)))))

#|
  (cat-init)
  (setf h (cobar-hrzn-dffr (soft-delta-infinity)))
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) (d gmsm)) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (setf allp (random-allp 4))
  (? h (apply #'+ (mapcar #'car (allp-list allp))) allp)
  (? h (? h (apply #'+ (mapcar #'car (allp-list allp))) allp))
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (print allp)
        (print (? h (apply #'+ (mapcar #'car (allp-list allp))) allp))
        (print (? h (? h (apply #'+ (mapcar #'car (allp-list allp))) allp)))))
|#

(DEFUN COBAR-INTR-DFFR (vrtc-dffr hrzn-dffr)
   (declare (type morphism vrtc-dffr hrzn-dffr))
   (flet ((rslt (degr allp)
             (declare
                (fixnum degr)
                (type allp allp))
             (make-cmbn :degr (1- degr)
                :list (append       ;;; and not nconc, otherwise a terrible bug, when the
                                    ;;;   first result is stored in memory...
                         (cmbn-list (gnrt-? vrtc-dffr degr allp))
                         (cmbn-list (gnrt-? hrzn-dffr degr allp))))))
      (the intr-mrph #'rslt)))

(DEFMETHOD COBAR ((coalgebra coalgebra))
   (let ((vrtc-cobar (vrtc-cobar coalgebra))
         (cobar-hrzn-dffr (cobar-hrzn-dffr coalgebra)))
     (declare (type chain-complex vrtc-cobar)
              (type morphism cobar-hrzn-dffr))
      (the chain-complex
	 (let ((rslt (build-chcm
		      :cmpr (cmpr vrtc-cobar)
		      :basis (basis vrtc-cobar)
		      :bsgn +null-allp+
		      :intr-dffr (cobar-intr-dffr (dffr vrtc-cobar) cobar-hrzn-dffr)
		      :strt :gnrt
		      :orgn `(add ,vrtc-cobar ,cobar-hrzn-dffr))))
	   (declare (type chain-complex rslt))
	   (setf (slot-value rslt 'grmd) (grmd vrtc-cobar))
	   rslt))))

#|
  (cat-init)
  (setf c (cobar (deltab)))   
  (defun random-allp (length)
     (let ((rslt nil))
        (dotimes (i length)
           (let* ((gmsm (random (mask 9)))
                  (dmns (1- (logcount gmsm))))
              (when (plusp dmns)
                 (push (cbgn (1- dmns) gmsm) rslt))))
        (make-allp :list rslt)))
  (dotimes (i 10) (print (random-allp 5)))
  (setf allp (random-allp 4))
  (? c (apply #'+ (mapcar #'car (allp-list allp))) allp)
  (? c (? c (apply #'+ (mapcar #'car (allp-list allp))) allp))
  (dotimes (i 10)
     (let ((allp (random-allp 3)))
        (print allp)
        (print (? c (apply #'+ (mapcar #'car (allp-list allp))) allp))
        (print (? c (? c (apply #'+ (mapcar #'car (allp-list allp))) allp)))))
|#

(DEFUN CMBN-ALLP-CMBN-TNPR (cmbn allp-cmbn)
   (declare (type cmbn cmbn allp-cmbn))
   (the cmbn
      (with-cmbn (degr1 list1) cmbn
         (decf degr1)     ;; because allp organization
      (with-cmbn (degrr listr) allp-cmbn
         (make-cmbn
            :degr (+ degr1 degrr)
            :list
            (mapcan
               #'(lambda (term1)
                    (declare (type term term1))
                    (with-term (cffc1 gnrt1) term1
                       (let ((cbgn1 (cbgn degr1 gnrt1)))
                          (declare (type cbgn cbgn1))
                          (mapcar
                             #'(lambda (termr)
                                  (declare (type term termr))
                                  (with-term (cffcr allpr) termr
                                     (term (* cffc1 cffcr)
                                        (make-allp
                                           :list (cons cbgn1 (allp-list allpr))))))
                             listr))))
               list1))))))

(DEFUN NCMBN-COBAR (cmbn-list)
   (declare (list cmbn-list))
   (the cmbn
      (progn
         (unless cmbn-list
            (return-from ncmbn-cobar (cmbn 0 1 +null-allp+)))
         (cmbn-allp-cmbn-tnpr
            (first cmbn-list)
            (ncmbn-cobar (rest cmbn-list))))))
#|
  (ncmbn-cobar nil)
  (ncmbn-cobar (list (cmbn 3 2 'a 3 'b)))
  (ncmbn-cobar (list (cmbn 1 2 'a 3 'b) (cmbn 2 4 'c 5 'd)))
  (ncmbn-cobar (list (cmbn 1 2 'a 3 'b) (cmbn 1 4 'c 5 'd) (cmbn 1 6 'e 7 'f)))
|#

(DEFUN MRPH-VRTC-COBAR-INTR (mrph)
  (declare (type morphism mrph))
  (flet ((rslt (degr allp)
               (declare
                (ignore degr)
                (type allp allp))
               (the cmbn
                 (ncmbn-cobar
                   (mapcar #'(lambda (cbgn)
                                (declare (type cbgn cbgn))
                                (with-cbgn (degr gnrt) cbgn
                                   (gnrt-? mrph (1+ degr) gnrt)))
                      (allp-list allp))))))
      (the intr-mrph #'rslt)))

#|
  (setf cc (build-chcm :cmpr #'f-cmpr :strt :cmbn))
  (setf m (build-mrph :sorc cc :trgt cc :degr 0 :intr
            #'(lambda (degr gnrt) (cmbn degr 2 gnrt 3 (1+ gnrt)))
            :strt :gnrt :orgn '(test)))
  (setf r (mrph-vrtc-cobar-intr m))
  (funcall r 4 (allp 2 3 2 4))
|#

(DEFMETHOD VRTC-COBAR ((mrph morphism))
   (the morphism
      (if (eq (first (orgn mrph)) 'idnt-mrph)
         (idnt-mrph (vrtc-cobar (sorc mrph)))
         (build-mrph
            :sorc (vrtc-cobar (sorc mrph))
            :trgt (vrtc-cobar (trgt mrph))
            :degr 0
            :intr (mrph-vrtc-cobar-intr mrph)
            :strt :gnrt
            :orgn `(vrtc-cobar ,mrph)))))

#|
  (cat-init)
  (setf f (aw (soft-delta-infinity) (soft-delta-infinity)))
  (setf cf (vrtc-cobar f))
  (? cf 2 (allp 1 (crpr 0 (d 7) 0 (d 7)) 1 (crpr 0 (d 56) 0 (d 56))))
|#

(DEFUN HMTP-VRTC-COBAR-INTR (h gf)
   (declare (type morphism h gf))
   (flet ((rslt (degr allp)
             (declare
                (fixnum degr)
                (type allp allp))
             (let* ((cbgn-list (reverse (allp-list allp)))
                    (rslt (list nil))
                    (sign (oddp (apply #'+ (mapcar #'car cbgn-list))))  ;; nil = -1 ;; t = +1
                    (gfgf-tail (term-cmbn 0 1 +null-allp+)))
                (declare
                   (list cbgn-list rslt)
                   (fixnum sign)
                   (type cmbn gfgf-tail))
                (unless cbgn-list
                   (return-from rslt (zero-cmbn 1)))
                (loop
                   (let ((cbgn (car cbgn-list)))
                      (declare (type cbgn cbgn))
                      (with-cbgn (degr gnrt) cbgn
                         (when (evenp degr)
                            (setf sign (not sign)))
                         (incf degr)
                         (let ((h-gnrt (gnrt-? h degr gnrt)))
                            (declare (type cmbn h-gnrt))
                            (let ((hgfgf (cmbn-list (cmbn-allp-cmbn-tnpr h-gnrt gfgf-tail))))
                               (declare (type cmbn hgfgf))
                               (when sign
                                  (mapc #'(lambda (term)
                                             (declare (type term term))
                                             (setf (cffc term) (- (cffc term))))
                                     hgfgf))
                               (setf cbgn-list (rest cbgn-list))
                               (let ((head (reverse cbgn-list)))
                                  (declare (list head))
                                  (mapc #'(lambda (term)
                                             (setf (allp-list (gnrt term))
                                                   (append head (allp-list (gnrt term)))))
                                     hgfgf))
                               (nconc rslt hgfgf)))
                         (unless cbgn-list (return))
                         (setf gfgf-tail
                               (cmbn-allp-cmbn-tnpr
                                  (gnrt-? gf degr gnrt) gfgf-tail)))))
                (make-cmbn
                   :degr (1+ degr)
                   :list (rest rslt)))))
      (the intr-mrph #'rslt)))

#|
  (require "special-smsts")
  (require "eilenberg-zilber")
  (cat-init)
  (setf ez (ez (delta-infinity) (delta-infinity)))
  (setf h (h ez) gf (cmps (g ez) (f ez)))
  (setf r (hmtp-vrtc-cobar-intr h gf))
  (funcall r 3 (allp 1 (crpr 0 7 0 7) 1 (crpr 0 14 0 14) 1 (crpr 0 14 0 14)))
|#

(DEFUN HMTP-VRTC-COBAR (h gf)
   (declare (type morphism h gf))
   (unless (and (= +1 (degr h))
                (= +0 (degr gf)))
      (error "In HMTP-VRTC-COBAR, the morphism degrees are not the right ones."))
   (unless (and (eq (sorc h) (trgt h))
                (eq (trgt h) (sorc gf))
                (eq (sorc gf) (trgt gf)))
      (error "In HMTP-VRTC-COBAR, fg-h sources and targets are not the same."))
   (the morphism
      (if (eq (first (orgn h)) 'zero-mrph)
         (zero-mrph (vrtc-cobar (sorc h)))
         (build-mrph
            :sorc (vrtc-cobar (sorc h)) :trgt (vrtc-cobar (sorc h)) :degr +1
            :intr (hmtp-vrtc-cobar-intr h gf)
            :strt :gnrt
            :orgn `(hmtp-vrtc-cobar ,h ,gf)))))

(DEFMETHOD VRTC-COBAR ((rdct reduction))
   (the reduction
      (if (eq (first (orgn rdct)) 'trivial-rdct)
         (trivial-rdct (vrtc-cobar (bcc rdct)))
         (with-slots (f g h) rdct
            (build-rdct
               :f (vrtc-cobar f)
               :g (vrtc-cobar g)
               :h (hmtp-vrtc-cobar h (cmps g f))
               :orgn `(vrtc-cobar ,rdct))))))

#|
  (cat-init)
  (setf tcc (build-chcm
               :cmpr #'s-cmpr
               :basis #'(lambda (degr) '(a b c d))
               :bsgn 'd
               :intr-dffr #'(lambda (degr gnrt)
                               (ecase gnrt
                                  (a (cmbn (1- degr) 1 'b 1 'd))
                                  ((b d) (cmbn (1- degr)))
                                  (c (cmbn (1- degr) 1 'd))))
               :strt :gnrt
               :orgn '(tcc)))
  (setf bcc (build-chcm
               :cmpr #'s-cmpr
               :basis #'(lambda (degr) '(c d))
               :bsgn 'd
               :intr-dffr #'(lambda (degr gnrt)
                               (ecase gnrt
                                  (d (cmbn (1- degr)))
                                  (c (cmbn (1- degr) 1 'd))))
               :strt :gnrt
               :orgn '(bcc)))
  (setf f (build-mrph :sorc tcc :trgt bcc :degr 0
             :intr #'(lambda (degr gnrt)
                        (ecase gnrt
                           (a (cmbn degr 1 'c 1 'd))
                           (b (cmbn degr))
                           ((c d) (cmbn degr 1 gnrt))))
             :strt :gnrt :orgn '(f)))
  (setf g (build-mrph :sorc bcc :trgt tcc :degr 0
             :intr #'identity :strt :cmbn :orgn '(g)))
  (setf h (build-mrph :sorc tcc :trgt tcc :degr +1
             :intr #'(lambda (degr gnrt)
                        (ecase gnrt
                           ((a b) (cmbn (1+ degr) 1 'a -1 'b -1 'c -1 'd))
                           ((c d) (cmbn (1+ degr)))))
             :strt :gnrt :orgn '(h)))
  (setf rdct (build-rdct :f f :g g :h h :orgn '(rdct)))
  (tcc rdct 3 'a)
  (g rdct (f rdct 3 'a))
  (h rdct 3 'a)
  (setf cobar (vrtc-cobar rdct))
  (pre-check-rdct cobar)
  (defun aleat-tc ()
     (do ((tdegr 0 (+ tdegr degr))
          (degr (1+ (random 4)) (1+ (random 4)))
          (gnrt (intern (coerce (vector (+ 65 (random 4))) 'string))
                (intern (coerce (vector (+ 65 (random 4))) 'string)))
          (rslt nil (cons (cbgn degr gnrt) rslt)))
         ((> tdegr 10) (setf *tc* (cmbn tdegr 1 (make-allp :list rslt))))))
  (aleat-tc)
  (defun aleat-bc ()
     (do ((tdegr 0 (+ tdegr degr))
          (degr (1+ (random 4)) (1+ (random 4)))
          (gnrt (intern (coerce (vector (+ 67 (random 2))) 'string))
                (intern (coerce (vector (+ 67 (random 2))) 'string)))
          (rslt nil (cons (cbgn degr gnrt) rslt)))
         ((> tdegr 10) (setf *bc* (cmbn tdegr 1 (make-allp :list rslt))))))
  (aleat-bc)
  (defun c ()
     (aleat-tc)
     (aleat-bc)
     (check-rdct))
  (loop (c))  ;; degrees >= 15 is possible => error.
|#

(DEFMETHOD VRTC-COBAR ((hmeq homotopy-equivalence))
   (the homotopy-equivalence
      (if (eq (first (orgn hmeq)) 'trivial-hmeq)
         (trivial-hmeq (vrtc-cobar (lbcc hmeq)))
         (with-slots (lrdct rrdct) hmeq
            (build-hmeq 
               :lrdct (vrtc-cobar lrdct)
               :rrdct (vrtc-cobar rrdct)
               :orgn `(vrtc-cobar ,hmeq))))))

(DEFMETHOD COBAR ((hmeq homotopy-equivalence))
   (unless (typep (lbcc hmeq) 'coalgebra)
      (error "In (COBAR HMEQ), the LBCC should be a coalgebra."))
   (the homotopy-equivalence
      (if (eq (first (orgn hmeq)) 'trivial-hmeq)
         (trivial-hmeq (cobar (lbcc hmeq)))
         (add (vrtc-cobar hmeq) (cobar-hrzn-dffr (lbcc hmeq))))))

#|
  (cat-init)
  (setf h (left-hmeq (sphere 3)))
  (setf c (cobar h))
  (inspect c)
|#