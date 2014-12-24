;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES
;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES
;;;  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES  LOOP-SPACES

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "loop-spaces")

(DEFCONSTANT +NULL-LOOP+ (make-loop :list +empty-list+))

;;; NILOOP = non-normalized iloop; a common dgop is possible.

(DEFUN NORMALIZE-LOOP (dmns niloop)
   (declare (list niloop))      
   (the absm
      (progn
         (unless niloop
            (return-from normalize-loop
               (absm (mask dmns) +null-loop+)))
         (let ((dgop (apply #'logand (mapcar #'car niloop))))
            (declare (fixnum dgop))
            (if (zerop dgop)
               (absm 0 (make-loop :list niloop))
               (absm dgop
                  (make-loop
                     :list (mapcar #'(lambda (apowr)
                                        (cons
                                           (dgop/dgop (apdgop apowr) dgop)
                                           (cdr apowr)))
                              niloop))))))))

#|
  (normalize-loop 4 +empty-list+)
  (normalize-loop 4 '((1 a . 1)))
  (normalize-loop 4 '((1 a . 1) (3 b . 1) (5 b . 1)))
|#

(DEFUN UNNORMALIZE-LOOP (absm)
  (declare (type absm absm))
  (the iloop
     (with-absm (dgop loop) absm
        (mapcar #'(lambda (apowr)
		    (declare (type apowr apowr))
		    (cons (dgop*dgop dgop (car apowr))
			  (cdr apowr)))
		(loop-list loop)))))

#|
  (unnormalize-loop (absm 1 (loop3 2 'a 3)))
|#

(DEFUN LOOP3 (&rest list)
  (when (and (= 1 (length list))
             (listp (first list)))
    (setf list (first list)))
  (unless (zerop (mod (length list) 3))
    (error "In LOOP3, 3 should divide the list length."))
  (flet ((fixnumpp (object)
                   (unless (typep object 'fixnum)
                     (error "In LOOP3, ~A should be a fixnum." object))
                   object))
    (do ((rslt +empty-list+ (cons (apowr
                                   (fixnumpp (car mark))
                                   (cadr mark)
                                   (fixnumpp (caddr mark)))
                                  rslt))
         (mark list (cdddr mark)))
        ((endp mark) (make-loop :list (nreverse rslt)))
      (declare (list rslt mark)))))

#|
()
(loop3 nil)
(loop3 '(1 a 2 3 b -2))
(loop3 '(1 a 2 3 b b))
(loop3 '(1 a 2 3 b))
(loop3 1 'a 2 3 'b 4)
(loop3 1 'a 2 3 'b)
|#

(DEFUN LOOP-PRINT (loop stream depth)
  (declare
   (type loop loop) (stream stream)
   (ignore depth))
  (the loop
    (progn
      (format stream "<<Loop")
      (dolist (apowr (loop-list loop))
        (declare (type apowr apowr))
        (with-apowr (dgop gmsm expn) apowr
                    (format stream "[")
                    (unless (zerop dgop)
                      (format stream "~A " (hyphenize-list (dgop-int-ext dgop))))
                    (format stream "~A" gmsm)
                    (unless (= 1 expn)
                      (format stream "\\~D" expn))
                    (format stream "]")))
      (format stream ">>")
      loop)))

#|
  (loop3 nil)
  (loop3 '(0 a 2))
  (loop3 '(1 a 2))
  (loop3 '(3 a 2 5 b -1 6 c 4))
  (loop3 '(7 a 1 11 b 1 13 c 1 14 d 1))
  (loop3 '(7 a 1 11 b 1 13 c 1 7 d 1))
  (loop3 '(0 a 1 0 b 2 0 c -1 1 d 1))
|#

(DEFUN APOWR-NILOOP (cmpr apowr niloop)
   (declare
      (type cmprf cmpr)
      (type apowr apowr)
      (type iloop niloop))
   (the iloop
      (progn
         (unless niloop
            (return-from apowr-niloop
               (list apowr)))
         (let ((apowr2 (first niloop)))
            (declare (type apowr apowr2))
            (with-apowr (dgop2 gmsm2 expn2) apowr2
               (unless (and (= (apdgop apowr) dgop2)
                            (eq :equal (funcall cmpr (apgmsm apowr) gmsm2)))
                  (return-from apowr-niloop
                     (cons apowr niloop)))
               (incf expn2 (apexpn apowr))
               (if (zerop expn2)
                  (rest niloop)
                  (cons (apowr dgop2 gmsm2 expn2) (rest niloop))))))))

#|
  (apowr-niloop #'s-cmpr '(0 a . 1) +empty-list+)
  (apowr-niloop #'s-cmpr '(0 a . 1) '((1 a . 1)))
  (apowr-niloop #'s-cmpr '(0 a . 1) '((0 b . 1)))
  (apowr-niloop #'s-cmpr '(0 a . 1) '((0 a . 1) (0 b . 1)))
  (apowr-niloop #'s-cmpr '(0 a . 1) '((0 a . -1) (0 b . 1)))
|#

(DEFUN LOOP-SPACE-CMPR (cmpr)
   (declare (type cmprf cmpr))
   (flet ((2apowr-cmpr (apowr1 apowr2)
           (declare (type apowr apowr1 apowr2))
           (lexico
              (f-cmpr (apdgop apowr1) (apdgop apowr2))
              (funcall cmpr (apgmsm apowr1) (apgmsm apowr2))
              (f-cmpr (apexpn apowr1) (apexpn apowr2)))))
      (flet ((rslt (loop1 loop2)
              (declare (type loop loop1 loop2))
              (maplexico #'2apowr-cmpr (loop-list loop1) (loop-list loop2))))
         (the cmprf #'rslt))))

#|
  (setf cmpr (loop-space-cmpr #'s-cmpr))
  (funcall cmpr (loop3 '(1 a 2)) (loop3 '(1 a -1)))
  (funcall cmpr (loop3 '(3 b 4 1 a 2)) (loop3 '(3 b 4 1 a -1)))
|#

(DEFUN APOWR-FACE4 (face indx dmns apowr)
   ;; dmns = dimension in the loop-space
   (declare
      (type face face)
      (type apowr apowr))
   (with-apowr (dgop gmsm expn) apowr
      (let ((face (a-face4 face indx (1+ dmns) (absm dgop gmsm))))
         (declare (type absm face))
         (with-absm (dgop gmsm) face
            (if (logbitp (1- dmns) dgop)
               nil
               (apowr dgop gmsm expn))))))

#|
  (setf face (face (delta-infinity)))
  (apowr-face4 face 3 4 (apowr 1 31 5))
  (setf face (face (sphere 5)))
  (apowr-face4 face 2 5 (apowr 1 's5 4))
  (apowr-face4 face 2 5 (apowr 2 's5 4))
|#

(DEFUN APOWR-LASTFACE4 (cmpr face dmns apowr)
   ;; indx =dmns
   (declare
      (type cmprf cmpr)
      (type face face)
      (fixnum dmns)
      (type apowr apowr))
   (the list
      (with-apowr (dgop gmsm expn) apowr
         (let ((absm (absm dgop gmsm)))
            (declare (type absm absm))
            (let ((face-n (a-face4 face dmns (1+ dmns) absm))
                  (face-n+1 (a-face4 face (1+ dmns) (1+ dmns) absm)))
               (declare (type absm face-n face-n+1))
               (with-absm (dgop1 gmsm1) face-n
               (with-absm (dgop2 gmsm2) face-n+1
                  (if (logbitp (1- dmns) dgop1)
                     (if (logbitp (1- dmns) dgop2)
                        (return-from apowr-lastface4 +empty-list+)
                        (return-from apowr-lastface4
                           (list (apowr dgop2 gmsm2 (- expn)))))
                     (when (logbitp (1- dmns) dgop2)
                        (return-from apowr-lastface4
                           (list (apowr dgop1 gmsm1 expn)))))
                  (when (and (= dgop1 dgop2)
                             (eq :equal (funcall cmpr gmsm1 gmsm2)))
                     (return-from apowr-lastface4 +empty-list+))
                  (case expn
                     (+1 (list (apowr dgop2 gmsm2 -1) (apowr dgop1 gmsm1 +1)))
                     (-1 (list (apowr dgop1 gmsm1 -1) (apowr dgop2 gmsm2 +1)))
                     (otherwise
                        (if (plusp expn)
                           (do ((rslt +empty-list+ (cons apowr2 (cons apowr1 rslt)))
                                (apowr1 (apowr dgop1 gmsm1 +1))
                                (apowr2 (apowr dgop2 gmsm2 -1))
                                (nark expn (1- nark)))
                               ((zerop nark) rslt)
                              (declare
                                 (list rslt)
                                 (type apowr apowr1 apowr2)))
                              (do ((rslt +empty-list+ (cons apowr1 (cons apowr2 rslt)))
                                   (apowr1 (apowr dgop1 gmsm1 -1))
                                   (apowr2 (apowr dgop2 gmsm2 +1))
                                   (nark expn (1+ nark)))
                                  ((zerop nark) rslt)
                                 (declare
                                    (list rslt)
                                    (type apowr apowr1 apowr2)))))))))))))

#|
  (setf cmpr #'f-cmpr face (face (delta-infinity)))
  (apowr-lastface4 cmpr face 4 (apowr 0 63 1))
  (apowr-lastface4 cmpr face 4 (apowr 0 63 -1))
  (apowr-lastface4 cmpr face 4 (apowr 0 63 3))
  (apowr-lastface4 cmpr face 4 (apowr 0 63 -3))
  (apowr-lastface4 cmpr face 4 (apowr 8 31 1))
  (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                  (case indx
                     (5 (absm 8 'a))
                     (4 (absm 8 'a)))))
  (setf cmpr #'s-cmpr)
  (apowr-lastface4 cmpr face 4 (apowr 0 'a 2))
  (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                  (case indx
                     (5 (absm 1 'a))
                     (4 (absm 8 'a)))))
  (apowr-lastface4 cmpr face 4 (apowr 0 'a 2))
  (setf face #'(lambda (indx dmns gmsm)  ;  dmns = 5
                  (case indx
                     (5 (absm 1 'a))
                     (4 (absm 1 'a)))))
  (apowr-lastface4 cmpr face 4 (apowr 0 'a 2))
|#
                                   
(DEFUN LOOP-SPACE-FACE (cmpr face)
   (declare
      (type cmprf cmpr)
      (type face face))
   (flet ((rslt (indx dmns loop)
             (declare
                (fixnum indx dmns)
                (type loop loop))
             (the absm
                (let ((rslt +empty-list+))
                   (declare (type iloop rslt))
                   (if (< indx dmns)
                      (dolist (apowr (loop-list loop))
                         (declare (type apowr apowr))
                         (let ((lface (apowr-face4 face indx dmns apowr)))
                            (declare (type (or null apowr) lface))
                            (when lface (setf rslt (apowr-niloop cmpr lface rslt)))))
                      (dolist (apowr (loop-list loop))
                         (declare (type apowr apowr))
                         (let ((lface (apowr-lastface4 cmpr face dmns apowr)))
                            (declare (list lface))
                            (dolist (apowr lface)
                               (declare (type apowr apowr))
                               (setf rslt (apowr-niloop cmpr apowr rslt))))))
                   (normalize-loop (1- dmns) (nreverse rslt))))))
      (the face #'rslt)))

#|
  (setf cmpr #'f-cmpr
        face (face (delta-infinity)))
  (setf rslt (loop-space-face cmpr face))
  (funcall rslt 2 4 (loop3 '(0 63 3 1 31 -3)))
  (funcall rslt 2 4 (loop3 '(0 63 3 0 63 -3)))
  (funcall rslt 4 4 (loop3 '(0 63 3 0 63 -3)))
  (funcall rslt 4 4 (loop3 '(0 63 3)))
|#

(DEFUN LOOP-SPACE-FACE* (cmpr face)
   (declare
      (type cmprf cmpr)
      (type face face))
   (flet ((rslt (indx dmns loop)
             (declare
                (fixnum indx dmns)
                (type loop loop))
             (the (or loop (eql :degenerate))
                (let ((rslt +empty-list+))
                   (declare (type iloop rslt))
                   (if (< indx dmns)
                      (dolist (apowr (loop-list loop))
                         (declare (type apowr apowr))
                         (let ((lface (apowr-face4 face indx dmns apowr)))
                            (declare (type (or null apowr) lface))
                            (when lface (setf rslt (apowr-niloop cmpr lface rslt)))))
                      (dolist (apowr (loop-list loop))
                         (declare (type apowr apowr))
                         (let ((lface (apowr-lastface4 cmpr face dmns apowr)))
                            (declare (list lface))
                            (dolist (apowr lface)
                               (declare (type apowr apowr))
                               (setf rslt (apowr-niloop cmpr apowr rslt))))))
                   (if (zerop (apply #'logand (mapcar #'car rslt)))
                      (make-loop :list (nreverse rslt))
                      :degenerate)))))
      (the face* #'rslt)))

(DEFUN LOOP-SPACE-GRIN-SINTR (dmns loop)
  (declare
     (ignore dmns)
     (type loop loop))
  (the absm
     (absm 0 (make-loop
	      :list (mapcar #'(lambda (apowr)
				(declare (type apowr apowr))
				(with-apowr (dgop gmsm expn) apowr
					    (apowr dgop gmsm (- expn))))
			    (reverse (loop-list loop)))))))

#|
  (loop-space-grin-sintr 5 (loop3 0 'a 2 3 'b -3))
|#

(DEFUN LOOP-SPACE-GRML-SINTR (cmpr)
  (declare (type cmprf cmpr))
  (flet ((rslt (dmns crpr)
	    (declare
	       (fixnum dmns)
	       (type crpr crpr))
	    (the absm
	       (with-crpr (absm1 absm2) crpr
	         (let ((niloop1 (nreverse (unnormalize-loop absm1)))
		       (niloop2 (unnormalize-loop absm2)))
		   (declare (type iloop niloop1 niloop2))
		   (loop
		    (unless niloop1 (return))
		    (unless niloop2 (return))
		    (let ((apowr1 (first niloop1))
			  (apowr2 (first niloop2)))
		      (declare (type apowr apowr1 apowr2))
		      (with-apowr (dgop1 gmsm1 expn1) apowr1
		      (with-apowr (dgop2 gmsm2 expn2) apowr2
		         (unless (and (= dgop1 dgop2)
				      (eq :equal (funcall cmpr gmsm1 gmsm2)))
			    (return))
			 (incf expn1 expn2)
			 (cond ((zerop expn1)
				(pop niloop1)
				(pop niloop2))
			       (t
				(pop niloop1)
				(setf niloop2
				      (cons (apowr dgop2 gmsm2 expn1)
					    (rest niloop2)))
				(return)))))))
		   (normalize-loop dmns
				   (nreconc niloop1 niloop2)))))))
     (the sintr #'rslt)))

#|
  (setf cmpr #'s-cmpr)
  (setf ml (loop-space-grml-sintr cmpr))
  (funcall ml 2 (crpr 3 (loop3) 0 (loop3 0 'a 2)))
  (funcall ml 2 (crpr 0 (loop3 0 'a 2) 3 (loop3)))
  (funcall ml 2 (crpr 0 (loop3 0 'a 2) 0 (loop3 0 'a -2)))
  (funcall ml 2 (crpr 0 (loop3 0 'a 2) 0 (loop3 0 'a -3)))
  (funcall ml 2 (crpr 0 (loop3 0 'a 2) 0 (loop3 0 'b -3)))
  (funcall ml 2 (crpr 0 (loop3 0 'a 2 0 'b -3)
                      0 (loop3 0 'b +3 0 'a -2)))
|#

(DEFUN LOOP-SPACE (smst &optional (n 1))
   (declare
      (type simplicial-set smst)
      (fixnum n))   
   (the simplicial-group
      (if (= n 1)
         (with-slots (cmpr face) smst
            (declare
               (type cmprf cmpr)
	       (type face face))
	    (build-smgr
	       :cmpr (loop-space-cmpr cmpr)
	       :basis :locally-effective
	       :bspn +null-loop+
	       :face (loop-space-face cmpr face)
	       :face* (loop-space-face* cmpr face)
	       :sintr-grml (loop-space-grml-sintr cmpr)
	       :sintr-grin #'loop-space-grin-sintr
	       :orgn `(loop-space ,smst)))
	(loop-space (loop-space smst) (1- n)))))

(DEFUN GDELTAB ()
   (loop-space (deltab)))

#|
  (cat-init)
  (setf g (gdeltab))
  (cmpr g (loop3 0 3 2) (loop3 0 3 2))
  (cmpr g (loop3 0 3 2) (loop3 0 3 3))
  (cmpr g (loop3 0 3 2) (loop3 0 5 2))
  (cmpr g (loop3 0 5 2) (loop3 0 3 2))
  (face g 3 3 (loop3 12 7 3))
  (face g 3 3 (loop3 5 7 3))
  (face g 3 3 (loop3 6 7 3 5 7 3 3 7 3))
  (face g 4 4 (absm 4 (loop3 6 7 3 5 7 3 3 7 3)))
  (check-faces (cmpr g) (face g) 3 (loop3 6 7 -3 5 7 -3 3 7 -3))
  (setf dd (cmps g g))
  (? dd 3 (loop3 6 7 3 5 7 3 3 7 3))
  (grml g 2 (crpr 0 (loop3 0 15 3) 0 (loop3 0 15 4)))
  (grin g 2 (loop3 0 15 -2))
  (setf rslt (loop3 0 31 1))
  (setf hat (mapcar #'(lambda (i) (face g i 3 rslt))
                    (<a-b> 0 3)))
  (dotimes (i 4)
    (print (kfll g i 3 (remove (nth i hat) hat :test #'equal)))
    (check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

  (setf g (loop-space (sphere 2)))
  (setf rslt (loop3 3 's2 2 5 's2 -2 6 's2 2))
  (setf hat (mapcar #'(lambda (i) (face g i 3 rslt))
                    (<a-b> 0 3)))
  (dotimes (i 4)
    (print (kfll g i 3 (remove (nth i hat) hat :test #'equal)))
    (check-kan g i 3 (remove (nth i hat) hat :test #'equal)))

  (setf g (loop-space (sphere 3) 2))
  (setf x (first (basis (echcm g) 4)))
  (setf efhm (efhm g))
  (setf x (lf efhm (rg efhm 4 x)))
  (setf rslt (gnrt (first (cmbn-list x))))
  (setf hat (mapcar #'(lambda (i) (face g i 4 rslt))
                    (<a-b> 0 4)))
  (dotimes (i 5)
    (print (kfll g i 4 (remove (nth i hat) hat :test #'equal)))
    (check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

  (setf rslt (gnrt (third (cmbn-list x))))
  (setf hat (mapcar #'(lambda (i) (face g i 4 rslt))
                    (<a-b> 0 4)))
  (dotimes (i 5)
    (print (kfll g i 4 (remove (nth i hat) hat :test #'equal)))
    (check-kan g i 4 (remove (nth i hat) hat :test #'equal)))

  (setf g (loop-space (sphere 3) 2))
  (setf efhm (efhm g))
  (setf echcm (rbcc efhm))
  (setf basis (basis echcm 4))
  (setf cmbn (cmbn 4 1 (first basis) 10 (second basis)
                     100 (third basis)))
  (setf cmbn2 (lf efhm (rg efhm cmbn)))
  (setf loop-list (mapcar #'cdr (cmbn-list cmbn2)))
  (dolist (loop loop-list)
    (setf hat (mapcar #'(lambda (i) (face g i 4 loop))
                      (<a-b> 0 4)))
    (dotimes (i 5)
      (print (kfll g i 4 (remove (nth i hat) hat)))
      (check-kan g i 4 (remove (nth i hat) hat))))
|#

#|
  (cat-init)
  (setf p (r-proj-space 3))
  (setf op (loop-space p))
  (defun random-apowr (dmns max-expn)
     (loop
        (let* ((dgop (random (2-exp (1- dmns))))
               (gmsm (- dmns (logcount dgop))))
           (unless (< 0 gmsm 3)
              (return-from random-apowr
                 (apowr dgop gmsm (srandom max-expn)))))))
  (dotimes (i 15) (print (random-apowr 8 3)))
  (defun random-niloop (dmns max-expn length)
     (mapcar #'(lambda (dummy)
                  (random-apowr (1+ dmns) max-expn))
        (make-list length)))
  (dotimes (i 20)
     (print (normalize-loop 7 (random-niloop 7 3 3))))
  (defun random-loop-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
     (do ((rslt +empty-list+ (cons term rslt))
          (i cmbn-length (1- i))
          (term))
         ((zerop i)
          (apply #'nterm-add cmpr degr rslt))
        (setf term
              (term (srandom max-cffc)
                 (make-loop :list (random-niloop degr max-expn loop-length))))))
  (setf cmpr (cmpr op))
  (setf +too-much-time+ -1)
  (setf c (random-loop-cmbn cmpr 8 10 4 4 100))
  (time (? op (? op c)))
  (time (? op (? op c)))
|#
  
#|  ;; in EAT.
  (DEFUN PR-INFINITY-EQS (gsm1 gsm2)
     (declare (fixnum gsm1 gsm2))
     t)

  (DEFUN PR-INFINITY-BASIS (k)
     (declare (fixnum k))
     (flet ((rslt (dmns)
               (declare (fixnum dmns))
               (if (< 0 dmns k)
                  empty-list
                  (list dmns))))
        (the basis #'rslt)))

  (DEFUN PR-INFINITY-GDL (k)
     (declare (fixnum k))
     (flet ((rslt (indx dmns gsm)
             (declare (fixnum indx dmns gsm))
             (if (<= dmns k)
                (asm (nreverse (<a-b< 0 (1- dmns))) 0)
                (if (or (zerop indx)
                        (= indx dmns))
                   (asm nil (1- dmns))
                   (if (= dmns (1+ k))
                      (asm (nreverse (<a-b< 0 (1- dmns))) 0)
                      (asm (list (1- indx)) (- dmns 2)))))))
        (the function #'rslt)))

  (DEFUN PR-INFINITY (&optional (k 1))
     (declare (fixnum k))
     (build-ss
         :eqs #'pr-infinity-eqs
         :sbs (pr-infinity-basis k)
         :bsp 0
         :gdl (pr-infinity-gdl k)
         :org `(pr-infinity ,k)))

  (setf p (pr-infinity))
  (sbs p 4)
  (dotimes (i 5)
     (print (gdl p i 4 4)))
  (setf cc (ss-cc p))
  (dotimes (i 5)
     (print (d-? cc i i)))
  (setf dd (cmp-mrp (cc-d cc) (cc-d cc)))
  (dotimes (i 6)
     (print (? dd i i)))
  (setf p (pr-infinity 3))
  (dotimes (i 7)
     (print (sbs p i)))
  (dotimes (i 5)
     (print (gdl p i 4 4)))
  (setf d (cc-d (ss-cc p)))
  (dotimes (i 7)
     (print (? d i i)))
  (setf dd (cmp-mrp d d))
  (dotimes (i 7)
     (print (? dd i i)))

  (setf p (pr-infinity 3))
  (setf op (loop-space p))
  (defun srandom (max)
     (let ((rslt (- (random (+ max max)) max)))
        (if (zerop rslt)
           max
           rslt)))
  (dotimes (i 20) (print (srandom 3)))
  (DEFUN DGOP-INT-EXT (dop)
     (declare (fixnum dop))
     (the list
        (do ((dop dop (ash dop -1))
             (rslt empty-list)
             (bmark 0 (1+ bmark)))
            ((zerop dop) rslt)
           (declare
              (fixnum dop bmark)
              (list rslt))
           (when (oddp dop)
              (push bmark rslt)))))
  (defun random-apowr (dmns max-expn)
     (let ((dgop (dgop-int-ext (* 2 (random (2-exp (1- dmns)))))))
        (pwr (asm dgop (- 8 (length dgop))) (srandom max-expn))))
  (dotimes (i 5) (print (random-apowr 8 3)))
  (defun random-niloop (dmns max-expn length)
     (mapcar #'(lambda (dummy)
                  (random-apowr (1+ dmns) max-expn))
        (make-list length)))
  (dotimes (i 20)
     (print (normalize-loop 7 (make-strloop :lst (random-niloop 7 3 3)))))
  (defun random-loop-cmbn (degr max-cffc max-expn loop-length cmbn-length)
     (do ((rslt empty-list (cons mnm rslt))
          (i cmbn-length (1- i))
          (term))
         ((zerop i)
          (make-cmb :dgr degr :lst rslt))
        (setf mnm
              (mnm (srandom max-cffc)
                 (make-strloop :lst (random-niloop degr max-expn loop-length))))))
  (setf d (cc-d (ss-cc op)))
  (setf c (random-loop-cmbn 8 10 4 4 100))
  (time (??? d (??? d c)))
  (time (??? d (??? d c)))
|#

