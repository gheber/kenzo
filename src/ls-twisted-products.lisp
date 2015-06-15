;;;  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS
;;;  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS
;;;  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS  LS-TWISTED-PRODUCTS

(IN-PACKAGE #:cat)

(PROVIDE "ls-twisted-products")

;; ABSM-LOOPABSM is the "twisted" product of ABSM, a simplex of the base
;;   space of dimension DMNS by LOOP-ABSM, a simplex of the corresponding
;;   loop-space, of dimension (1- DMNS). The returned object is
;;   a loop-absm of dimension (1- DMNS).

(DEFUN ABSM-LOOPABSM (cmpr dmns absm loop-absm)
   (declare
      (type cmprf cmpr)
      (fixnum dmns)  ;; dimension of ABSM
      (type absm absm loop-absm))
   (the absm
      (with-absm (dgop1 gmsm1) absm
         (when (logbitp (1- dmns) dgop1)
            (return-from absm-loopabsm loop-absm))                  
         (with-absm (dgop2 loop2) loop-absm
            (let ((iloop2 (loop-list loop2)))
               (declare (list iloop2))
               (unless iloop2
                  (return-from absm-loopabsm
                     (absm dgop1 (make-loop :list (list (apowr 0 gmsm1 +1))))))
               (let ((apowr (first iloop2)))
                  (declare (type apowr apowr))
                  (with-apowr (dgop22 gmsm2 expn2) apowr
                     (if (and (= dgop1 (dgop*dgop dgop2 dgop22))
                              (eq :equal (funcall cmpr gmsm1 gmsm2)))
                        ;; the absm is eaten by the first component of the loop
                        (if (= -1 expn2)
                           ;; the first item of the loop disappears.
                           (let ((iloop2-rest (rest iloop2)))
                              (if iloop2-rest
                                 (let ((loop-rest (normalize-loop (1- dmns) iloop2-rest)))
                                    (declare (type absm loop-rest))
                                    (absm (dgop*dgop dgop2 (dgop loop-rest)) (gmsm loop-rest)))
                                 (absm (mask (1- dmns)) +null-loop+)))
                           ;; the exponent of the first item is increased by 1
                           (absm dgop2 (make-loop :list (cons (apowr dgop22 gmsm2 (1+ expn2))
                                                           (rest iloop2)))))
                        ;; a concatenation must be done
                        (multiple-value-bind (dgop dgop1 dgop2) (extract-common-dgop dgop1 dgop2)
                           (declare (fixnum dgop dgop1 dgop2))
                           (absm dgop
                              (make-loop
                                 :list (cons (apowr dgop1 gmsm1 +1)
                                          (if (zerop dgop2)
                                             iloop2
                                             (mapcar #'(lambda (apowr)
                                                          (declare (type apowr apowr))
                                                          (cons (dgop*dgop dgop2 (apdgop apowr))
                                                             (cdr apowr)))
                                                iloop2))))))))))))))

#|
()
(setf cmpr (cmpr (deltab)))
(absm-loopabsm cmpr 3 (absm 4 7) (absm 7 +null-loop+))
(absm-loopabsm cmpr 3 (absm 0 15) (absm 7 +null-loop+))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 7 4)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 7 4 1 12 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 7 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 7 -1 1 12 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 7 4)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 0 (loop3 2 7 4 1 14 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 0 (loop3 2 7 -1)))  ;; in principle illegal but works
(absm-loopabsm cmpr 3 (absm 2 7) (absm 0 (loop3 2 7 -1 1 14 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 0 (loop3 0 15 -1 1 14 -1)))
(absm-loopabsm cmpr 3 (absm 0 15) (absm 2 (loop3 0 7 -1 0 14 -1)))
(absm-loopabsm cmpr 3 (absm 2 7) (absm 2 (loop3 0 28 -1 0 14 -1)))
|#

(DEFUN TWISTED-CRTS-PRDC-FACE (cmpr space-face loop-space-face non-twisted-face)
  (declare
   (type cmprf cmpr)
   (type face space-face loop-space-face non-twisted-face))
  (flet ((rslt (indx dmns crpr)
               (declare
                (fixnum indx dmns)
                (type crpr crpr))
               (the absm
                 (progn
                   (unless (= indx dmns)
                     (return-from rslt
                       (funcall non-twisted-face indx dmns crpr)))
                   (with-crpr (absm1 absm2) crpr
                              (let ((face1 (a-face4 space-face dmns dmns absm1))
                                    (face2 (a-face4 loop-space-face dmns dmns absm2)))
                                (declare (type absm face1 face2))
                                (setf face2 (absm-loopabsm cmpr dmns absm1 face2))
                                (2absm-acrpr face1 face2)))))))
      (the face #'rslt)))

(DEFUN TWISTED-CRTS-PRDC (space
                            &aux (loop-space (loop-space space))
                                 (crts-prdc (crts-prdc space loop-space)))
   (declare (type simplicial-set space loop-space crts-prdc))
   (the simplicial-set
      (with-slots ((space-cmpr cmpr) (space-face face)) space
         (declare
            (type cmprf space-cmpr)
            (type face space-face))
      (with-slots ((loop-space-face face)) loop-space
         (declare (type face loop-space-face))
      (with-slots ((crts-prdc-cmpr cmpr) basis bsgn (crts-prdc-face face)) crts-prdc
        (declare
         (type cmprf crts-prdc-cmpr)
         (type basis basis)
         (type face crts-prdc-face)
         (type gmsm bsgn))
        (let ((rslt (build-smst
                        :cmpr crts-prdc-cmpr
                        :basis basis
                        :bspn bsgn
                        :face (twisted-crts-prdc-face
                                 space-cmpr space-face loop-space-face crts-prdc-face)
                        :orgn `(twisted-crts-prdc ,space))))
            (setf (slot-value rslt 'grmd) crts-prdc)
            rslt))))))

#|
  (setf p (r-proj-space 3))
  (setf tw (twisted-crts-prdc p))
  (defun random-apowr (dmns max-expn)
     (loop
        (let* ((dgop (random (2-exp (1- dmns))))
               (gmsm (- dmns (logcount dgop))))
           (unless (< 0 gmsm 3)
              (return-from random-apowr
                 (apowr dgop gmsm (srandom max-expn)))))))
  (defun random-niloop (dmns max-expn length)
     (mapcar #'(lambda (dummy)
                  (random-apowr (1+ dmns) max-expn))
        (make-list length)))
  (defun random-crpr (dmns max-expn length)
     (loop
        (let ((loop (normalize-loop dmns (random-niloop dmns max-expn length)))
              (dgop (random (2-exp dmns))))
           (let ((absm (2absm-acrpr (absm dgop (- dmns (logcount dgop))) loop)))
              (when (and (zerop (dgop absm))
                         (not (< (gmsm1 (gmsm absm)) 3)))
                 (return (gmsm absm)))))))
  (setf cmpr (cmpr tw))
  (defun random-cmbn (degr max-cffc max-expn loop-length cmbn-length)
     (apply #'nterm-add cmpr degr
        (mapcar #'(lambda (dummy)
                     (term (srandom max-cffc) (random-crpr degr max-expn loop-length)))
           (make-list cmbn-length))))
  (setf c (random-cmbn 8 10 4 4 100))
  (time (? tw (? tw c)))
  (time (? tw (? tw c)))
|#

(DEFUN DTAU-D-INTR (crts-prdc-cmpr crts-prdc-face twisted-crts-prdc-face)
   (declare
      (type cmprf crts-prdc-cmpr)
      (type face crts-prdc-face twisted-crts-prdc-face))
   (flet ((rslt (dmns crpr)
                (declare
                 (fixnum dmns)
                 (type crpr crpr))
             (when (zerop dmns)
                (return-from rslt +zero-negative-cmbn+))
             (the cmbn
                (let ((dmns-1 (1- dmns)))
                   (declare (fixnum dmns-1))
                   (when (logbitp dmns-1 (dgop1 crpr))
                      (return-from rslt (zero-cmbn dmns-1)))
                   (let ((face-tau (funcall twisted-crts-prdc-face dmns dmns crpr))
                         (face (funcall crts-prdc-face dmns dmns crpr))
                         (sign (-1-expt-n dmns)))
                      (declare
                         (type absm face-tau face)
                         (fixnum sign))
                      (with-absm (dgop-tau gmsm-tau) face-tau
                      (with-absm (dgop gmsm) face
                         (if (plusp dgop-tau)
                            (if (plusp dgop)
                               (zero-cmbn dmns-1)
                               (term-cmbn dmns-1 (- sign) gmsm))
                            (if (plusp dgop)
                               (term-cmbn dmns-1 sign gmsm-tau)
                               (ecase (funcall crts-prdc-cmpr gmsm-tau gmsm)
                                  (:less
                                   (cmbn dmns-1 sign gmsm-tau (- sign) gmsm))
                                  (:greater
                                   (cmbn dmns-1 (- sign) gmsm sign gmsm-tau))
                                  (:equal
                                   (error "Surprise in DTAU-D-INTR.~@
                                           Crpr = ~A~@
                                           D-tau = ~A~@
                                           D = ~A"
                                      crpr gmsm-tau gmsm))))))))))))
      (the intr-mrph #'rslt)))

#|
  (setf d (deltab))
  (setf p (crts-prdc d (loop-space d)))
  (setf tw (twisted-crts-prdc d))
  (setf crts-prdc-cmpr (cmpr p)
        crts-prdc-face (face p)
        twisted-crts-prdc-face (face tw))
  (setf delta (dtau-d-intr crts-prdc-cmpr crts-prdc-face twisted-crts-prdc-face))
  (funcall delta 3 (crpr 0 15 0 (loop3 0 (- 2048 64) 1)))
  (funcall delta 5 (crpr 10 15 5 (loop3 0 (- 2048 64) 1)))
  (funcall delta 5 (crpr 5 15 10 (loop3 0 (- 2048 64) 1)))
|#

(DEFUN DTAU-D (space
                 &aux (loop-space (loop-space space))
                      (crts-prdc (crts-prdc space loop-space))
                      (crts-prdc-cmpr (cmpr crts-prdc))
                      (crts-prdc-face (face crts-prdc))
                      (twisted-crts-prdc (twisted-crts-prdc space))
                      (twisted-crts-prdc-face (face twisted-crts-prdc)))
   (declare
      (type simplicial-set space loop-space crts-prdc twisted-crts-prdc)
      (type cmprf crts-prdc-cmpr)
      (type face crts-prdc-face twisted-crts-prdc-face))
   (the morphism
      (build-mrph
         :sorc crts-prdc :trgt crts-prdc :degr -1
         :intr (dtau-d-intr crts-prdc-cmpr crts-prdc-face twisted-crts-prdc-face)
         :strt :gnrt
         :orgn `(dtau-d ,space))))

(DEFUN SZCZARBA (space
                   &aux (loop-space (loop-space space))
                        (twisted-crts-prdc (twisted-crts-prdc space))
                        (ez (ez space loop-space))
                        (dtau-d (dtau-d space)))
  (declare
   (type simplicial-set space loop-space twisted-crts-prdc)
   (type reduction ez)
   (type morphism dtau-d))
  (the (values reduction morphism)
    (multiple-value-bind (szczarba bottom-perturbation)
        (add ez dtau-d)
      (with-slots (tcc f g h) szczarba
            (setf tcc twisted-crts-prdc
                  (slot-value f 'sorc) twisted-crts-prdc
                  (slot-value g 'trgt) twisted-crts-prdc
                  (slot-value h 'sorc) twisted-crts-prdc
                  (slot-value h 'trgt) twisted-crts-prdc))
         (values szczarba bottom-perturbation))))
 
#|
  (cat-init)
  (setf rdct (szczarba (deltab)))
  (pre-check-rdct rdct)
  (setf *tc* (cmbn 2 1 (crpr 0 7 0 (loop3 0 (- 512 32) 1)))
        *bc* (cmbn 2 1 (tnpr 0 1 2 (loop3 0 (- 512 32) 1))
                     10 (tnpr 1 (- 4096 1024) 1 (loop3 0 7 1))
                     100 (tnpr 2 7 0 (loop3 0 (- 4096 1024) 1))))
  (check-rdct) 
  (setf bcc (bcc rdct))
  (time (? bcc 5 (tnpr 5 (mask 6) 0 +null-loop+)))
  (time (? bcc 6 (tnpr 6 (mask 7) 0 +null-loop+)))
|#

(DEFUN TWISTED-TNSR-PRDC (space
                            &aux (szczarba (szczarba space)))
   (declare
      (type simplicial-set space)
      (type reduction szczarba))
   (the chain-complex
      (bcc szczarba)))

;;;
;;;  The important contraction.
;;;

(DEFUN POP-FIRST-ABSM (dmns iloop)
   (declare (type iloop iloop))
   (the (values (member -1 0 1) (or nil absm) absm)
      (progn
         (unless iloop
            (return-from pop-first-absm
               (values 0 nil iloop)))
         (let ((apowr (first iloop))
               (niloop-rest (rest iloop)))
            (declare
               (type apowr apowr)
               (type iloop niloop-rest))
            (with-apowr (dgop gmsm expn) apowr
               (let ((popped-absm (absm dgop gmsm)))
                  (declare (type absm popped-absm))
                  (cond ((< expn -1)
                         (values -1 popped-absm
                            (absm 0
                               (make-loop
                                  :list (cons (apowr dgop gmsm (1+ expn)) niloop-rest)))))
                        ((> expn +1)
                         (values +1 popped-absm
                            (absm 0
                               (make-loop
                                  :list (cons (apowr dgop gmsm (1- expn)) niloop-rest)))))
                        (t
                           (values expn popped-absm
                              (normalize-loop dmns niloop-rest))))))))))

#|
()
(pop-first-absm 4 (cdr +null-loop+))
(pop-first-absm 4 (cdr (loop3 0 'a -2)))
(pop-first-absm 4 (cdr (loop3 0 'a +2)))
(pop-first-absm 4 (cdr (loop3 0 'a -1)))
(pop-first-absm 4 (cdr (loop3 0 'a +1)))
(pop-first-absm 4 (cdr (loop3 1 'a -2 2 'b 3)))
(pop-first-absm 4 (cdr (loop3 0 'a +2 2 'b -3)))
(pop-first-absm 4 (cdr (loop3 1 'a -1 2 'b 3)))
(pop-first-absm 4 (cdr (loop3 0 'a +1 2 'b 5)))
(pop-first-absm 4 (cdr (loop3 2 'a -1 1 'b 3)))
(pop-first-absm 4 (cdr (loop3 2 'a +1 1 'b 3)))
|#


(DEFUN CRTS-CONTRACTION-INTR (base-cmpr base-bspn base-face crts-cmpr)
   (declare
      (type cmprf base-cmpr crts-cmpr)
      (type gmsm base-bspn)
      (type face base-face))
   
   (flet ((rslt (n crpr)
             (declare
                (fixnum n)
                (type crpr crpr))
             (the cmbn
                (let ((n+1 (1+ n)))
                   (declare (fixnum n+1))
 (labels (
  (hh (x-n g-n)
     (declare (type absm x-n g-n))
     (if (bspn-p base-cmpr base-bspn n x-n)
        (if (degenerate-p g-n)
           (zero-cmbn n+1)
           (h-hat (loop-list (gmsm g-n))))
        (if (degenerate-p g-n)
           (h-tilde (1dgnr n x-n) g-n)
           (2cmbn-add crts-cmpr (h-hat (loop-list (gmsm g-n)))
              (h-tilde (1dgnr n x-n) g-n)))))
  (h-tilde (x-n+1 g-n)
     (declare (type absm x-n+1 g-n))
     (when (bspn-p base-cmpr base-bspn n+1 x-n+1)
        (return-from h-tilde
           (zero-cmbn n+1)))
     (do ((i n (1- i))
          (dgop 0 (+ dgop (2-exp i)))
          (sign (-1-expt-n+1 n) (- sign))
          (del-i x-n+1  (a-face4 base-face i (1+ i) del-i))
          (rslt (zero-cmbn n+1)))
         ((minusp i) rslt)
        (declare
           (fixnum i dgop sign)
           (type absm del-i)
           (type cmbn rslt))
        (let ((absm1 (ndgnr dgop del-i))
              (loop-absm2 (1dgnr i g-n)))
           (declare (type absm absm1 loop-absm2))
           (unless (plusp (logand (dgop absm1) (dgop loop-absm2)))
             (dstr-add-term-to-cmbn
                 crts-cmpr
                 sign (crpr absm1 loop-absm2)
                 rslt)))))
  (h-hat (g-n)
     (declare (type iloop g-n))  ;;; ***
     (multiple-value-bind (expn y-n+1 gp-n) (pop-first-absm n g-n)
        (declare
           (fixnum expn)
           (type absm y-n+1)
           (type loop gp-n))  ;;; ***
        (ecase expn
           (0 (zero-cmbn (1+ n)))
           (+1
            (let ((h-tildes (2cmbn-sbtr crts-cmpr
                               (h-tilde y-n+1 gp-n)
                               (h-tilde
                                  (1dgnr n (a-face4 base-face (1+ n) (1+ n) y-n+1))
                                  (absm 0 (make-loop :list g-n))))))
               (declare (type cmbn h-tildes))
               (if (degenerate-p gp-n)
                  h-tildes 
                  (2cmbn-add crts-cmpr (h-hat (loop-list (gmsm gp-n))) h-tildes))))
           (-1
            (let ((next-x-n (a-face4 base-face (1+ n) (1+ n) y-n+1))
                  (h-tilde (h-tilde y-n+1 (absm 0 (make-loop :list g-n)))))
               (declare
                  (type absm next-x-n)
                  (type cmbn h-tilde))
               (if (zerop (logand (dgop next-x-n) (dgop gp-n)))
                  (2cmbn-sbtr crts-cmpr
                     (hh next-x-n gp-n) h-tilde)
                  (cmbn-opps h-tilde))))))))
    
    (with-crpr (absm1 loop-absm2) crpr
       (hh absm1 loop-absm2)))))))
    
    (the intr-mrph #'rslt)))


#|
()
(cat-init)
(setf delta (deltab))
(setf tw (twisted-crts-prdc delta))
(setf d (bndr tw))
(setf base-cmpr (cmpr delta)
  base-bspn (bspn delta)
  base-face (face delta)
  crts-cmpr (cmpr tw))
(setf r (crts-contraction-intr base-cmpr base-bspn base-face crts-cmpr))
(setf h (build-mrph :sorc tw :trgt tw :degr +1
                    :intr r :strt :gnrt :orgn `(crts-contraction ,delta)))
(setf z (i-sbtr (idnt-mrph tw) (cmps d h) (cmps h d)))
(? z 0 (crpr 0 1 0 +null-loop+))
(funcall r 0 (crpr 0 1 0 (loop3 0 96 1)))
(? z 0 (crpr 0 1 0 (loop3 0 96 1)))
(? z 0 (crpr 0 1 0 (loop3 0 96 1 0 (+ 256 128) 1)))
(? z 0 (crpr 0 1 0 (loop3 0 96 1 0 (+ 256 128) 1 0 (+ 512 1024) 1)))
(? z 0 (crpr 0 1 0 (loop3 0 96 2)))
(? z 1 (crpr 0 3 0 (loop3 0 (+ 32 64 128) 1)))
(? z 2 (crpr 0 7 0 (loop3 0 (+ 32 64 128 256) 1)))
(? z 3 (crpr 2 7 1 (loop3 2 (+ 32 64 128) 2 4 (+ 32 64 128) -2)))
(? z 3 (crpr 2 7 4 (loop3 2 (+ 32 64 128) 2 1 (+ 32 64 128) -2)))
(? z 3 (crpr 2 7 1 (loop3 2 (+ 32 64 128) -2 4 (+ 32 64 128) 2)))
(? z 3 (crpr 2 7 4 (loop3 2 (+ 32 64 128) -2 1 (+ 32 64 128) 2)))
(? z 3 (crpr 1 7 2 (loop3 2 (+ 32 64 128) 2 4 (+ 32 64 128) -2)))
(? z 3 (crpr 4 7 2 (loop3 2 (+ 32 64 128) 2 1 (+ 32 64 128) -2)))
(? z 3 (crpr 1 7 2 (loop3 2 (+ 32 64 128) -2 4 (+ 32 64 128) 2)))
(? z 3 (crpr 4 7 2 (loop3 2 (+ 32 64 128) -2 1 (+ 32 64 128) 2)))
|#
 

#|
  (cat-init)
  (setf p (r-proj-space 3))
  (setf tw (twisted-crts-prdc p))
  (setf r (crts-contraction-intr (cmpr p) (bspn p) (face p) (cmpr tw)))
  (defun random-apowr (dmns max-expn)
     (loop
        (let* ((dgop (random (2-exp (1- dmns))))
               (gmsm (- dmns (logcount dgop))))
           (unless (< 0 gmsm 3)
              (return-from random-apowr
                 (apowr dgop gmsm (srandom max-expn)))))))
  (defun random-niloop (dmns max-expn length)
     (mapcar #'(lambda (dummy)
                  (random-apowr (1+ dmns) max-expn))
        (make-list length)))
  (defun random-crpr (dmns max-expn length)
     (loop
        (let ((loop (normalize-loop dmns (random-niloop dmns max-expn length)))
              (dgop (random (2-exp dmns))))
           (let ((absm (2absm-acrpr (absm dgop (- dmns (logcount dgop))) loop)))
              (when (and (zerop (dgop absm))
                         (not (< (gmsm1 (gmsm absm)) 3)))
                 (return (gmsm absm)))))))
  (setf cmpr (cmpr tw))
  (defun random-cmbn (degr max-cffc max-expn loop-length cmbn-length)
     (apply #'nterm-add cmpr degr
        (mapcar #'(lambda (dummy)
                     (term (srandom max-cffc) (random-crpr degr max-expn loop-length)))
           (make-list cmbn-length))))
  (setf h (build-mrph :sorc tw :trgt tw :degr +1
             :intr r :strt :gnrt :orgn `(crts-contraction ,p)))
  (setf d (bndr tw))
  (setf z (i-sbtr (idnt-mrph tw) (cmps h d) (cmps d h)))
  (setf c (random-cmbn 5 10 2 2 1))
  (? z c)
  (setf c (random-cmbn 6 10 3 3 3))
  (? z c)
  (setf c (random-cmbn 7 10 4 4 6))
  (? z c)
  (setf c (random-cmbn 8 10 4 4 20))
  (? z c)
  (? h c)
  (? d *)
  (length *)
|#

(DEFUN CRTS-CONTRACTION (space
                           &aux (twisted-crts-prdc (twisted-crts-prdc space)))
   (declare (type simplicial-set space twisted-crts-prdc))
   (the morphism
      (build-mrph
         :sorc twisted-crts-prdc :trgt twisted-crts-prdc :degr +1
         :intr (crts-contraction-intr (cmpr space) (bspn space) (face space)
                  (cmpr twisted-crts-prdc))
         :strt :gnrt
         :orgn `(crts-contraction ,space))))

#|
  (setf h (crts-contraction (deltab)))
  (setf d (bndr (twisted-crts-prdc (deltab))))
  (setf z (i-sbtr (idnt-mrph (sorc d)) (cmps d h) (cmps h d)))
  (? z 3 (crpr 4 7 2 (loop3 2 (+ 32 64 128) -2 1 (+ 32 64 128) 2)))
|#

(DEFUN TNPR-CONTRACTION (space
                           &aux
                           (szczarba (szczarba space))
                           (f (f szczarba))
                           (g (g szczarba))
                           (crts-contraction (crts-contraction space)))
   (declare
      (type simplicial-set space)
      (type reduction szczarba)
      (type morphism f g crts-contraction))
   (the morphism
      (i-cmps f crts-contraction g)))

#|
  (cat-init)
  (setf delta (deltab))
  (setf sz (szczarba (deltab)))
  (setf tw (bcc sz))
  (setf h (tnpr-contraction delta))
  (setf z (i-sbtr (idnt-mrph tw) (cmps tw h) (cmps h tw)))
  (time (? z 3 (tnpr 0 1 3 (loop3 0 (mask 5) 2 0 (* 32 (mask 5)) -1))))
  (? h 4 (tnpr 0 1 0 (loop3 0 (mask 6) 1)))
|#
