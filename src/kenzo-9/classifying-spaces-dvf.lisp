;;;  SIMPLICIAL-GROUPS-VF  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-VF
;;;  SIMPLICIAL-GROUPS-VF  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-VF
;;;  SIMPLICIAL-GROUPS-VF  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-V  SIMPLICIAL-GROUPS-VF

(IN-PACKAGE #:cat-9)

(provide "classifying-spaces-dvf")

;; The vector field of a classifying space.
;; Comments not yet written.

(DEFUN CS-VF (G &aux (idnt (bspn G)))
  (declare (type simplicial-group G) (type gmsm idnt))
  (the function
    (labels
        ((frslt
          (dmns gbar)
          (declare (type fixnum dmns) (type gbar gbar))
          (the vctr
            (progn
              (when (zerop dmns)
                (return-from frslt (vctr :crtc)))
              ;; Program designed only for reduced simplicial groups.
              (assert (> dmns 1))
              (with-slots (list) gbar
                (declare (type list list))
                (let ((first (first list))
                      (rest (normalize-gbar (cons (1- dmns) (rest list)))))
                  (declare (type absm first rest))
                  (let ((vctr (ez-vf (1- dmns) (crpr rest first))))
                    (unless (eq :crtc (stts vctr))
                      (setf (valu vctr)
                        (with-slots (dgop1 gmsm1 dgop2 gmsm2) (valu vctr)
                          (declare (type fixnum dgop1 dgop2)
                                   (type gbar gmsm1)
                                   (type gmsm gmsm2))
                          (make-gbar
                           :dmns (ecase (stts vctr)
                                   (:sorc (1+ dmns))
                                   (:trgt (1- dmns)))
                           :list (cons (absm dgop2 gmsm2)
                                       (rest (unnormalize-gbar (absm dgop1 gmsm1) idnt))))))
                      (return-from frslt vctr))
                    (with-slots ((dgopfirst dgop) (gmsmfirst gmsm)) first
                      (declare (type fixnum dgopfirst) (type gmsm gmsmfirst))
                      (with-slots ((dgoprest dgop) (gmsmrest gmsm)) rest
                        (declare (type fixnum dgoprest) (type gmsm gmsmrest))
                        (setf vctr (frslt (- dmns 1 (logcount dgoprest)) gmsmrest))
                        (ecase (stts vctr)
                          (:crtc)
                          (:sorc
                           (setf (valu vctr)
                             (make-gbar
                              :dmns (1+ dmns)
                              :list (cons (absm (1+ (ash dgopfirst 1)) gmsmfirst)
                                          (rest (unnormalize-gbar
                                                 (absm (ash dgoprest 1) (valu vctr)) idnt))))))
                          (:trgt
                           (setf (valu vctr)
                             (make-gbar
                              :dmns (1- dmns)
                              :list (cons (absm (ash dgopfirst -1) gmsmfirst)
                                          (rest (unnormalize-gbar
                                                 (absm (ash dgoprest -1) (valu vctr)) idnt)))))))
                        (return-from frslt vctr))))))))))
      #'frslt)))


(DEFUN G-CS-ISOG-GNRT (G &aux (bspn (bspn G)))
  (declare (type ab-simplicial-group G) (type gmsm bspn))
  (the intr-mrph
    (flet
        ((impl
          (degr abar)
          (declare (type fixnum degr) (type abar abar))
          (the gbar
            (with-slots (list) abar
              (declare (type list list))
              (do ((mark list (cdr mark))
                   (cuml-dmns 0 (+ cuml-dmns (caar mark)))
                   (rslt +empty-list+))
                  ((endp mark)
                   (assert (= degr cuml-dmns))
                   (make-gbar :dmns degr :list rslt))
                (declare (type list mark rslt) (type fixnum cuml-dmns))
                (do ((nth cuml-dmns (1+ nth))
                     (nthlast (+ cuml-dmns (caar mark) -1)))
                    ((= nth nthlast)
                     (push (absm (mask cuml-dmns) (cdar mark)) rslt))
                  (declare (type fixnum nth nthlast))
                  (push (absm (mask nth) bspn) rslt)))))))
      (declare (ftype (function (fixnum abar) gbar) impl))
      #'impl)))

#|
(cat-init)
(setf G (k-z-1))
(setf isog (g-cs-isog-gnrt G))
(funcall isog 7 (abar 2 '(1) 2 '(100) 3 '(10 1)))
|#

(DEFUN G-CS-ISOG (G &aux
                    (isog-gnrt (g-cs-isog-gnrt G))
                    (cmpr (cmpr (classifying-space G))))
  (declare
   (type ab-simplicial-group G)
   (type intr-mrph isog-gnrt)
   (type cmprf cmpr))
  (the function
    (flet
        ((rslt
          (cmbn)
          (declare (type cmbn cmbn))
          (the cmbn
            (with-cmbn (degr list) cmbn
              (make-cmbn
               :degr degr
               :list
               (sort
                (mapcar #'(lambda (term)
                            (declare (type term term))
                            (the term
                              (with-term (cffc abar) term
                                (declare (type abar abar))
                                (term cffc (funcall isog-gnrt degr abar)))))
                  list)
                #'(lambda (gbar1 gbar2)
                    (declare (type gbar gbar1 gbar2))
                    (the boolean
                      (eq (funcall cmpr gbar1 gbar2) :less)))
                :key #'cdr))))))
         (declare (ftype (function (cmbn) cmbn) rslt))
      #'rslt)))

#|
(cat-init)
(setf k (k-z 1))
(setf isog (g-cs-isog k))
(funcall isog (cmbn 4 1 (abar 4 '(1 2 3))))
(funcall isog (cmbn 4 1 (abar 4 '(1 2 3)) 1 (abar 2 '(5) 2 '(6))))
(funcall isog (cmbn 4 1 (abar 2 '(5) 2 '(6)) 1 (abar 4 '(1 2 3))))
|#

(DEFUN G-CS-ISOF-GNRT (G)
  (declare (type ab-simplicial-group G) (ignore G))
  (the intr-mrph
    (flet
        ((impl
          (degr gbar)
          (declare (type fixnum degr) (type gbar gbar))
          (when (zerop degr)
            (return-from impl +null-abar+))
          (the abar
            (with-slots (list) gbar
              (declare (type list list))
              (do ((mark list (cdr mark))
                   (rslt +empty-list+)
                   (dgop (mask (1- (length list))) (ash dgop -1)))
                  ((endp mark)
                   (make-abar :list rslt))
                (declare (type list mark rslt) (type fixnum dgop))
                (if (eql dgop (dgop (car mark)))
                    (incf (caar rslt))
                  (push (cons 1 (gmsm (car mark))) rslt)))))))
      (declare (ftype (function (fixnum gbar) abar) impl))
      #'impl)))

#|
(cat-init)
(setf G (k-z-1))
(setf isof (g-cs-isof-gnrt G))
(setf isog (g-cs-isog-gnrt G))
(funcall isog 7 (abar 2 '(1) 2 '(100) 3 '(10 1)))
(funcall isof 7 *)
(funcall isof 0 (gbar 0))
(funcall isog 0 (abar))
|#

(DEFUN G-CS-ISOF (G &aux
                    (isof-gnrt (g-cs-isof-gnrt G))
                    (cmpr (cmpr (bar G))))
  (declare
   (type ab-simplicial-group G)
   (type intr-mrph isof-gnrt)
   (type cmprf cmpr))
  (the function
    (flet
        ((rslt
          (cmbn)
          (declare (type cmbn cmbn))
          (the cmbn
            (with-cmbn (degr list) cmbn
              (make-cmbn
               :degr degr
               :list
               (sort
                (mapcar #'(lambda (term)
                            (declare (type term term))
                            (the term
                              (with-term (cffc gbar) term
                                (declare (type gbar gbar))
                                (term cffc (funcall isof-gnrt degr gbar)))))
                  list)
                #'(lambda (abar1 abar2)
                    (declare (type abar abar1 abar2))
                    (the boolean
                      (eq (funcall cmpr abar1 abar2) :less)))
                :key #'cdr))))))
         (declare (ftype (function (cmbn) cmbn) rslt))
      #'rslt)))

#|
(cat-init)
(setf G (k-z-1))
(setf isof (g-cs-isof G))
(setf isog (g-cs-isog G))
(funcall isog (cmbn 7 1 (abar 2 '(1) 2 '(100) 3 '(10 1))))
(funcall isof *)
(funcall isof (cmbn 0 1 (gbar 0)))
(funcall isog (cmbn 0 1 (abar)))
|#

(DEFUN SPECIAL-CHCM-VF-REDUCTION (chcm vf bcc isof isog)
  ;; bcc given with its *definitive* differential
  (declare (type chain-complex chcm bcc)
           (type vector-field vf)
           (type function isof isog))
  ;;            (function (cmbn) cmbn)
  (the reduction
    (build-rdct
     :f (chcm-vf-reduction-f chcm vf bcc isof)
     :g (chcm-vf-reduction-g chcm vf bcc isog)
     :h (chcm-vf-reduction-h chcm vf)
     :orgn `(chcm-vf-reduction ,chcm ,vf))))


(DEFUN G-CS-RDCT (G)
  (declare (type ab-simplicial-group G))
  (the reduction
    (special-chcm-vf-reduction (classifying-space G) (cs-vf G) (bar G)
                               (g-cs-isof G) (g-cs-isog G))))

#|
(cat-init)
(setf k1 (k-z-1))
(setf k2-rdct (g-cs-rdct k1))
(setf *tc* (cmbn 4 1 (gbar 4 0 '(1 2 3) 0 '(4 5) 0 '(6) 0 nil)))
(setf *bc* (cmbn 6 1 (abar 2 '(1) 2 '(3) 2 '(5))))
(pre-check-rdct k2-rdct)
(check-rdct)
|#


#|
(cat-init)
(setf k1 (k-z-1))
(setf r (g-cs-rdct k1))
(setf *tc* (cmbn 3 1 (gbar 3 0 '(1 2) 1 nil 0 nil)))
(f r *tc*)
(bcc r *)
(tcc r *tc*)
(f r *)

(setf *bc* (cmbn 3 1 (abar 3 '(1 2))))
(pre-check-rdct r)
(check-rdct)
|#

;;; To be removed.

#|
(cat-init)
(setf k1 (k-z 1))
(setf k2 (classifying-space k1))
(setf hmeq (cs-left-hmeq k1))
(setf f (cmps (rf hmeq) (lg hmeq)))
(setf g (cmps (lf hmeq) (rg hmeq)))
(setf h (cmps (lf hmeq) (cmps (rh hmeq) (lg hmeq))))
(setf rk (build-rdct :f f :g g :h h :orgn '(essai)))
(setf rv (g-cs-rdct k1))
(setf gb (cmbn 3 1 (gbar 3 0 '(1 2) 1 nil 0 nil)))
(setf ab (cmbn 3 1 (abar 3 '(1 2))))
(setf *tc* gb)
(setf *bc* ab)
(pre-check-rdct rk)
(check-rdct)
(pre-check-rdct rv)
(check-rdct)
(setf *tc* (cmbn 7 1 (gbar 7 0 '(1 2 3 4 5 6) 0 '(7 8 9 10 11) 0 '(12 13 14 15)
                           0 '(16 17 18) 0 '(19 20) 0 '(21) 0 '())))
(setf *tc* (cmbn 6 1 (gbar 6 0 '(7 8 9 10 11) 0 '(12 13 14 15)
                           0 '(16 17 18) 0 '(19 20) 0 '(21) 0 '())))
(setf *tc* (cmbn 5 1 (gbar 5 0 '(12 13 14 15)
                           0 '(16 17 18) 0 '(19 20) 0 '(21) 0 '())))
(setf *bc* (cmbn 9 1 (abar 3 '(1 2) 3 '(4 5) 3 '(6 7))))
|#

#|
(cat-init)
(setf k1 (k-z 1))
(setf k2 (k-z 2))
(setf k3 (k-z 3))
(setf r1 (rrdct (efhm (k-z-1))))
(setf r21 (bar r1))
(setf r22 (g-cs-rdct k1))
(setf r2 (cmps r21 r22))
(setf r31 (bar r2))
(setf r32 (g-cs-rdct (tcc r2)))
(setf r3 (cmps r31 r32))
(setf a (gbar 6
              0 (gbar 5 0 '(1 2 3 4) 0 '(5 6 7) 0 '(8 9) 0 '(10) 0 nil)
              0 (gbar 4 0 '(11 12 13) 0 '(14 15) 0 '(16) 0 nil)
              0 (gbar 3 0 '(17 18) 0 '(19) 0 nil)
              0 (gbar 2 0 '(20) 0 nil)
              1 (gbar 0)
              0 (gbar 0)))
(setf b (first (basis (bcc r3) 6)))
(setf *tc* (cmbn 6 1 a))
(setf *bc* (cmbn 6 1 b))
(pre-check-rdct r3)
(check-rdct)

(setf a (gbar 5
              0 (gbar 4 0 '(11 12 13) 0 '(14 15) 0 '(16) 0 nil)
              0 (gbar 3 0 '(17 18) 0 '(19) 0 nil)
              0 (gbar 2 0 '(20) 0 nil)
              1 (gbar 0)
              0 (gbar 0)))
(time (h r3 5 a))
(setf *tc* (cmbn 5 1 a))
(setf *bc* (zero-cmbn 0))
(pre-check-rdct r3)
(check-rdct)

|#

;;; End to be removed.

(DEFUN CLASSIFYING-SPACE-EFHM (smgr)
  (declare (type simplicial-group smgr))
  (the reduction
    (let ((rdct-BG-AG (g-cs-rdct smgr))
          (smgr-efhm (efhm smgr)))
      (declare (type reduction rdct-BG-AG)
               (type effective-homology smgr-efhm))
      (etypecase smgr-efhm
        (chain-complex rdct-BG-AG)
        (reduction (cmps (bar smgr-efhm) rdct-BG-AG))))))


(DEFMETHOD SEARCH-EFHM (classifying-space (orgn (eql 'classifying-space)))
  (declare (type simplicial-set classifying-space))
  (etypecase classifying-space
    (ab-simplicial-group
     (classifying-space-efhm (second (orgn classifying-space))))
    (chain-complex
     (print (list :AAAAA))
     (classifying-space-efhm-2 (second (orgn classifying-space))))))

#|
(cat-init)
(setf r3 (efhm (k-z 3)))
(setf a (gbar 5
              0 (gbar 4 0 '(11 12 13) 0 '(14 15) 0 '(16) 0 nil)
              0 (gbar 3 0 '(17 18) 0 '(19) 0 nil)
              0 (gbar 2 0 '(20) 0 nil)
              1 (gbar 0)
              0 (gbar 0)))
(h r3 5 a)
(setf *tc* (cmbn 5 1 a))
(setf *bc* (cmbn 7 1 (first (basis (bcc r3) 7))))
(pre-check-rdct r3)
(check-rdct)
|#
