;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY

(IN-PACKAGE "COMMON-LISP-USER")

(PROVIDE "effective-homology")

;;;
;;;  REDUCTIONS
;;;

(DEFVAR *RDCT-LIST*)
(SETF *RDCT-LIST* +empty-list+)
(PUSHNEW '*RDCT-LIST* *list-list*)

(DEFMETHOD PRINT-OBJECT ((rdct reduction) stream)
  (declare (type stream stream))
  (the reduction
    (progn
      (format stream "[K~D Reduction K~D => K~D]"
        (idnm rdct) (idnm (tcc rdct)) (idnm (bcc rdct)))
      rdct)))

(DEFUN RDCT (n)
   (declare (type fixnum n))
   (the (or null reduction)
      (find n *rdct-list* :key #'idnm)))

(DEFUN BUILD-RDCT (&key f g h orgn)
   (declare
      (type morphism f g h)
      (list orgn))
   (the reduction
      (progn
         (unless orgn
            (setf orgn `(build-rdct ,f ,g ,h)))
         (let ((already (find orgn *rdct-list* :test #'equal :key #'orgn)))
            (declare (type (or reduction null) already))
            (when already
               (return-from build-rdct already)))
         (with-slots ((fsorc sorc) (ftrgt trgt) (fdegr degr)) f
            (declare
               (type chain-complex fsorc ftrgt)
               (type fixnum fdegr))
         (with-slots ((gsorc sorc) (gtrgt trgt) (gdegr degr)) g
            (declare
               (type chain-complex gsorc gtrgt)
               (type fixnum gdegr))
         (with-slots ((hsorc sorc) (htrgt trgt) (hdegr degr)) h
            (declare
               (type chain-complex hsorc htrgt)
               (type fixnum hdegr))
            (unless (and (eq gsorc ftrgt)
                         (eq fsorc gtrgt)
                         (eq hsorc fsorc)
                         (eq htrgt fsorc)
                         (zerop fdegr)
                         (zerop gdegr)
                         (= +1 hdegr))
               (error "In BUILD-RDCT, the data are non coherent."))
            (let ((rdct (make-instance 'reduction
                           :tcc fsorc :bcc ftrgt
                           :f f :g g :h h
                           :orgn orgn)))
               (declare (type reduction rdct))
               (push rdct *rdct-list*)
               rdct)))))))

(DEFUN TRIVIAL-RDCT (chcm)
  (declare (type chain-complex chcm))
   (build-rdct
    :f (idnt-mrph chcm)
    :g (idnt-mrph chcm)
    :h (zero-mrph chcm chcm +1) 
    :orgn `(trivial-rdct ,chcm)))

;;;
;;;  EQUIVALENCES
;;;

(DEFVAR *HMEQ-LIST*)
(SETF *HMEQ-LIST* +empty-list+)
(PUSHNEW '*HMEQ-LIST* *list-list*)

(DEFGENERIC BUILD-HMEQ (arg1 arg2 &key))

(DEFMETHOD BUILD-HMEQ ((keyword1 (eql :lrdct)) lrdct &key rrdct orgn)
   (declare
      (type reduction lrdct rrdct)
      (list orgn))
   (the equivalence
      (progn
         (with-slots ((lf f) (lg g) (lh h) (ltcc tcc) (lbcc bcc)) lrdct
            (declare
               (type morphism lf lg lh)
               (type chain-complex ltcc lbcc))
         (with-slots ((rf f) (rg g) (rh h) (rtcc tcc) (rbcc bcc)) rrdct
            (declare
               (type morphism rf rg rh)
               (type chain-complex rtcc rbcc))         
            (unless (eq ltcc rtcc)
               (error "In BUILD-HMEQ (version from rdct), the tcc's are not the same."))
            (unless orgn
               (setf orgn `(build-hmeq ,lrdct ,rrdct)))
            (let ((already (find orgn *hmeq-list* :test #'equal :key #'orgn)))
               (declare (type (or equivalence null) already))
               (when already
                  (return-from build-hmeq already)))
            (let ((hmeq (make-instance 'equivalence
                           :lbcc lbcc :tcc ltcc :rbcc rbcc
                           :lf lf :lg lg :lh lh
                           :rf rf :rg rg :rh rh
                           :lrdct lrdct :rrdct rrdct
                           :orgn orgn)))
               (declare (type equivalence hmeq))
               (push hmeq *hmeq-list*)
               hmeq))))))
#|
  (cat-init)
  (progn ;;;; reused in another test
   (defun cdelta (dmns)
     (build-chcm
        :cmpr #'l-cmpr
        :basis :locally-effective
        :bsgn '(0)
        :intr-dffr #'(lambda (degr gmsm)
                        (make-cmbn
                           :degr (1- degr)
                           :list (do ((rslt +empty-list+
                                           (cons (cons sign (append
                                                               (subseq gmsm 0 nark)
                                                               (subseq gmsm (1+ nark))))
                                                 rslt))
                                     (sign 1 (- sign))
                                     (nark 0 (1+ nark)))
                                    ((> nark degr) rslt))))                                           
        :strt :gnrt
        :orgn `(locally effective version of C_* delta ,dmns)))
   (defun make-f (tdmns bdmns)
     (build-mrph
        :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
        :intr #'(lambda (degr gmsm)
                   (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns)) gmsm)))
                      (if pos
                         (if (< pos degr)
                            (zero-cmbn degr)
                            (cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                         (cmbn degr 1 gmsm))))
        :strt :gnrt
        :orgn `(projection delta ,tdmns => delta ,bdmns)))
   (defun make-g (tdmns bdmns)
     (build-mrph
        :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
        :intr #'identity
        :strt :cmbn
        :orgn `(injection delta ,bdmns => delta ,tdmns)))
   (defun make-h (tdmns bdmns)
     (build-mrph
        :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
        :intr #'(lambda (degr gmsm)
                   (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns)) gmsm)))
                      (if pos
                         (if (member bdmns gmsm)
                            (zero-cmbn (1+ degr))
                            (cmbn (1+ degr) (-1-expt-n pos)
                               (append (subseq gmsm 0 pos) (list bdmns) (subseq gmsm pos))))
                         (zero-cmbn (1+ degr)))))
        :strt :gnrt
        :orgn `(homotopy for delta ,tdmns => ,bdmns)))
   (defun make-rdct (tdmns bdmns)
       (setf rdct (build-rdct
                    :f (make-f tdmns bdmns)
                    :g (make-g tdmns bdmns)
                    :h (make-h tdmns bdmns)
                    :orgn `(reduction delta ,tdmns ,bdmns)))))
  (setf rdct (make-rdct 6 3))
  (setf f (f rdct) g (g rdct) h (h rdct)
        tcc (tcc rdct) bcc (bcc rdct))
  (setf dh (cmps (dffr tcc) h))
  (setf hd (cmps h (dffr tcc)))
  (setf gf (cmps g f))
  (setf id-gf-dh-hd (i-sbtr (idnt-mrph tcc) gf dh hd))
  (setf c (cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4) 1000 '(2 3 4)))
  (cmbn-? id-gf-dh-hd c)
|#                                   

(DEFMETHOD PRINT-OBJECT ((hmeq equivalence) stream)
  (declare (type stream stream))
  (the equivalence
    (progn
      (format stream "[K~D Equivalence K~D <= K~D => K~D]"
        (idnm hmeq)
        (idnm (lbcc hmeq)) (idnm (tcc hmeq)) (idnm (rbcc hmeq)))
      hmeq)))


(DEFUN HMEQ (n)
   (declare (type fixnum n))
   (the (or equivalence null)
      (find n *hmeq-list* :key #'idnm)))

(DEFMETHOD BUILD-HMEQ ((keyword1 (eql :lf)) lf &key lg lh rf rg rh orgn)
   (declare
      (type morphism lf lg lh rf rg rh)
      (list orgn))
   (the equivalence
      (progn
         (unless orgn
            (setf orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh)))
         (let ((already (find orgn *hmeq-list* :test #'equal :key #'orgn)))
            (declare (type (or null equivalence) already))
            (when already
               (return-from build-hmeq already)))
         (build-hmeq
          :lrdct (build-rdct :f lf :g lg :h lh :orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh lrdct))
          :rrdct (build-rdct :f rf :g rg :h rh :orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh rrdct))
          :orgn orgn))))

(DEFUN TRIVIAL-HMEQ (chcm)
   (declare (type chain-complex chcm))
   (build-hmeq
      :lrdct (trivial-rdct chcm)
      :rrdct (trivial-rdct chcm)
      :orgn `(trivial-hmeq ,chcm)))

#|
  (cat-init)
  (setf cc (cdelta 5))
  (setf hmeq (trivial-hmeq cc))
|# 

;; Functions.

(DEFVAR *TDD*)
(DEFVAR *BDD*)
(DEFVAR *ID-FG*)
(DEFVAR *ID-GF-DH-HD*)
(DEFVAR *HH*)
(DEFVAR *FH*)
(DEFVAR *HG*)
(DEFVAR *DF-FD*)
(DEFVAR *DG-GD*)

(DEFUN PRE-CHECK-RDCT (rdct)
   (declare (type reduction rdct))
   (with-slots (bcc tcc f g h) rdct
      (declare
         (type chain-complex bcc tcc)
         (type morphism f g h))
      (setf *tdd* (cmps tcc tcc)
            *bdd* (cmps bcc bcc)
            *df-fd* (sbtr
                       (cmps bcc f)
                       (cmps f tcc))
            *dg-gd* (sbtr
                       (cmps tcc g)
                       (cmps g bcc))
            *id-fg* (sbtr (idnt-mrph bcc) (cmps f g))
            *id-gf-dh-hd* (i-sbtr (idnt-mrph tcc)
                             (cmps g f)
                             (cmps tcc h)
                             (cmps h tcc))
            *hh* (cmps h h)
            *fh* (cmps f h)
            *hg* (cmps h g)))
   (done))

(DEFVAR *TC*)
(DEFVAR *BC*)

(DEFUN CHECK-RDCT ()
  (dolist (cmbn '(*tc* *bc*))
    (declare (type symbol cmbn))
    (format t "~%~A => ~A" cmbn (eval cmbn)))
  (dolist (phi '(*tdd* *bdd* *df-fd* *dg-gd* *id-fg* *id-gf-dh-hd*
		       *hh* *fh* *hg*))
    (declare (type symbol phi))
    (format t "~%Checking ~A = 0" phi)
    (format t "~%Result: ")
    (princ (cmbn-? (eval phi)
              (if (member phi '(*bdd* *dg-gd* *id-fg* *dg-gd* *hg*))
                 *bc* *tc*)))
    (read-line))
  (done))

#|
  (setf rdct (make-rdct 6 3))        ;;; defined in a previous test
  (pre-check-rdct rdct)
  (setf *tc* (cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4) 1000 '(2 3 4)))
  (setf *bc* (cmbn 3 4 '(0 1 2 3)))  
  (check-rdct))
|#                                   

(DEFMETHOD CMPS ((eqvl equivalence) (chcm chain-complex) &optional dummy)
  (declare (ignore dummy))
  (assert (eq (rbcc eqvl) chcm))
  (the equivalence eqvl))
  

(DEFMETHOD CMPS ((brdct reduction) (trdct reduction) &optional dummy)
  (declare (ignore dummy))
  (when (eq (first (orgn brdct)) 'trivial-rdct)
     (return-from cmps trdct))
  (when (eq (first (orgn trdct)) 'trivial-rdct)
     (return-from cmps brdct))
  (with-slots ((tf f) (tg g) (th h)) trdct
    (declare (type morphism tf tg th))
  (with-slots ((bf f) (bg g) (bh h)) brdct
    (declare (type morphism bf bg bh))
    (build-rdct
       :f (cmps bf tf)
       :g (cmps tg bg)
       :h (add th (i-cmps tg bh tf))
       :orgn `(cmps ,brdct ,trdct)))))

#|
  (setf trdct (make-rdct 6 4))
  (setf brdct (make-rdct 4 3))
  (setf rdct (cmps brdct trdct))
  (pre-check-rdct rdct)
  (setf *tcc* (cmbn 2 1 '(0 1 2) 10 '(1 2 3) 100 '(1 2 4)
                      100 '(1 3 5) 10 '(1 4 5) 1 '(3 4 5))
        *bcc* (cmbn 2 1 '(0 1 3)))
  (check-rdct))
|#

;
; BASIC PERTURBATION LEMMA.
;

(DEFMETHOD ADD ((rdct reduction) (perturbation morphism) &optional dummy)
   (declare (type null dummy))
   (when dummy
      (error "Why a third argument in (ADD REDUCTION MORPHISM)?"))
   (when (eq (grmd (tcc rdct)) (grmd (sorc perturbation)))
      (return-from add
         (basic-perturbation-lemma rdct perturbation)))
   (when (eq (grmd (bcc rdct)) (grmd (sorc perturbation)))
      (return-from add
         (easy-perturbation-lemma rdct perturbation)))
   (error "In (METHOD ADD REDUCTION MORPHISM),~@
           the data do not sound like coherent."))

(DEFMETHOD SBTR ((chcm chain-complex) (rdct reduction) &optional dummy)
  (declare (type null dummy))
  (when dummy
    (error "Why a third argument in (SBTR CHAIN-COMPLEX REDUCTION)?"))
  (the reduction
    (add rdct (sbtr (dffr chcm) (dffr (tcc rdct))))))

(DEFMETHOD SBTR ((chcm chain-complex) (hmeq equivalence) &optional dummy)
  (declare (type null dummy))
  (when dummy
    (error "Why a third argument in (SBTR CHAIN-COMPLEX EQUIVALENCE)?"))
  (the equivalence
    (add hmeq (sbtr (dffr chcm) (dffr (rbcc hmeq))))))

(DEFUN BASIC-PERTURBATION-LEMMA (reduction top-perturbation)
   (declare
      (type reduction reduction)
      (type morphism top-perturbation))
   (the (values reduction morphism)
      (with-slots ((old-tcc tcc) (old-bcc bcc)
                   (old-f f) (old-g g) (old-h h)) reduction
         (declare
            (type chain-complex old-tcc old-bcc)
            (type morphism old-f old-g old-h))
         (when (eq (first (orgn top-perturbation)) 'zero-mrph)
            (return-from basic-perturbation-lemma
               (values reduction (zero-mrph old-bcc))))
         (when (eq (first (orgn (h reduction))) 'zero-mrph)
            (return-from basic-perturbation-lemma
               (values
                  (trivial-rdct (add old-tcc top-perturbation))
                  (i-cmps old-f top-perturbation old-g))))      
         (let* ((sigma (bpl-*-sigma old-h top-perturbation))
                (new-f (cmps old-f 
                          (sbtr (idnt-mrph old-tcc)
                             (i-cmps
                                top-perturbation
                                sigma
                                old-h))))
                (new-g (cmps sigma old-g))
                (new-h (cmps sigma old-h))
                (bottom-perturbation (i-cmps
                                        old-f
                                        top-perturbation
                                        new-g))
                (new-tcc (add old-tcc top-perturbation))
                (new-bcc (add old-bcc bottom-perturbation)))
            (declare
               (type chain-complex new-tcc new-bcc)
               (type morphism sigma new-f new-g new-h bottom-perturbation))
            (setf new-f (dstr-change-sorc-trgt new-f :new-sorc new-tcc :new-trgt new-bcc)
                  new-g (dstr-change-sorc-trgt new-g :new-sorc new-bcc :new-trgt new-tcc)
                  new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc :new-trgt new-tcc))
            (values
               (build-rdct :f new-f :g new-g :h new-h
                  :orgn `(basic-perturbation-lemma ,reduction ,top-perturbation))
               bottom-perturbation)))))

(DEFUN EASY-PERTURBATION-LEMMA (reduction bottom-perturbation)
   (declare
      (type reduction reduction)
      (type morphism bottom-perturbation))
   (the (values reduction morphism)
      (with-slots ((old-tcc tcc) (old-bcc bcc)
                   (old-f f) (old-g g) (old-h h)) reduction
         (declare
            (type chain-complex old-tcc old-bcc)
            (type morphism old-f old-g old-h))
         (when (eq 'zero-mrph (first (orgn bottom-perturbation)))
            (return-from easy-perturbation-lemma
               (values reduction (zero-mrph old-tcc))))
	 (when (eq 'trivial-rdct (first (orgn reduction)))
	    (return-from easy-perturbation-lemma
	       (trivial-rdct (add (bcc reduction) bottom-perturbation))))
         (let ((top-perturbation (i-cmps old-g bottom-perturbation old-f)))
            (declare (type morphism top-perturbation))
            (let ((new-bcc (add old-bcc bottom-perturbation))
                  (new-tcc (add old-tcc top-perturbation)))
               (declare (type chain-complex new-bcc new-tcc))
               (values
                  (build-rdct
                     :f (dstr-change-sorc-trgt old-f :new-sorc new-tcc :new-trgt new-bcc)
                     :g (dstr-change-sorc-trgt old-g :new-sorc new-bcc :new-trgt new-tcc)
                     :h (dstr-change-sorc-trgt old-h :new-sorc new-tcc :new-trgt new-tcc)
                     :orgn `(easy-perturbation-lemma ,reduction ,bottom-perturbation))
                  top-perturbation))))))

(DEFUN SPECIAL-BPL-1 (reduction top-perturbation)
   (declare
      (type reduction reduction)
      (type morphism top-perturbation))
   (when (eq (first (orgn top-perturbation)) 'zero-mrph)
      (return-from special-bpl-1 reduction))
   (the reduction
      (with-slots ((old-tcc tcc) (old-bcc bcc)
                   (old-f f) (old-g g) (old-h h)) reduction
         (declare
            (type chain-complex old-tcc old-bcc)
            (type morphism old-f old-g old-h))
         (let* ((sigma (bpl-*-sigma old-h top-perturbation))
                (new-f (cmps old-f 
                          (sbtr (idnt-mrph old-tcc)
                             (i-cmps
                                top-perturbation
                                sigma
                                old-h))))
                (new-h (cmps sigma old-h))
                (new-tcc (add old-tcc top-perturbation)))
            (declare
               (type chain-complex new-tcc)
               (type morphism sigma new-f new-h))
            (setf new-f (dstr-change-sorc-trgt new-f :new-sorc new-tcc)
                  old-g (dstr-change-sorc-trgt old-g :new-trgt new-tcc)
                  new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc :new-trgt new-tcc))
            (build-rdct :f new-f :g old-g :h new-h
               :orgn `(special-bpl ,reduction ,top-perturbation))))))

(DEFUN SPECIAL-BPL-2 (reduction top-perturbation)
   (declare
      (type reduction reduction)
      (type morphism top-perturbation))
   (the reduction
      (with-slots ((old-tcc tcc) (old-bcc bcc)
                   (old-f f) (old-g g) (old-h h)) reduction
         (declare
            (type chain-complex old-tcc old-bcc)
            (type morphism old-f old-g old-h))
         (when (eq (first (orgn top-perturbation)) 'zero-mrph)
            (return-from special-bpl-2
               (values reduction (zero-mrph old-bcc))))
         (when (eq (first (orgn (h reduction))) 'zero-mrph)
            (return-from special-bpl-2
               (values
                  (trivial-rdct (add old-tcc top-perturbation))
                  (i-cmps old-f top-perturbation old-g))))      
         (let* ((sigma (bpl-*-sigma old-h top-perturbation))
                (new-g (cmps sigma old-g))
                (new-h (cmps sigma old-h))
                (new-tcc (add old-tcc top-perturbation)))
            (declare
               (type chain-complex new-tcc)
               (type morphism sigma new-g new-h))
            (setf old-f (dstr-change-sorc-trgt old-f :new-sorc new-tcc)
		  new-g (dstr-change-sorc-trgt new-g :new-trgt new-tcc)
                  new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc :new-trgt new-tcc))
            (values
               (build-rdct :f old-f :g new-g :h new-h
                  :orgn `(special-bpl-2 ,reduction ,top-perturbation)))))))


(DEFUN BPL-*-sigma (homotopy perturbation)
   (declare (type morphism homotopy perturbation))
   (the morphism
      (let ((cmpr (cmpr (sorc perturbation)))
            (h-delta (cmps homotopy perturbation)))
         (declare
            (type cmprf cmpr)
            (type morphism h-delta))
         (flet
            ((sigma-* (degr gnrt)
                (declare
                   (type fixnum degr)
                   (type gnrt gnrt))
                (do ((rslt (zero-cmbn degr) (2cmbn-add cmpr rslt iterated))
                     (iterated (term-cmbn degr 1 gnrt) (cmbn-opps (cmbn-? h-delta iterated))))
                    ((cmbn-zero-p iterated) rslt)
                   (declare (type cmbn rslt iterated)))))
            (build-mrph
               :sorc (sorc homotopy) :trgt (sorc homotopy) :degr 0
               :intr #'sigma-*
               :strt :gnrt
               :orgn `(bpl-*-sigma ,homotopy ,perturbation))))))
     
#|
  (cat-init)
  (setf rdct (make-rdct 6 3))
  (setf perturb (zero-mrph (cdelta 6) (cdelta 6) -1))
  (setf new-rdct (add rdct perturb))
  (pre-check-rdct new-rdct)
  (setf *tc* (cmbn 2 3 '(0 1 2))
        *bc* (cmbn 3 4 '(0 1 2 3)))
  (check-rdct))
|#

#|
  (cat-init)
  (setf rdct (make-rdct 6 3))
  (setf perturb (opps (dffr (tcc rdct))))
  (setf new-rdct (add rdct perturb))
  (pre-check-rdct new-rdct)
  (setf *tc* (cmbn 2 3 '(0 1 2))
        *bc* (cmbn 3 4 '(0 1 2 3)))
  (check-rdct))
|#

#|  ;; an absurd reduction ; just to test bpl-*-sigma
  (cat-init)
  (setf tcc (build-chcm
               :cmpr #'l-cmpr
               :basis :locally-effective
               :bsgn '(0)
               :intr-dffr #'(lambda (degr gnrt)
                               (cmbn (1- degr) 1 gnrt))
               :strt :gnrt
               :orgn '(test1)))
  (setf rdct (trivial-rdct tcc))
  (setf (slot-value rdct 'h)
        (build-mrph
           :sorc tcc :trgt tcc :degr +1
           :intr #'(lambda (degr gnrt)
                      (cmbn (1+ degr) 1 gnrt))
           :strt :gnrt
           :orgn '(test2)))
  (setf perturb
        (build-mrph
           :sorc tcc :trgt tcc :degr -1
           :intr #'(lambda (degr gnrt)
                      (if (zerop (first gnrt))
                         (zero-cmbn (1- degr))   
                         (cmbn (1- degr) 1 (list (1- (first gnrt)) (1+ (second gnrt))))))
           :strt :gnrt
           :orgn '(test3)))
  (setf new-rdct (add rdct perturb))
  (gnrt-? (dffr (tcc new-rdct)) 3 '(3 5))
  (gnrt-? (dffr (bcc new-rdct)) 3 '(3 5))
|#

(DEFMETHOD ADD ((hmeq equivalence) (lb-perturbation morphism) &optional dummy)
   (declare (ignore dummy))
   (the equivalence
      (with-slots (lrdct rrdct) hmeq
         (declare (type reduction lrdct rrdct))
         (multiple-value-bind (new-lrdct top-perturbation)
                              (add lrdct lb-perturbation)
            (declare
               (type reduction new-lrdct)
               (type morphism lb-perturbation))
            (build-hmeq
               :lrdct new-lrdct
               :rrdct (add rrdct top-perturbation)
               :orgn `(add ,hmeq ,lb-perturbation))))))
                        
#|
(cat-init)
(setf hmeq (trivial-hmeq (cdelta 4)))
(add (lrdct hmeq) (opps (dffr (cdelta 4))))
(setf hmeq (add hmeq (opps (dffr (cdelta 4)))))
(gnrt-? (dffr (rbcc hmeq)) 3 '(0 1 2 3)))
|#

(DEFMETHOD SBTR ((hmeq equivalence) (lb-perturbation morphism) &optional dummy)
   (declare (ignore dummy))
   (the equivalence
      (with-slots (lrdct rrdct) hmeq
         (declare (type reduction lrdct rrdct))
         (multiple-value-bind (new-lrdct top-perturbation)
                              (sbtr lrdct lb-perturbation)
            (declare
               (type reduction new-lrdct)
               (type morphism lb-perturbation))
            (build-hmeq
               :lrdct new-lrdct
               :rrdct (sbtr rrdct top-perturbation)
               :orgn `(sbtr ,hmeq ,lb-perturbation))))))
                        
#|
(cat-init)
(setf hmeq (trivial-hmeq (cdelta 4)))
(add (lrdct hmeq) (opps (dffr (cdelta 4))))
(setf hmeq (add hmeq (opps (dffr (cdelta 4)))))
(gnrt-? (dffr (rbcc hmeq)) 3 '(0 1 2 3)))
|#

(DEFUN EFHM-RDCT (efhm)
  (declare (type equivalence efhm))
  (the reduction
    (with-slots (lf lg lh rf rg rh) efhm
      (declare (type morphism lf lg lh rf rg rh))
      (let ((f (cmps rf lg))
            (g (cmps lf rg))
            (h (cmps lf (cmps rh lg))))
        (declare (type morphism f g h))
        (build-rdct :f f :g g :h h :orgn `(efhm-rdct ,efhm))))))

#|
(setf efhm (efhm (k-z-1)))
(setf rdct (efhm-rdct efhm))
(setf *tc* (cmbn 3 1 '(1 2 3)))
(setf *bc* (cmbn 1 1 's1))
(pre-check-rdct rdct)
(check-rdct)
|#
  