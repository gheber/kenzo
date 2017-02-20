;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY

(IN-PACKAGE "CAT")

(PROVIDE "effective-homology")

;;;
;;;  REDUCTIONS
;;;

(DEFMETHOD PRINT-OBJECT ((rdct reduction) (stream stream))
 (the reduction
   (progn
      (format stream "[K~D Reduction K~D => K~D]"
	      (idnm rdct) (idnm (tcc rdct)) (idnm (bcc rdct)))
      rdct)))

(DEFUN REDUCTION (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already reduction dfnt)
  (the reduction
    (apply #'make-instance 'reduction rest)))

(DEFMETHOD SHARED-INITIALIZE
    ((rdct reduction) slot-names &key f g h)
  (declare
   (type (or (eql t) list) slot-names)
   (type morphism f g h))
  (call-next-method)
  (when-slot f
             (with-slots (tcc bcc (fs f) (gs g) (hs h) dfnt) rdct
               (declare
                (type morphism fs gs hs)
                (type list dfnt))
               (when f
                 (setf fs f tcc (sorc f) bcc (trgt f)))
               (when g
                 (setf gs g tcc (trgt g) bcc (sorc g)))
               (when h
                 (setf hs h tcc (sorc h)))
               (when (and f g)
                 (unless (eq (grmd (sorc f)) (grmd (trgt g)))
                   (error "In REDUCTION ~S, (sorc f) <> (trgt g)." rdct))
                 (unless (eq (grmd (trgt f)) (grmd (sorc g)))
                   (error "In REDUCTION ~S, (trgt f) <> (sorc g)." rdct)))
               (when h
                 (unless (eq (sorc h) (trgt h))
                   (error "In REDUCTION ~S, (sorc h) <> (trgt h)." rdct)))
               (when (and f h)
                 (unless (eq (grmd (sorc f)) (grmd (sorc h)))
                   (error "In REDUCTION ~S, (sorc f) <> (sorc h)." rdct)))
               (when (and g h)
                 (unless (eq (grmd (trgt g)) (grmd (sorc h)))
                   (error "In REDUCTION ~S, (trgt g) <> (sorc h)." rdct)))
               (unless dfnt
                 (setf dfnt `(reduction ,f ,g ,h)))))
  rdct)

(DEFUN TRIVIAL-RDCT (chcm)
  (declare (type chain-complex chcm))
   (reduction
    :f (idnt-mrph chcm)
    :g (idnt-mrph chcm)
    :h (zero-mrph chcm chcm +1) 
    :dfnt `(trivial-rdct ,chcm)))

;;;
;;;  EQUIVALENCES
;;;

(DEFUN EQUIVALENCE (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already equivalence dfnt)
  (the equivalence
    (apply #'make-instance 'equivalence rest)))

(DEFMETHOD SHARED-INITIALIZE ((eqvl equivalence) slot-names &key lrdct rrdct)
  (declare
   (type (or (eql t) list) slot-names)
   (type reduction lrdct rrdct))
  (call-next-method)
  (when-slot lrdct
             (with-slots ((lrdcts lrdct) (rrdcts rrdct)
                          lbcc tcc rbcc
                          lf lg lh rf rg rh
                          dfnt)
                 eqvl
               (declare
                (type reduction lrdcts rrdcts)
                (type chain-complex lbcc tcc rbcc)
                (type morphism lf lg lh rf rg rh)
                (type list dfnt))
               (when lrdct
                 (setf lrdcts lrdct
                   lf (f lrdct) lg (g lrdct) lh (h lrdct)
                   lbcc (bcc lrdct) tcc (tcc lrdct)))
               (when rrdct
                 (setf rrdcts rrdct
                   rf (f rrdct) rg (g rrdct) rh (h rrdct)
                   tcc (tcc rrdct) rbcc (bcc rrdct)))
               (when (and lrdct rrdct)
                 (unless (eq (grmd (tcc lrdct)) (grmd (tcc rrdct)))
                   (error "In EQUIVALENCE ~S, (tcc lrdct) <> (tcc rrdct)." eqvl)))
               (unless dfnt
                 (setf dfnt `(equivalence ,lrdct ,rrdct)))))
  eqvl)

#|
  (cat-init)
  (progn ;;;; reused in another test
   (defun cdelta (dmns)
     (chain-complex
        :cmpr #'l-cmpr
        :basis :locally-effective
        :bsgn '(0)
        :dffr-intr #'(lambda (degr gmsm)
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
        :dffr-strt :gnrt
        :dfnt `(locally effective version of C_* delta ,dmns)))
   (defun make-f (tdmns bdmns)
     (morphism
        :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
        :intr #'(lambda (degr gmsm)
                   (let ((pos (position-if #'(lambda (vertex) (>= vertex bdmns)) gmsm)))
                      (if pos
                         (if (< pos degr)
                            (zero-cmbn degr)
                            (cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                         (cmbn degr 1 gmsm))))
        :strt :gnrt
        :dfnt `(projection delta ,tdmns => delta ,bdmns)))
   (defun make-g (tdmns bdmns)
     (morphism
        :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
        :intr #'identity
        :strt :cmbn
        :dfnt `(injection delta ,bdmns => delta ,tdmns)))
   (defun make-h (tdmns bdmns)
     (morphism
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
        :dfnt `(homotopy for delta ,tdmns => ,bdmns)))
   (defun make-rdct (tdmns bdmns)
       (setf rdct (reduction
                    :f (make-f tdmns bdmns)
                    :g (make-g tdmns bdmns)
                    :h (make-h tdmns bdmns)
                    :dfnt `(reduction delta ,tdmns ,bdmns)))))
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

(DEFMETHOD PRINT-OBJECT ((eqvl equivalence) (stream stream))
 (the equivalence
   (progn
      (format stream "[K~D Equivalence K~D <= K~D => K~D]"
	      (idnm eqvl)
	      (idnm (lbcc eqvl)) (idnm (tcc eqvl)) (idnm (rbcc eqvl)))
      eqvl)))

(DEFUN TRIVIAL-EQVL (chcm)
   (declare (type chain-complex chcm))
   (equivalence
    :lrdct (trivial-rdct chcm)
    :rrdct (trivial-rdct chcm)
    :dfnt `(trivial-eqvl ,chcm)))

#|
  (cat-init)
  (setf cc (cdelta 5))
  (setf eqvl (trivial-eqvl cc))
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

(DEFMETHOD CMPS ((brdct reduction) (trdct reduction) &optional dummy)
  (declare (ignore dummy))
  (when (eq (first (dfnt brdct)) 'trivial-rdct)
     (return-from cmps trdct))
  (when (eq (first (dfnt trdct)) 'trivial-rdct)
     (return-from cmps brdct))
  (with-slots ((tf f) (tg g) (th h)) trdct
    (declare (type morphism tf tg th))
  (with-slots ((bf f) (bg g) (bh h)) brdct
    (declare (type morphism bf bg bh))
    (reduction
       :f (cmps bf tf)
       :g (cmps tg bg)
       :h (add th (i-cmps tg bh tf))
       :dfnt `(cmps ,brdct ,trdct)))))

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
         (when (eq (first (dfnt top-perturbation)) 'zero-mrph)
            (return-from basic-perturbation-lemma
               (values reduction (zero-mrph old-bcc))))
         (when (eq (first (dfnt (h reduction))) 'zero-mrph)
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
               (reduction :f new-f :g new-g :h new-h
                  :dfnt `(basic-perturbation-lemma ,reduction ,top-perturbation))
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
         (when (eq 'zero-mrph (first (dfnt bottom-perturbation)))
            (return-from easy-perturbation-lemma
               (values reduction (zero-mrph old-tcc))))
	 (when (eq 'trivial-rdct (first (dfnt reduction)))
	    (return-from easy-perturbation-lemma
	       (trivial-rdct (add (bcc reduction) bottom-perturbation))))
         (let ((top-perturbation (i-cmps old-g bottom-perturbation old-f)))
            (declare (type morphism top-perturbation))
            (let ((new-bcc (add old-bcc bottom-perturbation))
                  (new-tcc (add old-tcc top-perturbation)))
               (declare (type chain-complex new-bcc new-tcc))
               (values
                  (reduction
                     :f (dstr-change-sorc-trgt old-f :new-sorc new-tcc :new-trgt new-bcc)
                     :g (dstr-change-sorc-trgt old-g :new-sorc new-bcc :new-trgt new-tcc)
                     :h (dstr-change-sorc-trgt old-h :new-sorc new-tcc :new-trgt new-tcc)
                     :dfnt `(easy-perturbation-lemma ,reduction ,bottom-perturbation))
                  top-perturbation))))))

(DEFUN SPECIAL-BPL (reduction top-perturbation)
   (declare
      (type reduction reduction)
      (type morphism top-perturbation))
   (when (eq (first (dfnt top-perturbation)) 'zero-mrph)
      (return-from special-bpl reduction))
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
            (reduction :f new-f :g old-g :h new-h
               :dfnt `(special-bpl ,reduction ,top-perturbation))))))

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
         (when (eq (first (dfnt top-perturbation)) 'zero-mrph)
            (return-from special-bpl-2
               (values reduction (zero-mrph old-bcc))))
         (when (eq (first (dfnt (h reduction))) 'zero-mrph)
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
               (reduction :f old-f :g new-g :h new-h
                  :dfnt `(special-bpl-2 ,reduction ,top-perturbation)))))))

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
                   (fixnum degr)
                   (type gnrt gnrt))
                (do ((rslt (zero-cmbn degr) (2cmbn-add cmpr rslt iterated))
                     (iterated (term-cmbn degr 1 gnrt) (cmbn-opps (cmbn-? h-delta iterated))))
                    ((cmbn-zero-p iterated) rslt)
                   (declare (type cmbn rslt iterated)))))
            (morphism
               :sorc (sorc homotopy) :trgt (sorc homotopy) :degr 0
               :intr #'sigma-*
               :strt :gnrt
               :dfnt `(bpl-*-sigma ,homotopy ,perturbation))))))
     
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
  (setf tcc (chain-complex
               :cmpr #'l-cmpr
               :basis :locally-effective
               :bsgn '(0)
               :dffr-intr #'(lambda (degr gnrt)
                               (cmbn (1- degr) 1 gnrt))
               :dffr-strt :gnrt
               :dfnt '(test1)))
  (setf rdct (trivial-rdct tcc))
  (setf (slot-value rdct 'h)
        (morphism
           :sorc tcc :trgt tcc :degr +1
           :intr #'(lambda (degr gnrt)
                      (cmbn (1+ degr) 1 gnrt))
           :strt :gnrt
           :dfnt '(test2)))
  (setf perturb
        (morphism
           :sorc tcc :trgt tcc :degr -1
           :intr #'(lambda (degr gnrt)
                      (if (zerop (first gnrt))
                         (zero-cmbn (1- degr))   
                         (cmbn (1- degr) 1 (list (1- (first gnrt)) (1+ (second gnrt))))))
           :strt :gnrt
           :dfnt '(test3)))
  (setf new-rdct (add rdct perturb))
  (gnrt-? (dffr (tcc new-rdct)) 3 '(3 5))
  (gnrt-? (dffr (bcc new-rdct)) 3 '(3 5))
|#

(DEFMETHOD ADD ((eqvl equivalence) (lb-perturbation morphism) &optional dummy)
  (declare (ignore dummy))
  (the equivalence
    (with-slots (lrdct rrdct) eqvl
      (declare (type reduction lrdct rrdct))
      (multiple-value-bind (new-lrdct top-perturbation)
          (add lrdct lb-perturbation)
        (declare
         (type reduction new-lrdct)
         (type morphism lb-perturbation))
        (equivalence
         :lrdct new-lrdct
         :rrdct (add rrdct top-perturbation)
         :dfnt `(add ,eqvl ,lb-perturbation))))))

#|
(cat-init)
(setf eqvl (trivial-eqvl (cdelta 4)))
(add (lrdct eqvl) (opps (dffr (cdelta 4))))
(setf eqvl (add eqvl (opps (dffr (cdelta 4)))))
(gnrt-? (dffr (rbcc eqvl)) 3 '(0 1 2 3)))
|#
