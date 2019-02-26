;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY
;;;  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY  EFFECTIVE-HOMOLOGY

(IN-PACKAGE #:cat-7)

(PROVIDE "effective-homology")

;;;
;;;  REDUCTIONS
;;;


#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((rdct reduction) stream)
  (the reduction
       (progn
         (format stream "[K~D Reduction K~D => K~D]"
                 (idnm rdct) (idnm (tcc rdct)) (idnm (bcc rdct)))
         rdct)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))


(DEFUN RDCT (n)
  (declare (fixnum n))
  "--------------------------------------------------------------[function-doc]
RDCT
Args: (n)
Returns the N-th user-created reduction from the list *RDCT-LIST*. Returns NIL
if it doesn't exist.
------------------------------------------------------------------------------"
  (the (or null reduction)
       (find n *rdct-list* :key #'idnm)))


(DEFUN BUILD-RDCT (&key f g h orgn)
  (declare
   (type morphism f g h)
   (list orgn))
  "--------------------------------------------------------------[function-doc]
BUILD-RDCT
Args: (&key f g h orgn)
Returns an instance of the class REDUCTION. The keyword arguments are as
follows:

:F F, an object of type MORPHISM representing the chain morphism f of a
            reduction

:G G, an object of type MORPHISM representing the chain morphism g of a
            reduction

:H H, an object of type MORPHISM representing the morphism of graded modules h
            of a reduction

:ORGN ORGN, a list which is the copy of a Lisp statement, in principle the
            statement which was at the origin of the creation of this Kenzo
            object. A caching process, using this slot, prevents the creation
            of multiple copies of the same mathematical object, which is
            important for efficiency. It is used also for debugging and
            analyzing the program.

Use this function instead of creating instances via the standard constructor
MAKE-INSTANCE.
------------------------------------------------------------------------------"
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
            (fixnum fdegr))
           (with-slots ((gsorc sorc) (gtrgt trgt) (gdegr degr)) g
             (declare
              (type chain-complex gsorc gtrgt)
              (fixnum gdegr))
             (with-slots ((hsorc sorc) (htrgt trgt) (hdegr degr)) h
               (declare
                (type chain-complex hsorc htrgt)
                (fixnum hdegr))
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
  "--------------------------------------------------------------[function-doc]
TRIVIAL-RDCT
Args: (chcm)
Builds the trivial reduction according to the following diagram, which
involves the chain complex CHCM only:

          Zero    s
     C  ------->   C
      ^
    | |
 Id | | Id
    | |
    v
     C

Zero is the zero morphism of degree 1 of C and Id is its identity morphism.

See also: ZERO-MRPH, IDNT-MRPH.
------------------------------------------------------------------------------"
  (build-rdct
   :f (idnt-mrph chcm)
   :g (idnt-mrph chcm)
   :h (zero-mrph chcm chcm +1)
   :orgn `(trivial-rdct ,chcm)))


;;;
;;;  HOMOTOPY-EQUIVALENCES
;;;

;;(defgeneric build-hmeq (keyword lrdct &key &allow-other-keys)
(DEFGENERIC BUILD-HMEQ (keyword lrdct &key))


(DEFMETHOD BUILD-HMEQ ((keyword1 (eql :lrdct)) lrdct &key rrdct orgn)
  (declare
   (type reduction lrdct rrdct)
   (list orgn))
  (the homotopy-equivalence
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
               (declare (type (or homotopy-equivalence null) already))
               (when already
                 (return-from build-hmeq already)))
             (let ((hmeq (make-instance 'homotopy-equivalence
                                        :lbcc lbcc :tcc ltcc :rbcc rbcc
                                        :lf lf :lg lg :lh lh
                                        :rf rf :rg rg :rh rh
                                        :lrdct lrdct :rrdct rrdct
                                        :orgn orgn)))
               (declare (type homotopy-equivalence hmeq))
               (push hmeq *hmeq-list*)
               hmeq))))))


#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((hmeq homotopy-equivalence) stream)
  (the homotopy-equivalence
       (progn
         (format stream "[K~D Homotopy-Equivalence K~D <= K~D => K~D]"
                 (idnm hmeq)
                 (idnm (lbcc hmeq)) (idnm (tcc hmeq)) (idnm (rbcc hmeq)))
         hmeq)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFGENERIC HMEQ (integer))

(DEFMETHOD HMEQ ((n integer))
  (declare (fixnum n))
  (the (or homotopy-equivalence null)
       (find n *hmeq-list* :key #'idnm)))

(DEFMETHOD BUILD-HMEQ ((keyword1 (eql :lf)) lf &key lg lh rf rg rh orgn)
  (declare
   (type morphism lf lg lh rf rg rh)
   (list orgn))
  (the homotopy-equivalence
       (progn
         (unless orgn
           (setf orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh)))
         (let ((already (find orgn *hmeq-list* :test #'equal :key #'orgn)))
           (declare (type (or null homotopy-equivalence) already))
           (when already
             (return-from build-hmeq already)))
         (build-hmeq
          :lrdct (build-rdct :f lf :g lg :h lh
                             :orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh lrdct))
          :rrdct (build-rdct :f rf :g rg :h rh
                             :orgn `(build-hmeq ,lf ,lg ,lh ,rf ,rg ,rh rrdct))
          :orgn orgn))))

(DEFUN TRIVIAL-HMEQ (chcm)
  (declare (type chain-complex chcm))
  (build-hmeq
   :lrdct (trivial-rdct chcm)
   :rrdct (trivial-rdct chcm)
   :orgn `(trivial-hmeq ,chcm)))

;; Functions.

(DEFUN PRE-CHECK-RDCT (rdct)
  "--------------------------------------------------------------[function-doc]
PRE-CHECK-RDCT
Args: (rdct)
Assigns the following Lisp global variables, using morphisms from RDCT and the
differentials and identity morphisms of the underlying chain complexes,
according to these formulas:

*TDD*         = d  ° d
                 ^    ^
                 C    C

*BDD*         = d  ° d
                 C    C

*ID-FG*       = Id  - f ° g
                  C

*ID-GF-DH-HD* = Id  - g ° f - (d ° h + h ° d )
                  ^             ^           ^
                  C             C           C

*HH*          = h ° h
*FH*          = f ° h
*HG*          = h ° g

*DF-FD*       = d  ° f - f ° d
                 C            ^
                              C

*DG-GD*       = d  ° g - g ° d
                 ^            C
                 C

------------------------------------------------------------------------------"
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


(DEFUN CHECK-RDCT ()
  "--------------------------------------------------------------[function-doc]
CHECK-RDCT
Args: ()
Maps combinations of the top and bottom chain complexes of a reduction using
the morphisms created by PRE-CHECK-RDCT. Having no parameters, the
combinations must be provided via the Lisp global variables *TC* and *BC*,
for the top and bottom chain complex, respectively. If the morphisms are
coherent, the result of each mapping is a null combination.

Note: This function is intended for interactive use. It pauses after each
evaluation for the user to inspect the result. Resume execution, by pressing
the <Enter> key.
------------------------------------------------------------------------------"
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


(DEFUN CHECK-RDCT-NO-WAIT ()
  "--------------------------------------------------------------[function-doc]
CHECK-RDCT-NO-WAIT
Args: ()
Maps combinations of the top and bottom chain complexes of a reduction using
the morphisms created by PRE-CHECK-RDCT. Having no parameters, the
combinations must be provided via the Lisp global variables *TC* and *BC*,
for the top and bottom chain complex, respectively. If the morphisms are
coherent, the result of each mapping is a null combination.
------------------------------------------------------------------------------"
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
                       *bc* *tc*))))
  (done))


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

;;
;; BASIC PERTURBATION LEMMA.
;;

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
           (setf new-f (dstr-change-sorc-trgt new-f :new-sorc new-tcc
                                              :new-trgt new-bcc)
                 new-g (dstr-change-sorc-trgt new-g :new-sorc new-bcc
                                              :new-trgt new-tcc)
                 new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc
                                              :new-trgt new-tcc))
           (values
            (build-rdct :f new-f :g new-g :h new-h
                        :orgn `(basic-perturbation-lemma ,reduction
                                                         ,top-perturbation))
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
               :f (dstr-change-sorc-trgt old-f :new-sorc new-tcc
                                         :new-trgt new-bcc)
               :g (dstr-change-sorc-trgt old-g :new-sorc new-bcc
                                         :new-trgt new-tcc)
               :h (dstr-change-sorc-trgt old-h :new-sorc new-tcc
                                         :new-trgt new-tcc)
               :orgn `(easy-perturbation-lemma ,reduction
                                               ,bottom-perturbation))
              top-perturbation))))))

(DEFUN SPECIAL-BPL (reduction top-perturbation)
  (declare
   (type reduction reduction)
   (type morphism top-perturbation))
  (when (eq (first (orgn top-perturbation)) 'zero-mrph)
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
                 new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc
                                              :new-trgt new-tcc))
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
                 new-h (dstr-change-sorc-trgt new-h :new-sorc new-tcc
                                              :new-trgt new-tcc))
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
                 (fixnum degr)
                 (type gnrt gnrt))
                (do ((rslt (zero-cmbn degr) (2cmbn-add cmpr rslt iterated))
                     (iterated (term-cmbn degr 1 gnrt)
                               (cmbn-opps (cmbn-? h-delta iterated))))
                    ((cmbn-zero-p iterated) rslt)
                  (declare (type cmbn rslt iterated)))))
           (build-mrph
            :sorc (sorc homotopy) :trgt (sorc homotopy) :degr 0
            :intr #'sigma-*
            :strt :gnrt
            :orgn `(bpl-*-sigma ,homotopy ,perturbation))))))

(DEFMETHOD ADD ((hmeq homotopy-equivalence) (lb-perturbation morphism)
                &optional dummy)
  (declare (ignore dummy))
  (the homotopy-equivalence
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
