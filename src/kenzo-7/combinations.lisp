;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS
;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS
;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS

(IN-PACKAGE #:cat)

(provide "combinations")

;;;;
;;;;  COMPARISONS.
;;;;

(DEFUN CMBN-PRINT (cmbn stream depth)
  (declare
   (type cmbn cmbn) (stream stream)
   (ignore depth))
  "--------------------------------------------------------------[function-doc]
CMBN-PRINT
Args: (cmbn stream depth)
Prints the combination CMBN to the stream STREAM. The third argument, DEPTH,
is ignored.
------------------------------------------------------------------------------"
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (format stream "~%~70@{-~}{CMBN ~D}" degr)
                  (do ((mark list (cdr mark))
                       (nth 0 (1+ nth)))
                      ((endp mark))
                    (declare
                     (list mark)
                     (fixnum nth))
                    (when (and *print-length* (= nth *print-length*))
                      (format stream "~%... ...")
                      (return))
                    (format stream "~%<~D * ~A>" (-cffc mark) (-gnrt mark)))
                  (format stream "~%~78@{-~}~%" t)
                  cmbn)))


(DEFUN MAPLEXICO (cmpr list1 list2)
  (declare
   (type cmprf cmpr)
   (list list1 list2))
  (the boolean
       (do ((mark1 list1 (cdr mark1))
            (mark2 list2 (cdr mark2)))
           (nil)
         (declare (list mark1 mark2))
         (unless mark1
           (return-from maplexico
             (if mark2 :less :equal)))
         (unless mark2
           (return-from maplexico :greater))
         (case (funcall cmpr (car mark1) (car mark2))
           (:less (return-from maplexico :less))
           (:greater (return-from maplexico :greater))))))


(DEFUN S-CMPR (symbol1 symbol2)
  (declare (symbol symbol1 symbol2))
  "--------------------------------------------------------------[function-doc]
S-CMPR
Args: (symbol1 symbol2)
Returns :LESS, :EQUAL, or :GREATER, according to the result of the Lisp string
comparison function of the strings (SYMBOL-NAME SYMBOL1) and
(SYMBOL-NAME SYMBOL2).
------------------------------------------------------------------------------"
  (the cmpr
       (if (#+aclpc acl:whole-string<
            #-aclpc string< symbol1 symbol2)
       :less
       (if (#+aclpc acl:whole-string=
            #-aclpc string= symbol1 symbol2)
           :equal
           :greater))))


(DEFUN F-CMPR (n1 n2)
  (declare (fixnum n1 n2))
  "--------------------------------------------------------------[function-doc]
F-CMPR
Args: (n1 n2)
Returns :LESS, :EQUAL, or :GREATER, according to the result of the canonical
comparison of the integers N1 and N2.
------------------------------------------------------------------------------"
  (the cmpr
       (if (< n1 n2)
           :less
           (if (= n1 n2)
               :equal
               :greater))))


(DEFUN L-CMPR (list1 list2)
  (declare (list list1 list2))
  "--------------------------------------------------------------[function-doc]
L-CMPR
Args: (list1 list2)
Returns :LESS, :EQUAL, or :GREATER, according to the lexicographical ordering
of the generator lists LIST1 and LIST2.
------------------------------------------------------------------------------"
  (unless list1
    (return-from l-cmpr (if list2 :less :equal)))
  (unless list2
    (return-from l-cmpr :greater))
  (if (symbolp (first list1))
      (if (symbolp (first list2))
          (lexico
           (s-cmpr (first list1) (first list2))
           (l-cmpr (rest list1) (rest list2)))
          :greater)
      (if (symbolp (first list2))
          :less
          (lexico
           (f-cmpr (first list1) (first list2))
           (l-cmpr (rest list1) (rest list2))))))

;;;;
;;;;  COMBINATIONS
;;;;

#|
(DEFUN CMBN (degr &rest rest)
  (declare (fixnum degr))
  (the cmbn
       (let ((rslt (list degr :cmbn)))
         (do ((mark rest (cddr mark)))
             ((endp mark) (nreverse rslt))
           (declare (list mark))
           (push (term (car mark) (cadr mark)) rslt)))))
|#

(DEFUN CMBN (degr &rest rest)
  (declare (fixnum degr))
  "--------------------------------------------------------------[function-doc]
CMBN
Args: (degr &rest rest)
Returns a combination of degree DEGR from a sum of terms provided as a sequence
CF1 GNRT1 CF2 GNRT2 ... CFn GNRTn of coefficient / generator pairs in REST.
REST can be of arbitrary length including NIL, in which case the combination
is an instance of the null combination of degree DEGR.
------------------------------------------------------------------------------"
  (the cmbn
       (make-cmbn :degr degr
                  :list (do ((rslt +empty-list+
                                   (cons (term (car mark) (cadr mark))
                                         rslt))
                             (mark rest (cddr mark)))
                            ((endp mark) (nreverse rslt))
                          (declare (list rslt mark))))))


(setf *print-pretty* nil)


(DEFUN CHECK-CMBN (chcm cmbn)
  (declare
   (type chain-complex chcm)
   (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
CHECK-CMBN
Args: (chcm cmbn)
Tests if the combination CMBN is a valid combination of chain complex CHCM.
CHECK-CMBN raises an error if the generators of CMBN are out-of-order.
------------------------------------------------------------------------------"
  (the boolean
       (do ((cmpr (cmpr chcm))
            (mark2 (cmbn-list cmbn) mark1)
            (mark1 (rest (cmbn-list cmbn)) (cdr mark1)))
           ((endp mark1) t)
         (declare
          (type cmprf cmpr)
          (list mark1 mark2))
         (unless (eq :less (funcall cmpr (-gnrt mark2) (-gnrt mark1)))
           (error "In CHECK-CMBN, bad order; the generator:~@
                ~A~@
                is not less than the generator:~@
                ~A."
                  (-gnrt mark2) (-gnrt mark1))))))


(DEFUN ZERO-CMBN (degr)
  (declare (fixnum degr))
  "--------------------------------------------------------------[function-doc]
ZERO-CMBN
Args: (degr)
Returns an instance of the null combination of degree DEGR.
------------------------------------------------------------------------------"
  (the cmbn
       (make-cmbn
        :degr degr
        :list +empty-list+)))


(DEFINE-CONSTANT +ZERO-NEGATIVE-CMBN+
    (make-cmbn :degr -1 :list +empty-list+)
  "--------------------------------------------------------------[constant-doc]
+ZERO-NEGATIVE-CMBN+ is bound to an instance of the null combination of
degree -1.
------------------------------------------------------------------------------")


(DEFUN ZERO-INTR-DFFR (cmbn)
  (declare (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
ZERO-INTR-DFFR
Args: (cmbn)
Returns the null combination of degree p-1, for any combination CMBN of
degree p.
------------------------------------------------------------------------------"
  (the cmbn (zero-cmbn (1- (cmbn-degr cmbn)))))


(DEFUN CMBN-OPPS (cmbn)
  (declare (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
CMBN-OPPS
Args: (cmbn)
Returns the combination opposite combination of CMBN.
------------------------------------------------------------------------------"
  (the cmbn
       (with-cmbn (degr list) cmbn
                  (make-cmbn
                   :degr degr
                   :list (mapcar #'(lambda (term)
                                     (declare (type term term))
                                     (with-term
                                         (cffc gnrt) term
                                         (term (- cffc) gnrt)))
                                 list)))))


(DEFUN N-CMBN (n cmbn)
  ;; n is assumed non-zero
  (declare
   (fixnum n)
   (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
N-CMBN
Args: (n cmbn)
Returns N times the combination CMBN. N must be non-zero.
------------------------------------------------------------------------------"
  (the cmbn
       (if (= n 1)
           cmbn
           (with-cmbn (degr list) cmbn
                      (make-cmbn
                       :degr degr
                       :list (mapcar #'(lambda (term)
                                         (declare (type term term))
                                         (with-term
                                             (cffc gnrt) term
                                             (term (* n cffc) gnrt)))
                                     list))))))


(DEFUN 2CMBN-ADD (cmpr cmbn1 cmbn2)
  (declare
   (type cmprf cmpr)
   (type cmbn cmbn1 cmbn2))
  "--------------------------------------------------------------[function-doc]
2CMBN-ADD
Args: (cmpr cmbn1 cmbn2)
Returns the combination, which is the sum of CMBN1 and CMBN2. The first
argument, CMPR, must be a function or macro, which is used to compare the
generators of the combination arguments and to order the terms of the result
combination.
------------------------------------------------------------------------------"
  (the cmbn
       (with-cmbn
           (degr1 list1) cmbn1
           (with-cmbn
               (degr2 list2) cmbn2
               (unless (= degr1 degr2)
                 (error "In ADD, the combination arguments have different degrees ~D and ~D."
                        degr1 degr2))
               (unless list1
                 (return-from 2cmbn-add cmbn2))
               (unless list2
                 (return-from 2cmbn-add cmbn1))
               (let ((pre-rslt +empty-list+)
                     (mark1 list1)
                     (mark2 list2))
                 (declare (list pre-rslt mark1 mark2))
                 (with--term
                     (cffc1 gnrt1) mark1
                     (with--term
                         (cffc2 gnrt2) mark2
                         (tagbody
                          :begin
                            (ecase (funcall cmpr gnrt1 gnrt2)
                              (:equal
                               (let ((cffc (+ cffc1 cffc2)))
                                 (declare (fixnum cffc))
                                 (unless (zerop cffc)
                                   (push (term cffc gnrt1) pre-rslt))
                                 (setf mark1 (cdr mark1)
                                       mark2 (cdr mark2))
                                 (unless mark1 (go :cmbn1-is-finished))
                                 (unless mark2 (go :cmbn2-is-finished))
                                 (setf cffc1 (-cffc mark1)
                                       gnrt1 (-gnrt mark1)
                                       cffc2 (-cffc mark2)
                                       gnrt2 (-gnrt mark2))
                                 (go :begin)))
                              (:less
                               (push (-term mark1) pre-rslt)
                               (setf mark1 (cdr mark1))
                               (unless mark1 (go :cmbn1-is-finished))
                               (setf cffc1 (-cffc mark1)
                                     gnrt1 (-gnrt mark1))
                               (go :begin))
                              (:greater
                               (push (-term mark2) pre-rslt)
                               (setf mark2 (cdr mark2))
                               (unless mark2 (go :cmbn2-is-finished))
                               (setf cffc2 (-cffc mark2)
                                     gnrt2 (-gnrt mark2))
                               (go :begin)))
                          :cmbn1-is-finished
                            (setf pre-rslt (nreconc pre-rslt mark2))
                            (go :exit)
                          :cmbn2-is-finished
                            (setf pre-rslt (nreconc pre-rslt mark1))
                          :exit))
                     (make-cmbn :degr degr1 :list pre-rslt)))))))


(DEFUN 2CMBN-SBTR (cmpr cmbn1 cmbn2)
  (declare
   (type cmprf cmpr)
   (type cmbn cmbn1 cmbn2))
  "--------------------------------------------------------------[function-doc]
2CMBN-SBTR
Args: (cmpr cmbn1 cmbn2)
Returns the combination, which is the difference of CMBN1 and CMBN2. The first
argument, CMPR, must be a function or macro, which is used to compare the
generators of the combination arguments and to order the terms of the result
combination.
------------------------------------------------------------------------------"
  (the cmbn
       (with-cmbn
           (degr1 list1) cmbn1
           (with-cmbn
               (degr2 list2) cmbn2
               (unless (= degr1 degr2)
                 (error "In SBTR, the combination arguments have different degrees ~D and ~D."
                        degr1 degr2))
               (unless list1
                 (return-from 2cmbn-sbtr (cmbn-opps cmbn2)))
               (unless list2
                 (return-from 2cmbn-sbtr cmbn1))
               (let ((pre-rslt +empty-list+)
                     (mark1 list1)
                     (mark2 list2))
                 (declare (list pre-rslt mark1 mark2))
                 (with--term
                     (cffc1 gnrt1) mark1
                     (with--term
                         (cffc2 gnrt2) mark2
                         (tagbody
                          :begin
                            (ecase (funcall cmpr gnrt1 gnrt2)
                              (:equal
                               (let ((cffc (- cffc1 cffc2)))
                                 (declare (fixnum cffc))
                                 (unless (zerop cffc)
                                   (push (term cffc gnrt1) pre-rslt))
                                 (unless mark1 (go :cmbn1-is-finished))
                                 (setf mark1 (cdr mark1)
                                       mark2 (cdr mark2))
                                 (unless mark1 (go :cmbn1-is-finished))
                                 (unless mark2 (go :cmbn2-is-finished))
                                 (setf cffc1 (-cffc mark1)
                                       gnrt1 (-gnrt mark1)
                                       cffc2 (-cffc mark2)
                                       gnrt2 (-gnrt mark2))
                                 (go :begin)))
                              (:less
                               (push (-term mark1) pre-rslt)
                               (setf mark1 (cdr mark1))
                               (unless mark1 (go :cmbn1-is-finished))
                               (setf cffc1 (-cffc mark1)
                                     gnrt1 (-gnrt mark1))
                               (go :begin))
                              (:greater
                               (push (term (- cffc2) gnrt2) pre-rslt)
                               (setf mark2 (cdr mark2))
                               (unless mark2 (go :cmbn2-is-finished))
                               (setf cffc2 (-cffc mark2)
                                     gnrt2 (-gnrt mark2))
                               (go :begin)))
                          :cmbn1-is-finished
                            (setf pre-rslt
                                  (nreconc pre-rslt
                                           (mapcar #'(lambda (term)
                                                       (declare (cons term))
                                                       (with-term
                                                           (cffc gnrt) term
                                                           (term (- cffc)
                                                                 gnrt)))
                                                   mark2)))
                            (go :exit)
                          :cmbn2-is-finished
                            (setf pre-rslt (nreconc pre-rslt mark1))
                          :exit))
                     (make-cmbn
                      :degr degr1
                      :list pre-rslt)))))))


(DEFUN 2N-2CMBN (cmpr n1 cmbn1 n2 cmbn2)
  ;; n1 * cmbn1 + n2 * cmbn2
  ;; combinations may be null but not n1 and n2
  (declare
   (type cmprf cmpr)
   (fixnum n1 n2)
   (type cmbn cmbn1 cmbn2))
  "--------------------------------------------------------------[function-doc]
2N-2CMBN
Args: (cmpr n1 cmbn1 n2 cmbn2)
Returns the combination N1*CMBN1+N2*CMBN2. Both integers N1 and N2 must be
non-zero. The first argument, CMPR, must be a function or macro, which is used
to compare the generators of the input combinations and to order the terms of
the result combination.
------------------------------------------------------------------------------"
  (the cmbn
       (with-cmbn
           (degr1 list1) cmbn1
           (with-cmbn
               (degr2 list2) cmbn2
               (unless (= degr1 degr2)
                 (error "In 2N-2CMBN, the combination arguments have different degrees ~D and ~D."
                        degr1 degr2))
               (unless list1
                 (return-from 2n-2cmbn (n-cmbn n2 cmbn2)))
               (unless list2
                 (return-from 2n-2cmbn (n-cmbn n1 cmbn1)))
               (let ((pre-rslt +empty-list+)
                     (mark1 list1)
                     (mark2 list2))
                 (declare (list pre-rslt mark1 mark2))
                 (with--term
                     (cffc1 gnrt1) mark1
                     (with--term
                         (cffc2 gnrt2) mark2
                         (setf cffc1 (* n1 cffc1)
                               cffc2 (* n2 cffc2))
                         (tagbody
                          :begin
                            (ecase (funcall cmpr gnrt1 gnrt2)
                              (:equal
                               (let ((cffc (+ cffc1 cffc2)))
                                 (declare (fixnum cffc))
                                 (unless (zerop cffc)
                                   (push (term cffc gnrt1) pre-rslt))
                                 (setf mark1 (cdr mark1)
                                       mark2 (cdr mark2))
                                 (unless mark1 (go :cmbn1-is-finished))
                                 (unless mark2 (go :cmbn2-is-finished))
                                 (setf cffc1 (* n1 (-cffc mark1))
                                       gnrt1 (-gnrt mark1)
                                       cffc2 (* n2 (-cffc mark2))
                                       gnrt2 (-gnrt mark2))
                                 (go :begin)))
                              (:less
                               (push (term cffc1 gnrt1) pre-rslt)
                               (setf mark1 (cdr mark1))
                               (unless mark1 (go :cmbn1-is-finished))
                               (setf cffc1 (* n1 (-cffc mark1))
                                     gnrt1 (-gnrt mark1))
                               (go :begin))
                              (:greater
                               (push (term cffc2 gnrt2) pre-rslt)
                               (setf mark2 (cdr mark2))
                               (unless mark2 (go :cmbn2-is-finished))
                               (setf cffc2 (* n2 (-cffc mark2))
                                     gnrt2 (-gnrt mark2))
                               (go :begin)))
                          :cmbn1-is-finished
                            (setf pre-rslt
                                  (nreconc pre-rslt
                                           (if (= 1 n2)
                                               mark2
                                               (mapcar #'(lambda (term)
                                                           (declare (cons term))
                                                           (with-term
                                                               (cffc gnrt) term
                                                               (term (* n2 cffc)
                                                                     gnrt)))
                                                       mark2))))
                            (go :exit)
                          :cmbn2-is-finished
                            (setf pre-rslt
                                  (nreconc pre-rslt
                                           (if (= 1 n1)
                                               mark1
                                               (mapcar #'(lambda (term)
                                                           (declare (cons term))
                                                           (with-term
                                                               (cffc gnrt) term
                                                               (term (* n1 cffc)
                                                                     gnrt)))
                                                       mark1))))
                          :exit))
                     (make-cmbn
                      :degr degr1
                      :list pre-rslt)))))))


(DEFUN CMBN-CMBN (cmpr n-cmbn-list)
  ;; n-cmbn-list = list of (cons n cmbn)
  ;;               non-empty-list
  ;;            every n is non-null
  (declare
   (type cmprf cmpr)
   (list n-cmbn-list))
  "--------------------------------------------------------------[function-doc]
CMBN-CMBN
Args: (cmpr n-cmbn-list)
Returns the linear combination of a list of integer/combination pairs. The
first argument, CMPR, must be a function or macro, which is used to compare the
generators of the input combinations and to order the terms of the result
combination. The second argument, N-CMBN-LIST, is a list dotted pairs (CONSes),
where the first element is a non-zero integer and the second element is a
combination.
------------------------------------------------------------------------------"
  (unless (rest n-cmbn-list)
    (return-from cmbn-cmbn
      (n-cmbn (caar n-cmbn-list) (cdar n-cmbn-list))))
  (unless (cddr n-cmbn-list)
    (return-from cmbn-cmbn
      (2n-2cmbn cmpr
                (caar n-cmbn-list) (cdar n-cmbn-list)
                (caadr n-cmbn-list) (cdadr n-cmbn-list))))
  (the cmbn
       (let ((new-rslt +empty-list+))
         (declare (list new-rslt))
         (do ((mark1 (rest n-cmbn-list) (cddr mark1))
              (mark2 n-cmbn-list (cdr mark1)))
             ((endp mark2))
           (declare (list mark1 mark2))
           (when (endp mark1)
             (push (n-cmbn (caar mark2) (cdar mark2)) new-rslt)
             (return))
           (push (2n-2cmbn cmpr
                           (caar mark2) (cdar mark2)
                           (caar mark1) (cdar mark1))
                 new-rslt))
         (let ((old-rslt +empty-list+))
           (declare (list old-rslt))
           (loop
              (unless (rest new-rslt)
                (return-from cmbn-cmbn (first new-rslt)))
              (setf old-rslt new-rslt
                    new-rslt +empty-list+)
              (do ((mark1 (rest old-rslt) (cddr mark1))
                   (mark2 old-rslt (cdr mark1)))
                  ((endp mark2))
                (declare (list mark1 mark2))
                (when (endp mark1)
                  (push (car mark2) new-rslt)
                  (return))
                (push (2cmbn-add cmpr (car mark2) (car mark1))
                      new-rslt)))))))


(DEFUN NTERM-ADD (cmpr degr &rest rest)
  (declare
   (type cmprf cmpr)
   (fixnum degr)
   (list rest))
  ;; (list term)
  "--------------------------------------------------------------[function-doc]
NTERM-ADD
Args: (cmpr degr &rest rest)
Returns a combination of degree DEGR which is the sum of terms supplied in
REST. The first argument, CMPR, must be a function or macro, which is used to
compare the generators of the input terms and to order the terms of the result
combination. If REST is NIL, an instance of the null combination of degree DEGR
is returned.
------------------------------------------------------------------------------"
  (unless rest
    (return-from nterm-add (zero-cmbn degr)))
  (the cmbn
       (let ((rslt (list (first rest))))
         (declare (list rslt))
         (dolist (term (rest rest))
           (declare (type term term))
           (with-term  (cffc gnrt) term
                       (do ((mark1 rslt (cdr mark1))
                            (mark2 nil mark1))
                           ((endp mark1) (setf (cdr mark2) (list term)))
                         (ecase (funcall cmpr gnrt (-gnrt mark1))
                           (:less
                            (if mark2
                                (push term (cdr mark2))
                                (push term rslt))
                            (return))
                           (:equal
                            (incf (-cffc mark1) cffc)
                            (return))
                           (:greater )))))
         (make-cmbn
          :degr degr
          :list (delete 0 rslt :key #'car)))))


(DEFUN NCMBN-ADD (cmpr cmbn &rest rest)
  (declare
   (type cmprf cmpr)
   (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
NCMBN-ADD
Args: (cmpr cmbn &rest rest)
Returns the sum of an arbitrary number of combinations. The first argument,
CMPR, must be a function or macro, which is used to compare the generators of
the input combinations and to order the terms of the result combination.
------------------------------------------------------------------------------"
  (the cmbn
       (cmbn-cmbn cmpr (mapcar #'(lambda (cmbn)
                                   (declare (type cmbn cmbn))
                                   (cons 1 cmbn))
                               (cons cmbn rest)))))


(DEFUN DSTR-ADD-TERM-TO-CMBN (cmpr cffc gnrt cmbn)
  (declare
   (type cmprf cmpr)
   (fixnum cffc)  ;; non-zero
   (type gnrt gnrt)
   (type cmbn cmbn))
  "--------------------------------------------------------------[function-doc]
DSTR-ADD-TERM-TO-CMBN
Args: (cmpr cffc gnrt cmbn)
Returns a combination, which is the result of adding the term CFFC*GNRT to the
combination CMBN. The first argument, CMPR, must be a function or macro, which
is used to compare the generators of the inputs and to order the terms of the
result combination.
------------------------------------------------------------------------------"
  (the cmbn
       (let ((list (cmbn-list cmbn)))
         (declare (list list))
         (unless list
           (push (term cffc gnrt) (cmbn-list cmbn))
           (return-from dstr-add-term-to-cmbn cmbn))
         (do ((mark2 nil mark1)
              (mark1 list (cdr mark1)))
             ((endp mark1) (setf (cdr mark2) (list (term cffc gnrt))))
           (declare (list mark1 mark2))
           (ecase (funcall cmpr gnrt (-gnrt mark1))
             (:less
              (if mark2
                  (push (term cffc gnrt) (cdr mark2))
                  (push (term cffc gnrt) (cmbn-list cmbn)))
              (return))
             (:equal
              (let ((new-cffc (+ cffc (-cffc mark1))))
                (declare (fixnum new-cffc))
                (if (zerop new-cffc)
                    (if mark2
                        (pop (cdr mark2))
                        (pop (cmbn-list cmbn)))
                    (setf (-cffc mark1) new-cffc))
                (return)))
             (:greater)))
         cmbn)))
