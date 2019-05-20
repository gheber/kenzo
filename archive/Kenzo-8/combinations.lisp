;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS
;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS
;;;  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS  COMBINATIONS

(IN-PACKAGE "COMMON-LISP-USER")

(DEFMETHOD PRINT-OBJECT ((cmbn cmbn) stream)
  (declare (type stream stream))
  (the cmbn
    (with-cmbn (degr list) cmbn
      (format stream "~%~70@{-~}{CMBN ~D}" degr)
      (do ((mark list (cdr mark))
           (nth 0 (1+ nth)))
          ((endp mark))
        (declare (list mark) (type fixnum nth))
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

#|
(maplexico #'f-cmpr '(1 2 3) '(1 3))
(maplexico #'f-cmpr '(1 2 3) '(1 1))
(maplexico #'f-cmpr '(1 1) '(1 1 0))
(maplexico #'f-cmpr '(1 1 0) '(1 1))
(maplexico #'f-cmpr '(1 1) '(1 1))
(maplexico #'f-cmpr nil nil)
|#

(DEFUN S-CMPR (symbol1 symbol2)
   (declare (symbol symbol1 symbol2))
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
   (the cmpr
      (if (< n1 n2)
         :less
         (if (= n1 n2)
            :equal
            :greater))))

(DEFUN L-CMPR (list1 list2)
  (declare (list list1 list2))
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

#|
  (s-cmpr 'a 'b)
  (s-cmpr 'b 'b)
  (s-cmpr 'c 'b)
  (f-cmpr 1 2)
  (f-cmpr 2 2)
  (f-cmpr 3 2)
  (l-cmpr nil nil)
  (l-cmpr nil '(1))
  (l-cmpr '(1) nil)
  (l-cmpr '(a) '(a))
  (l-cmpr '(a) '(1))
  (l-cmpr '(1) '(a))
  (l-cmpr '(1) '(1))
  (l-cmpr '(1 a) '(1 1))
  (l-cmpr '(1 a) '(1 a b)))
|# 


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
   (the cmbn
     (make-cmbn :degr degr
                :list (do ((rslt +empty-list+
                                 (cons (term (car mark) (cadr mark)) rslt))
                           (mark rest (cddr mark)))
                          ((endp mark) (nreverse rslt))
                        (declare (list rslt mark))))))


#|
  (cmbn 2)
  (cmbn 2 2 'a)
  (cmbn 2 'a)
  (cmbn 2 2 'a -3 'b))
  (term-cmbn 3 -5 'a)
|#

#+allegro
(setf *print-pretty* nil)


(DEFUN CHECK-CMBN (chcm cmbn)
  (declare
    (type chain-complex chcm)
    (type cmbn cmbn))
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
   (the cmbn
      (make-cmbn
       :degr degr
       :list +empty-list+)))

#|
  (zero-cmbn 3))
|#  

(DEFUN ZERO-INTR-DFFR (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn (zero-cmbn (1- (cmbn-degr cmbn)))))

#|
  (zero-intr-dffr (cmbn 2))
|#

(DEFINE-CONSTANT +ZERO-NEGATIVE-CMBN+
   (make-cmbn :degr -1 :list +empty-list+))

#|
  (cmbn-non-zero-p (cmbn 0))
  (cmbn-non-zero-p (cmbn 0 1 'a)))
  (cmbn-zero-p (cmbn 0))
  (cmbn-zero-p (cmbn 0 1 'a)))
|#

(DEFUN CMBN-OPPS (cmbn)
   (declare (type cmbn cmbn))
   (the cmbn
      (with-cmbn (degr list) cmbn
         (make-cmbn
            :degr degr
            :list (mapcar #'(lambda (term)
                               (declare (type term term))
                               (with-term (cffc gnrt) term
                                  (term (- cffc) gnrt)))
                     list)))))

#|
  (setf c (cmbn 0 1 'a -2 'b))
  (cmbn-opps c)
  (symbol-value 'c))
|#

(DEFUN N-CMBN (n cmbn)
   ;; n is assumed non-zero
   (declare
      (fixnum n)
      (type cmbn cmbn))
   (the cmbn
      (if (= n 1)
         cmbn
         (with-cmbn (degr list) cmbn
            (make-cmbn
               :degr degr
               :list (mapcar #'(lambda (term)
                                  (declare (type term term))
                                  (with-term (cffc gnrt) term
                                     (term (* n cffc) gnrt)))
                        list))))))

#|
  (setf c (cmbn 2 3 'a))
  (setf c2 (n-cmbn 1 c))
  (eq c c2)
  (n-cmbn -1 c)
  (n-cmbn -3 c))
|#

(DEFUN 2CMBN-ADD (cmpr cmbn1 cmbn2)
   (declare
      (type cmprf cmpr)
      (type cmbn cmbn1 cmbn2))
   (the cmbn
      (with-cmbn (degr1 list1) cmbn1
      (with-cmbn (degr2 list2) cmbn2
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
            (with--term (cffc1 gnrt1) mark1
            (with--term (cffc2 gnrt2) mark2
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

#|
  (2cmbn-add #'s-cmpr (cmbn 0) (cmbn 1))
  (2cmbn-add #'s-cmpr (cmbn 0) (cmbn 0))
  (2cmbn-add #'s-cmpr (cmbn 0 1 'a) (cmbn 0))
  (2cmbn-add #'s-cmpr (cmbn 0 1 'a) (cmbn 0 2 'a))
  (2cmbn-add #'s-cmpr (cmbn 0 1 'a) (cmbn 0 -1 'a))
  (2cmbn-add #'s-cmpr (cmbn 0 1 'a) (cmbn 0 2 'b))
  (2cmbn-add #'s-cmpr (cmbn 0 2 'b) (cmbn 0 1 'a))
  (2cmbn-add #'s-cmpr (cmbn 0 1 'a -2 'b) (cmbn 0 2 'b))
  (2cmbn-add #'s-cmpr (cmbn 0 2 'b) (cmbn 0 1 'a 3 'c)))
|#

(DEFUN 2CMBN-SBTR (cmpr cmbn1 cmbn2)
   (declare
      (type cmprf cmpr)
      (type cmbn cmbn1 cmbn2))
   (the cmbn
      (with-cmbn (degr1 list1) cmbn1
      (with-cmbn (degr2 list2) cmbn2
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
            (with--term (cffc1 gnrt1) mark1
            (with--term (cffc2 gnrt2) mark2
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
                                        (with-term (cffc gnrt) term
                                           (term (- cffc) gnrt)))
                              mark2)))
                  (go :exit)
                  :cmbn2-is-finished
                  (setf pre-rslt (nreconc pre-rslt mark1))
                  :exit))
            (make-cmbn
               :degr degr1
               :list pre-rslt)))))))

#|
  (2cmbn-sbtr #'s-cmpr (cmbn 0) (cmbn 1))
  (2cmbn-sbtr #'s-cmpr (cmbn 0) (cmbn 0))
  (2cmbn-sbtr #'s-cmpr (cmbn 0) (cmbn 0 1 'a))
  (2cmbn-sbtr #'s-cmpr (cmbn 0 2 'b) (cmbn 0))
  (2cmbn-sbtr #'s-cmpr (cmbn 0 3 'b) (cmbn 0 3 'b))
  (2cmbn-sbtr #'s-cmpr (cmbn 0 3 'b) (cmbn 0 4 'b))
  (2cmbn-sbtr #'s-cmpr
    (cmbn 0 1 'a 2 'c  2 'd       3 'g)
    (cmbn 0      1 'c -2 'd 4 'f -3 'g 4 'h))
  (2cmbn-sbtr #'s-cmpr
    (cmbn 0      1 'c -2 'd 4 'f -3 'g 4 'h)
    (cmbn 0 1 'a 2 'c  2 'd       3 'g))
  (2cmbn-sbtr #'s-cmpr
    (cmbn 0      1 'c -2 'd 4 'f -3 'g)
    (cmbn 0 1 'a 2 'c  2 'd       3 'g))
  (2cmbn-sbtr #'s-cmpr
    (cmbn 0 1 'b 2 'a)
    (cmbn 0 1 'a 1 'b)))  ;;; !!!
  (2cmbn-sbtr #'s-cmpr
    (cmbn 0 1 'b 2 'c)
    (cmbn 0 1 'a 1 'b)))
|#

(DEFUN 2N-2CMBN (cmpr n1 cmbn1 n2 cmbn2)
   ;; n1 * cmbn1 + n2 * cmbn2
   ;; combinations may be null but not n1 and n2
   (declare
      (type cmprf cmpr)
      (fixnum n1 n2)
      (type cmbn cmbn1 cmbn2))
   (the cmbn
      (with-cmbn (degr1 list1) cmbn1
      (with-cmbn (degr2 list2) cmbn2
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
            (with--term (cffc1 gnrt1) mark1
            (with--term (cffc2 gnrt2) mark2
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
                                           (with-term (cffc gnrt) term
                                              (term (* n2 cffc) gnrt)))
                                 mark2))))
                  (go :exit)
                  :cmbn2-is-finished
                  (setf pre-rslt
                        (nreconc pre-rslt
                           (if (= 1 n1)
                              mark1
                              (mapcar #'(lambda (term)
                                           (declare (cons term))
                                           (with-term (cffc gnrt) term
                                              (term (* n1 cffc) gnrt)))
                                 mark1))))
                  :exit))
            (make-cmbn
               :degr degr1
               :list pre-rslt)))))))

#|
  (2n-2cmbn #'s-cmpr 3 (cmbn 0) 4 (cmbn 1))
  (2n-2cmbn #'s-cmpr 3 (cmbn 0) 4 (cmbn 0))
  (2n-2cmbn #'s-cmpr 3 (cmbn 0 1 'a) 4 (cmbn 0))
  (2n-2cmbn #'s-cmpr 3 (cmbn 0  1 'a 2 'b 3 'c     )
                     1 (cmbn 0 -3 'a 2 'b      4 'd))
  (2n-2cmbn #'s-cmpr 3 (cmbn 0 -3 'a 2 'b     4 'd)
                     1 (cmbn 0  1 'a 2 'b 3 'c    ))
  (2n-2cmbn #'s-cmpr 1 (cmbn 0  1 'a 2 'b 3 'c     )
                     1 (cmbn 0 -3 'a 2 'b      4 'd))
  (2n-2cmbn #'s-cmpr 1 (cmbn 0 -3 'a 2 'b     4 'd)
                     1 (cmbn 0  1 'a 2 'b 3 'c    ))
  (2n-2cmbn #'s-cmpr 1 (cmbn 0 -3 'a 2 'b     4 'd)
                     3 (cmbn 0  1 'a 2 'b 3 'c    )))
|#

(DEFUN CMBN-CMBN (cmpr n-cmbn-list)
  ;; n-cmbn-list = list of (cons n cmbn)
  ;;               non-empty-list
  ;;            every n is non-null
  (declare
     (type cmprf cmpr)
     (list n-cmbn-list))
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

#|
  (setf cons (cons 3 (cmbn 0 4 'a)))
  (cmbn-cmbn #'s-cmpr (make-list 5 :initial-element cons))
  (dotimes (i 10)
     (print
       (cffc (first (cmbn-list
          (cmbn-cmbn #'s-cmpr (make-list (1+ i) :initial-element cons))))))))
|#

(DEFUN NTERM-ADD (cmpr degr &rest rest)
   (declare
      (type cmprf cmpr)
      (fixnum degr)
      (list rest))
      ;; (list term)
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

#|
  (nterm-add #'s-cmpr 11)
  (nterm-add #'s-cmpr 11 (term 1 'a))
  (nterm-add #'s-cmpr 11 (term 1 'a) (term 2 'b))
  (nterm-add #'s-cmpr 11 (term 1 'b) (term 2 'a) (term 3 'aa)
                         (term -2 'aa) (term -2 'a) (term 4 'aab)
                         (term 5 'c))
|#

(DEFUN NCMBN-ADD (cmpr cmbn &rest rest)
   (declare
      (type cmprf cmpr)
      (type cmbn cmbn))
   (the cmbn
      (cmbn-cmbn cmpr (mapcar #'(lambda (cmbn)
                                   (declare (type cmbn cmbn))
                                   (cons 1 cmbn))
                         (cons cmbn rest)))))

#|
(setf c (cmbn 3 4 'a))
(ncmbn-add #'s-cmpr c c c c c)
|#

(DEFUN DSTR-ADD-TERM-TO-CMBN (cmpr cffc gnrt cmbn)
   (declare
      (type cmprf cmpr)
      (fixnum cffc)  ;; non-zero
      (type gnrt gnrt)
      (type cmbn cmbn))
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

#|
()
(setf c (zero-cmbn 10))
(dstr-add-term-to-cmbn #'s-cmpr 3 'f c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr 3 'g c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr 3 'a c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr 3 'd c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr -3 'd c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr -2 'a c)
(dstr-add-term-to-cmbn #'s-cmpr -1 'a c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr -3 'g c)
(eq * c)
(dstr-add-term-to-cmbn #'s-cmpr -3 'f c)
(eq * c))
|#
