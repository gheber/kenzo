
(in-package :kenzo-test)

(in-suite :kenzo)

(test result-print
      (let ((rslt (format nil "~a"
			  (cat:make-result :gnrt 'a :value '(a a)
					   :clnm 23 :rntm 2.345))))
	(is (equal rslt (format nil "~{~a~}"
				     '(#\Newline "<Rslt>" #\Newline
				       "    GNRT-> A" #\Newline
				       "   VALUE-> (A A)" #\Newline
				       "    CLNM->     23" #\Newline
				       "    RNTM->       2.345"))))))

#|
  (do-control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'c))
  (do-control #'s-cmpr (cmbn 0 1 'b 1 'b -1 'c))
  (do-control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
  (setf *cmbn-control* nil)
  (control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
  (setf *cmbn-control* t)
  (control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
|#

(test do-control
      (let ((comb (cat:do-control #'cat:s-cmpr (cat:cmbn 0 1 'a 1 'b -1 'c))))
	(is (equal (cat:cmbn-degr comb) 0))
	(is (equal (cat:cmbn-list comb) '((1 . a) (1 . b) (-1 . c)))))

      (signals simple-error (cat:do-control #'cat:s-cmpr
			      (cat:cmbn 0 1 'b 1 'b -1 'c)))

      (signals simple-error (cat:do-control #'cat:s-cmpr
			      (cat:cmbn 0 1 'a 1 'b -1 'b)))

      (setf cat:*cmbn-control* nil))

#|
()
(cat-init)
(setf cc (build-chcm :cmpr #'f-cmpr
                     :basis :locally-effective
                     :bsgn 0
                     :intr-dffr #'(lambda (cmbn)
                                    (cmbn (1- (cmbn-degr cmbn))))
                     :strt :cmbn
                     :orgn '(Z of Z)))
(setf cc (build-chcm :cmpr #'f-cmpr
                     :basis :locally-effective
                     :bsgn 0
                     :intr-dffr #'(lambda (cmbn)
                                    (cmbn (1- (cmbn-degr cmbn))))
                     :strt :cmbn
                     :orgn '(Z of Z)))
(setf ff (build-mrph :sorc cc :trgt cc :degr 0
                     :intr #'(lambda (degr n) (cmbn degr 1 n))
                     :strt :gnrt :orgn '(test)))
(setf ff (build-mrph :sorc cc :trgt cc :degr 0
                     :intr #'(lambda (degr n) (cmbn degr 1 n))
                     :strt :gnrt :orgn '(test)))
(dotimes (i 20)
  (let ((n (- (random 50) 50)))
    (format t "~%~D   ~D" n (gnrt-? ff 0 n))))
(setf +too-much-time+ -1)
(dotimes (i 20)
  (let ((n (- (random 50) 50)))
    (format t "~%~D   ~D" n (gnrt-? ff 0 n))))
(setf +too-much-time+ 50)
|#


#|
()
(cat-init)
(setf cc (build-chcm :cmpr #'f-cmpr
                     :basis :locally-effective
                     :bsgn 0
                     :intr-dffr #'(lambda (cmbn)
                                    (cmbn (1- (cmbn-degr cmbn))))
                     :strt :cmbn
                     :orgn '(Z of Z)))
(setf *n* 10)
(defun ff (degr i)
  (do ((*2n* (ash *n* 1))
       (rslt +empty-list+
             (cons (cons (let ((cffc (- (random *2n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (make-cmbn
        :degr 0
        :list rslt))))
(ff 0 20)
(compile 'ff)
(setf mrph (build-mrph :sorc cc :trgt cc :degr 0
                       :intr #'ff :strt :gnrt :orgn '(test)))
(cmbn-? mrph (cmbn 0 1 100))
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(time (cmbn-? mrph *))
(inspect mrph)
|#
