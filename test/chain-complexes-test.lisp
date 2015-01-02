
(in-package :kenzo-test)

(in-suite :kenzo)


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


(test gnrt-?
      (progn
	(cat:cat-init)
	(let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
				  :basis :locally-effective
				  :bsgn 0
				  :intr-dffr #'(lambda (cmbn)
						 (cat:cmbn
						  (1- (cat:cmbn-degr cmbn))))
				  :strt :cmbn
				  :orgn '(Z of Z)))
	       (ff (cat:build-mrph :sorc cc :trgt cc :degr 0
				   :intr #'(lambda (degr n)
					     (cat:cmbn degr 1 n))
				   :strt :gnrt :orgn '(test))))
	  ;; setting this to a small value (-1) triggers some kind
	  ;; of caching which appears to be broken (???)
	  (setf cat:+too-much-time+ 50)
	  (dotimes (i 20)
	    (let* ((n (- (random 50) 50))
		  (comb (cat:gnrt-? ff 0 n)))
	      (is (equal (cat:cmbn-degr comb) 0))
	      (is (equal (cat:cmbn-list comb) (cons (cons 1 n) nil))))))))


(defparameter *n* 10)
(defun ff (degr i)
  (do ((2*n* (ash *n* 1))
       (rslt cat:+empty-list+
             (cons (cons (let ((cffc (- (random 2*n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat:make-cmbn
        :degr degr
        :list rslt))))

(test cmbn-?
      (progn
	(compile 'ff)
	(cat:cat-init)
	(let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
				   :basis :locally-effective
				   :bsgn 0
				   :intr-dffr #'(lambda (cmbn)
						  (cat:cmbn
						   (1- (cat:cmbn-degr cmbn))))
				   :strt :cmbn
				   :orgn '(Z of Z)))
	       (mrph (cat:build-mrph :sorc cc :trgt cc :degr 0
				     :intr #'ff :strt :gnrt :orgn '(test))))

	  (let ((comb (cat:cmbn-? mrph (cat:cmbn 0 1 100))))
	    (is (equal (cat:cmbn-degr comb) 0))
	    (setq comb (cat:cmbn-? mrph comb))
	    (setq comb (cat:cmbn-? mrph comb))
	    (setq comb (cat:cmbn-? mrph comb))
	    (setq comb (cat:cmbn-? mrph comb))
	    (setq comb (cat:cmbn-? mrph comb))
	    ;; (time (cat:cmbn-? mrph *))
	    ;; (inspect mrph)
	    ))))
