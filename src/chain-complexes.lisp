;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES
;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES
;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES

(IN-PACKAGE #:cat)

(PROVIDE "chain-complexes")

(DEFUN CAT-INIT ()
  (declare (special *idnm-counter* *results-n* *results-cmlt-tm*))
  (setf *idnm-counter* 0 *results-n* 0 *results-cmlt-tm* 0.0)
  (map nil #'(lambda (listname)
               (declare (symbol listname))
               (setf (symbol-value listname) +empty-list+))
       *list-list*)
  (done))

(DEFUN HOW-MANY-OBJECTS ()
  (mapc #'(lambda (symbol)
	    (declare (type (or string symbol) symbol))
	    (let ((length (length (eval symbol))))
	      (declare (fixnum length))
	      (setf symbol (symbol-name symbol))
	      (setf symbol (subseq symbol 1 (- (length symbol) 6)))
	      (format t "~%~6D ~As" length symbol)))
	*list-list*)
  (done))

(DEFUN ALL-OBJECTS ()
  (let ((object-list
	 (sort (delete-duplicates
		(mapcan #'(lambda (symbol)
			    (copy-list (eval symbol)))
			*list-list*)
		:test #'eq)
	       #'< :key #'idnm)))
    (declare (list object-list))
    (dolist (item object-list)
      (format t "~%~A = ~A" item (orgn item))))
  (done))

(DEFUN KD (idnm)
  (declare (fixnum idnm))
  (dolist (list *list-list*)
    (let ((found (find idnm (eval list) :key #'idnm)))
      (when found
	(format t "~%Object: ~A
~3TOrigin: ~A~2%" found (orgn found))
	(return-from kd (values))))))

(DEFUN K (idnm)
  (declare (fixnum idnm))
  (dolist (list *list-list*)
    (let ((found (find idnm (eval list) :key #'idnm)))
      (when found
	(return-from k found)))))

(DEFUN KD2 (idnm)
  (declare (fixnum idnm))
  (let ((k-list (k idnm)))
    (declare (list k-list))
    (unless k-list
      (return-from kd2 nil))
    (setf k-list (list k-list))
    (do ((idnm (1- idnm) (1- idnm)))
	((zerop idnm))
      (declare (fixnum idnm))
      (let ((found (find-if
		    #'(lambda (item)
			(declare (type (or chain-complex morphism) item))
			(member (k idnm) (orgn item)))
		    k-list)))
	(declare (type (or null chain-complex morphism) found))
	(when found
	  (push (k idnm) k-list))))
    (mapc #'kd (nreverse (mapcar #'idnm k-list)))))


#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((chcm chain-complex) stream)
  (the chain-complex
       (progn
	 (format stream "[K~D Chain-Complex]" (idnm chcm))
	 chcm)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFUN CHCM (idnm)
  (declare (fixnum idnm))
  (the (or chain-complex null)
       (find idnm *chcm-list* :key #'idnm)))

(DEFUN RESULT-PRINT (result stream depth)
  (declare
   (type result result) (stream stream)
   (ignore depth))
  (format stream "~%<Rslt>
~4TGNRT-> ~A
~3TVALUE-> ~A
~4TCLNM-> ~6D
~4TRNTM-> ~11,3F"
	  (result-gnrt result) (result-value result)
	  (result-clnm result) (result-rntm result))
  result)


(DEFMETHOD ?2 ((mrph morphism) cmbn)
  (declare (type cmbn cmbn))
  (the cmbn
       (cmbn-? mrph cmbn)))

(DEFMETHOD ?2 ((chcm chain-complex) cmbn)
  (declare
   (type cmbn cmbn))
  (the cmbn
       (cmbn-? (dffr1 chcm) cmbn)))

(DEFMETHOD ?3 ((mrph morphism) degr gnrt)
  (declare
   (fixnum degr)
   (type gnrt gnrt))
  (the cmbn
       (gnrt-? mrph degr gnrt)))

(DEFMETHOD ?3 ((chcm chain-complex) degr gnrt)
  (declare
   (fixnum degr)
   (type gnrt gnrt))
  (gnrt-? (dffr1 chcm) degr gnrt))

#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) nil))
(DEFMETHOD PRINT-OBJECT ((mrph morphism) stream)
  (the morphism
       (progn
	 (if (eq (first (orgn (trgt mrph))) 'z-chcm)
	     (format stream "[K~D Cohomology-Class on K~D of degree ~D]"
		     (idnm mrph) (idnm (sorc mrph)) (- (degr mrph)))
	     (format stream "[K~D Morphism (degree ~D): K~D -> K~D]"
		     (idnm mrph) (degr mrph)
		     (idnm (sorc mrph)) (idnm (trgt mrph))))
         mrph)))
#+clisp(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (ext:package-lock :clos) t))

(DEFUN MRPH (n)
  (declare (fixnum n))
  (the (or morphism null)
       (find n *mrph-list* :key #'idnm)))

(DEFUN BUILD-CHCM (&key cmpr basis bsgn intr-dffr strt orgn)
  (declare
   (type cmprf cmpr)
   (type basis basis)
   (type gnrt bsgn)
   (type intr-mrph intr-dffr)
   (type strt strt)
   (type list orgn))
  (the chain-complex
       (progn
         (let ((already (find orgn *chcm-list* :test #'equal :key #'orgn)))
	   (declare (type (or chain-complex null) already))
	   (when already
	     (return-from build-chcm already)))
         (unless basis
	   (setf basis :locally-effective))
         (let ((chcm (make-instance 'chain-complex
				    :cmpr cmpr
				    :basis basis
				    :bsgn bsgn
				    :orgn orgn)))
	   (declare (type chain-complex chcm))
	   (setf (slot-value chcm 'dffr)
		 (build-mrph
		  :sorc chcm :trgt chcm :degr -1
		  :intr intr-dffr :strt strt
		  :orgn `(dffr ,chcm)))
	   (setf (slot-value chcm 'grmd) chcm)
	   (push chcm *chcm-list*)
	   chcm))))

(DEFUN BASIS2 (object n)         ;;;
  (declare
   (type t object)
   (fixnum n))
  (the list
       (with-slots (basis) object
         (declare (type basis basis))
         (when (eq :locally-effective basis)
	   (error "The object ~A is locally-effective." object))
         (funcall basis n))))

(DEFUN BUILD-MRPH (&key sorc trgt degr intr strt orgn)
  (declare
   (type chain-complex sorc trgt)
   (fixnum degr)
   (type intr-mrph intr)
   (type strt strt)
   (list orgn))
  (the morphism
       (progn
         (let ((already (find orgn *mrph-list* :test #'equal :key #'orgn)))
	   (declare (type (or morphism null) already))
	   (when already
	     (return-from build-mrph already)))
         (let ((mrph (make-instance 'morphism
				    :sorc sorc :trgt trgt :degr degr
				    :intr intr :strt strt
				    :orgn orgn)))
	   (declare (type morphism mrph))
	   (setf (slot-value mrph 'rslts)
		 (ecase strt
		   (:gnrt (map 'simple-vector
                               ;; (vector (vector result))
                               #'(lambda (dummy)
				   (declare (ignore dummy))
				   (make-array 0
					       :adjustable t
					       :fill-pointer 0))
                               (make-list +maximal-dimension+)))
		   (:cmbn nil)))
	   (push mrph *mrph-list*)
	   mrph))))

;;; FUNCTIONS

(DEFUN NRESULTS ()
  (the fixnum
       (let ((nrslts 0))
	 (declare (fixnum nrslts))
	 (dolist (item *mrph-list*)
	   (declare (type morphism item))
	   (when (eq :gnrt (strt item))
	     (map nil
		  #'(lambda (drslt)
		      (declare (vector drslt))
		      (incf nrslts (length drslt)))
		  (rslts item))))
	 nrslts)))

#|
(DEFUN TIME-LIST ()
  (the list
       (let ((time-list +empty-list+))
	 (declare (list time-list))
	 (dolist (item *mrph-list*)
	   (declare (type morphism item))
	   (when (eq :gnrt (strt item))
	     (map nil #'(lambda (drslt)
			  (declare (vector drslt))
			  (map nil #'(lambda (rslt)
				       (declare (type result rslt))
				       (let ((rntm (result-rntm rslt)))
					 (declare (integer rntm))
					 (when (plusp rntm)
					   (push rntm time-list))))
			       drslt))
		  (rslts item))))
	 (sort time-list #'>))))
|#

(DEFUN CLEAN-RESULTS (&optional (results-coef *results-coef*))
  (declare
   (single-float results-coef)
   (special *results-coef* *results-cmlt-tm* *results-n* *results-verbose*))
  (the (values )
       (let ((cut (* results-coef (/ *results-cmlt-tm* *results-n*))))
	 (declare (single-float cut))
	 (setf *results-n* 0 *results-cmlt-tm* 0.0)
	 (dolist (item *mrph-list*)
	   (declare (type morphism item))
	   (when (eq :gnrt (strt item))
	     (let ((rslts (rslts item)))
	       (declare (vector rslts))
	       (dotimes (i +maximal-dimension+)
		 (declare (fixnum i))
		 (let ((rslts-i (aref rslts i))
		       (output (make-array 0 :adjustable t :fill-pointer 0)))
		   (declare (vector rslts-i output))
		   (map nil
			#'(lambda (rslt)
			    (declare (type result rslt))
			    (let ((rntm (result-rntm rslt)))
			      (declare (single-float rntm))
			      (when (> rntm cut)
				(incf *results-n*)
				(incf *results-cmlt-tm* rntm)
				(vector-push-extend rslt output))))
			rslts-i)
		   (setf (aref rslts i) output))))))
	 (when *results-verbose*
	   (format t "~%*CR* -- Cut = ~A -- N = ~A" cut *results-n*)))))


(DEFUN MRPH-GNRT (cmpr2 intr degr gnrt memory
		  &optional (left -1) (right (fill-pointer memory)))
  (declare
   (type intr-mrph intr)
   ;; (function (fixnum gnrt) cmbn)
   (type cmprf cmpr2)
   (fixnum degr left right)
   (type gnrt gnrt)
   (vector memory)
   (special *results-n*)
   (fixnum *results-n*))
  ;; (vector result)
  (the (values cmbn fixnum)
       ;; cmbn = image of the generator
       ;; fixnum = index "exact" or just "lower" of the maybe stored result
       (loop
	  (when (= right (1+ left)) ; the result for gnrt is not available
	    (push (get-internal-run-time) *start-stack*)
	    (let ((value (funcall intr degr gnrt))
		  (runtime (- (get-internal-run-time) (pop *start-stack*))))
	      (declare
	       (type cmbn value)
	       (integer runtime))
	      (if (> runtime +too-much-time+)
		  ;; the condition deciding whether
		  ;; storing must happen
		  (let ((old-length (vector-push-extend nil memory)))
		    (declare (fixnum old-length))
		    (replace memory memory
			     :start1 (1+ right) :end1 (1+ old-length)
			     :start2 right :end2 old-length)
		    (setf (aref memory right)
			  (make-result
			   :gnrt gnrt :value value :clnm 1
			   :rntm (let ((rntm
					(float
					 (/ runtime
					    internal-time-units-per-second))))
				   (declare (single-float rntm))
				   (incf *results-cmlt-tm* rntm)
				   rntm)))
		    (mapl #'(lambda (mark)
			      (declare (cons mark))
			      (incf (car mark) runtime))
			  *start-stack*)
		    (when (> (incf *results-n*) *results-max*)
		      (clean-results))
		    (return (values value right)))
		  (return (values value left)))))
	  (let ((middle (truncate (+ right left) 2)))
            (declare (fixnum middle))
            (ecase (funcall cmpr2 gnrt (result-gnrt (aref memory middle)))
	      (:equal (let ((rslt (aref memory middle)))
			(declare (type result rslt))
			(incf (result-clnm rslt))
			(return (values (result-value rslt) middle))))
	      (:less (setf right middle))
              (:greater (setf left middle)))))))


(DEFUN MRPH-CMBN (scmpr2 tcmpr2 intr cmbn memory)
  (declare
   (type intr-mrph intr)
   ;; (function (fixnum gnrt) cmbn)
   (type cmprf scmpr2 tcmpr2)
   (type cmbn cmbn)                    ;;; cmbn is a non-zero one
   (vector memory))
  ;; (vector result)
  (the cmbn
       (with-cmbn (degr list) cmbn
		  (let ((n-cmbn-list +empty-list+))
		    (declare (list n-cmbn-list))
		    (do ((mark list (cdr mark))
			 (left -1))
			((endp mark))
		      (declare (list mark))
		      (multiple-value-bind (rslt new-left)
			  (mrph-gnrt scmpr2 intr degr (-gnrt mark) memory left)
			(setf left new-left)
			(push (cons (-cffc mark) rslt) n-cmbn-list)))
		    (cmbn-cmbn tcmpr2 n-cmbn-list)))))

(DEFUN DO-CONTROL (cmpr cmbn)
  (declare
   (type cmprf cmpr)
   (type cmbn cmbn))
  (let ((list (cmbn-list cmbn)))
    (declare (list list))
    (do ((mark1 (rest list) (cdr mark1))
	 (mark2 list mark1))
	((endp mark1))
      (declare (list mark1 mark2))
      (unless (eq :less (funcall cmpr (-gnrt mark2) (-gnrt mark1)))
	(setf *wrong-cmbn* cmbn)
	(error "In the combination located by *WRONG-CMBN*, the generators:~@
                    ~A and ~A are in a wrong order." (cdar mark2)
		    (cdar mark1)))))
  cmbn)

#|
(DEFVAR *PROFILER-STACK*)
(SETF *PROFILER-STACK* +empty-list+)

(DEFUN PROFILER-INIT ()
  (mapc #'(lambda (mrph)
	    (declare (type morphism mrph))
	    (setf (wrtm mrph) 0))
	*mrph-list*))

(DEFUN PROFILER-ON ()
  (push (get-internal-run-time) *profiler-stack*))

(DEFUN PROFILER-OFF (mrph)
  (declare (type morphism mrph))
  (let ((time-spent 
	 (- (get-internal-run-time) (pop *profiler-stack*))))
    (declare (integer time-spent))
    (mapl #'(lambda (mark)
	      (declare (cons mark))
	      (incf (car mark) time-spent))
	  *profiler-stack*)
    (incf (wrtm mrph) time-spent)))
|#


(DEFUN GNRT-? (mrph degr gnrt)
  (declare
   (type morphism mrph)
   (fixnum degr)
   (type gnrt gnrt))
  (the cmbn
       (progn
	 ;;         (profiler-on)
         (when (and *future-display*
                    (> (get-internal-run-time) *future-display*))
	   (format t "~%GNRT-?
~3T~A  ORGN = ~A
~3TDEGR = ~D
~3TGNRT = ~A"
		   mrph (orgn mrph) degr gnrt)
	   (setf *future-display* (+ (get-internal-run-time) *time-interval*)))
         (with-slots (sorc (mdegr degr) intr strt ?-clnm) mrph
	   (declare
	    (type chain-complex sorc)
	    (fixnum mdegr ?-clnm)
	    (type intr-mrph intr)
	    (type strt strt))
	   (with-slots ((scmpr cmpr)) sorc
	     (declare (type cmprf scmpr))
	     (control (slot-value (slot-value mrph 'trgt) 'cmpr)
		      (progn
			(incf ?-clnm)
			(prog1
			    (ecase strt
			      (:gnrt
			       (mrph-gnrt scmpr intr
					  degr gnrt (svref (rslts mrph) degr)))
			      (:cmbn
			       (funcall intr (term-cmbn degr 1 gnrt))))
			  ;;                     (profiler-off mrph)
			  ))))))))


(DEFUN CMBN-? (mrph cmbn)
  (declare
   (type morphism mrph)
   (type cmbn cmbn))
  (the cmbn
       (progn
	 ;;         (profiler-on)
         (when (and *future-display*
                    (> (get-internal-run-time) *future-display*))
	   (format t "~%CMBN-?
~3T~A  ORGN = ~A
~3TCMBN = ~A"
		   mrph (orgn mrph) cmbn)
	   (setf *future-display* (+ (get-internal-run-time) *time-interval*)))
         (with-slots (sorc trgt degr intr strt ???-clnm) mrph
	   (declare
	    (type chain-complex sorc trgt)
	    (fixnum degr ???-clnm)
	    (type intr-mrph intr)
	    (type strt strt))
	   (with-slots ((scmpr cmpr)) sorc
	     (declare (type cmprf scmpr))
	     (with-slots ((tcmpr cmpr)) trgt
	       (declare (type cmprf tcmpr))
	       (control tcmpr
			(progn
			  (controln scmpr cmbn)
			  (incf ???-clnm)
			  (when (eq strt :cmbn)
			    (return-from cmbn-? (funcall intr cmbn)))
			  (prog1
			      (if (cmbn-zero-p cmbn)
				  (zero-cmbn (+ (cmbn-degr cmbn) degr))
				  (mrph-cmbn scmpr tcmpr intr
					     cmbn (svref (rslts mrph)
							 (cmbn-degr cmbn))))
			    ;;                     (profiler-off mrph)
			    )))))))))
