;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES
;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES
;;;  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES  CHAIN-COMPLEXES

(IN-PACKAGE "CAT")

(PROVIDE "chain-complexes")

(DEFVAR *K-LIST* +empty-list+)

(DEFUN CAT-INIT (&optional (idnm 0))
  (declare (type fixnum idnm))
  (setf *idnm-counter* idnm
    *k-list* (member idnm *k-list* :key #'idnm))
  (done))

(DEFUN ALL-OBJECTS ()
  (dolist (item *k-list*)
    (declare (type kenzo-object item))
    (format t "~%~A = ~A" item (dfnt item)))
  (done))

(DEFUN KD (idnm)
  (declare (fixnum idnm))
  (let ((found (find idnm *k-list* :key #'idnm)))
    (when found
      (format t "~%Object: ~A~@
                   ~3TOrigin: ~A~2%" found (dfnt found))))
  (values))

(DEFUN K (idnm)
  (declare (fixnum idnm))
  (let ((found (find idnm *k-list* :key #'idnm)))
    (when found
      (return-from k found))))

(DEFUN KD2 (idnm)
  (declare (fixnum idnm))
  (let ((k-list (k idnm)))
    (declare (type (or kenzo-object list) k-list))
    (unless k-list
      (return-from kd2 nil))
    (setf k-list (list k-list))
    (do ((idnm (1- idnm) (1- idnm)))
        ((zerop idnm))
      (declare (fixnum idnm))
      (let ((found (find-if
                    #'(lambda (item)
                        (declare (type (or chain-complex morphism) item))
                        (member (k idnm) (dfnt item)))
                    k-list)))
        (declare (type (or null chain-complex morphism) found))
        (when found
          (push (k idnm) k-list))))
    (mapc #'kd (nreverse (mapcar #'idnm k-list)))))
    
(DEFMETHOD PRINT-OBJECT ((chcm chain-complex) stream)
 (the chain-complex
   (progn
      (format stream "[K~D Chain-Complex]" (idnm chcm))
      chcm)))

;;; Result = (:rslt gnrt value clnm rntm . prpr)

(DEFMETHOD PRINT-KEYCONS ((key (eql :rslt)) cdr stream)
   (the list
      (progn
         (setf cdr (cons key cdr))
         (format stream "~%<Rslt>~@
	                 ~4TGNRT-> ~A~@
                         ~3TVALUE-> ~A~@
                         ~4TCLNM-> ~6D~@
                         ~4TRNTM-> ~11,3F"
            (rslt-gnrt cdr) (rslt-value cdr)
            (rslt-clnm cdr) (rslt-rntm cdr))
         cdr)))

#|
  (make-rslt :gnrt 'a :value '(a a) :clnm 23 :rntm 2.345)
|#

(DEFPARAMETER +MAXIMAL-DIMENSION+ 15)

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

(DEFMETHOD PRINT-OBJECT ((mrph morphism) stream)
   (the morphism
      (progn
	(if (eq (first (dfnt (trgt mrph))) 'z-chcm)
	    (format stream "[K~D Cohomology-Class on K~D of degree ~D]"
		    (idnm mrph) (idnm (sorc mrph)) (- (degr mrph)))
	  (format stream "[K~D Morphism (degree ~D): K~D -> K~D]"
		  (idnm mrph) (degr mrph)
		  (idnm (sorc mrph)) (idnm (trgt mrph))))
        mrph)))

(DEFMETHOD SHARED-INITIALIZE ((k kenzo-object) slot-names &key dfnt prpr cmmn)
  (declare
   (type (or (eql t) list) slot-names)
   (type list dfnt))
  (when-slot dfnt
             (with-slots (idnm (dfnts dfnt) (prprs prpr) (cmmns cmmn)) k
               (declare
                (type fixnum idnm)
                (type list dfnts))
               (setf idnm (incf *idnm-counter*))
               (when dfnt
                 (setf dfnts dfnt))
               (when prpr
                 (setf prprs prpr))
               (when cmmn
                 (setf cmmns cmmn))))
  k)
             
(DEFMETHOD INITIALIZE-INSTANCE :after ((k kenzo-object) &rest rest)
  (push k *k-list*))
             
(DEFUN CHAIN-COMPLEX (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already chain-complex dfnt)
  (the chain-complex
    (apply #'make-instance 'chain-complex rest)))

(DEFMETHOD SHARED-INITIALIZE
    ((chcm chain-complex) slot-names
     &key cmpr basis (bsgn :undefined) dffr-intr dffr-strt grmd)
  (declare
   (type (or (eql t) list) slot-names)
   (type cmprf cmpr)
   (type basis basis)
   (type gnrt bsgn)
   (type intr dffr-intr)
   (type strt dffr-strt)
   (type chain-complex grmd))
  (call-next-method)
  (when-slot cmpr
             (with-slots ((cmprs cmpr) (basiss basis) (bsgns bsgn)
                          dffr (grmds grmd)) chcm
               (declare
                (type cmprf cmprs)
                (type basis basiss)
                (type gnrt bsgns)
                (type morphism dffr)
                (type chain-complex grmds))
               (when cmpr (setf cmprs cmpr))
               (when basis (setf basiss basis))
               (unless (eq bsgn :undefined)
                 (setf bsgns bsgn))
               (when dffr-intr
                 (unless dffr-strt
                   (error "In CHAIN-COMPLEX, a DFFR-INTR is given but not its DFFR-STRT."))
                 (setf dffr
                   (morphism :sorc chcm :trgt chcm :degr -1
                             :intr dffr-intr :strt dffr-strt
                             :dfnt `(dffr ,chcm))))
               (when grmd (setf grmds grmd))))
  chcm)

(DEFMETHOD SLOT-UNBOUND (class (chcm chain-complex) (slot-name (eql 'grmd)))
  (declare (ignore class))
  (the chain-complex
    (setf (slot-value chcm 'grmd) chcm)))

    
(DEFUN MORPHISM (&rest rest &key dfnt &allow-other-keys)
  (declare (type list dfnt))
  (already morphism dfnt)
  (the morphism
    (apply #'make-instance 'morphism rest)))

(DEFMETHOD SHARED-INITIALIZE
    ((mrph morphism) slot-names &key sorc trgt degr intr strt)
  (declare
   (type (or (eql t) list) slot-names)
   (type chain-complex sorc trgt)
   (type fixnum degr)
   (type intr intr)
   (type strt strt))
  (call-next-method)
  (when-slot sorc
             (with-slots ((sorcs sorc) (trgts trgt) (degrs degr)
                          (intrs intr) (strts strt)) mrph
               (declare
                (type chain-complex sorc trgt)
                (type fixnum degrs)
                (type intr intrs)
                (type strt strts))
               (when sorc (setf sorcs sorc))
               (when trgt (setf trgts trgt))
               (when degr (setf degrs degr))
               (when intr
                 (unless strt
                   (error "IN MORPHISM, an INTR is given, but not its STRT."))
                 (setf intrs intr)
                 (setf strts strt))))
  mrph)

(DEFMETHOD SLOT-UNBOUND (class (mrph morphism) slot-name)
  (declare
   (ignore class)
   (type (or symbol string) slot-name))
  (setf slot-name (symbol-name slot-name))
  (unless (member slot-name '("RSLTS" "?-CLNM" "???-CLNM") :test #'string=)
    (call-next-method))
  (with-slots (strt rslts ?-clnm ???-clnm) mrph
    (declare
     (type strt strt)
     (type (or null vector) rslts)
     (type fixnum ?-clnm ???-clnm))
    (setf rslts
      (ecase (slot-value mrph 'strt)
        (:gnrt (map 'simple-vector
                 ;; (vector (vector result))
                 #'(lambda (dummy)
                     (declare (ignore dummy))
                     (make-array 0
                                 :adjustable t
                                 :fill-pointer 0))
                 (make-list +maximal-dimension+)))
        (:cmbn nil)))
    (setf ?-clnm (setf ???-clnm 0))
    (cond ((string= slot-name "RSLTS") rslts)
          ((string= slot-name "?-CLNM") ?-clnm)
          ((string= slot-name "???-CLNM") ???-clnm)
          (t (error "Something wrong in (SLOT-UNBOUND t morphism t).")))))

               
               
               
;;; FUNCTIONS

(DEFVAR *START-STACK* +empty-list+)

(DEFPARAMETER +TOO-MUCH-TIME+ -1)

(DEFUN MRPH-GNRT (cmpr2 intr degr gnrt memory &optional (left -1) (right (fill-pointer memory)))
  (declare
     (type intr intr)
     ;; (function (fixnum gnrt) cmbn)
     (type cmprf cmpr2)
     (fixnum degr left right)
     (type gnrt gnrt)
     (vector memory))
   ;; (vector result)
   (the (values cmbn fixnum)
      ;;; cmbn = image of the generator
      ;;; fixnum = index "exact" or just "lower" of the maybe stored result
      (loop
         (when (= right (1+ left))            ;; the result for gnrt is not available
            (push (get-internal-run-time) *start-stack*)
            (let ((value (funcall intr degr gnrt))
                  (runtime (- (get-internal-run-time) (pop *start-stack*))))
               (declare
                  (type cmbn value)
                  (integer runtime))
               (if (> runtime +too-much-time+)
                  ;;; the condition deciding whether
                  ;;;   storing must happen
                  (let ((old-length (vector-push-extend nil memory)))
                     (declare (fixnum old-length))
                     (replace memory memory
                        :start1 (1+ right) :end1 (1+ old-length)
                        :start2 right :end2 old-length)
                     (setf (aref memory right)
                           (make-rslt
                              :gnrt gnrt :value value
                              :clnm 1
                              :rntm (float (/ runtime internal-time-units-per-second))))
                     (mapl #'(lambda (mark)
                                (declare (cons mark))
                                (incf (car mark) runtime))
                        *start-stack*)
                     (return (values value right)))
                  (return (values value left)))))
         (let ((middle (truncate (+ right left) 2)))
            (declare (fixnum middle))
            (ecase (funcall cmpr2 gnrt (rslt-gnrt (aref memory middle)))
               (:equal (let ((rslt (aref memory middle)))
			  (declare (type cons rslt))
			  (incf (rslt-clnm rslt))
			  (return (values (rslt-value rslt) middle))))
               (:less (setf right middle))
               (:greater (setf left middle)))))))
	   
(DEFUN MRPH-CMBN (scmpr2 tcmpr2 intr cmbn memory)
   (declare
      (type intr intr)
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
                    ~A and ~A are in a wrong order." (cdar mark2) (cdar mark1)))))
   cmbn)

#|
  (do-control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'c))
  (do-control #'s-cmpr (cmbn 0 1 'b 1 'b -1 'c))
  (do-control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
  (setf *cmbn-control* nil)
  (control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
  (setf *cmbn-control* t)
  (control #'s-cmpr (cmbn 0 1 'a 1 'b -1 'b))
|#

#|
(DEFVAR *PROFILER-STACK*)
(SETF *PROFILER-STACK* +empty-list+)

(DEFUN PROFILER-INIT ()
   (mapc #'(lambda (mrph)
              (declare (type morphism mrph))
              (setf (wrtm mrph) 0))
      *k-list*))

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

(DEFVAR *FUTURE-DISPLAY* nil)

(DEFVAR *TIME-INTERVAL* 60000)

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
            (format t "~%GNRT-?~@
                       ~3T~A  DFNT = ~A~@
                       ~3TDEGR = ~D~@
                       ~3TGNRT = ~A"
               mrph (dfnt mrph) degr gnrt)
            (setf *future-display* (+ (get-internal-run-time) *time-interval*)))
         (with-slots (sorc (mdegr degr) intr strt ?-clnm) mrph
            (declare
               (type chain-complex sorc)
               (fixnum mdegr ?-clnm)
               (type intr intr)
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

#|
(cat-init)
(setf cc (chain-complex :cmpr #'f-cmpr
                        :basis :locally-effective
                        :bsgn 0
                        :dffr-intr #'(lambda (cmbn)
                                       (cmbn (1- (cmbn-degr cmbn))))
                        :dffr-strt :cmbn
                        :dfnt '(Z of Z)))
(setf cc (chain-complex :cmpr #'f-cmpr
                        :basis :locally-effective
                        :bsgn 0
                        :dffr-intr #'(lambda (cmbn)
                                       (cmbn (1- (cmbn-degr cmbn))))
                        :dffr-strt :cmbn
                        :dfnt '(Z of Z)))
(setf ff (morphism :sorc cc :trgt cc :degr 0
                   :intr #'(lambda (degr n) (cmbn degr 1 n))
                     :strt :gnrt :dfnt '(test)))
(setf ff (morphism :sorc cc :trgt cc :degr 0
                     :intr #'(lambda (degr n) (cmbn degr 1 n))
                     :strt :gnrt :dfnt '(test)))
(dotimes (i 20)
  (let ((n (- (random 50) 50)))
    (format t "~%~D   ~D" n (gnrt-? ff 0 n))))
(setf +too-much-time+ -1)
(dotimes (i 20)
  (let ((n (- (random 50) 50)))
    (format t "~%~D   ~D" n (gnrt-? ff 0 n))))
(setf +too-much-time+ 50)
|#

(DEFUN CMBN-? (mrph cmbn)
   (declare
      (type morphism mrph)
      (type cmbn cmbn))
   (the cmbn
      (progn
;;         (profiler-on)
         (when (and *future-display*
                    (> (get-internal-run-time) *future-display*))
            (format t "~%CMBN-?~@
                       ~3T~A  DFNT = ~A~@
                       ~3TCMBN = ~A"
               mrph (dfnt mrph) cmbn)
            (setf *future-display* (+ (get-internal-run-time) *time-interval*)))
         (with-slots (sorc trgt degr intr strt ???-clnm) mrph
            (declare
               (type chain-complex sorc trgt)
               (fixnum degr ???-clnm)
               (type intr intr)
               (type strt strt))
         (with-slots ((scmpr cmpr)) sorc
            (declare (type cmprf scmpr))
         (with-slots ((tcmpr cmpr)) trgt
            (declare (type cmprf scmpr))
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
                           cmbn (svref (rslts mrph) (cmbn-degr cmbn))))
;;                     (profiler-off mrph)
                     )))))))))

#|
(cat-init)
(setf cc (chain-complex :cmpr #'f-cmpr
                        :basis :locally-effective
                        :bsgn 0
                        :dffr-intr #'(lambda (cmbn)
                                       (cmbn (1- (cmbn-degr cmbn))))
                        :dffr-strt :cmbn
                        :dfnt '(Z of Z)))
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
(setf mrph (morphism :sorc cc :trgt cc :degr 0
                     :intr #'ff :strt :gnrt :dfnt '(test)))
(cmbn-? mrph (cmbn 0 1 100))
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(cmbn-? mrph *)
(time (cmbn-? mrph *))
(inspect mrph))
|#
