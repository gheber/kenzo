
(in-package :kenzo-test)

(def-suite :kenzo)

(defparameter *n* -1)

(defun ff (degr i)
  (declare (special *n*))
  (do ((2*n* (ash *n* 1))
       (rslt cat:+empty-list+
             (cons (cons (let ((cffc (- (random 2*n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat:make-cmbn :degr degr :list rslt))))


(defun cdelta (dmns)
  (the cat:chain-complex
       (cat:build-chcm
        :cmpr #'cat:l-cmpr
        :basis :locally-effective
        :bsgn '(0)
        :intr-dffr #'(lambda (degr gmsm)
                       (cat:make-cmbn
                        :degr (1- degr)
                        :list (do ((rslt cat:+empty-list+
                                         (cons (cons sign
                                                     (append
                                                      (subseq gmsm 0 nark)
                                                      (subseq gmsm (1+ nark))))
                                               rslt))
                                   (sign 1 (- sign))
                                   (nark 0 (1+ nark)))
                                  ((> nark degr) rslt))))
        :strt :gnrt
        :orgn `(locally effective version of C_* delta ,dmns))))


(defun make-f (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat:zero-cmbn degr)
                       (cat:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat:zero-cmbn (1+ degr))
                       (cat:cmbn (1+ degr) (cat:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct (tdmns bdmns)
  (let ((rdct (cat:build-rdct
              :f (make-f tdmns bdmns)
              :g (make-g tdmns bdmns)
              :h (make-h tdmns bdmns)
              :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))

(defun cdelta1 (dmns)
  (cat:build-chcm
   :cmpr #'cat:l-cmpr
   :basis #'(lambda (n)
              (mapcar #'cat:dlop-int-ext (funcall (cat:delta-n-basis dmns) n)))
   :bsgn '(0)
   :intr-dffr #'(lambda (degr gmsm)
                  (cat:make-cmbn
                   :degr (1- degr)
                   :list (do ((rslt cat:+empty-list+
                                    (cons (cons sign (append
                                                      (subseq gmsm 0 nark)
                                                      (subseq gmsm (1+ nark))))
                                          rslt))
                              (sign 1 (- sign))
                              (nark 0 (1+ nark)))
                             ((> nark degr) rslt))))
   :strt :gnrt
   :orgn `(locally effective version of C_* delta ,dmns)))

(defun make-f1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat:zero-cmbn degr)
                       (cat:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 bdmns) :trgt (cdelta1 tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h1 (tdmns bdmns)
  (cat:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat:zero-cmbn (1+ degr))
                       (cat:cmbn (1+ degr) (cat:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct1 (tdmns bdmns)
  (let ((rdct (cat:build-rdct
              :f (make-f1 tdmns bdmns)
              :g (make-g1 tdmns bdmns)
              :h (make-h1 tdmns bdmns)
              :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))


(defun check-rdct ()
  (dolist (phi '(cat:*tdd* cat:*bdd* cat:*df-fd* cat:*dg-gd* cat:*id-fg*
                 cat:*id-gf-dh-hd* cat:*hh* cat:*fh* cat:*hg*))
    (declare (type symbol phi))
    (is (cat:cmbn-zero-p
         (cat:cmbn-? (eval phi)
                     (if (member phi '(cat:*bdd* cat:*dg-gd* cat:*id-fg*
                                       cat:*dg-gd* cat:*hg*))
                         cat:*bc*
                         cat:*tc*))))))


(defun aleat-tc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (+ 2 (random 3)) (+ 2 (random 3)))
       (gnrt (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string))
             (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string)))
       (rslt nil (cons (cat:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat:*tc* (cat:cmbn tdegr 1 (cat:make-abar
                                                  :list rslt))))))


(defun aleat-bc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string))
             (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string)))
       (rslt nil (cons (cat:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat:*bc* (cat:cmbn tdegr 1 (cat:make-abar
                                                  :list rslt))))))


(defun c ()
  (aleat-tc)
  (aleat-bc)
  (check-rdct))


(defun random-abar (tot-degr~ max-degr)
  (do ((rslt nil)
       (cum-degr 0 (+ cum-degr degr 1))
       (degr))
      ((>= cum-degr tot-degr~) (cat:make-abar :list rslt))
    (setf degr (1+ (random max-degr)))
    (push (cat:brgn (1+ degr)
                (let ((list (make-list degr)))
                  (mapl
                   #'(lambda (sublist)
                       (setf (car sublist) (- (random 21) 10)))
                   list)
                  list))
          rslt)))


(defun random-allp (length)
  (let ((rslt nil))
    (dotimes (i length)
      (let* ((gmsm (random (cat:mask 9)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat:cbgn (1- dmns) gmsm) rslt))))
    (cat:make-allp :list rslt)))


(defun random-apowr (dmns max-expn)
  (loop
     (let* ((dgop (random (cat:2-exp (1- dmns))))
            (gmsm (- dmns (logcount dgop))))
       (unless (< 0 gmsm 3)
         (return-from random-apowr
           (cat:apowr dgop gmsm (cat:srandom max-expn)))))))


(defun random-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (apply #'cat:nterm-add cmpr degr
         (mapcar #'(lambda (dummy)
                     (declare (ignore dummy))
                     (cat:term (cat:srandom max-cffc)
                               (random-crpr degr max-expn loop-length)))
                 (make-list cmbn-length))))


(defun random-crpr (dmns max-expn length)
  (loop
     (let ((loop (cat:normalize-loop dmns
                                     (random-niloop dmns max-expn length)))
           (dgop (random (cat:2-exp dmns))))
       (let ((absm (cat:2absm-acrpr (cat:absm dgop (- dmns (logcount dgop)))
                                    loop)))
         (when (and (zerop (cat:dgop absm))
                    (not (< (cat:gmsm1 (cat:gmsm absm)) 3)))
           (return (cat:gmsm absm)))))))


(defun random-loop-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (do ((rslt cat:+empty-list+ (cons term rslt))
       (i cmbn-length (1- i))
       (term))
      ((zerop i)
       (apply #'cat:nterm-add cmpr degr rslt))
    (setf term
          (cat:term (cat:srandom max-cffc)
                    (cat:make-loop :list (random-niloop degr max-expn
                                                        loop-length))))))


(defun random-niloop (dmns max-expn length)
  (mapcar #'(lambda (dummy)
              (declare (ignore dummy))
              (random-apowr (1+ dmns) max-expn))
          (make-list length)))
