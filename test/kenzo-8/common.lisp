;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

;;(in-suite :kenzo-8)

(defparameter *n* -1)

(defun ff (degr i)
  (declare (special *n*))
  (do ((2*n* (ash *n* 1))
       (rslt cat-8:+empty-list+
             (cons (cons (let ((cffc (- (random 2*n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat-8:make-cmbn :degr degr :list rslt))))


(defun cdelta (dmns)
  (the cat-8:chain-complex
       (cat-8:build-chcm
        :cmpr #'cat-8:l-cmpr
        :basis :locally-effective
        :bsgn '(0)
        :intr-dffr #'(lambda (degr gmsm)
                       (cat-8:make-cmbn
                        :degr (1- degr)
                        :list (do ((rslt cat-8:+empty-list+
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
  (cat-8:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat-8:zero-cmbn degr)
                       (cat-8:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat-8:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g (tdmns bdmns)
  (cat-8:build-mrph
   :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h (tdmns bdmns)
  (cat-8:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat-8:zero-cmbn (1+ degr))
                       (cat-8:cmbn (1+ degr) (cat-8:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat-8:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct (tdmns bdmns)
  (let ((rdct (cat-8:build-rdct
               :f (make-f tdmns bdmns)
               :g (make-g tdmns bdmns)
               :h (make-h tdmns bdmns)
               :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))

(defun cdelta1 (dmns)
  (cat-8:build-chcm
   :cmpr #'cat-8:l-cmpr
   :basis #'(lambda (n)
              (mapcar #'cat-8:dlop-int-ext (funcall (cat-8:delta-n-basis dmns) n)))
   :bsgn '(0)
   :intr-dffr #'(lambda (degr gmsm)
                  (cat-8:make-cmbn
                   :degr (1- degr)
                   :list (do ((rslt cat-8:+empty-list+
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
  (cat-8:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat-8:zero-cmbn degr)
                       (cat-8:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat-8:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g1 (tdmns bdmns)
  (cat-8:build-mrph
   :sorc (cdelta1 bdmns) :trgt (cdelta1 tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h1 (tdmns bdmns)
  (cat-8:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat-8:zero-cmbn (1+ degr))
                       (cat-8:cmbn (1+ degr) (cat-8:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat-8:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct1 (tdmns bdmns)
  (let ((rdct (cat-8:build-rdct
               :f (make-f1 tdmns bdmns)
               :g (make-g1 tdmns bdmns)
               :h (make-h1 tdmns bdmns)
               :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))


(defun check-rdct ()
  (dolist (phi '(cat-8:*tdd* cat-8:*bdd* cat-8:*df-fd* cat-8:*dg-gd* cat-8:*id-fg*
                 cat-8:*id-gf-dh-hd* cat-8:*hh* cat-8:*fh* cat-8:*hg*))
    (declare (type symbol phi))
    (is (cat-8:cmbn-zero-p
         (cat-8:cmbn-? (eval phi)
                     (if (member phi '(cat-8:*bdd* cat-8:*dg-gd* cat-8:*id-fg*
                                       cat-8:*dg-gd* cat-8:*hg*))
                         cat-8:*bc*
                         cat-8:*tc*))))))


(defun aleat-tc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (+ 2 (random 3)) (+ 2 (random 3)))
       (gnrt (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string))
             (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string)))
       (rslt nil (cons (cat-8:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat-8:*tc* (cat-8:cmbn tdegr 1 (cat-8:make-abar
                                                     :list rslt))))))


(defun aleat-bc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string))
             (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string)))
       (rslt nil (cons (cat-8:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat-8:*bc* (cat-8:cmbn tdegr 1 (cat-8:make-abar
                                                     :list rslt))))))


(defun c ()
  (aleat-tc)
  (aleat-bc)
  (check-rdct))


(defun random-abar (tot-degr~ max-degr)
  (do ((rslt nil)
       (cum-degr 0 (+ cum-degr degr 1))
       (degr))
      ((>= cum-degr tot-degr~) (cat-8:make-abar :list rslt))
    (setf degr (1+ (random max-degr)))
    (push (cat-8:brgn (1+ degr)
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
      (let* ((gmsm (random (cat-8:mask 9)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-8:cbgn (1- dmns) gmsm) rslt))))
    (cat-8:make-allp :list rslt)))


(defun random-apowr (dmns max-expn)
  (loop
     (let* ((dgop (random (cat-8:2-exp (1- dmns))))
            (gmsm (- dmns (logcount dgop))))
       (unless (< 0 gmsm 3)
         (return-from random-apowr
           (cat-8:apowr dgop gmsm (cat-8:srandom max-expn)))))))


(defun random-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (apply #'cat-8:nterm-add cmpr degr
         (mapcar #'(lambda (dummy)
                     (declare (ignore dummy))
                     (cat-8:term (cat-8:srandom max-cffc)
                               (random-crpr degr max-expn loop-length)))
                 (make-list cmbn-length))))


(defun random-crpr (dmns max-expn length)
  (loop
     (let ((loop (cat-8:normalize-loop dmns
                                     (random-niloop dmns max-expn length)))
           (dgop (random (cat-8:2-exp dmns))))
       (let ((absm (cat-8:2absm-acrpr (cat-8:absm dgop (- dmns (logcount dgop)))
                                    loop)))
         (when (and (zerop (cat-8:dgop absm))
                    (not (< (cat-8:gmsm1 (cat-8:gmsm absm)) 3)))
           (return (cat-8:gmsm absm)))))))


(defun random-loop-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (do ((rslt cat-8:+empty-list+ (cons term rslt))
       (i cmbn-length (1- i))
       (term))
      ((zerop i)
       (apply #'cat-8:nterm-add cmpr degr rslt))
    (setf term
          (cat-8:term (cat-8:srandom max-cffc)
                    (cat-8:make-loop :list (random-niloop degr max-expn
                                                        loop-length))))))


(defun random-niloop (dmns max-expn length)
  (mapcar #'(lambda (dummy)
              (declare (ignore dummy))
              (random-apowr (1+ dmns) max-expn))
          (make-list length)))
