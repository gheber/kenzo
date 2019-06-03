;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(defparameter *n* -1)

(defun ff (degr i)
  (declare (special *n*))
  (do ((2*n* (ash *n* 1))
       (rslt cat-9:+empty-list+
             (cons (cons (let ((cffc (- (random 2*n*) *n*)))
                           (if (minusp cffc) cffc (1+ cffc)))
                         (decf gnrt (1+ (random *n*))))
                   rslt))
       (gnrt i)
       (k 0 (1+ k)))
      ((= k *n*)
       (cat-9:make-cmbn :degr degr :list rslt))))


(defun cdelta (dmns)
  (the cat-9:chain-complex
       (cat-9:build-chcm
        :cmpr #'cat-9:l-cmpr
        :basis :locally-effective
        :bsgn '(0)
        :intr-dffr #'(lambda (degr gmsm)
                       (cat-9:make-cmbn
                        :degr (1- degr)
                        :list (do ((rslt cat-9:+empty-list+
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
  (cat-9:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat-9:zero-cmbn degr)
                       (cat-9:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat-9:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g (tdmns bdmns)
  (cat-9:build-mrph
   :sorc (cdelta bdmns) :trgt (cdelta tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h (tdmns bdmns)
  (cat-9:build-mrph
   :sorc (cdelta tdmns) :trgt (cdelta tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat-9:zero-cmbn (1+ degr))
                       (cat-9:cmbn (1+ degr) (cat-9:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat-9:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct (tdmns bdmns)
  (let ((rdct (cat-9:build-rdct
               :f (make-f tdmns bdmns)
               :g (make-g tdmns bdmns)
               :h (make-h tdmns bdmns)
               :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))

(defun cdelta1 (dmns)
  (cat-9:build-chcm
   :cmpr #'cat-9:l-cmpr
   :basis #'(lambda (n)
              (mapcar #'cat-9:dlop-int-ext (funcall (cat-9:delta-n-basis dmns) n)))
   :bsgn '(0)
   :intr-dffr #'(lambda (degr gmsm)
                  (cat-9:make-cmbn
                   :degr (1- degr)
                   :list (do ((rslt cat-9:+empty-list+
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
  (cat-9:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 bdmns) :degr 0
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (< pos degr)
                       (cat-9:zero-cmbn degr)
                       (cat-9:cmbn degr 1 (nconc (butlast gmsm) (list bdmns))))
                   (cat-9:cmbn degr 1 gmsm))))
   :strt :gnrt
   :orgn `(projection delta ,tdmns => delta ,bdmns)))


(defun make-g1 (tdmns bdmns)
  (cat-9:build-mrph
   :sorc (cdelta1 bdmns) :trgt (cdelta1 tdmns) :degr 0
   :intr #'identity
   :strt :cmbn
   :orgn `(injection delta ,bdmns => delta ,tdmns)))


(defun make-h1 (tdmns bdmns)
  (cat-9:build-mrph
   :sorc (cdelta1 tdmns) :trgt (cdelta1 tdmns) :degr +1
   :intr #'(lambda (degr gmsm)
             (let ((pos (position-if #'(lambda (vertex)
                                         (>= vertex bdmns)) gmsm)))
               (if pos
                   (if (member bdmns gmsm)
                       (cat-9:zero-cmbn (1+ degr))
                       (cat-9:cmbn (1+ degr) (cat-9:-1-expt-n pos)
                                 (append (subseq gmsm 0 pos) (list bdmns)
                                         (subseq gmsm pos))))
                   (cat-9:zero-cmbn (1+ degr)))))
   :strt :gnrt
   :orgn `(homotopy for delta ,tdmns => ,bdmns)))


(defun make-rdct1 (tdmns bdmns)
  (let ((rdct (cat-9:build-rdct
               :f (make-f1 tdmns bdmns)
               :g (make-g1 tdmns bdmns)
               :h (make-h1 tdmns bdmns)
               :orgn `(reduction delta ,tdmns ,bdmns))))
    rdct))


(defun check-rdct ()
  (dolist (phi '(cat-9:*tdd* cat-9:*bdd* cat-9:*df-fd* cat-9:*dg-gd* cat-9:*id-fg*
                 cat-9:*id-gf-dh-hd* cat-9:*hh* cat-9:*fh* cat-9:*hg*))
    (declare (type symbol phi))
    (is (cat-9:cmbn-zero-p
         (cat-9:cmbn-? (eval phi)
                     (if (member phi '(cat-9:*bdd* cat-9:*dg-gd* cat-9:*id-fg*
                                       cat-9:*dg-gd* cat-9:*hg*))
                         cat-9:*bc*
                         cat-9:*tc*))))))


(defun aleat-tc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (+ 2 (random 3)) (+ 2 (random 3)))
       (gnrt (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string))
             (intern (coerce (vector (code-char (+ 65 (random 4)))) 'string)))
       (rslt nil (cons (cat-9:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat-9:*tc* (cat-9:cmbn tdegr 1 (cat-9:make-abar
                                                     :list rslt))))))


(defun aleat-bc ()
  (do ((tdegr 0 (+ tdegr degr))
       (degr (1+ (random 4)) (1+ (random 4)))
       (gnrt (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string))
             (intern (coerce (vector (code-char (+ 67 (random 2)))) 'string)))
       (rslt nil (cons (cat-9:brgn degr gnrt) rslt)))
      ((> tdegr 8) (setf cat-9:*bc* (cat-9:cmbn tdegr 1 (cat-9:make-abar
                                                     :list rslt))))))


(defun c ()
  (aleat-tc)
  (aleat-bc)
  (check-rdct))


(defun random-abar (tot-degr~ max-degr)
  (do ((rslt nil)
       (cum-degr 0 (+ cum-degr degr 1))
       (degr))
      ((>= cum-degr tot-degr~) (cat-9:make-abar :list rslt))
    (setf degr (1+ (random max-degr)))
    (push (cat-9:brgn (1+ degr)
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
      (let* ((gmsm (random (cat-9:mask 9)))
             (dmns (1- (logcount gmsm))))
        (when (plusp dmns)
          (push (cat-9:cbgn (1- dmns) gmsm) rslt))))
    (cat-9:make-allp :list rslt)))


(defun random-apowr (dmns max-expn)
  (loop
     (let* ((dgop (random (cat-9:2-exp (1- dmns))))
            (gmsm (- dmns (logcount dgop))))
       (unless (< 0 gmsm 3)
         (return-from random-apowr
           (cat-9:apowr dgop gmsm (cat-9:srandom max-expn)))))))


(defun random-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (apply #'cat-9:nterm-add cmpr degr
         (mapcar #'(lambda (dummy)
                     (declare (ignore dummy))
                     (cat-9:term (cat-9:srandom max-cffc)
                               (random-crpr degr max-expn loop-length)))
                 (make-list cmbn-length))))


(defun random-crpr (dmns max-expn length)
  (loop
     (let ((loop (cat-9:normalize-loop dmns
                                     (random-niloop dmns max-expn length)))
           (dgop (random (cat-9:2-exp dmns))))
       (let ((absm (cat-9:2absm-acrpr (cat-9:absm dgop (- dmns (logcount dgop)))
                                    loop)))
         (when (and (zerop (cat-9:dgop absm))
                    (not (< (cat-9:gmsm1 (cat-9:gmsm absm)) 3)))
           (return (cat-9:gmsm absm)))))))


(defun random-loop-cmbn (cmpr degr max-cffc max-expn loop-length cmbn-length)
  (do ((rslt cat-9:+empty-list+ (cons term rslt))
       (i cmbn-length (1- i))
       (term))
      ((zerop i)
       (apply #'cat-9:nterm-add cmpr degr rslt))
    (setf term
          (cat-9:term (cat-9:srandom max-cffc)
                    (cat-9:make-loop :list (random-niloop degr max-expn
                                                        loop-length))))))


(defun random-niloop (dmns max-expn length)
  (mapcar #'(lambda (dummy)
              (declare (ignore dummy))
              (random-apowr (1+ dmns) max-expn))
          (make-list length)))
