;;; DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO
;;; DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO
;;; DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO

(IN-PACKAGE :USER)

(DEFVAR *STATEMENT*)

;; C = comment
(DEFUN C (string)
  (terpri)
  (dotimes (i 50) (princ "-"))
  (princ "<Comment>")
;  (dotimes (i 31) (princ "-"))
;  (terpri)
  (princ string)
;  (terpri)
  (dotimes (i 50) (princ "-"))
  (values))

;; R = read
(DEFMACRO R (statement)
  (declare (special *statement*))
  (setf *statement* statement)
  `(progn
     (terpri)
     (dotimes (i 50) (princ "-"))
     (princ "<Lisp Statement>")
;     (dotimes (i 29) (princ "-"))
;     (terpri)
     (pprint ',statement)
     (terpri) ; (terpri)
     (dotimes (i 50) (princ "-"))
     (values)))

(DEFUN STRIP-INITIAL-SPACES (st)
  (let ((st2 (make-string (- (length st) (count #\Newline st)))))
    (declare (type string st2))
    (do ((i 0 (1+ i))
         (j 0 (1+ j)))
        ((= i (length st)))
      (let ((char (aref st i)))
        (when (char= char #\newline)
          (incf i))
        (setf (aref st2 j) char)))
    st2))

(DEFMACRO R2 (dstatement)
  (declare (special *statement* *dstatement*))
  (setf *dstatement* dstatement)
  (setf *statement* (read-from-string dstatement))
  `(progn
     (terpri)
      (dotimes (i 50) (princ "-"))
      (princ "<Lisp Statement>")
      ;  (dotimes (i 31) (princ "-"))
      ;  (terpri)
      (princ (strip-initial-spaces *dstatement*))
      ;  (terpri)
      (dotimes (i 50) (princ "-"))
      (values)))

;; E = EXECUTE
(DEFUN E ()
  (declare (special *statement*))
  (terpri)
  (dotimes (i 50) (princ "-"))
  (princ "<Result>")
;  (dotimes (i 32) (princ "-"))
  (terpri) ; (terpri)
  (prin1 (eval *statement*))
  (terpri) ; (terpri)
  (dotimes (i 50) (princ "-"))
  (values))

;; EP = EXECUTE by PRINT
(DEFUN EP ()
  (declare (special *statement*))
  (terpri)
  (dotimes (i 50) (princ "-"))
  (princ "<Result>")
;  (dotimes (i 32) (princ "-"))
  (terpri) ; (terpri)
  (eval *statement*)
  (terpri) ; (terpri)
  (dotimes (i 50) (princ "-"))
  (values))

(DEFUN EE ()
  (declare (special *statement*))
  (terpri)
  (dotimes (i 10) (princ "-"))
  (princ " Error ")
  (dotimes (i 33) (princ "-"))
  (terpri) (terpri)
  (restart-case
      (handler-bind ((error
                         #'(lambda (condition)
                             (format t "Type-error: ~S~&"
                               (class-name (class-of condition)))
                             (apply #'format t
                                    (simple-condition-format-control condition)
                                    (simple-condition-format-arguments condition))
                             (invoke-restart 'my-restart))))
                  (eval *statement*))
    (my-restart () (values)))
  (terpri) (terpri)
  (dotimes (i 50) (princ "-"))
  (values))

(DEFMACRO M (statement)
  (eval statement)
  '(values ))