;;;; kenzo.lisp

(IN-PACKAGE #:cat)

;;; "kenzo" goes here. Hacks and glory await!

(DEFUN KENZO-VERSION ()
  (format t "~%*** Kenzo-Version 1.1.7 ***~2%")
  (values))

;;(PROCLAIM '(OPTIMIZE (speed 3) (safety 1) (space 0) (debug 0)))
(DECLAIM '(OPTIMIZE (speed 0) (space 0) (debug 3)))

(DEFVAR *CMBN-CONTROL*)
(SETF *CMBN-CONTROL* T)


;; Don't mess with ANSI's definition of DEFCONSTANT!

(DEFMACRO DEFINE-CONSTANT (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))
