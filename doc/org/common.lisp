(in-package :cl-user)

(defparameter *echo* t)

(defmacro ==> (&rest body)
  "Echos a form (controlled by *echo*) and prints its value."
  `(progn
     (when *echo*
       (pprint (quote ,@body))
       (pprint '==>))
     (pprint ,@body)
     (terpri)))
