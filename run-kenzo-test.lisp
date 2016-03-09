;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(DECLAIM (OPTIMIZE (speed 3) (space 0) (debug 3)))

(require 'asdf)

(asdf:operate 'asdf:load-op 'kenzo-test)
(fiveam:run!)

#+(or ecl ccl) (quit)
#+sbcl(sb-ext:exit)
