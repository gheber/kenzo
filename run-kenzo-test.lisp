
(DECLAIM (OPTIMIZE (speed 0) (space 0) (debug 3)))

(require 'asdf)

(asdf:operate 'asdf:load-op 'kenzo-test)
(fiveam:run!)

#+ccl(quit)
#+sbcl(sb-ext:exit)
