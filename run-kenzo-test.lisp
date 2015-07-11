(require 'asdf)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(asdf:operate 'asdf:load-op 'kenzo-test)

(fiveam:run!)

#+ccl(quit)
#+sbcl(sb-ext:exit)
