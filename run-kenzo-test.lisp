(require 'asdf)

(asdf:operate 'asdf:load-op 'kenzo-test)
(fiveam:run!)

#+ccl(quit)
#+sbcl(sb-ext:exit)
