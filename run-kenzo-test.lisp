#+sbcl(require 'asdf)

(asdf:operate 'asdf:load-op 'kenzo)
(asdf:operate 'asdf:load-op 'kenzo-test)
(in-package :kenzo-test)
(run!)

#+sbcl(sb-ext:exit)
