(require :uiop)

(load  "quicklisp.lisp")
(quicklisp-quickstart:install :path "./.quicklisp")
(ql:add-to-init-file)
(ql:quickload "bordeaux-threads")
(ql:quickload "usocket")
(ql:quickload "cl-json")
(ql:quickload "flexi-streams")
