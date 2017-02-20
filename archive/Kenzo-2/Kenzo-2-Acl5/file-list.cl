;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES
;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES
;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES

(IN-PACKAGE "COMMON-LISP-USER")

(DEFPACKAGE "CAT"
  (:use "COMMON-LISP"))

(IN-PACKAGE "CAT")

(USE-PACKAGE "CAT" "COMMON-LISP-USER")

(PROCLAIM '(OPTIMIZE (speed 3) (safety 1) (space 0) (debug 0)))

(EXPORT '(+file-list+ load-sfiles compile-files load-cfiles cat-files))

(DEFCONSTANT +FILE-LIST+
   '("classes"
     "macros"
     "various"
     "combinations"
     "chain-complexes"
     "chcm-elementary-op"
     "effective-homology"
     "homology-groups"
     "searching-homology"
     "cones"
     "tensor-products"
     "coalgebras"
     "cobar"
     "algebras"
     "bar"
     "simplicial-sets"
     "simplicial-mrphs"
     "delta"
     "special-smsts"
     "suspensions"
     "disk-pasting"
     "cartesian-products"
     "eilenberg-zilber"
     "kan"
     "simplicial-groups"
     "fibrations"
     "loop-spaces"
     "ls-twisted-products"
     "lp-space-efhm"
     "classifying-spaces"
     "k-pi-n"
     "serre"
     "cs-twisted-products"
     "cl-space-efhm"
     "whitehead"
     "smith"
     ))

(DO ((mark +file-list+ (cdr mark))
     (i 1 (1+ i)))
    ((endp mark) (terpri))
   (format t "~% FILE ~2D: ~A" i (car mark)))

(DEFCONSTANT +SOURCE-EXTENSION+ ".cl")

(DEFCONSTANT +COMPILED-EXTENSION+ ".fasl")

(DEFUN LOAD-SFILES ()
  (mapc #'(lambda (file-name)
            (load (concatenate 'string file-name +source-extension+)))
    +file-list+)
  (princ "--- done ---")
  (values ))

(DEFVAR *CMBN-CONTROL*)
(SETF *CMBN-CONTROL* T)

(DEFUN COMPILE-FILES ()
  (format t
      "~%SPEED-PARAMETER = ~D~@
       ~% *CMBN-CONTROL* = ~A~2%"
    excl::.speed. *cmbn-control*)
  (mapc #'(lambda (file-name)
            (compile-file (concatenate 'string file-name +source-extension+))
            (load (concatenate 'string file-name +compiled-extension+)))
    +file-list+)
  (princ "--- done ---")
  (values))

(DEFUN LOAD-CFILES ()
  (mapc #'(lambda (file-name)
            (load (concatenate 'string file-name +compiled-extension+)))
    +file-list+)
  (princ "--- done ---")
  (values ))

(DEFUN CAT-FILES (file1)
  (declare (type string file1))
  (setf file1 (concatenate 'string file1 +source-extension+))
  (when (probe-file file1)
    (delete-file file1))
  (with-open-file (output file1 :direction :output)
    (dolist (file2 +file-list+)
      (declare (type string file2))
      (setf file2 (concatenate 'string file2 +source-extension+))
      (with-open-file (input file2 :direction :input)
        (do ((line (read-line input)
                   (read-line input nil :eof)))
            ((eq line :eof))
          (declare (type (or string (eql :eof)) line))
          (princ line output)
          (terpri output))))))