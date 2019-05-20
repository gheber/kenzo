;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES
;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES
;;;  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES  FILES

(IN-PACKAGE "COMMON-LISP-USER")

(DEFUN KENZO-VERSION ()
  (format t "~%*** Kenzo-Version 9 ***~2%")
  (values))

(DECLAIM (OPTIMIZE (speed 3) (safety 1) (space 0) (debug 0)))

#+SBCL
(DECLAIM (SB-EXT:MUFFLE-CONDITIONS style-warning compiler-note))

(DEFCONSTANT +FILE-LIST+
    '("macros"
      "various"
      "classes"
      "combinations"
      "chain-complexes"
      "chcm-elementary-op"
      "effective-homology"
      "homology-groups"
      "searching-homology"
      "cones"
      "bicones"
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
      "chain-complexes-dvf"
      "cartesian-products"
      "eilenberg-zilber"
      "kan"
      "simplicial-groups"
      "fibrations"
      "fibrations-dvf"
      "loop-spaces"
      "ls-twisted-products"
      "lp-space-efhm"
      "classifying-spaces"
      "classifying-spaces-dvf"
      "k-pi-n"
      "whitehead"
      "new-smith"
      ))

(DO ((i 1 (1+ i))
     (mark +file-list+ (cdr mark)))
    ((endp mark) (terpri))
   (format t "~% FILE ~2D: ~A" i (car mark)))

(DEFCONSTANT +SOURCE-EXTENSION+
  #+(or allegro sbcl) "lisp"
  #-(or allegro sbcl)
    (error "Not an Allegro or Sbcl environment."))

(DEFCONSTANT +COMPILED-EXTENSION+ 
    #+allegro "afasl"
    #+sbcl "sfasl")

(DEFUN LOAD-SFILES ()
   (mapc #'(lambda (file-name)
             (load (concatenate 'string file-name "." +source-extension+)))
    +file-list+))

(DEFVAR *CMBN-CONTROL*)
(SETF *CMBN-CONTROL* T)

(DEFUN COMPILE-FILES ()
  (format t "~%*CMBN-CONTROL*  = ~A~2%" *cmbn-control*)
  (mapc #'(lambda (file-name)
            (compile-file (concatenate 'string file-name "." +source-extension+)
                          :output-file (concatenate 'string file-name "." +compiled-extension+))
            (load (concatenate 'string file-name "." +compiled-extension+)))
    +file-list+))

(DEFUN LOAD-CFILES ()
   (mapc #'(lambda (file-name)
             (load (concatenate 'string file-name "." +compiled-extension+)))
    +file-list+))

(KENZO-VERSION)

