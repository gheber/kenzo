(in-package "CAT")
(load "file-list")
(load-cfiles)

(cat-init)

(setf s3 (sphere 3))
(setf oos3 (loop-space s3 2))
(homology oos3 5)

(cat-init)

(setf p (r-proj-space 4))
(setf oop (loop-space p 2))
(setf faces
  (list (loop3 0 (loop3 0 'p 1) 2)
        +null-loop+ +null-loop+ +null-loop+ ))
(setf doop (disk-pasting oop 3 'D3 faces))
(setf odoop (loop-space doop))
(homology odoop 4)

(cat-init)

(setf s3 (sphere 3))
(setf ch3 (chml-clss s3 3))
(setf f3 (z-whitehead s3 ch3))
(setf x4 (fibration-total f3))
(homology x4 4)

(setf ch4 (chml-clss x4 4))
(setf f4 (z2-whitehead x4 ch4))
(setf x5 (fibration-total f4))
(homology x5 5)

(setf ch5 (chml-clss x5 5))
(setf f5 (z2-whitehead x5 ch5))

(right-serre-efhm f5)

(setf x6 (fibration-total f5))
(homology x6 6)

