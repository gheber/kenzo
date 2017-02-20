
(setf p (k-z2 1))

(setf sp (suspension p))

(setf s2p (suspension sp))

(setf s3p (suspension s2p))

(setf s4p (suspension s3p))

(homology s4p 5)

(setf ch5 (chml-clss s4p 5))

(setf f5 (z2-whitehead s4p ch5))

(setf x6 (fibration-total f5))

(homology x6 6)

(setf ch6 (chml-clss x6 6))

(setf f6 (z2-whitehead x6 ch6))

(setf x7 (fibration-total f6))

(homology x7 7)

(setf ch71 (chml-clss x7 7))

(setf f71 (z2-whitehead x7 ch71))

(setf x71 (fibration-total f71))

(homology x71 7)

(setf ch72 (chml-clss x71 7))

(setf f72 (z2-whitehead x71 ch72))

(setf x72 (fibration-total f72))

(homology x72 7)

(setf ch73 (chml-clss x72 7))

(setf f73 (z2-whitehead x72 ch73))

(setf x8 (fibration-total f73))

(homology x71 7)

(homology x71 8)


