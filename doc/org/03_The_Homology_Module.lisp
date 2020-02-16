(load "common.lisp")

(ql:quickload "kenzo")
;;(use-package :cat-7)
;;(use-package :cat-8)
;;(use-package :cat-9)
(use-package :cat)

(==> (def diabolo (build-chcm
                   :cmpr #'s-cmpr
                   :basis #'(lambda (dmn)
                              (case dmn
                                (0 '(s0 s1 s2 s3 s4 s5))
                                (1 '(s01 s02 s12 s23 s34 s35 s45))
                                (2 '(s345))
                                (otherwise nil)))
                   :bsgn 's0
                   :intr-dffr #'(lambda (dmn gnr)
                                  (unless (<= 0 dmn 2)
                                    (error "Incorrect dimension for diabolo-dp."))
                                  (case dmn
                                    (0 (cmbn -1))  ; Note the null combination of degree -1
                                    (1 (case gnr
                                         (s01 (cmbn 0 -1 's0 1 's1))
                                         (s02 (cmbn 0 -1 's0 1 's2))
                                         (s12 (cmbn 0 -1 's1 1 's2))
                                         (s23 (cmbn 0 -1 's2 1 's3))
                                         (s34 (cmbn 0 -1 's3 1 's4))
                                         (s35 (cmbn 0 -1 's3 1 's5))
                                         (s45 (cmbn 0 -1 's4 1 's5))))
                                    (2 (case gnr
                                         (s345 (cmbn 1 1 's34 -1 's35 1 's45))))
                                    (otherwise (error "Bad generator for complex diabolo"))))
                   :strt :GNRT
                   :orgn '(diabolo-for-example))))

(==> (chcm-homology diabolo 0))

(==> (chcm-homology diabolo 1))

(==> (chcm-homology diabolo 2))

(==> (def duncehat-basis #'(lambda (dmn)
                             (case dmn
                               (0 '((0) (1) (2) (3) (4) (5) (6) (7)))
                               (1 '((0 1) (0 2) (0 3)
                                    (0 4) (0 5) (0 6)
                                    (0 7) (1 2) (1 3)
                                    (1 4) (1 5) (1 6)
                                    (1 7) (2 3) (2 4)
                                    (2 6) (2 7) (3 4)
                                    (3 5) (4 5) (4 6)
                                    (5 6) (5 7) (6 7)))
                               (2 '((0 1 5) (0 1 6) (0 1 7)
                                    (0 2 3) (0 2 4) (0 2 6)
                                    (0 3 4) (0 5 7) (1 2 3)
                                    (1 2 4) (1 2 7) (1 3 5)
                                    (1 4 6) (2 6 7) (3 4 5)
                                    (4 5 6) (5 6 7)))
                               (otherwise nil)))))

(==> (def duncehat-df #'(lambda (dmn gnr)
                          (case dmn
                            (0 (cmbn -1))
                            (1 (cmbn 0 -1 (list (first gnr)) 1 (rest gnr)))
                            (2 (cmbn 1 1 (list (first gnr) (second gnr))
                                     -1 (list (first gnr) (third gnr))
                                     1 (rest gnr)))
                            (otherwise nil)))))

(==> (def duncehat (build-chcm :cmpr #'l-cmpr
                               :basis duncehat-basis
                               :bsgn '(0)
                               :intr-dffr duncehat-df
                               :strt :gnrt
                               :orgn '(dunce hat))))

(==> (chcm-homology duncehat 0))

(==> (chcm-homology duncehat 1))

(==> (chcm-homology duncehat 2))

(==> (def mz (chcm-mat duncehat 1)))

(==> (def nb (chcm-mat duncehat 2)))

(==> (homologie mz nb))

(==> (cat-init))

(==> (def s2 (sphere 2)))

(==> (inspect s2))

(==> (homology s2 1))

(==> (inspect s2))

(==> (orgn (hmeq 9)))

(==> (def os2 (loop-space s2)))

(==> (efhm os2))

(==> (inspect os2))

(==> (def s4 (sphere 4)))

(==> (inspect s4))

(==> (def ooos4 (loop-space (loop-space (loop-space s4)))))

(==> (orgn ooos4))

(==> (orgn (second (orgn ooos4))))

(==> (orgn (second (orgn (second (orgn ooos4))))))

(==> (orgn (second (orgn (second (orgn (second (orgn ooos4))))))))

(==> (inspect ooos4))

(==> (inspect (smgr 124)))

(==> (inspect (smst 119)))

(==> (efhm ooos4))

(==> (inspect (smgr 124)))

(==> (inspect (smst 119)))

(sb-ext:exit)
