(load "common.lisp")

(ql:quickload "kenzo")
;;(use-package :cat-7)
;;(use-package :cat-8)
;;(use-package :cat-9)
(use-package :cat)

(==> (def smp-deltab2
         (build-smst
          :cmpr #'(lambda (gsm1 gsm2)
                    (if (rest gsm1) (l-cmpr gsm1 gsm2) :equal))
          :basis :locally-effective
          :bspn '(0)
          :face #'(lambda (i dmn gsm)
                    (case dmn
                      (0 (error "No face in dimension 0"))
                      (1 (error "No non-degenerate simplex in dimension 1"))
                      (2 (absm 1 '(0)))
                      (otherwise (absm 0 (append (subseq gsm 0 i)
                                                 (subseq gsm (1+ i)))))))
          :orgn '(simple-deltab2))))

(==> (cmpr smp-deltab2 '(5) '(0)))

(==> (? smp-deltab2 2 '(0 1 2)))

(==> (? smp-deltab2 3 '(0 1 2 3)))

(==> (def precobar (vrtc-cobar smp-deltab2)))

(==> (bsgn precobar))

(==> (def loop-1 (allp 2 '(0 1 2 3))))

(==> (def loop-2 (allp 2 '(1 2 3 4))))

(==> (cmpr precobar loop-1 loop-2))

(==> (? precobar 2 loop-1))

(==> (? precobar (? precobar 2 loop-1)))

(==> (def loop-3 (allp 3 '(0 1 2 3 4) 3 '(1 3 5 7 9))))

(==> (? precobar 6 loop-3))

(==> (? precobar (? precobar 6 loop-3)))

(==> (def loop-4 (allp 3 '(0 1 4 5 6) 4 '(2 3 4 5 6 8) 4 '(0 4 5 6 7 8))))

(==> (? precobar 11 loop-4))

(==> (? precobar (? precobar 11 loop-4)))

(==> (cprd smp-deltab2 2 '(0 1 2)))

(==> (cprd smp-deltab2 3 '(1 2 3 4)))

(==> (cprd smp-deltab2 4 '(0 1 2 3 4)))

(==> (def dh-mrph (cobar-hrzn-dffr smp-deltab2)))

(==> loop-1)

(==> (? dh-mrph 2 loop-1))

(==> loop-3)

(==> (? dh-mrph 6 loop-3))

(==> (? dh-mrph (? dh-mrph 6 loop-3)))

(==> loop-4)

(==> (? dh-mrph 11 loop-4))

(==> (? dh-mrph (? dh-mrph 11 loop-4)))

(==> (def cobar-deltab2 (cobar smp-deltab2)))

(==> loop-3)

(==> (? cobar-deltab2 6 loop-3))

(==> (? cobar-deltab2 (? cobar-deltab2 6 loop-3)))

(==> loop-4)

(==> (? cobar-deltab2 11 loop-4))

(==> (? cobar-deltab2 (? cobar-deltab2 11 loop-4)))

(sb-ext:exit)
