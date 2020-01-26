(defparameter *echo* t)

(defmacro ==> (&rest body)
  `(progn
     (when *echo*
       (pprint (quote ,@body))
       (pprint '==>))
     (pprint ,@body)
     (terpri)))

(ql:quickload "kenzo")
;;(use-package :cat-7)
;;(use-package :cat-8)
;;(use-package :cat-9)
(use-package :cat)

(==> (f-cmpr 123 789))

(==> (s-cmpr 'circulation 'circular))

(==> (s-cmpr 'qwerty 'qwerty))

(==> (l-cmpr '(1 a b) '(1 a)))

(==> (def comb1 (cmbn 1 1 'u 2 'v 3 'w 4 'z)))

(==> (cmbn-non-zero-p comb1))

(==> (cmbn-list comb1))

(==> (def term3 (third (cmbn-list comb1))))

(==> (cffc term3))

(==> (gnrt term3))

(==> (def mcomb1 (cmbn-opps comb1)))

(==> (def comb2 (n-cmbn 10 comb1)))

(==> (def cmb12 (2cmbn-add #'s-cmpr comb1 comb2)))

(==> (2cmbn-sbtr #'s-cmpr comb1 cmb12))

(==> (ncmbn-add #'s-cmpr
                comb1 comb2 comb1 comb2 comb1 comb2 comb1 comb2 comb1 comb2))

(==> (def diabolo-cmpr #'s-cmpr))

(==> (def diabolo-basis #'(lambda (dmn)
                            (case dmn
                              (0 '(s0 s1 s2 s3 s4 s5))
                              (1 '(s01 s02 s12 s23 s34 s35 s45))
                              (2 '(s345))
                              (otherwise nil)))))

(==> (def diabolo-bspn 's0))

(==> (def diabolo-pure-dffr
         #'(lambda (dmn gnr)
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
               (otherwise (error "Bad generator for complex diabolo"))))))

(==> (def diabolo-strt :GNRT))

(==> (def diabolo-orgn '(diabolo-for-example)))

(==> (def diabolo (build-chcm :cmpr diabolo-cmpr :basis diabolo-basis
                              :bsgn diabolo-bspn :intr-dffr diabolo-pure-dffr
                              :strt diabolo-strt :orgn diabolo-orgn)))

(==> *chcm-list*)

(==> (chcm 1))

(==> (chcm 1))

(==> (idnm diabolo))

(==> (idnm diabolo))

(==> (basis diabolo 0))

(==> (basis diabolo 1))

(==> (basis diabolo 2))

(==> (basis diabolo 10))

(==> (dffr diabolo 2 's345))

(==> (dffr diabolo (dffr diabolo 2 's345)))

(==> (def ZCC
         (the chain-complex
              (build-chcm
               :cmpr #'(lambda (gnrt1 gnrt2)
                         (declare (ignore gnrt1 gnrt2))
                         (the cmpr :equal))
               :basis #'(lambda (n)
                          (the list
                               (if (zerop n) '(:Z-gnrt) +empty-list+)))
               :bsgn :Z-gnrt
               :intr-dffr #'(lambda (cmbn)
                              (the cmbn (zero-cmbn (1- (cmbn-degr cmbn)))))
               :strt :cmbn
               :orgn '(zcc-constant)))))

(==> (defun MY-CIRCLE ()
       (the chain-complex
            (build-chcm
             :cmpr #'(lambda (gnrt1 gnrt2)
                       (declare (ignore gnrt1 gnrt2))
                       (the cmpr :equal))
             :basis #'(lambda (dmns)
                        (the list
                             (case dmns (0 '(*)) (1 '(s1))
                                   (otherwise +empty-list+))))
             :bsgn '*
             :intr-dffr #'zero-intr-dffr
             :strt :cmbn
             :orgn '(circle)))))

(==> (cat-init))

(==> (def ZCC (z-chcm)))

(==> (def zero-morphism (build-mrph :sorc ZCC
                                    :trgt ZCC
                                    :degr -1
                                    :intr #'(lambda (comb)
                                              (cmbn (1- (degr comb))))
                                    :strt :cmbn
                                    :orgn '(zero morphism on ZCC))))

(==> (def id-morphism (build-mrph :sorc ZCC
                                  :trgt ZCC
                                  :degr 0
                                  :intr #'identity
                                  :strt :cmbn
                                  :orgn '(identity morphism on ZCC))))

(==> (def ccn-boundary #'(lambda (dgr gnr)
                           (if (evenp (+ dgr gnr))
                               (cmbn (1- dgr) 1 (- gnr 10))
                               (cmbn (1- dgr))))))

(==> (def ccn (build-chcm :cmpr #'f-cmpr
                          :basis #'(lambda (n) (<a-b< (* 10 n) (* 10 (1+ n))))
                          :bsgn 0
                          :intr-dffr ccn-boundary
                          :strt :gnrt
                          :orgn '(ccn))))

(==> (def upper-shift (build-mrph
                       :sorc ccn :trgt ccn :strt :gnrt :degr +1
                       :intr #'(lambda (d gn) (cmbn (1+ d) 1 (+ gn 10)))
                       :orgn '(ccn shift +10))))

(==> (def lower-shift (build-mrph
                       :sorc ccn :trgt ccn :strt :gnrt :degr -1
                       :intr #'(lambda (d gn) (cmbn (1- d) 1 (- gn 10)))
                       :orgn '(ccn shift -10))))

(==> (? ccn 2 22))

(==> (? ccn (? ccn 2 22)))

(==> (def combn (cmbn 5 1 50 5 55 9 59)))

(==> (? ccn combn))

(==> (? ccn(? ccn combn)))

(==> (? upper-shift 0 6))

(==> (? lower-shift 5 51))

(==> (? lower-shift (? lower-shift 5 51)))

(==> (def comb1 (cmbn 1 1 10 2 11 3 12 4 13)))

(==> (def identity? (cmps upper-shift lower-shift)))

(==> (degr identity?))

(==> (? identity? comb1))

(==> (2cmbn-sbtr (cmpr ccn) comb1 (? identity? comb1)))

(==> (def upper-shift2 (cmps upper-shift upper-shift)))

(==> (degr upper-shift2))

(==> (? upper-shift2 comb1))

(==> (def twice-up-shift (add upper-shift upper-shift)))

(==> (degr twice-up-shift))

(==> (? twice-up-shift comb1))

(==> (def up-d (cmps upper-shift (dffr1 ccn))))

(==> (def d-up (cmps (dffr1 ccn) upper-shift)))

(==> (? up-d 1 11))

(==> (? d-up 1 11))

(==> (def comb3 (cmbn 1 1 10 2 11 3 12 4 13 5 14 6 15)))

(==> (? up-d comb3))

(==> (? d-up comb3))

(==> (k 1))

(==> (kd 1))

(==> (k 3))

(==> (kd 3))

(==> (kd 8))

(==> (kd 5))

(==> (kd 9))

(==> (kd2 9))

(sb-ext:exit)
