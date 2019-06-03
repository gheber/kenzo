;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test -1-expt-n
      (is (equal -1 (cat-8:-1-expt-n 5)))
      (is (equal 1 (cat-8:-1-expt-n 6))))


(test -1-expt-n+1
      (is (equal 1 (cat-8:-1-expt-n+1 5)))
      (is (equal -1 (cat-8:-1-expt-n+1 6))))


(test -1-expt-n-1
      (is (equal 1 (cat-8:-1-expt-n+1 5)))
      (is (equal -1 (cat-8:-1-expt-n+1 6))))

(test lexico
      (setf cl:*print-level* nil)
      (pprint (macroexpand-1
               '(cat-8:lexico
                 comparison1
                 comparison2
                 comparison3
                 comparison4))))

(test with-term
      (macroexpand-1 '(cat-8:with-term (cffc gnrt) term
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat-8:with-term (nil gnrt) term
                       (statement-1)
                       (statement-2))))

(test with--term
      (macroexpand-1 '(cat-8:with--term (cffc gnrt) mark
                       (statement-1)
                       (statement-2))))


(test with-cmbn
      (macroexpand-1 '(cat-8:with-cmbn (degr list) cmbn
                       (statement-1)
                       (statement-2))))


(test cmpr
      (macroexpand '(cat-8:cmpr chcm))
      (macroexpand '(cat-8:cmpr chcm item1 item2)))


(test dffr
      (macroexpand '(cat-8:dffr chcm))
      (macroexpand '(cat-8:dffr chcm cmbn))
      (macroexpand '(cat-8:dffr chcm degr gnrt)))


(test i-cmps
      (macroexpand '(cat-8:i-cmps m1))
      (macroexpand '(cat-8:i-cmps m1 m2))
      (macroexpand '(cat-8:i-cmps m1 m2 m3)))


(test i-add
      (macroexpand '(cat-8:i-add m1))
      (macroexpand '(cat-8:i-add m1 m2))
      (macroexpand '(cat-8:i-add m1 m2 m3)))


(test i-sbtr
      (macroexpand '(cat-8:i-sbtr m1 m2))
      (macroexpand '(cat-8:i-sbtr m1 m2 m3)))


(test f
      (macroexpand '(cat-8:f rdct))
      (macroexpand '(cat-8:f rdct cmbn))
      (macroexpand '(cat-8:f rdct degr gnrt)))


(test tnpr
      (macroexpand '(cat-8:tnpr 1 a 2 b))
      (cat-8:tnpr 2 'a 4 (cat-8:tnpr 2 'b 2 'c)))


(test with-tnpr
      (macroexpand-1 '(cat-8:with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat-8:with-tnpr (nil gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2))))


(test crpr
      (cat-8:crpr (cat-8:absm 3 'a) (cat-8:absm 4 'b))
      (cat-8:crpr 3 'a 4 'b)
      (macroexpand '(cat-8:crpr absm1 absm2))
      (macroexpand '(cat-8:crpr dgop1 gmsm1 dgop2 gmsm2))
      (let ((c (cat-8:crpr 1 'a 2 'b)))
        (cat-8:dgop1 c)
        (cat-8:gmsm1 c)
        (cat-8:dgop2 c)
        (cat-8:gmsm2 c)
        (cat-8:absm1 c)
        (cat-8:absm2 c)))


(test with-crpr
      (cat-8:with-crpr (absm1 absm2) (cat-8:crpr 3 'a 4 'b) (list absm1 absm2))
      (macroexpand-1
       (macroexpand-1 '(cat-8:with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                        (statement-1)
                        (statement-2))))
      (macroexpand-1
       (macroexpand-1 '(cat-8:with-crpr (absm1 absm2) crpr
                        (statement-1)
                        (statement-2)))))
