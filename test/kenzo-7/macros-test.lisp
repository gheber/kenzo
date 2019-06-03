;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test -1-expt-n
      (is (equal -1 (cat-7:-1-expt-n 5)))
      (is (equal 1 (cat-7:-1-expt-n 6))))


(test -1-expt-n+1
      (is (equal 1 (cat-7:-1-expt-n+1 5)))
      (is (equal -1 (cat-7:-1-expt-n+1 6))))


(test -1-expt-n-1
      (is (equal 1 (cat-7:-1-expt-n+1 5)))
      (is (equal -1 (cat-7:-1-expt-n+1 6))))

(test lexico
      (setf cl:*print-level* nil)
      (pprint (macroexpand-1
               '(cat-7:lexico
                 comparison1
                 comparison2
                 comparison3
                 comparison4))))

(test with-term
      (macroexpand-1 '(cat-7:with-term (cffc gnrt) term
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat-7:with-term (nil gnrt) term
                       (statement-1)
                       (statement-2))))

(test with--term
      (macroexpand-1 '(cat-7:with--term (cffc gnrt) mark
                       (statement-1)
                       (statement-2))))


(test with-cmbn
      (macroexpand-1 '(cat-7:with-cmbn (degr list) cmbn
                       (statement-1)
                       (statement-2))))


(test cmpr
      (macroexpand '(cat-7:cmpr chcm))
      (macroexpand '(cat-7:cmpr chcm item1 item2)))


(test dffr
      (macroexpand '(cat-7:dffr chcm))
      (macroexpand '(cat-7:dffr chcm cmbn))
      (macroexpand '(cat-7:dffr chcm degr gnrt)))


(test i-cmps
      (macroexpand '(cat-7:i-cmps m1))
      (macroexpand '(cat-7:i-cmps m1 m2))
      (macroexpand '(cat-7:i-cmps m1 m2 m3)))


(test i-add
      (macroexpand '(cat-7:i-add m1))
      (macroexpand '(cat-7:i-add m1 m2))
      (macroexpand '(cat-7:i-add m1 m2 m3)))


(test i-sbtr
      (macroexpand '(cat-7:i-sbtr m1 m2))
      (macroexpand '(cat-7:i-sbtr m1 m2 m3)))


(test f
      (macroexpand '(cat-7:f rdct))
      (macroexpand '(cat-7:f rdct cmbn))
      (macroexpand '(cat-7:f rdct degr gnrt)))


(test tnpr
      (macroexpand '(cat-7:tnpr 1 a 2 b))
      (cat-7:tnpr 2 'a 4 (cat-7:tnpr 2 'b 2 'c)))


(test with-tnpr
      (macroexpand-1 '(cat-7:with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat-7:with-tnpr (nil gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2))))


(test crpr
      (cat-7:crpr (cat-7:absm 3 'a) (cat-7:absm 4 'b))
      (cat-7:crpr 3 'a 4 'b)
      (macroexpand '(cat-7:crpr absm1 absm2))
      (macroexpand '(cat-7:crpr dgop1 gmsm1 dgop2 gmsm2))
      (let ((c (cat-7:crpr 1 'a 2 'b)))
        (cat-7:dgop1 c)
        (cat-7:gmsm1 c)
        (cat-7:dgop2 c)
        (cat-7:gmsm2 c)
        (cat-7:absm1 c)
        (cat-7:absm2 c)))


(test with-crpr
      (cat-7:with-crpr (absm1 absm2) (cat-7:crpr 3 'a 4 'b) (list absm1 absm2))
      (macroexpand-1
       (macroexpand-1 '(cat-7:with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                        (statement-1)
                        (statement-2))))
      (macroexpand-1
       (macroexpand-1 '(cat-7:with-crpr (absm1 absm2) crpr
                        (statement-1)
                        (statement-2)))))
