;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)


(test -1-expt-n
      (is (equal -1 (cat:-1-expt-n 5)))
      (is (equal 1 (cat:-1-expt-n 6))))


(test -1-expt-n+1
      (is (equal 1 (cat:-1-expt-n+1 5)))
      (is (equal -1 (cat:-1-expt-n+1 6))))


(test -1-expt-n-1
      (is (equal 1 (cat:-1-expt-n+1 5)))
      (is (equal -1 (cat:-1-expt-n+1 6))))

(test lexico
      (setf cl:*print-level* nil)
      (pprint (macroexpand-1
               '(cat:lexico
                 comparison1
                 comparison2
                 comparison3
                 comparison4))))

(test with-term
      (macroexpand-1 '(cat:with-term (cffc gnrt) term
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat:with-term (nil gnrt) term
                       (statement-1)
                       (statement-2))))

(test with--term
      (macroexpand-1 '(cat:with--term (cffc gnrt) mark
                       (statement-1)
                       (statement-2))))


(test with-cmbn
      (macroexpand-1 '(cat:with-cmbn (degr list) cmbn
                       (statement-1)
                       (statement-2))))


(test cmpr
      (macroexpand '(cat:cmpr chcm))
      (macroexpand '(cat:cmpr chcm item1 item2)))


(test dffr
      (macroexpand '(cat:dffr chcm))
      (macroexpand '(cat:dffr chcm cmbn))
      (macroexpand '(cat:dffr chcm degr gnrt)))


(test i-cmps
      (macroexpand '(cat:i-cmps m1))
      (macroexpand '(cat:i-cmps m1 m2))
      (macroexpand '(cat:i-cmps m1 m2 m3)))


(test i-add
      (macroexpand '(cat:i-add m1))
      (macroexpand '(cat:i-add m1 m2))
      (macroexpand '(cat:i-add m1 m2 m3)))


(test i-sbtr
      (macroexpand '(cat:i-sbtr m1 m2))
      (macroexpand '(cat:i-sbtr m1 m2 m3)))


(test f
      (macroexpand '(cat:f rdct))
      (macroexpand '(cat:f rdct cmbn))
      (macroexpand '(cat:f rdct degr gnrt)))


(test tnpr
      (macroexpand '(cat:tnpr 1 a 2 b))
      (cat:tnpr 2 'a 4 (cat:tnpr 2 'b 2 'c)))


(test with-tnpr
      (macroexpand-1 '(cat:with-tnpr (degr1 gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2)))
      (macroexpand-1 '(cat:with-tnpr (nil gnrt1 degr2 gnrt2) tnpr
                       (statement-1)
                       (statement-2))))


(test crpr
      (cat:crpr (cat:absm 3 'a) (cat:absm 4 'b))
      (cat:crpr 3 'a 4 'b)
      (macroexpand '(cat:crpr absm1 absm2))
      (macroexpand '(cat:crpr dgop1 gmsm1 dgop2 gmsm2))
      (let ((c (cat:crpr 1 'a 2 'b)))
        (cat:dgop1 c)
        (cat:gmsm1 c)
        (cat:dgop2 c)
        (cat:gmsm2 c)
        (cat:absm1 c)
        (cat:absm2 c)))


(test with-crpr
      (cat:with-crpr (absm1 absm2) (cat:crpr 3 'a 4 'b) (list absm1 absm2))
      (macroexpand-1
       (macroexpand-1 '(cat:with-crpr (dgop1 gmsm1 dgop2 gmsm2) crpr
                        (statement-1)
                        (statement-2))))
      (macroexpand-1
       (macroexpand-1 '(cat:with-crpr (absm1 absm2) crpr
                        (statement-1)
                        (statement-2)))))
