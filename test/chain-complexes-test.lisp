;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test)

(in-suite :kenzo)


(test do-control
      (let ((comb (cat:do-control #'cat:s-cmpr (cat:cmbn 0 1 'a 1 'b -1 'c))))
        (is (equal (cat:cmbn-degr comb) 0))
        (is (equal (cat:cmbn-list comb) '((1 . a) (1 . b) (-1 . c)))))

      (signals simple-error (cat:do-control #'cat:s-cmpr
                              (cat:cmbn 0 1 'b 1 'b -1 'c)))

      (signals simple-error (cat:do-control #'cat:s-cmpr
                              (cat:cmbn 0 1 'a 1 'b -1 'b)))

      (setf cat:*cmbn-control* nil)
      (is (not (null (cat:do-control #'cat:s-cmpr
                       (cat:cmbn 0 1 'a 1 'b -1 'b)))))
      (setf cat:*cmbn-control* t)
      (signals simple-error (cat:do-control #'cat:s-cmpr
                              (cat:cmbn 0 1 'a 1 'b -1 'b))))

(test gnrt-?
      (progn
        (cat:cat-init)
        (let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat:cmbn
                                                   (1- (cat:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (ff (cat:build-mrph :sorc cc :trgt cc :degr 0
                                   :intr #'(lambda (degr n)
                                             (cat:cmbn degr 1 n))
                                   :strt :gnrt :orgn '(test))))
          (setf cat:+too-much-time+ -1)
          (dotimes (i 20)
            (let* ((n (- (random 50) 50))
                   (comb (cat:gnrt-? ff 0 n)))
              (is (equal (cat:cmbn-degr comb) 0))
              (is (equal (cat:cmbn-list comb) (cons (cons 1 n) nil))))))))


(setf *n* 10)

(test cmbn-?
      (progn
        (compile 'ff)
        (cat:cat-init)
        (let* ((cc (cat:build-chcm :cmpr #'cat:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat:cmbn
                                                   (1- (cat:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph (cat:build-mrph :sorc cc :trgt cc :degr 0
                                     :intr #'ff :strt :gnrt :orgn '(test))))

          (let ((comb (cat:cmbn-? mrph (cat:cmbn 0 1 100))))
            (is (equal (cat:cmbn-degr comb) 0))
            (setq comb (cat:cmbn-? mrph comb))
            (setq comb (cat:cmbn-? mrph comb))
            (setq comb (cat:cmbn-? mrph comb))
            (setq comb (cat:cmbn-? mrph comb))
            (setq comb (cat:cmbn-? mrph comb))))))
