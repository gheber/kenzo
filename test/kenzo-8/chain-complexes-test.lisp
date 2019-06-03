;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test do-control
      (let ((comb (cat-8:do-control #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a 1 'b -1 'c))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (1 . b) (-1 . c)))))


      (signals simple-error (cat-8:do-control #'cat-8:s-cmpr
                              (cat-8:cmbn 0 1 'b 1 'b -1 'c)))

      (signals simple-error (cat-8:do-control #'cat-8:s-cmpr
                              (cat-8:cmbn 0 1 'a 1 'b -1 'b)))

      (setf cat-8:*cmbn-control* nil)
      (is (not (null (cat-8:do-control #'cat-8:s-cmpr
                       (cat-8:cmbn 0 1 'a 1 'b -1 'b)))))

      (setf cat-8:*cmbn-control* t)
      (signals simple-error (cat-8:do-control #'cat-8:s-cmpr
                              (cat-8:cmbn 0 1 'a 1 'b -1 'b))))


(test gnrt-?
      (progn
        (cat-8:cat-init)
        (let* ((cc (cat-8:build-chcm :cmpr #'cat-8:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-8:cmbn
                                                   (1- (cat-8:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (ff (cat-8:build-mrph :sorc cc :trgt cc :degr 0
                                   :intr #'(lambda (degr n)
                                             (cat-8:cmbn degr 1 n))
                                   :strt :gnrt :orgn '(test))))
          (setf cat-8:+too-much-time+ -1)
          (dotimes (i 20)
            (let* ((n (- (random 50) 50))
                   (comb (cat-8:gnrt-? ff 0 n)))
              (is (equal (cat-8:cmbn-degr comb) 0))
              (is (equal (cat-8:cmbn-list comb) (cons (cons 1 n) nil))))))))


(setf *n* 10)

(test cmbn-?
      (progn
        (compile 'ff)
        (cat-8:cat-init)
        (let* ((cc (cat-8:build-chcm :cmpr #'cat-8:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-8:cmbn
                                                   (1- (cat-8:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph (cat-8:build-mrph :sorc cc :trgt cc :degr 0
                                     :intr #'ff :strt :gnrt :orgn '(test))))

          (let ((comb (cat-8:cmbn-? mrph (cat-8:cmbn 0 1 100))))
            (is (equal (cat-8:cmbn-degr comb) 0))
            (setq comb (cat-8:cmbn-? mrph comb))
            (setq comb (cat-8:cmbn-? mrph comb))
            (setq comb (cat-8:cmbn-? mrph comb))
            (setq comb (cat-8:cmbn-? mrph comb))
            (setq comb (cat-8:cmbn-? mrph comb))))))
