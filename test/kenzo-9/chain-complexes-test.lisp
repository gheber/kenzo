;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo-9)

(test do-control
      (let ((comb (cat-9:do-control #'cat-9:s-cmpr (cat-9:cmbn 0 1 'a 1 'b -1 'c))))
        (is (equal (cat-9:cmbn-degr comb) 0))
        (is (equal (cat-9:cmbn-list comb) '((1 . a) (1 . b) (-1 . c)))))


      (signals simple-error (cat-9:do-control #'cat-9:s-cmpr
                              (cat-9:cmbn 0 1 'b 1 'b -1 'c)))

      (signals simple-error (cat-9:do-control #'cat-9:s-cmpr
                              (cat-9:cmbn 0 1 'a 1 'b -1 'b)))

      (setf cat-9:*cmbn-control* nil)
      (is (not (null (cat-9:do-control #'cat-9:s-cmpr
                       (cat-9:cmbn 0 1 'a 1 'b -1 'b)))))

      (setf cat-9:*cmbn-control* t)
      (signals simple-error (cat-9:do-control #'cat-9:s-cmpr
                              (cat-9:cmbn 0 1 'a 1 'b -1 'b))))


(test gnrt-?
      (progn
        (cat-9:cat-init)
        (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-9:cmbn
                                                   (1- (cat-9:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (ff (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                   :intr #'(lambda (degr n)
                                             (cat-9:cmbn degr 1 n))
                                   :strt :gnrt :orgn '(test))))
          (setf cat-9:+too-much-time+ -1)
          (dotimes (i 20)
            (let* ((n (- (random 50) 50))
                   (comb (cat-9:gnrt-? ff 0 n)))
              (is (equal (cat-9:cmbn-degr comb) 0))
              (is (equal (cat-9:cmbn-list comb) (cons (cons 1 n) nil))))))))


(setf *n* 10)

(test cmbn-?
      (progn
        (compile 'ff)
        (cat-9:cat-init)
        (let* ((cc (cat-9:build-chcm :cmpr #'cat-9:f-cmpr
                                   :basis :locally-effective
                                   :bsgn 0
                                   :intr-dffr #'(lambda (cmbn)
                                                  (cat-9:cmbn
                                                   (1- (cat-9:cmbn-degr cmbn))))
                                   :strt :cmbn
                                   :orgn '(Z of Z)))
               (mrph (cat-9:build-mrph :sorc cc :trgt cc :degr 0
                                     :intr #'ff :strt :gnrt :orgn '(test))))

          (let ((comb (cat-9:cmbn-? mrph (cat-9:cmbn 0 1 100))))
            (is (equal (cat-9:cmbn-degr comb) 0))
            (setq comb (cat-9:cmbn-? mrph comb))
            (setq comb (cat-9:cmbn-? mrph comb))
            (setq comb (cat-9:cmbn-? mrph comb))
            (setq comb (cat-9:cmbn-? mrph comb))
            (setq comb (cat-9:cmbn-? mrph comb))))))
