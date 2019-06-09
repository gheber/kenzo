;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)

(test f-cmpr
      (is (equal (cat-8:f-cmpr 1 2) :less))
      (is (equal (cat-8:f-cmpr 2 2) :equal))
      (is (equal (cat-8:f-cmpr 3 2) :greater))
      (is (equal (cat-8:f-cmpr 123 789) :less))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr '(1 2 3) '(1 3)) :less))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr '(1 2 3) '(1 1)) :greater))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr '(1 1) '(1 1 0)) :less))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr '(1 1 0) '(1 1)) :greater))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr '(1 1) '(1 1)) :equal))
      (is (equal (cat-8:maplexico #'cat-8:f-cmpr nil nil) :equal)))

(test l-cmpr
      (is (equal (cat-8:l-cmpr nil nil) :equal))
      (is (equal (cat-8:l-cmpr nil '(1)) :less))
      (is (equal (cat-8:l-cmpr '(1) nil) :greater))
      (is (equal (cat-8:l-cmpr '(a) '(a)) :equal))
      (is (equal (cat-8:l-cmpr '(a) '(1)) :greater))
      (is (equal (cat-8:l-cmpr '(1) '(a)) :less))
      (is (equal (cat-8:l-cmpr '(1) '(1)) :equal))
      (is (equal (cat-8:l-cmpr '(1 a) '(1 1)) :greater))
      (is (equal (cat-8:l-cmpr '(1 a b) '(1 a)) :greater))
      (is (equal (cat-8:l-cmpr '(1 a) '(1 a b)) :less)))

(test s-cmpr
      (is (equal (cat-8:s-cmpr 'a 'b) :less))
      (is (equal (cat-8:s-cmpr 'b 'b) :equal))
      (is (equal (cat-8:s-cmpr 'c 'b) :greater))
      (is (equal (cat-8:s-cmpr 'circulation 'circular) :greater))
      (is (equal (cat-8:s-cmpr 'qwerty 'qwerty) :equal)))

(test cmbn
      (let ((comb (cat-8:cmbn 2)))
        (is (equal (cat-8:cmbn-degr comb) 2))
        (is (equal (cat-8:cmbn-list comb) nil)))

      (let ((comb (cat-8:cmbn 2 2 'a)))
        (is (equal (cat-8:cmbn-degr comb) 2))
        (is (equal (cat-8:cmbn-list comb) '((2 . a)))))

      (let ((comb (cat-8:cmbn 2 'a)))
        (is (equal (cat-8:cmbn-degr comb) 2))
        (is (equal (cat-8:cmbn-list comb) '((a)))))

      (let ((comb (cat-8:cmbn 2 2 'a -3 'b)))
        (is (equal (cat-8:cmbn-degr comb) 2))
        (is (equal (cat-8:cmbn-list comb) '((2 . a) (-3 . b)))))

      (let ((comb (cat-8:term-cmbn 3 -5 'a)))
        (is (equal (cat-8:cmbn-degr comb) 3))
        (is (equal (cat-8:cmbn-list comb) '((-5 . a))))))

(test zero-cmbn
      (let ((comb (cat-8:zero-cmbn 3)))
        (is (equal (cat-8:cmbn-degr comb) 3))
        (is (equal (cat-8:cmbn-list comb) nil)))

      (let ((comb (cat-8:zero-intr-dffr (cat-8:cmbn 2))))
        (is (equal (cat-8:cmbn-degr comb) 1))
        (is (equal (cat-8:cmbn-list comb) nil)))

      (is (null (cat-8:cmbn-non-zero-p (cat-8:cmbn 0))))
      (is (cat-8:cmbn-non-zero-p (cat-8:cmbn 0 1 'a)))
      (is (cat-8:cmbn-zero-p (cat-8:cmbn 0)))
      (is (null (cat-8:cmbn-zero-p (cat-8:cmbn 0 1 'a)))))

(test cmbn-opps
      (let* ((comb (cat-8:cmbn 0 1 'a -2 'b))
             (comb1 (cat-8:cmbn-opps comb)))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (-2 . b))))
        (is (equal (cat-8:cmbn-degr comb1) 0))
        (is (equal (cat-8:cmbn-list comb1) '((-1 . a) (2 . b))))))

(test n-cmbn
      (let* ((comb (cat-8:cmbn 2 3 'a))
             (comb1 (cat-8:n-cmbn 1 comb))
             (comb2 (cat-8:n-cmbn -1 comb))
             (comb3 (cat-8:n-cmbn -3 comb)))
        (is (eq comb comb1))
        (is (equal (cat-8:cmbn-degr comb2) 2))
        (is (equal (cat-8:cmbn-list comb2) '((-3 . a))))
        (is (equal (cat-8:cmbn-degr comb3) 2))
        (is (equal (cat-8:cmbn-list comb3) '((-9 . a))))))

(test 2cmbn-add
      (signals simple-error (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0) (cat-8:cmbn 1)))

      (is (cat-8:cmbn-zero-p (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0) (cat-8:cmbn 0))))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a) (cat-8:cmbn 0))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a)))))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a) (cat-8:cmbn 0 2 'a))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((3 . a)))))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a) (cat-8:cmbn 0 -1 'a))))
        (is (cat-8:cmbn-zero-p comb)))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a) (cat-8:cmbn 0 2 'b))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (2 . b)))))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 1 'a -2 'b)
                                 (cat-8:cmbn 0 2 'b))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a)))))

      (let ((comb (cat-8:2cmbn-add #'cat-8:s-cmpr (cat-8:cmbn 0 2 'b)
                                 (cat-8:cmbn 0 1 'a 3 'c))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (2 . b) (3 . c))))))

(test 2cmbn-sbtr
      (signals simple-error (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0) (cat-8:cmbn 1)))

      (is (cat-8:cmbn-zero-p (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0) (cat-8:cmbn 0))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0) (cat-8:cmbn 0 1 'a))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . a)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0 2 'b) (cat-8:cmbn 0))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((2 . b)))))

      (is (cat-8:cmbn-zero-p (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0 3 'b)
                                           (cat-8:cmbn 0 3 'b))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0 3 'b) (cat-8:cmbn 0 4 'b))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . b)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr
                                  (cat-8:cmbn 0 1 'a 2 'c  2 'd       3 'g)
                                  (cat-8:cmbn 0      1 'c -2 'd 4 'f -3 'g 4 'h))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (1 . c) (4 . d) (-4 . f) (6 . g)
                                          (-4 . h)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr
                                  (cat-8:cmbn 0      1 'c -2 'd 4 'f -3 'g 4 'h)
                                  (cat-8:cmbn 0 1 'a 2 'c  2 'd       3 'g))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . a) (-1 . c) (-4 . d) (4 . f)
                                          (-6 . g) (4 . h)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr
                                  (cat-8:cmbn 0      1 'c -2 'd 4 'f -3 'g)
                                  (cat-8:cmbn 0 1 'a 2 'c  2 'd       3 'g))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . a) (-1 . c) (-4 . d) (4 . f)
                                          (-6 . g)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0 1 'b 2 'a)
                                  (cat-8:cmbn 0 1 'a 1 'b))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . a) (2 . a)))))

      (let ((comb (cat-8:2cmbn-sbtr #'cat-8:s-cmpr (cat-8:cmbn 0 1 'b 2 'c)
                                  (cat-8:cmbn 0 1 'a 1 'b))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-1 . a) (2 . c))))))

(test 2n-2cmbn
      (signals simple-error (cat-8:2n-2cmbn #'cat-8:s-cmpr 3 (cat-8:cmbn 0)
                                          4 (cat-8:cmbn 1)))

      (is (cat-8:cmbn-zero-p (cat-8:2n-2cmbn #'cat-8:s-cmpr 3 (cat-8:cmbn 0)
                                         4 (cat-8:cmbn 0))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr 3 (cat-8:cmbn 0 1 'a) 4 (cat-8:cmbn 0))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((3 . a)))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr
                                3 (cat-8:cmbn 0  1 'a 2 'b 3 'c)
                                1 (cat-8:cmbn 0 -3 'a 2 'b      4 'd))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((8 . b) (9 . c) (4 . d)))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr
                                3 (cat-8:cmbn 0 -3 'a 2 'b      4 'd)
                                1 (cat-8:cmbn 0  1 'a 2 'b 3 'c))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-8 . a) (8 . b) (3 . c) (12 . d)))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr
                                1 (cat-8:cmbn 0  1 'a 2 'b 3 'c)
                                1 (cat-8:cmbn 0 -3 'a 2 'b      4 'd))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-2 . a) (4 . b) (3 . c) (4 . d)))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr
                                1 (cat-8:cmbn 0 -3 'a 2 'b      4 'd)
                                1 (cat-8:cmbn 0  1 'a 2 'b 3 'c))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((-2 . a) (4 . b) (3 . c) (4 . d)))))

      (let ((comb (cat-8:2n-2cmbn #'cat-8:s-cmpr
                                1 (cat-8:cmbn 0 -3 'a 2 'b      4 'd)
                                3 (cat-8:cmbn 0  1 'a 2 'b 3 'c))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((8 . b) (9 . c) (4 . d))))))

(test cmbn-cmbn
      (let* ((cons (cons 3 (cat-8:cmbn 0 4 'a)))
             (comb (cat-8:cmbn-cmbn #'cat-8:s-cmpr
                                  (make-list 5 :initial-element cons))))
        (is (equal (cat-8:cmbn-degr comb) 0))
        (is (equal (cat-8:cmbn-list comb) '((60 . a)))))

      (let* ((cffc '(12 24 36 48 60 72 84 96 108 120))
             (cons (cons 3 (cat-8:cmbn 0 4 'a))))
        (dotimes (i 10)
          (is (equal (nth i cffc)
                     (cat-8:cffc (first (cat-8:cmbn-list
                                       (cat-8:cmbn-cmbn #'cat-8:s-cmpr
                                                      (make-list (1+ i)
                                                                 :initial-element
                                                                 cons))))))))))

(test nterm-add
      (let ((comb (cat-8:nterm-add #'cat-8:s-cmpr 11)))
        (is (equal (cat-8:cmbn-degr comb) 11))
        (is (equal (cat-8:cmbn-list comb) nil)))

      (let ((comb (cat-8:nterm-add #'cat-8:s-cmpr 11 (cat-8:term 1 'a))))
        (is (equal (cat-8:cmbn-degr comb) 11))
        (is (equal (cat-8:cmbn-list comb) '((1 . a)))))

      (let ((comb (cat-8:nterm-add #'cat-8:s-cmpr 11 (cat-8:term 1 'a)
                                 (cat-8:term 2 'b))))
        (is (equal (cat-8:cmbn-degr comb) 11))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (2 . b)))))

      (let ((comb (cat-8:nterm-add #'cat-8:s-cmpr 11 (cat-8:term 1 'b)
                                 (cat-8:term 2 'a) (cat-8:term 3 'aa)
                                 (cat-8:term -2 'aa) (cat-8:term -2 'a)
                                 (cat-8:term 4 'aa) (cat-8:term 5 'c))))
        (is (equal (cat-8:cmbn-degr comb) 11))
        (is (equal (cat-8:cmbn-list comb) '((5 . aa) (1 . b) (5 . c))))))

(test ncmbn-add
      (let* ((c (cat-8:cmbn 3 4 'a))
             (comb (cat-8:ncmbn-add #'cat-8:s-cmpr c c c c c)))
        (is (equal (cat-8:cmbn-degr comb) 3))
        (is (equal (cat-8:cmbn-list comb) '((20 . a))))))

(test dstr-add-term-to-cmbn
      (let* ((c (cat-8:zero-cmbn 10))
             (comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr 3 'f c)))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . f))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr 3 'g c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr 3 'a c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . a) (3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr 3 'd c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . a) (3 . d) (3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr -3 'd c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . a) (3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr -2 'a c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((1 . a) (3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr -1 'a c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . f) (3 . g))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr -3 'g c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) '((3 . f))))
        (setq comb (cat-8:dstr-add-term-to-cmbn #'cat-8:s-cmpr -3 'f c))
        (is (eq comb c))
        (is (equal (cat-8:cmbn-degr comb) 10))
        (is (equal (cat-8:cmbn-list comb) nil))))

(test uvwz
      (let* ((comb1 (cat-8:cmbn 1 1 'u 2 'v 3 'w 4 'z))
             (comb1-list (cat-8:cmbn-list comb1))
             (term3 (third comb1-list))
             (mcomb1 (cat-8:cmbn-opps comb1))
             (comb2 (cat-8:n-cmbn 10 comb1))
             (comb12 (cat-8:2cmbn-add #'cat-8:s-cmpr comb1 comb2))
             (comb112 (cat-8:2cmbn-sbtr #'cat-8:s-cmpr comb1 comb12))
             (comb12n (cat-8:ncmbn-add #'cat-8:s-cmpr comb1 comb2 comb1 comb2
                                     comb1 comb2 comb1 comb2 comb1 comb2)))
        (is (cat-8:cmbn-non-zero-p comb1))
        (is (equal comb1-list '((1 . u) (2 . v) (3 . w) (4 . z))))
        (is (equal (cat-8:cffc term3) 3))
        (is (equal (cat-8:gnrt term3) 'w))
        (is (equal (cat-8:cmbn-degr comb1) (cat-8:cmbn-degr mcomb1)))
        (is (equal (cat-8:cmbn-list mcomb1) '((-1 . u) (-2 . v)
                                            (-3 . w) (-4 . z))))
        (is (equal (cat-8:cmbn-degr comb1) (cat-8:cmbn-degr comb2)))
        (is (equal (cat-8:cmbn-list comb2) '((10 . u) (20 . v)
                                           (30 . w) (40 . z))))
        (is (equal (cat-8:cmbn-degr comb1) (cat-8:cmbn-degr comb12)))
        (is (equal (cat-8:cmbn-list comb12) '((11 . u) (22 . v)
                                            (33 . w) (44 . z))))
        (is (equal (cat-8:cmbn-degr comb1) (cat-8:cmbn-degr comb112)))
        (is (equal (cat-8:cmbn-list comb112) '((-10 . u) (-20 . v)
                                             (-30 . w) (-40 . z))))
        (is (equal (cat-8:cmbn-degr comb1) (cat-8:cmbn-degr comb12n)))
        (is (equal (cat-8:cmbn-list comb12n) '((55 . u) (110 . v)
                                             (165 . w) (220 . z))))))
