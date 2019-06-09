;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-8)

(in-suite :kenzo-8)


(test normalize-gbar
      (cat-8:normalize-gbar (list 0))
      (cat-8:normalize-gbar (list 1 (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 2 (cat-8:absm 1 'i) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 2 (cat-8:absm 0 'a) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 7 'i) (cat-8:absm 3 'i)
                                (cat-8:absm 1 'i) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 0 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 1 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 2 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 4 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 3 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 5 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 6 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 0 'c) (cat-8:absm 0 'i)))
      (cat-8:normalize-gbar (list 4 (cat-8:absm 1 'a) (cat-8:absm 3 'i)
                                (cat-8:absm 1 'i) (cat-8:absm 0 'i))))


(test unnormalize-gbar
      (cat-8:unnormalize-gbar (cat-8:absm 1 cat-8:+null-gbar+) 'i)
      (cat-8:unnormalize-gbar (cat-8:absm 15 cat-8:+null-gbar+) 'i)
      (cat-8:normalize-gbar (cat-8:unnormalize-gbar
                           (cat-8:absm 15 cat-8:+null-gbar+) 'i))
      (cat-8:unnormalize-gbar
       (cat-8:absm 0 (cat-8:make-gbar :dmns 4
                                  :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                              (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i)
      (cat-8:normalize-gbar
       (cat-8:unnormalize-gbar
        (cat-8:absm 0 (cat-8:make-gbar :dmns 4
                                   :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                               (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i))
      (cat-8:unnormalize-gbar
       (cat-8:absm 5 (cat-8:make-gbar :dmns 4
                                  :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                              (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i)
      (cat-8:normalize-gbar
       (cat-8:unnormalize-gbar
        (cat-8:absm 5 (cat-8:make-gbar :dmns 4
                                   :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                               (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i))
      (cat-8:unnormalize-gbar
       (cat-8:absm 9 (cat-8:make-gbar :dmns 4
                                  :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                              (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i)
      (cat-8:normalize-gbar
       (cat-8:unnormalize-gbar
        (cat-8:absm 9 (cat-8:make-gbar :dmns 4
                                   :list (list (cat-8:absm 0 'a) (cat-8:absm 0 'b)
                                               (cat-8:absm 0 'c) (cat-8:absm 0 'i)))) 'i)))


(test gbar
      (cat-8:gbar 0)
      (signals simple-error (cat-8:gbar 1))
      (cat-8:gbar 2 1 'a 2 'b))


(test classifying-space-cmpr
      (let ((cmpr (cat-8:classifying-space-cmpr #'cat-8:s-cmpr)))
        (is (equal :less (funcall cmpr
                                  (cat-8:gbar 2 0 'a 0 'a)
                                  (cat-8:gbar 2 1 'a 0 'a))))
        (is (equal :less (funcall cmpr
                                  (cat-8:gbar 2 0 'a 0 'a)
                                  (cat-8:gbar 2 0 'b 0 'a))))
        (is (equal :equal (funcall cmpr
                                   (cat-8:gbar 2 0 'a 0 'a)
                                   (cat-8:gbar 2 0 'a 0 'a))))))


(test classifying-space-basis
      (cat-8:cat-init)
      (let* ((k (cat-8:k-z2-1))
             (b (cat-8:classifying-space-basis (cat-8:basis k))))
        (funcall b 0)
        (funcall b 1)
        (dotimes (i 5) (print (funcall b i)))))

(test classifying-space-face
      (let* ((om (cat-8:loop-space (cat-8:moore 2 2)))
             (face (cat-8:classifying-space-face (cat-8:face om)
                                               (cat-8:sintr (cat-8:grml om))))
             (gbar (cat-8:gbar 4 0 (cat-8:loop3 3 'm2 1 4 'n3 1)
                             0 (cat-8:loop3 0 'n3 1)
                             0 (cat-8:loop3 0 'm2 1)
                             0 cat-8:+null-loop+)))
        (dotimes (i 5)
          (print (funcall face i 4 gbar)))))


(test classifying-space
      (cat-8:cat-init)
      (let ((c (cat-8:classifying-space (cat-8:k-z2-1))))
        (cat-8:orgn c)
        (first (cat-8:basis c 4))
        (cat-8:? c 4 (first (cat-8:basis c 4)))
        (cat-8:? c (cat-8:? c 4 (first (cat-8:basis c 4))))
        (cat-8:cprd c 4 (first (cat-8:basis c 4)))
        (dotimes (i 5)
          (print (cat-8:face c i 4 (first (cat-8:basis c 4)))))))


(test classifying-space-grml-sintr
      (let ((grml (cat-8:classifying-space-grml-sintr
                   '() (cat-8:sintr (cat-8:grml (cat-8:k-z-1))))))
        (funcall grml 3 (cat-8:crpr 0 (cat-8:gbar 3 0 '(1 2) 0 '(3) 0 '())
                                  0 (cat-8:gbar 3 0 '(-1 -2) 0 '(-3) 0 '())))
        (funcall grml 3 (cat-8:crpr 0 (cat-8:gbar 3 0 '(1 2) 0 '(3) 0 '())
                                  4 (cat-8:gbar 2 0 '(-3) 0 '())))
        (funcall grml 3 (cat-8:crpr 0 (cat-8:gbar 3 0 '(1 2) 0 '(3) 0 '())
                                  1 (cat-8:gbar 2 0 '(-3) 0 '())))))


(test classifying-space-grin-sintr
      (let ((grin (cat-8:classifying-space-grin-sintr
                   (cat-8:sintr (cat-8:grin (cat-8:k-z-1))))))
        (funcall grin 3 (cat-8:gbar 3 0 '(1 2) 1 '() 0 '()))))


(test classifying-space1
      (cat-8:cat-init)
      (let* ((k-z-1 (cat-8:k-z-1))
             (k-z-2 (cat-8:classifying-space k-z-1))
             (k-z-3 (cat-8:classifying-space k-z-2))
             (k-z2-1 (cat-8:k-z2-1))
             (k-z2-2 (cat-8:classifying-space k-z2-1))
             (k-z2-3 (cat-8:classifying-space k-z2-2))
             (k-z2-4 (cat-8:classifying-space k-z2-3))
             (k-z2-5 (cat-8:classifying-space k-z2-4)))
        (cat-8:homology k-z-3 0 10)
        (cat-8:homology k-z2-5 0 7)))
