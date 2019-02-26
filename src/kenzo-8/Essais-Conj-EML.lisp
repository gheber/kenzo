
(setf k1 (k-z-1))
(setf k2 (k-z 2))

(setf fibr (smgr-fibration k1))

(setf brown (brown-reduction fibr))

(setf delta (third (orgn (bcc brown))))

(? delta 0 (tnpr 0 (gbar 0) 0 nil))

(? delta 2 (tnpr 2 (gbar 2 0 '(1) 0 nil) 0 nil))

(? delta 3 (tnpr 3 (gbar 3 0 '(1 1) 0 '(10) 0 nil) 0 nil))

(? delta 4 (tnpr 4 (gbar 4 0 '(1 1 1) 0 '(10 10) 0 '(100) 0 nil) 0 nil))

(? delta 5 (tnpr 5 (gbar 5 0 '(1 1 1 1) 0 '(10 10 10) 0 '(100 100) 0 '(1000) 0 nil) 0 nil))
(dolist (term (cmbn-list *))
  (print (cons (list (car term)) (gnrt2 (gnrt term)))))

(? delta 6 (tnpr 6 (gbar 6
                         0 '(1 1 1 1 1)
                         0 '(10 10 10 10)
                         0 '(100 100 100)
                         0 '(1000 1000)
                         0 '(10000)
                         0 nil)
                 0 nil))
(dolist (term (cmbn-list *))
  (print (cons (list (car term)) (gnrt2 (gnrt term)))))


(? delta 2 (tnpr 2 (gbar 2 0 '(1) 0 nil) 0 nil))
(? delta 3 (tnpr 3 (gbar 3 0 '(1 1) 0 '(10) 0 nil) 0 nil))
(? delta 4 (tnpr 4 (gbar 4 0 '(1 1 1) 0 '(10 10) 0 '(100) 0 nil) 0 nil))
(? delta 5 (tnpr 5 (gbar 5 0 '(1 1 1 1) 0 '(10 10 10) 0 '(100 100) 0 '(1000) 0 nil) 0 nil))
(? delta 5 (tnpr 5
                 (gbar 5 0 '(1 1 1 1) 0 '(10 10 10) 0 '(100 100) 0 '(1000) 0 nil) 0 nil))

(defun essai (n)
  (do ((i 0 (1+ i))
       (rslt nil (cons 0
                       (cons (make-list i :initial-element (expt 10 (1- i)))
                             rslt))))
      ((= i n)
       (dolist (term (cmbn-list (? delta n (tnpr n (apply #'gbar n rslt) 0 nil))))
         (print (cons (list (car term)) (gnrt2 (gnrt term))))))))

(essai 3)
(essai 4)
(essai 5)
(essai 6)
(essai 7)
(essai 8)
(essai 9)
(essai 10)

(? delta 2 (tnpr 2 (gbar 2 0 '(1) 0 nil) 0 nil))

(? delta 3 (tnpr 3 (gbar 3 0 '(1 1) 0 '(10) 0 nil) 0 nil))

(? delta 4 (tnpr 4 (gbar 4 0 '(1 1 1) 0 '(10 10) 0 '(100) 0 nil) 0 nil))
 
