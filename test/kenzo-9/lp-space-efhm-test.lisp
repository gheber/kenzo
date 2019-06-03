;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-9)

(in-suite :kenzo)


(test ls-hat-t-u
      (cat-9:cat-9-init)
      (let ((c (cat-9:ls-hat-t-u (cat-9:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 4
                                 (cat-9:tnpr 2 7 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-9:? c (+ 4 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 3 15 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 2 7 3 (cat-9:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))))


(test ls-hat-u-t
      (cat-9:cat-9-init)
      (let ((c (cat-9:ls-hat-u-t (cat-9:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 4
                                 (cat-9:tnpr 2 7 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-9:? c (+ 4 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 3 15 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 2 7 3 (cat-9:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))))


(test ls-left-hmeq-hat
      (cat-9:cat-9-init)
      (let ((c (cat-9:ls-left-hmeq-hat (cat-9:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 4
                                 (cat-9:tnpr 2 7 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-9:? c (+ 4 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 3 15 2 (cat-9:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
            (setf gnrt (cat-9:tnpr allp-degr allp 5
                                 (cat-9:tnpr 2 7 3 (cat-9:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-9:? c (+ 5 allp-degr) gnrt))
              (print (cat-9:? c (cat-9:? c (+ 5 allp-degr) gnrt))))))))


(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat-9:ls-pre-left-hmeq-left-reduction-intr-f
       (cat-9:cmbn 6 100 (cat-9:tnpr 0 'a 6 (cat-9:tnpr 0 'b 6 'c))
                 50 (cat-9:tnpr 0 'a 6 (cat-9:tnpr 0 'b 6 'cc))
                 10 (cat-9:tnpr 0 'a 6 (cat-9:tnpr 2 'b 4 'c))
                 1 (cat-9:tnpr 2 'a 4 (cat-9:tnpr 2 'b 2 'cc)))))


(test ls-left-hmeq-left-reduction-g-intr
      (let ((r (cat-9:ls-left-hmeq-left-reduction-g-intr '*)))
        (funcall r (cat-9:cmbn 3 4 'loop))))


(test ls-pre-left-hmeq-left-reduction-h-intr
      (cat-9:cat-9-init)
      (let ((r (cat-9:ls-pre-left-hmeq-left-reduction-h-intr (cat-9:deltab))))
        (setf cat-9:*tnpr-with-degrees* t)
        (funcall r (cat-9:cmbn 10 1 (cat-9:tnpr 3 (cat-9:allp 2 'a 1 'b)
                                            7 (cat-9:tnpr 5 'c 2 'd))
                             1 (cat-9:tnpr 7 (cat-9:allp 2 'a 5 'b)
                                         3 (cat-9:tnpr 0 '* 3 'c))
                             10 (cat-9:tnpr 8 (cat-9:allp 4 'aa 4 'bb)
                                          2 (cat-9:tnpr 0 '* 2 'cc))))
        (funcall r (cat-9:cmbn 10 1 (cat-9:tnpr 3 (cat-9:allp 2 'a 1 'b)
                                            7 (cat-9:tnpr 5 'c 2 'd))
                             1 (cat-9:tnpr 7 (cat-9:allp 4 'a 3 'b)
                                         3 (cat-9:tnpr 0 '* 3 'c))
                             10 (cat-9:tnpr 8 (cat-9:allp 3 'aa 5 'bb)
                                          2 (cat-9:tnpr 0 '* 2 'cc))))
        (setf cat-9:*tnpr-with-degrees* nil)))

#|
(test ls-pre-left-hmeq-left-reduction
      (cat-9:cat-9-init)
      (let* ((rdct (cat-9:ls-pre-left-hmeq-left-reduction (cat-9:deltab)))
             (allp (random-allp 3))
             (allp-degr (apply #'+ (mapcar #'car (cat-9:allp-list allp))))
             (gnrt (cat-9:tnpr allp-degr allp 4 (cat-9:tnpr 2 7 2
                                                        (cat-9:loop3 0 15 2)))))
        (cat-9:pre-check-rdct rdct)
        (setf cat-9:*tc* (cat-9:cmbn (+ 4 allp-degr) 1 gnrt))
        (setf cat-9:*bc* (cat-9:cmbn 2 1 (cat-9:loop3 0 15 2)))
        (check-rdct)
        (setf gnrt (cat-9:tnpr allp-degr allp 4
                             (cat-9:tnpr 0 1 4 (cat-9:loop3 0 (cat-9:mask 6) 2))))
        (setf cat-9:*tc* (cat-9:cmbn (+ 4 allp-degr) 1 gnrt))
        (check-rdct)
        (setf cat-9:*bc* (cat-9:cmbn 0 1 (cat-9:bspn (cat-9:bcc rdct))))
        (setf cat-9:*tc* (cat-9:cmbn 0 1 (cat-9:bsgn (cat-9:tcc rdct))))
        (check-rdct)))
|#

(defun a (d1 d2 d3)
  (setf cat-9:*tc* (cat-9:cmbn (+ d1 d2 d3)
                           1 (cat-9:tnpr
                              d1 (cat-9:allp d1 (cat-9:mask (+ d1 2)))
                              (+ d2 d3)
                              (cat-9:tnpr d2 (cat-9:mask (1+ d2))
                                        d3 (cat-9:loop3
                                            0 (cat-9:mask (+ d3 2)) 1))))
        cat-9:*bc* (cat-9:cmbn d3 1 (cat-9:loop3 0 (cat-9:mask (+ d3 2)) -1)))
  (check-rdct))


(test ls-left-hmeq-left-reduction
      (cat-9:cat-9-init)
      (let ((rdct (cat-9:ls-left-hmeq-left-reduction (cat-9:deltab2))))
        (cat-9:pre-check-rdct rdct)
        #|
        (a 1 0 1)
        (a 1 1 1)  ;; error because 3 does not exist in deltab2
        (a 1 2 1)
        (a 2 2 1)
        |#
        )
      )


(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat-9:ls-pre-left-hmeq-right-reduction-intr-f
       (cat-9:cmbn 3 12 (cat-9:tnpr 2 'a 1 'b) 14 (cat-9:tnpr 3 'aa 0 '*))))


(test ls-pre-left-hmeq-right-reduction-intr-g
      (let ((r (cat-9:ls-pre-left-hmeq-right-reduction-intr-g '*)))
        (funcall r (cat-9:cmbn 3 4 'a))))


(test ls-pre-left-hmeq-right-reduction
      (cat-9:cat-9-init)
      (let ((r (cat-9:ls-pre-left-hmeq-right-reduction (cat-9:deltab2))))
        (cat-9:pre-check-rdct r)
        (setf cat-9:*tc* (cat-9:cmbn 0 1 (cat-9:bsgn (cat-9:tcc r))))
        (setf cat-9:*bc* (cat-9:cmbn 0 1 (cat-9:bsgn (cat-9:bcc r))))
        (check-rdct)
        #|
        (setf cat-9:*tc* (cat-9:cmbn
        3 1 (cat-9:tnpr 3 (cat-9:allp 3 (cat-9:mask 5))
        0 (cat-9:tnpr 0 1 0 cat-9:+null-loop+))))
        (setf cat-9:*bc* (cat-9:cmbn 3 1 (cat-9:allp 1 7 2 15)))
        (check-rdct)
        (setf cat-9:*tc* (cat-9:cmbn
        6 1 (cat-9:tnpr 3 (cat-9:allp 1 7 2 15)
        3 (cat-9:tnpr 2 7 1 (cat-9:loop3 0 7 2)))))
        (check-rdct)
        (setf cat-9:*tc* (cat-9:cmbn
        6 1 (cat-9:tnpr
        3 (cat-9:allp 1 7 2 15)
        3 (cat-9:tnpr 0 1 3
        (cat-9:loop3 0 (cat-9:mask 5) 2)))))
        (check-rdct)
        |#
        ))


(test ls-left-hmeq-right-reduction
      (cat-9:cat-9-init)
      (let ((r (cat-9:ls-left-hmeq-right-reduction (cat-9:deltab2))))
        (cat-9:pre-check-rdct r)
        (setf cat-9:*tc* (cat-9:cmbn 0 1 (cat-9:bsgn (cat-9:tcc r))))
        (setf cat-9:*bc* (cat-9:cmbn 0 1 (cat-9:bsgn (cat-9:bcc r))))
        (check-rdct)
        #|
        (setf cat-9:*tc* (cat-9:cmbn 3 1 (cat-9:tnpr 3 (cat-9:allp 3 (cat-9:mask 5))
        0 (cat-9:tnpr 0 1 0
        cat-9:+null-loop+))))
        (setf cat-9:*bc* (cat-9:cmbn 3 1 (cat-9:allp 1 7 2 15)))
        (check-rdct)
        (setf cat-9:*tc* (cat-9:cmbn
        6 1 (cat-9:tnpr 3 (cat-9:allp 1 7 2 15)
        3 (cat-9:tnpr 2 7 1 (cat-9:loop3 0 7 2)))))
        (check-rdct)
        (setf cat-9:*tc* (cat-9:cmbn
        6 1 (cat-9:tnpr 3 (cat-9:allp 1 7 2 15)
        3 (cat-9:tnpr 0 1 3
        (cat-9:loop3 0 (mask 5) 2)))))
        (check-rdct)
        |#
        ))


#|
(test ls-left-hmeq
      (cat-9:cat-9-init)
      (let* ((h (cat-9:ls-left-hmeq (cat-9:deltab2)))
             (loop (cat-9:loop3 0 (cat-9:mask 5) 2))
             (allp (cat-9:allp 2 (cat-9:mask 4) 3 (cat-9:mask 5)))
             x)
        (setf x (cat-9:lg h 3 loop))
        (setf x (cat-9:rf h x))
        (setf x (cat-9:rg h x))
        (setf x (cat-9:lf h x))
        (setf x (cat-9:rg h 5 allp))
        (setf x (cat-9:lf h x))
        (setf x (cat-9:lg h x))
        (setf x (cat-9:rf h x)) ;; = allp, but why ?
        ))
|#

(test loop-space
      (cat-9:cat-9-init)
      (let ((l (cat-9:loop-space (cat-9:sphere 2)))
            (oos3 (cat-9:loop-space (cat-9:loop-space (cat-9:sphere 3))))
            (ooos4 (cat-9:loop-space (cat-9:loop-space (cat-9:loop-space
                                                    (cat-9:sphere 4))))))
        (cat-9:homology l 6)
        (cat-9:homology oos3 3)
        (cat-9:homology ooos4 2)))
