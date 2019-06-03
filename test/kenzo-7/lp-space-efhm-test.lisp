;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package :kenzo-test-7)

(in-suite :kenzo-7)


(test ls-hat-t-u
      (cat-7:cat-init)
      (let ((c (cat-7:ls-hat-t-u (cat-7:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 4
                                 (cat-7:tnpr 2 7 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-7:? c (+ 4 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 3 15 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 2 7 3 (cat-7:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))))


(test ls-hat-u-t
      (cat-7:cat-init)
      (let ((c (cat-7:ls-hat-u-t (cat-7:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 4
                                 (cat-7:tnpr 2 7 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-7:? c (+ 4 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 3 15 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 2 7 3 (cat-7:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))))


(test ls-left-hmeq-hat
      (cat-7:cat-init)
      (let ((c (cat-7:ls-left-hmeq-hat (cat-7:deltab)))
            allp allp-degr gnrt)
        (dotimes (i 10)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 4
                                 (cat-7:tnpr 2 7 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 11)
              (print (cat-7:? c (+ 4 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 4 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 3 15 2 (cat-7:loop3 0 15 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))
        (dotimes (i 20)
          (let ((allp (random-allp 3)))
            (setf allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
            (setf gnrt (cat-7:tnpr allp-degr allp 5
                                 (cat-7:tnpr 2 7 3 (cat-7:loop3 0 31 2))))
            (unless (>= allp-degr 10)
              (print (cat-7:? c (+ 5 allp-degr) gnrt))
              (print (cat-7:? c (cat-7:? c (+ 5 allp-degr) gnrt))))))))


(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat-7:ls-pre-left-hmeq-left-reduction-intr-f
       (cat-7:cmbn 6 100 (cat-7:tnpr 0 'a 6 (cat-7:tnpr 0 'b 6 'c))
                 50 (cat-7:tnpr 0 'a 6 (cat-7:tnpr 0 'b 6 'cc))
                 10 (cat-7:tnpr 0 'a 6 (cat-7:tnpr 2 'b 4 'c))
                 1 (cat-7:tnpr 2 'a 4 (cat-7:tnpr 2 'b 2 'cc)))))


(test ls-left-hmeq-left-reduction-g-intr
      (let ((r (cat-7:ls-left-hmeq-left-reduction-g-intr '*)))
        (funcall r (cat-7:cmbn 3 4 'loop))))


(test ls-pre-left-hmeq-left-reduction-h-intr
      (cat-7:cat-init)
      (let ((r (cat-7:ls-pre-left-hmeq-left-reduction-h-intr (cat-7:deltab))))
        (setf cat-7:*tnpr-with-degrees* t)
        (funcall r (cat-7:cmbn 10 1 (cat-7:tnpr 3 (cat-7:allp 2 'a 1 'b)
                                            7 (cat-7:tnpr 5 'c 2 'd))
                             1 (cat-7:tnpr 7 (cat-7:allp 2 'a 5 'b)
                                         3 (cat-7:tnpr 0 '* 3 'c))
                             10 (cat-7:tnpr 8 (cat-7:allp 4 'aa 4 'bb)
                                          2 (cat-7:tnpr 0 '* 2 'cc))))
        (funcall r (cat-7:cmbn 10 1 (cat-7:tnpr 3 (cat-7:allp 2 'a 1 'b)
                                            7 (cat-7:tnpr 5 'c 2 'd))
                             1 (cat-7:tnpr 7 (cat-7:allp 4 'a 3 'b)
                                         3 (cat-7:tnpr 0 '* 3 'c))
                             10 (cat-7:tnpr 8 (cat-7:allp 3 'aa 5 'bb)
                                          2 (cat-7:tnpr 0 '* 2 'cc))))
        (setf cat-7:*tnpr-with-degrees* nil)))

#|
(test ls-pre-left-hmeq-left-reduction
      (cat-7:cat-init)
      (let* ((rdct (cat-7:ls-pre-left-hmeq-left-reduction (cat-7:deltab)))
             (allp (random-allp 3))
             (allp-degr (apply #'+ (mapcar #'car (cat-7:allp-list allp))))
             (gnrt (cat-7:tnpr allp-degr allp 4 (cat-7:tnpr 2 7 2
                                                        (cat-7:loop3 0 15 2)))))
        (cat-7:pre-check-rdct rdct)
        (setf cat-7:*tc* (cat-7:cmbn (+ 4 allp-degr) 1 gnrt))
        (setf cat-7:*bc* (cat-7:cmbn 2 1 (cat-7:loop3 0 15 2)))
        (check-rdct)
        (setf gnrt (cat-7:tnpr allp-degr allp 4
                             (cat-7:tnpr 0 1 4 (cat-7:loop3 0 (cat-7:mask 6) 2))))
        (setf cat-7:*tc* (cat-7:cmbn (+ 4 allp-degr) 1 gnrt))
        (check-rdct)
        (setf cat-7:*bc* (cat-7:cmbn 0 1 (cat-7:bspn (cat-7:bcc rdct))))
        (setf cat-7:*tc* (cat-7:cmbn 0 1 (cat-7:bsgn (cat-7:tcc rdct))))
        (check-rdct)))
|#

(defun a (d1 d2 d3)
  (setf cat-7:*tc* (cat-7:cmbn (+ d1 d2 d3)
                           1 (cat-7:tnpr
                              d1 (cat-7:allp d1 (cat-7:mask (+ d1 2)))
                              (+ d2 d3)
                              (cat-7:tnpr d2 (cat-7:mask (1+ d2))
                                        d3 (cat-7:loop3
                                            0 (cat-7:mask (+ d3 2)) 1))))
        cat-7:*bc* (cat-7:cmbn d3 1 (cat-7:loop3 0 (cat-7:mask (+ d3 2)) -1)))
  (check-rdct))


(test ls-left-hmeq-left-reduction
      (cat-7:cat-init)
      (let ((rdct (cat-7:ls-left-hmeq-left-reduction (cat-7:deltab2))))
        (cat-7:pre-check-rdct rdct)
        #|
        (a 1 0 1)
        (a 1 1 1)  ;; error because 3 does not exist in deltab2
        (a 1 2 1)
        (a 2 2 1)
        |#
        )
      )


(test ls-pre-left-hmeq-left-reduction-intr-f
      (cat-7:ls-pre-left-hmeq-right-reduction-intr-f
       (cat-7:cmbn 3 12 (cat-7:tnpr 2 'a 1 'b) 14 (cat-7:tnpr 3 'aa 0 '*))))


(test ls-pre-left-hmeq-right-reduction-intr-g
      (let ((r (cat-7:ls-pre-left-hmeq-right-reduction-intr-g '*)))
        (funcall r (cat-7:cmbn 3 4 'a))))


(test ls-pre-left-hmeq-right-reduction
      (cat-7:cat-init)
      (let ((r (cat-7:ls-pre-left-hmeq-right-reduction (cat-7:deltab2))))
        (cat-7:pre-check-rdct r)
        (setf cat-7:*tc* (cat-7:cmbn 0 1 (cat-7:bsgn (cat-7:tcc r))))
        (setf cat-7:*bc* (cat-7:cmbn 0 1 (cat-7:bsgn (cat-7:bcc r))))
        (check-rdct)
        #|
        (setf cat-7:*tc* (cat-7:cmbn
        3 1 (cat-7:tnpr 3 (cat-7:allp 3 (cat-7:mask 5))
        0 (cat-7:tnpr 0 1 0 cat-7:+null-loop+))))
        (setf cat-7:*bc* (cat-7:cmbn 3 1 (cat-7:allp 1 7 2 15)))
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn
        6 1 (cat-7:tnpr 3 (cat-7:allp 1 7 2 15)
        3 (cat-7:tnpr 2 7 1 (cat-7:loop3 0 7 2)))))
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn
        6 1 (cat-7:tnpr
        3 (cat-7:allp 1 7 2 15)
        3 (cat-7:tnpr 0 1 3
        (cat-7:loop3 0 (cat-7:mask 5) 2)))))
        (check-rdct)
        |#
        ))


(test ls-left-hmeq-right-reduction
      (cat-7:cat-init)
      (let ((r (cat-7:ls-left-hmeq-right-reduction (cat-7:deltab2))))
        (cat-7:pre-check-rdct r)
        (setf cat-7:*tc* (cat-7:cmbn 0 1 (cat-7:bsgn (cat-7:tcc r))))
        (setf cat-7:*bc* (cat-7:cmbn 0 1 (cat-7:bsgn (cat-7:bcc r))))
        (check-rdct)
        #|
        (setf cat-7:*tc* (cat-7:cmbn 3 1 (cat-7:tnpr 3 (cat-7:allp 3 (cat-7:mask 5))
        0 (cat-7:tnpr 0 1 0
        cat-7:+null-loop+))))
        (setf cat-7:*bc* (cat-7:cmbn 3 1 (cat-7:allp 1 7 2 15)))
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn
        6 1 (cat-7:tnpr 3 (cat-7:allp 1 7 2 15)
        3 (cat-7:tnpr 2 7 1 (cat-7:loop3 0 7 2)))))
        (check-rdct)
        (setf cat-7:*tc* (cat-7:cmbn
        6 1 (cat-7:tnpr 3 (cat-7:allp 1 7 2 15)
        3 (cat-7:tnpr 0 1 3
        (cat-7:loop3 0 (mask 5) 2)))))
        (check-rdct)
        |#
        ))


#|
(test ls-left-hmeq
      (cat-7:cat-init)
      (let* ((h (cat-7:ls-left-hmeq (cat-7:deltab2)))
             (loop (cat-7:loop3 0 (cat-7:mask 5) 2))
             (allp (cat-7:allp 2 (cat-7:mask 4) 3 (cat-7:mask 5)))
             x)
        (setf x (cat-7:lg h 3 loop))
        (setf x (cat-7:rf h x))
        (setf x (cat-7:rg h x))
        (setf x (cat-7:lf h x))
        (setf x (cat-7:rg h 5 allp))
        (setf x (cat-7:lf h x))
        (setf x (cat-7:lg h x))
        (setf x (cat-7:rf h x)) ;; = allp, but why ?
        ))
|#

(test loop-space
      (cat-7:cat-init)
      (let ((l (cat-7:loop-space (cat-7:sphere 2)))
            (oos3 (cat-7:loop-space (cat-7:loop-space (cat-7:sphere 3))))
            (ooos4 (cat-7:loop-space (cat-7:loop-space (cat-7:loop-space
                                                    (cat-7:sphere 4))))))
        (cat-7:homology l 6)
        (cat-7:homology oos3 3)
        (cat-7:homology ooos4 2)))
