
(cat-init)

(setf G (loop-space (deltab2)))

(setf BG (classifying-space G))

(setf fibr (smgr-fibration G))

(setf brown (brown-reduction fibr))

(setf delta (third (orgn (bcc brown))))

(? delta 0 (tnpr 0 (gbar 0) 0 (loop3)))

(defun loo (n)
  (assert (plusp n))
  (loop3 0 (mask (+ n 2)) 1))

(? delta 2 (tnpr 2 (gbar 2 0 (loo 1) 0 (loop3)) 0 (loop3)))

(? delta 3 (tnpr 3 (gbar 3 0 (loo 2) 0 (loo 1) 0 (loop3)) 0 (loop3))) 

(? delta 4 (tnpr 4 (gbar 4 0 (loo 3) 0 (loo 2) 0 (loo 1) 0 (loop3)) 0 (loop3))) 



