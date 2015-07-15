
(require 'asdf)

(asdf:operate 'asdf:load-op 'kenzo)

(format t "~%Initializing Kenzo...")
(cat:cat-init)
(cat:kenzo-version)

(format t "~%Creating simplicial set Moore(Z/2Z,3):")
(defvar m23 (cat:moore 2 3))
(print m23)

(format t "~%~%Calculating the homology groups of dimensions 0 to 4:~%")
(cat:homology m23 0 5)

(format t "~%~%Creating the cartesian product Moore(Z/2Z,3)xMoore(Z/2Z,3):~%")
(defvar m23xm23 (cat:crts-prdc m23 m23))
(print m23xm23)

(let ((b (cat:basis m23xm23 6)))
  (format t "~%~%Basis in dimension 6:~%")
  (print b)
  (format t "~%~%Length: ~A~%" (length b)))

(format t "~%~%Creating the tensor product Moore(Z/2Z,3)#Moore(Z/2Z,3):~%")
(defvar t2m23 (cat:tnsr-prdc m23 m23))
(print t2m23)

(format t "Basis in dimension 6: ~%")
(print (cat:basis t2m23 6))

(format t "~%~%Calculating the homology groups of dimensions 0 to 8:~%")
(cat:homology t2m23 0 8)

(format t "~%Creating the abelian simplicial group K(Z,1):")
(defvar kz1 (cat:k-z 1))
(print kz1)

(format t "~%~%Calculating coproduct:")
(print (cat:cprd kz1 4 '(2 3 4 5)))

(format t "~%~%Calculating product:")
(print (cat:aprd kz1 6 (cat:tnpr 2 '(1 2) 4 '(3 4 5 6))))

(format t "~%Creating the abelian simplicial group K(Z/2Z,2):")
(defvar k-z2-2 (cat:k-z2 2))
(print k-z2-2)

(format t "~%~%Calculating the homology group of dimension 4:~%")
(cat:homology k-z2-2 4)

#+ccl (quit)
#+sbcl (sb-ext:exit)
