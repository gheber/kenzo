




(defun eff-prst-hmlg-basis-dvs-quotient (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (the list
     (let* ((degr (+ p q))
            (fltr-p-basis (fltrd-basis fltrcm degr p))
            (p-basis-l (length fltr-p-basis))
            (num-mtrx (spsq-num-mtrx fltrcm (1+ p) p q))
            (den-mtrx (spsq-den-mtrx fltrcm r p q)) 
            (cmbn-list nil)
            (divs nil))
        (declare
         (list fltr-p-basis cmbn-list divs)
         (type fixnum degr p-basis-l)
         (type matrix num-mtrx den-mtrx))
        (progn
         (if (= 0 (array-total-size num-mtrx))
            (progn
             (setq cmbn-list nil)
             (setq divs nil))
            (if (= 0 (array-total-size den-mtrx))
               (progn
                (setq cmbn-list (gnrt-mtrx-to-list num-mtrx))
                (setq divs (make-list (length cmbn-list) :initial-element 0)))
               (let ((quotient-list (mtrx-quotient num-mtrx den-mtrx)))
                  (declare (list quotient-list))
                  (setq cmbn-list (first quotient-list)
                    divs (second quotient-list)))))
         (list
          (mapcar 
            #'(lambda (int-list)
                (declare (list int-list))
                (the cmbn
                  (let* ((cmbn (cmbn degr))
                         (cmpr (cmpr fltrcm)))
                     (declare 
                       (type cmbn cmbn)
                       (type cmprf cmpr))
                     (do ((mark1 int-list (cdr mark1))
                          (mark2 fltr-p-basis (cdr mark2)))
                         ((or (endp mark1) (endp mark2)))
                        (declare (type list mark1 mark2))
                        (if (not (= 0 (car mark1)))
                           (setq cmbn (2cmbn-add cmpr cmbn 
                                        (cmbn degr (car mark1) (car mark2))))))
                     cmbn))) 
            cmbn-list)
          divs)))))

;; (setf fltrcm triangle r 1 p 5 q -4)
;; (setf degr (+ p q))
;; (setf fltr-p-basis (fltrd-basis fltrcm degr p))
;; (setf p-basis-l (length fltr-p-basis))
;; (setf z1-list (fltr-chcm-z-gnrt-list fltrcm r p q))
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r p q)
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r 1 -1)
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r 2 -2)
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r 3 -3)
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r 4 -4)
;;(eff-prst-hmlg-basis-dvs-2 fltrcm r 5 -5)

(defun eff-prst-hmlg-basis-dvs-total (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (the list
     (let* ((degr (+ p q))
            (fltr-p-basis (fltrd-basis fltrcm degr p))
            (p-basis-l (length fltr-p-basis))
            (fltr-p-1-list (fltr-basis-gnrt-list fltrcm degr (1- p)))
            (z1-list (fltr-chcm-z-gnrt-list fltrcm (1+ p) p q))
            (z1-mtrx (gnrt-list-to-mtrx z1-list))
            (z2-list (fltr-chcm-z-gnrt-list fltrcm (1- r) (1- (+ p r)) (+ 2 (- q r))))
            (z2-mtrx (gnrt-list-to-mtrx z2-list))
            (fltr-p-1-mtrx (gnrt-list-to-mtrx fltr-p-1-list))
            (dffr-mtrx (flcc-dffr-mtrx fltrcm (1+ degr) (1- (+ p r))))
            (nil-mtrx 
              #-ACLPC (make-array (list 0 0)
                        :element-type 'fixnum
                        :initial-element 0)
              #+ACLPC (make-array (list 0 0)
                        :element-type 'fixnum))
            (dffr-z2-mtrx 
              (if (or (= 0 (array-total-size z2-mtrx)) (= 0 (array-total-size dffr-mtrx)))
                 nil-mtrx
                 (mtrx-prdc dffr-mtrx z2-mtrx)))
            (bnd-z2-line-n p-basis-l)
            (bnd-z2-column-n (column-number dffr-z2-mtrx))
            (bnd-z2-mtrx
             (if (or (= 0 bnd-z2-line-n) (= 0 bnd-z2-column-n))
                nil-mtrx
                (submatrix dffr-z2-mtrx 0 (1- bnd-z2-line-n) 0 (1- bnd-z2-column-n))))
            (num-mtrx z1-mtrx)
            (den-mtrx bnd-z2-mtrx) 
            (cmbn-list nil)
            (divs nil))
        (declare
         (list fltr-p-basis cmbn-list divs)
         (type fixnum degr p-basis-l)
         (type matrix num-mtrx den-mtrx))
        (progn
         (if (= 0 (array-total-size num-mtrx))
            (progn
             (setq cmbn-list nil)
             (setq divs nil))
            (if (= 0 (array-total-size den-mtrx))
               (progn
                (setq cmbn-list (gnrt-mtrx-to-list num-mtrx))
                (setq divs (make-list (length cmbn-list) :initial-element 0)))
               (let ((quotient-list (mtrx-quotient num-mtrx den-mtrx)))
                  (declare (list quotient-list))
                  (setq cmbn-list (first quotient-list)
                    divs (second quotient-list)))))
         (list
          (mapcar 
            #'(lambda (int-list)
                (declare (list int-list))
                (the cmbn
                  (let* ((cmbn (cmbn degr))
                         (cmpr (cmpr fltrcm)))
                     (declare 
                       (type cmbn cmbn)
                       (type cmprf cmpr))
                     (do ((mark1 int-list (cdr mark1))
                          (mark2 fltr-p-basis (cdr mark2)))
                         ((or (endp mark1) (endp mark2)))
                        (declare (type list mark1 mark2))
                        (if (not (= 0 (car mark1)))
                           (setq cmbn (2cmbn-add cmpr cmbn 
                                        (cmbn degr (car mark1) (car mark2))))))
                     cmbn))) 
            cmbn-list)
          divs)))))


(defun eff-prst-hmlg-basis-dvs-bar (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (the list
     (let* ((degr (+ p q))
            (fltr-p-basis (fltrd-basis fltrcm degr p))
            (p-basis-l (length fltr-p-basis))
            (num-mtrx (spsq-den-mtrx fltrcm r p q))
            (den-mtrx (spsq-den-mtrx fltrcm (- r 1) p q)) 
            (cmbn-list nil)
            (divs nil))
        (declare
         (list fltr-p-basis cmbn-list divs)
         (type fixnum degr p-basis-l)
         (type matrix num-mtrx den-mtrx))
        (progn
         (if (= 0 (array-total-size num-mtrx))
            (progn
             (setq cmbn-list nil)
             (setq divs nil))
            (if (= 0 (array-total-size den-mtrx))
               (progn
                (setq cmbn-list (gnrt-mtrx-to-list num-mtrx))
                (setq divs (make-list (length cmbn-list) :initial-element 0)))
               (let ((quotient-list (mtrx-quotient num-mtrx den-mtrx)))
                  (declare (list quotient-list))
                  (setq cmbn-list (first quotient-list)
                    divs (second quotient-list)))))
         (list
          (mapcar 
            #'(lambda (int-list)
                (declare (list int-list))
                (the cmbn
                  (let* ((cmbn (cmbn degr))
                         (cmpr (cmpr fltrcm)))
                     (declare 
                       (type cmbn cmbn)
                       (type cmprf cmpr))
                     (do ((mark1 int-list (cdr mark1))
                          (mark2 fltr-p-basis (cdr mark2)))
                         ((or (endp mark1) (endp mark2)))
                        (declare (type list mark1 mark2))
                        (if (not (= 0 (car mark1)))
                           (setq cmbn (2cmbn-add cmpr cmbn 
                                        (cmbn degr (car mark1) (car mark2))))))
                     cmbn))) 
            cmbn-list)
          divs)))))





(defun eff-prst-hmlg-gnrts-total (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-total fltrcm r p q))
         (basis (first basis-dvs))
         (dvs (second basis-dvs))
         (rslt nil))
      (declare (type list basis-dvs basis dvs rslt))
      (do ((mark1 basis (cdr mark1))
           (mark2 dvs (cdr mark2)))
          ((endp mark1))
         (if (not (eql 1 (car mark2)))
            (setf rslt (append rslt (list (car mark1))))))
     rslt))

(defun eff-prst-hmlg-gnrts-quotient (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-quotient fltrcm r p q))
         (basis (first basis-dvs))
         (dvs (second basis-dvs))
         (rslt nil))
      (declare (type list basis-dvs basis dvs rslt))
      (do ((mark1 basis (cdr mark1))
           (mark2 dvs (cdr mark2)))
          ((endp mark1))
         (if (not (eql 1 (car mark2)))
            (setf rslt (append rslt (list (car mark1))))))
     rslt))

(defun eff-prst-hmlg-gnrts-bar (FltrCm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-bar fltrcm r p q))
         (basis (first basis-dvs))
         (dvs (second basis-dvs))
         (rslt nil))
      (declare (type list basis-dvs basis dvs rslt))
      (do ((mark1 basis (cdr mark1))
           (mark2 dvs (cdr mark2)))
          ((endp mark1))
         (if (not (eql 1 (car mark2)))
            (setf rslt (append rslt (list (car mark1))))))
     rslt))


(defun eff-prst-hmlg-group-total (fltrcm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))  
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-total fltrcm r p q))
          (cmbns (first basis-dvs))
          (divs (second basis-dvs)))
      (declare (list basis-dvs cmbns divs))
      
      (dolist (item divs)
         (declare (type fixnum item))
         (if (not (eql 1 item))
            (progn
             (format t "~2%Component Z")
             (unless (zerop item) 
                (format t "/~DZ" item))
             )))
     (terpri)))

(defun eff-prst-hmlg-group-quotient (fltrcm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))  
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-quotient fltrcm r p q))
          (cmbns (first basis-dvs))
          (divs (second basis-dvs)))
      (declare (list basis-dvs cmbns divs))
      
      (dolist (item divs)
         (declare (type fixnum item))
         (if (not (eql 1 item))
            (progn
             (format t "~2%Component Z")
             (unless (zerop item) 
                (format t "/~DZ" item))
             )))
     (terpri)))

(defun eff-prst-hmlg-group-bar (fltrcm r p q)
   (declare 
     (type filtered-chain-complex fltrcm)
     (type fixnum r p q))  
   (let* ((basis-dvs (eff-prst-hmlg-basis-dvs-bar fltrcm r p q))
          (cmbns (first basis-dvs))
          (divs (second basis-dvs)))
      (declare (list basis-dvs cmbns divs))
      
      (dolist (item divs)
         (declare (type fixnum item))
         (if (not (eql 1 item))
            (progn
             (format t "~2%Component Z")
             (unless (zerop item) 
                (format t "/~DZ" item))
             )))
     (terpri)))

(defun prst-hmlg-group-total-rpq (fltrcm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-group-total eff-chcm r p q))))

(defun prst-hmlg-group-quotient-rpq (fltrcm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-group-quotient eff-chcm r p q))))

(defun prst-hmlg-group-bar-rpq (fltrcm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-group-bar eff-chcm r p q))))

(defun prst-hmlg-gnrts-total-rpq (FltrCm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-gnrts-total eff-chcm r p q))))

(defun prst-hmlg-gnrts-quotient-rpq (FltrCm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-gnrts-quotient eff-chcm r p q))))

(defun prst-hmlg-gnrts-bar-rpq (FltrCm r p q)
   (declare
    (type filtered-chain-complex fltrcm)
    (type fixnum r p q))
   (let* ((hmtp-eq (efhm fltrcm))
          (eff-chcm (rbcc hmtp-eq))
          (degr (+ p q)))
      (declare
       (type homotopy-equivalence hmtp-eq)
       (type chain-complex eff-chcm)
       (type fixnum degr))
      (progn
       (if (not (typep eff-chcm 'filtered-chain-complex))
          (translate-filtration hmtp-eq))
        (eff-prst-hmlg-gnrts-bar eff-chcm r p q))))

 (defun total-prst-hmlg-gnrts (fltrcm i j n)
   (prst-hmlg-gnrts-total-rpq fltrcm (1+ (- j i)) i (- n i) ))

(defun total-prst-hmlg-group (fltrcm i j n)
  (format t "Persistent Homology H^{~D,~D}_~D" i j n)
  (prst-hmlg-group-total-rpq fltrcm (1+ (- j i)) i (- n i) ))

 (defun prst-hmlg-gnrts-quotient (fltrcm i j n)
   (prst-hmlg-gnrts-quotient-rpq fltrcm (1+ (- j i)) i (- n i) ))

(defun prst-hmlg-group-quotient (fltrcm i j n)
  (format t "Persistent Homology H^{~D,~D}_~D" i j n)
  (prst-hmlg-group-quotient-rpq fltrcm (1+ (- j i)) i (- n i) ))


 (defun prst-hmlg-gnrts (fltrcm i j n)
   (prst-hmlg-gnrts-bar-rpq fltrcm (1+ (- j i)) i (- n i) ))

(defun prst-hmlg-group (fltrcm i j n)
  (format t "Persistent Homology B^{~D,~D}_~D" i j n)
  (prst-hmlg-group-bar-rpq fltrcm (1+ (- j i)) i (- n i) ))






;; EXAMPLE: TRIANGLE


(defun contains-element (list element)
  (let ((rslt nil))
    (dotimes (i (length list))
      (if (eq element (nth i list))
          (setf rslt 't)))
    rslt))



(setf triangle
  (build-finite-ss
   '(a b c
     1 ab (b a) ac (c a) bc (c b)
       2 abc (bc ac ab) )))



(setf triangle-flin 
      #'(lambda (degr gnrt)
       (declare 
         (type fixnum degr)
        (type gnrt gnrt))
          (let (
                (l1 (list 'a))
                (l2 (list 'b))
                (l3 (list 'c))
                (l4 (list 'ab))
                (l5 (list 'bc))
                (l6 (list 'ac))
                (l7 (list 'abc))
                (rslt -1))
            
              (if (contains-element l1 gnrt)  (setf rslt 1)
                (if (contains-element l2 gnrt)  (setf rslt 2)
                  (if (contains-element l3 gnrt)  (setf rslt 3)
                    (if (contains-element l4 gnrt)  (setf rslt 4)
                      (if (contains-element l5 gnrt)  (setf rslt 5)
                        (if (contains-element l6 gnrt)  (setf rslt 6)
                           (if (contains-element l7 gnrt)  (setf rslt 7)


                        )))))))
            rslt)))


(setf triangle (change-chcm-to-flcc triangle triangle-flin 'triangle-flin))


(dotimes (p 8)
    (print (spsq-group triangle 1 p (- 0 p)))
    (print (spsq-group triangle 1 p (- 1 p)))
    (print (spsq-group triangle 1 p (- 2 p)))
    (print (spsq-group triangle 1 p (- 3 p))))


(dotimes (p 8)
    (print (spsq-group triangle 2 p (- 0 p)))
    (print (spsq-group triangle 2 p (- 1 p)))
    (print (spsq-group triangle 2 p (- 2 p)))
  (print (spsq-group triangle 2 p (- 3 p))))


(dotimes (n 3)
  (dotimes (p 8)
    (print (prst-hmlg-group triangle p p n))
    (print (prst-hmlg-group triangle p (1+ p) n))
    (print (prst-hmlg-group triangle p (+ 2 p) n))
    (print (prst-hmlg-group triangle p (+ 3 p) n))
    (print (prst-hmlg-group triangle p (+ 4 p) n))
))




