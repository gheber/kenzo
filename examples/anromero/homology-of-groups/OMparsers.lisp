(in-package :user)




(defun OMgetNextLabel (s)
   (let* ((p1 (search "<" s)))
      (delete-if #'(lambda (item) t) s :start 0 :end p1)
          (let ((p1 (search "<" s))
                (p2 (if (search " " s) (min (search ">" s) (search " " s))
                      (search ">" s) )))
      (subseq s (1+ p1) p2))))

(defun OMgetNextSymbol (s)
   (let* ((p1 (search "cd=" s))
          (p2 (search "name=" s))
          (p3 (search "/>" s))
          (rslt1 (subseq s (+ 3 p1) p2))
          (rslt2 (subseq s (+ 5 p2) p3))
          )
         (OMLispSymbol (list (delete #\" (delete #\space rslt1)) (delete #\" (delete #\space rslt2))))
         ))

(defun OMparseNextInteger (s)
   (if (not (equal (OMgetNextLabel s) "OMI"))
      (error "The next token is not an integer")
      (let* ((p1 (search "<OMI>" s))
             (p2 (search "</OMI>" s))
             (rslt (parse-integer (subseq s (+ 5 p1) p2))))
         (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 p2))
         rslt
         )))

(defun OMparseNextString (s)
   (if (not (equal (OMgetNextLabel s) "OMSTR"))
      (error "The next token is not a string")
      (let* ((p1 (search "<OMSTR>" s))
             (p2 (search "</OMSTR>" s))
             (rslt (subseq s (+ 7 p1) p2)))
         (delete-if #'(lambda (item) t) s :start 0 :end (+ 8 p2))
         rslt
         )))

(defun OMparseNextVar (s)
   (if (not (equal (OMgetNextLabel s) "OMV"))
      (error "The next token is not a variable")
      (let* ((p1 (search "<OMV>" s))
             (p2 (search "</OMV>" s))
             (rslt (intern (subseq s (+ 5 p1) p2))))
         (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 p2))
         rslt
         )))

(defun OMparseNextSymbol (s)
   (if (not (equal (OMgetNextLabel s) "OMS"))
      (error "The next token is not a symbol")
      (let* ((rslt (OMgetNextSymbol s))
             (p (search "/>" s))
             )
         (delete-if #'(lambda (item) t) s :start 0 :end (+ 2 p))
         rslt
         )))
  
(DEFTYPE direct-symbol () '(member list cyclicgroup))


(defun OMLispSymbol (list)
   (let ((cd (first list))
         (name (second list)))
      (if (equal cd "list1")
         (if (equal name "list")
            'list)
         (if (equal cd "resolutions")
            (if (equal name "zgcombination")
               "zgcombination"
               (if (equal name "zgterm")
                  "zgterm"
                  (if (equal name "zggnrt")
                     "zggnrt"
                     (if (equal name "resolution")
                        "resolution"))))
            (if (equal cd "group1")
               (if (equal name "cyclic_group")
                  'cyclicgroup))))))

(defun OMparseNextApplication (s)
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next token is not an application")
      (let* ((p1 (search "<OMA>" s)))
         (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 p1))
         (let ((head (if (equal (OMgetNextLabel s) "OMS")
                        (OMparseNextSymbol s)
                        (OMparseNextObject s)))
               (rest nil))
            (if (typep head 'direct-symbol)
               (do ((rest nil  (append rest (list nextobj))))
                   ((equal (OMgetNextLabel s) "/OMA") (let ((p (search "</OMA>" s)))
                                                         (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 p1))
                                                         (eval (append (list head) rest))))
                  (setf nextobj (OMparseNextObject s))
                  )
               (if (equal head "resolution")
                  (OMparseResolution s)
                  (error "imposible to parse")
            ))))))
         
         
         
           
(defun OMparseNextObject (s)
   (progn 
     (if (equal (OMgetNextLabel s) "OMOBJ")
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 7 (search "<OMOBJ>" s))))
   
     (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
      
     (let ((rslt
            (if (equal (OMgetNextLabel s) "OMI")
               (OMparseNextInteger s)
               (if (equal (OMgetNextLabel s) "OMSTR")
                  (OMparseNextString s)
                  (if (equal (OMgetNextLabel s) "OMV")
                     (OMparseNextVar s)
                     (if (equal (OMgetNextLabel s) "OMS")
                        (OMLispSymbol(OMparseNextSymbol s))
                        (if (equal (OMgetNextLabel s) "OMA")
                           (OMparseNextApplication s)
                           (error "imposible to parse"))))))))
        (progn
         (if (equal (OMgetNextLabel s) "/OMOBJ")
            (delete-if #'(lambda (item) t) s :start 0 :end (+ 8 (search "</OMOBJ>" s))))
         rslt))))



   


(defun OMparseZggnrt (s grbasis zgbasis-degr)
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application")
      (progn 
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
        (if (not (equal (OMgetNextSymbol s) "zggnrt"))
           (error "This is not specified as a zggnrt")
           (progn 
             (OMparseNextSymbol s)
             (let* ((i1 (OMparseNextInteger s))
                    (i2 (OMparseNextInteger s)))
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                (zggnrt (nth (1- i1) grbasis) (nth (1- i2) zgbasis-degr))))))))


(defun OMparseZgterm (s degr grbasis zgbasis)
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application")
      (progn 
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
        (if (not (equal (OMgetNextSymbol s) "zgterm"))
           (error "This is not specified as a zgterm")
           (progn 
             (OMparseNextSymbol s)
             (let* ((coeff (OMparseNextInteger s))
                    (zggnrt (OMparseZggnrt s grbasis (funcall zgbasis degr))))
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                (term-cmbn degr coeff zggnrt )))))))

(defun OMparseZgcmbn (s degr grbasis grcmpr zgbasis zgcmpr)
   (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
      (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application")
      (progn 
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
        (if (not (equal (OMgetNextSymbol s) "zgcombination"))
           (error "This is not specified as a zgcombination")
           (progn 
             (OMparseNextSymbol s)
             (let ((cmbn-list nil))
                (do ((term-list nil  (push  (cons 1 nexttermcmbn) term-list)))
                   ((equal (OMgetNextLabel s) "/OMA") (setf cmbn-list term-list) )
                  (setf nexttermcmbn (OMparseZgterm s degr grbasis zgbasis))
                  )
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                (if cmbn-list
                   (cmbn-cmbn (zggnrt-cmpr grcmpr zgcmpr) cmbn-list)
                   (zero-cmbn degr))))))))

(defun OMparseZgboundaryList (s grbasis grcmpr zgbasis zgcmpr maxdegr)
   (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
      (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application")
      (progn 
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
        (if (not (equal (OMgetNextSymbol s) 'list))
           (error "This is not specified as a list")
           (progn 
             (OMparseNextSymbol s)
             (let ((rslt nil))
                (dotimes (i maxdegr)
                   (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
                      (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
                   (if (not (equal (OMgetNextLabel s) "OMA"))
                      (error "The next label is not an application")
                      (progn 
                        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
                        (if (not (equal (OMgetNextSymbol s) 'list))
                           (error "This is not specified as a list")
                           (progn 
                             (OMparseNextSymbol s)
                             (let ((rslt-i nil))
                                (do ((aux nil (push cmbn aux)))
                                    ((equal (OMgetNextLabel s) "/OMA") (setf rslt-i aux))
                                   (setf cmbn (OMparseZgcmbn s i grbasis grcmpr zgbasis zgcmpr)))
                                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                                (push (nreverse rslt-i) rslt)))))))
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                (nreverse rslt)))))))
                   

(defun OMparseContractinghmtpList (s grbasis grcmpr zgbasis zgcmpr maxdegr)
   (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
      (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
   (if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application")
      (progn 
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
        (if (not (equal (OMgetNextSymbol s) 'list))
           (error "This is not specified as a list")
           (progn 
             (OMparseNextSymbol s)
             (let ((rslt nil))
                (dotimes (i (1- maxdegr))
                   (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
                      (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
                   (if (not (equal (OMgetNextLabel s) "OMA"))
                      (error "The next label is not an application")
                      (progn 
                        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
                        (if (not (equal (OMgetNextSymbol s) 'list))
                           (error "This is not specified as a list")
                           (progn 
                             (OMparseNextSymbol s)
                             (let ((rslt-i nil))
                                (do ((mark1 (funcall zgbasis (1+ i)) (cdr mark1)))
                                    ((endp mark1) (push (nreverse rslt-i) rslt))
                                    (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
                                       (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))
                                   (if (not (equal (OMgetNextLabel s) "OMA"))
                                      (error "The next label is not an application")
                                      (progn 
                                        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
                                        (if (not (equal (OMgetNextSymbol s) 'list))
                                           (error "This is not specified as a list")
                                           (progn 
                                             (OMparseNextSymbol s)
                                             (let ((rslt-i-g nil))
                                                
                                                (do ((mark2 grbasis (cdr mark2)))
                                                    ((endp mark2) (push (nreverse rslt-i-g) rslt-i))
                                                   (setf cmbn (OMparseZgcmbn s (1+ i) grbasis grcmpr zgbasis zgcmpr))
                                                   (push cmbn rslt-i-g))
                                                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                                                ))))))
                                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                                ))))))
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                (nreverse rslt)))))))



(defun OMrsltnZgbasis (rank-list maxdegr)
   (flet ((rslt (degr)
            (declare (fixnum degr))
            (if (> degr maxdegr) (error "unknown basis for this degree")
               (if (< degr 0) nil
                  (let ((rank (nth degr rank-list))
                        (list nil))
                     (dotimes (i rank)
                        (push (concatenate 'string "g" (Write-to-string degr) (Write-to-string (1+ i))) list))
                     (nreverse list))))))
     
     (the basis #'rslt)))

(defun OMrsltnZgboundary-intr (Zgboundary-list zgbasis maxdegr)
   (flet ((rslt (degr gnrt)
            (declare (fixnum degr) (type gnrt gnrt))
            (if (> degr maxdegr) (error "unknown basis for this degree")
               (if (< degr 1) (zero-cmbn (1- degr))
                  (nth (position gnrt (funcall zgbasis degr)  :test #'equal) (nth (1- degr) Zgboundary-list))))))
          (the intr-mrph #'rslt)))

(defun OMrsltnCntrHmtp-intr (Zgcontractinghmtp-list zgbasis grbasis maxdegr)
   (flet ((rslt (degr zggnrt)
            (declare (fixnum degr) (type zggnrt zggnrt))
            (if (> degr (1- maxdegr)) (error "unknown basis for this degree")
               (with-zggnrt (gnrt1 gnrt2) zggnrt
                 (let ((gnrt1-pos (position gnrt1 grbasis :test #'equal))
                       (gnrt2-pos (position gnrt2 (funcall zgbasis degr) :test #'equal)))
                    (nth gnrt1-pos (nth gnrt2-pos (nth degr Zgcontractinghmtp-list))))))))
               (the intr-mrph #'rslt)))



                             
                             
(defun OMparseResolution (s)
   ;;(if (not (equal (OMgetNextLabel s) "OMA"))
    ;;  (error "The next label is not an application")
    ;;  (progn 
    ;;    (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
    ;;    (if (not (equal (OMgetNextSymbol s) "resolution"))
    ;;       (error "This is not specified as a resolution")
    ;;       (progn 
    ;;         (OMparseNextSymbol s)
             (let* ((group (OMparseNextObject s))
                    (maxdegr (OMparseNextObject s))
                    (rank-list (OMparseNextObject s))
                    (zgbasis (OMrsltnZgbasis rank-list maxdegr)))
                (with-slots ((grcmpr cmpr) (grbasis elements) nullel) group
                  (let* ((zgboundary-list (OMparseZgboundaryList s grbasis grcmpr zgbasis #'s-cmpr maxdegr))
                         (cntrchmtp-list (OMparseContractinghmtpList s grbasis grcmpr zgbasis #'s-cmpr maxdegr))
                         (zgchcm (BUILD-ZGCHCM :group group :zgcmpr #'s-cmpr :zgbasis zgbasis :zgbsgn "g00" 
                                   :intr-zgbndr (OMrsltnZgboundary-intr Zgboundary-list zgbasis maxdegr)
                                   :zgbndr-strt :gnrt :orgn  `(ZG-Chain complex for ,group obtained from HAP)))
                         (cmpr (cmpr1 zgchcm))
                         (f-intr #'(lambda (degr zggnrt)
                                     (if (zerop degr) (cmbn degr 1 :zgnrt) (zero-cmbn degr))))
                         (g-intr #'(lambda (degr gnrt)
                                     (if (zerop degr) (cmbn degr 1 (zggnrt nullel "g01")) (zero-cmbn degr))))
                         (h-intr (OMrsltnCntrHmtp-intr cntrchmtp-list zgbasis grbasis maxdegr))

                         (f (build-mrph :sorc zgchcm :trgt (z-chcm) :degr 0 :intr f-intr :strt :gnrt :orgn `(f-mrph for ,group obtained from HAP)))
                         (g (build-mrph :sorc (z-chcm) :trgt zgchcm :degr 0 :intr g-intr :strt :gnrt :orgn  `(g-mrph for ,group obtained from HAP)))
                         (h (build-mrph :sorc zgchcm :trgt zgchcm :degr 1 :intr h-intr :strt :gnrt :orgn `(h-mrph for ,group obtained from HAP))))
                     (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                     (build-rdct :f f :g g :h h :orgn `(Resolution of ,group obtained from HAP))))))
;;;))))
                       
(defun FileToString (FileName)
     (with-open-file (f FileName :direction :input)
       (labels ((getString (f s)
                 (let* ((aux (read-line f nil 'EOF)))
                    (if (not (string= aux 'EOF))
                       (getString f (concatenate 'string s (if (not (string= s "")) (format nil "~%")) aux)) 
                       s))))
       (getString f ""))))

                             

#|



   

(cat-init)
(load "omparsers.lisp")
(setf s (filetostring "resolution.log")) 
(setf rsltn1 (OmparseNextObject s))
(setf group (group1 (tcc rsltn1)))
(setf (slot-value group 'resolution) rsltn1)
(setf k-g-1 (k-g-1 group))
(setf k-g-2 (k-g group 2))
(setf k-g-3 (k-g group 3))

(homology k-g-1 0 6)
(homology k-g-2 0 6)
(homology k-g-3 0 6)



(if (equal (OMgetNextLabel s) "OMOBJ")
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 7 (search "<OMOBJ>" s))))
   
     (if (equal (subseq (OMgetNextLabel s) 0 3) "!--")
        (delete-if #'(lambda (item) t) s :start 0 :end (+ 4 (search "--/>" s))))


(if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application"))
(delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
(if (not (equal (OMgetNextSymbol s) "resolution"))
   (error "This is not specified as a resolution"))

(OMparseNextSymbol s)

(setf group (OMparseNextObject s))
(setf maxdegr (OMparseNextObject s))
(setf rank-list (OMparseNextObject s))
(setf zgbasis (OMrsltnZgbasis rank-list maxdegr))

(setf grcmpr (cmpr1 group) grbasis (elements group) nullel (nullel group))
(setf zgcmpr #'s-cmpr)
(setf zgboundary-list (OMparseZgboundaryList s grbasis grcmpr zgbasis #'s-cmpr maxdegr))
;;(setf cntrchmtp-list (OMparseContractinghmtpList s grbasis grcmpr zgbasis #'s-cmpr maxdegr))                                  
                         
(if (not (equal (OMgetNextLabel s) "OMA"))
      (error "The next label is not an application"))

(delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))

(if (not (equal (OMgetNextSymbol s) 'list))
           (error "This is not specified as a list"))
(OMparseNextSymbol s)

(setf rslt nil)

(setf i 0)
(if (not (equal (OMgetNextLabel s) "OMA"))
                      (error "The next label is not an application"))
(delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
(OMparseNextSymbol s)
(setf rslt-i nil)  

(setf mark1 zgbasis)


                (dotimes (i maxdegr)
                   (if (not (equal (OMgetNextLabel s) "OMA"))
                      (error "The next label is not an application")
                      (progn 
                        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
                        (if (not (equal (OMgetNextSymbol s) 'list))
                           (error "This is not specified as a list")
                           (progn 
                             (OMparseNextSymbol s)
                             (let ((rslt-i nil))
                                (do ((mark1 (funcall zgbasis (1+ i)) (cdr mark1)))
                                    ((endp mark1) (push rslt-i rslt))
                                    
                                   (if (not (equal (OMgetNextLabel s) "OMA"))
                                      (error "The next label is not an application")
                                      (progn 
                                        (delete-if #'(lambda (item) t) s :start 0 :end (+ 5 (search "<OMA>" s)))
                                        (if (not (equal (OMgetNextSymbol s) 'list))
                                           (error "This is not specified as a list")
                                           (progn 
                                             (OMparseNextSymbol s)
                                             (let ((rslt-i-g nil))
                                                
                                                (do ((mark2 grbasis (cdr mark2)))
                                                    ((endp mark2) (push rslt-i-g rslt-i))
                                                   (setf cmbn (OMparseZgcmbn s (1+ i) grbasis grcmpr zgbasis zgcmpr))
                                                   (push cmbn rslt-i-g))
                                                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                                                ))))))
                                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                                ))))))
                (delete-if #'(lambda (item) t) s :start 0 :end (+ 6 (search "</OMA>" s)))
                rslt)))))



|# 







