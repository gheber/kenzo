
(DEFSTRUCT (matrice (:conc-name nil))
  leftcol uplig)

(DEFSTRUCT (t-mat (:conc-name nil))
  val ilig icol left up)

(DEFSTRUCT (CONE (:conc-name nil) (:print-function cone-print))
  (conx #+allegro :type #+allegro (member 0 1))
  (icon #+allegro :type #+allegro gnrt))

(DEFSTRUCT (BICN (:conc-name nil) (:print-function bicn-print))
  (bcnx #+allegro :type #+allegro (member :bcnb :bcnc :bcnd))
  (ibicn #+allegro :type #+allegro gnrt))

(DEFSTRUCT (TNPR (:conc-name nil) (:print-function tnpr-print))
  degr1 gnrt1 degr2 gnrt2)

(DEFSTRUCT (ALLP (:print-function allp-print))
  (list #+allegro :type #+allegro iallp))

(DEFSTRUCT ABAR
  (list #+allegro :type #+allegro list))

(DEFSTRUCT (ABSM (:conc-name nil) (:print-function absm-print))
  (dgop #+allegro :type #+allegro dgop)
  (gmsm #+allegro :type #+allegro gmsm))

(DEFSTRUCT (CRPR (:print-function crpr-print) (:conc-name nil))
  (dgop1 #+allegro :type #+allegro dgop)
  (gmsm1 #+allegro :type #+allegro gmsm)
  (dgop2 #+allegro :type #+allegro dgop)
  (gmsm2 #+allegro :type #+allegro gmsm))

(DEFSTRUCT (LOOP (:print-function loop-print))
  (list #+allegro :type #+allegro iloop))

(DEFSTRUCT (GBAR (:print-function gbar-print))
  (dmns #+allegro :type #+allegro fixnum)
  (list #+allegro :type #+allegro list))

;; In Chain-Complexes

(DEFSTRUCT (VCTR (:conc-name nil))
  stts valu incd)

;; In Delta

(DEFSTRUCT DELTA cdr)



