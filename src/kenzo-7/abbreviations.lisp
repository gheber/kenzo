;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(IN-PACKAGE #:cat-7)

(PROVIDE "abbreviations")

(DEFINE-CONSTANT +ABBREVIATIONS+
    '(
      ;; A
      :ABAR  "Algebraic-BAR"
      :ABSM  "ABstract SiMplex"
      :ALLP  "ALgebraic-LooP"
      :APOWR "Abstract POWeR"

      ;; B
      :BCC   "Bottom Chain Complex"
      :BICN  "BI-CoNe"
      :BNDR  "BouNDaRy"
      :BRGN  "BaR-GeNerator"
      :BSGN  "BaSe GeNerator"
      :BSPN  "BaSe-PoiNt"

      ;; C
      :CBGN  "CoBar-GeNerator"
      :CFFC  "CoeFFiCient"
      :CHCM  "CHain-CoMplex"
      :CHML  "CoHoMoLogy"
      :CLNM  "CaL NuMber"
      :CMBN  "CoMBiNation"
      :CMPR  "CoMPaRison"
      :CMPRF "CoMPaRison Function"
      :CMPS  "CoMPoSition"
      :CPRD  "CoPRoDuct"
      :CRPR  "CaRtesian-PRoduct"

      ;; D
      :DEGR  "DEGRee"
      :DFFR  "DiFFeRential"
      :DGOP  "DeGeneracy OPerator"
      :DLOP  "DeL OPerator"
      :DGNL  "DiaGoNaL"
      :DMNS  "DiMeNSion"

      ;; E
      :EFHM  "EFfective HoMology"
      :EXPN  "EXPoNent"

      ;; G
      :GBAR  "Geometrical BAR"
      :GMSM  "GeoMetric SiMplex"
      :GNRT  "GeNeRaTor"
      :GRIN  ""
      :GRMD  "GRound MoDule"

      ;; H
      :HMEQ  "HoMtopy-EQuivalence"
      :HMTP  "HoMoToPy"
      :HRZN  "HoRiZoNtal"

      ;; I
      :IABSM "Internal ABstract SiMplex"
      :IALLP "Internal-ALgebraic LooP"
      :ICMBN "Internal-CoMBiNation"
      :IDNM  "IDentification NuMber"
      :IDNT  "IDeNTity"
      :ILOOP "Internal LOOP"
      :INTR  "INTeRnal-"

      ;; K
      :KFLL  "\"Kan hat\" FiLLing"

      ;; L
      :LF    "Left F"
      :LG    "Left G"
      :LH    "Left H"
      :LRDCT "Left ReDuCTion"

      ;; M
      :MRPH  "MoRPHism"

      ;; O
      :OPPS  "OPPoSite"
      :ORGN  "ORiGiN"

      ;; P
      :POWR  "POWeR"
      :PRDC  "PRoDuCt"

      ;; R
      :RBCC  "Bottom Right Chain Complex"
      :RDCT  "ReDuCTion"
      :RF    "Right F"
      :RG    "Right G"
      :RH    "Right H"
      :RNTM  "RuNTiMe"
      :RRDCT "Right ReDuCTion"
      :RSLTS "ReSuLTs"

      ;; S
      :SBTR  "SuBTRact"
      :SMGR  "SiMplicial-GRoup"
      :SMMR  "SiMplicial-MorPhism"
      :SMST  "SiMplicial-SeT"
      :SORC  "SOuRCe"
      :STRT  "STRaTegy"

      ;;T
      :TCC   "Top Chain Complex"
      :TNPR  "TeNsorPRoduct"
      :TNSR  "TeNSoR"
      :TRGT  "TaRGeT"

      ;; V
      :VRTC  "VeRTiCal"
      ))


(DEFUN what-is (kwd)
  (declare (type (or keyword string symbol) kwd))
  "WHAT-IS returns the full word or term for an abbreviation provided as a
keyword, string, or symbol. It returns NIL for unknown abbreviations."
  (unless (null kwd)
    (getf +ABBREVIATIONS+
          (cond ((keywordp kwd) kwd)
                ((stringp kwd) (intern (string-upcase kwd) 'keyword))
                ((symbolp kwd) (intern (symbol-name kwd) 'keyword))))))
