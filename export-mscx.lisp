;;;========================
;;; OpenMusic export MSCX 
;;;========================

;;
;;
;; requires existing code in OMs 'import-export/export-mxml.lisp'
;;

(defpackage "MSCX" 
  (:use "COMMON-LISP")
  (:use "MusicXML")
  (:use :om)
  (:nicknames :mscx))

(in-package :mscx)

(pushnew :mscx *features*)

(defvar *xml-version* "XML 1.0")

(defun mscx-header ()
  (list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))


;;;
;;; PITCH, PITCH CLASS, ACCIDENTALS, MICROTONALS
;;;
;;;

(defun om-midic-to-midi (midic)
  "Legacy helper: e.g. 6000 -> midi 60.
For microtonal MSCX export, we dont use this as written pitch truth."
  (round (/ midic 100)))

(defparameter *mscx-tpc-table*
  '(("Cbb" . 0) ("Gbb" . 1) ("Dbb" . 2) ("Abb" . 3) ("Ebb" . 4) ("Bbb" . 5)
    ("Fb" . 6) ("Cb" . 7) ("Gb" . 8) ("Db" . 9) ("Ab" . 10) ("Eb" . 11)
    ("Bb" . 12) ("F" . 13) ("C" . 14) ("G" . 15) ("D" . 16) ("A" . 17)
    ("E" . 18) ("B" . 19) ("F#" . 20) ("C#" . 21) ("G#" . 22) ("D#" . 23)
    ("A#" . 24) ("E#" . 25) ("B#" . 26) ("F##" . 27) ("C##" . 28) ("G##" . 29)
    ("D##" . 30) ("A##" . 31) ("E##" . 32) ("B##" . 33)))

(defun alteration->acc-string (alteration)
  (case alteration
    ((nil 0) "")
    (-2 "bb")
    (-1 "b")
    (1 "#")
    (2 "##")
    (t "")))

(defun step+alter-to-tpc (step alteration)
  (or (cdr (assoc (format nil "~A~A" step (alteration->acc-string alteration))
                  *mscx-tpc-table* :test #'string=))
      14))

;;;
;;; MSCX pitch export policy
;;;
;;; export-mscx is primarily a preserving/export function, taking users OM data at face value
;;;
;;; - MIDIC is treated as the sounding-pitch source.
;;; - OM tonalite/editor spelling is treated as the written-spelling source when available.
;;; - APPROX defaults to :NONE and means: do not force pitch approximation.
;;; - Non-:NONE APPROX is only an explicit fallback/quantization request.
;;; - Unsupported microtonal spellings warn by default, but should not silently collapse.

(defvar *mscx-export-warnings* nil)

(defun approx-none-p (approx)
  "True when MSCX export should not force pitch approximation.
NIL is accepted as preserve-mode too, for robustness in older internal calls."
  (or (null approx) (eql approx :none)))

(defun mscx-export-warning (policy fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (push msg *mscx-export-warnings*)
    (case policy
      (:ignore nil)
      (:error (error "~A" msg))
      (otherwise (warn "~A" msg)))
    msg))

(defstruct mscx-written-pitch
  pitch                  ; MuseScore <pitch>, integer MIDI base pitch
  tpc                    ; MuseScore <tpc>
  accidental-subtype     ; MuseScore accidental subtype string, or NIL
  midic                  ; original OM midic
  cents-offset           ; offset from written base pitch, in cents
  spelling-source        ; :tonalite, :approx, :nearest, :fallback
  warning)               ; warning string, or NIL

;; ENHARMONICS
;;
;; lives in OMs 'tonalite class, for each note-object
;;
;; the symbols inside tonalite are set in om-package: eg om:do, om::bemol.
;; compare as keywords (string= / string-equal can compare :keywords)

(defun om-tonnote->step-string (tonnote)
  (cond ((string-equal tonnote :do)  "C")
        ((string-equal tonnote :re)  "D")
        ((string-equal tonnote :mi)  "E")
        ((string-equal tonnote :fa)  "F")
        ((string-equal tonnote :sol) "G")
        ((string-equal tonnote :la)  "A")
        ((string-equal tonnote :si)  "B")
        (t nil)))

(defun om-tonalt->acc-string (tonalt)
  (flet ((accidental-tag (x)
           (cond ((null x) :none)
                 ((and (consp x) (= (length x) 2) (every #'(lambda (y) (string-equal y :diese)) x))
                  :double-sharp)
                 ((and (consp x) (= (length x) 2) (every #'(lambda (y) (string-equal y :bemol)) x))
                  :double-flat)
                 ((string-equal x :becarre) :natural)
                 ((string-equal x :diese) :sharp)
                 ((string-equal x :bemol) :flat)
                 (t nil))))
    (case (accidental-tag tonalt)
      (:none "")
      (:natural "")
      (:sharp "#")
      (:flat "b")
      (:double-sharp "##")
      (:double-flat "bb")
      (otherwise nil))))

(defun acc-string->alter (acc)
  (cond ((or (null acc) (string= acc "")) 0)
        ((string= acc "#") 1)
        ((string= acc "##") 2)
        ((string= acc "b") -1)
        ((string= acc "bb") -2)
        (t 0)))

(defun om-tonalite-to-tpc (tonalite)
  (let* ((step (and tonalite (om-tonnote->step-string (om::tonnote tonalite))))
         (acc  (and tonalite (om-tonalt->acc-string (om::tonalt tonalite))))
         (name (and step acc (format nil "~A~A" step acc))))
    (and name (cdr (assoc name *mscx-tpc-table* :test #'string=)))))

(defun step-string->pc (step)
  (cond ((string= step "C") 0)
        ((string= step "D") 2)
        ((string= step "E") 4)
        ((string= step "F") 5)
        ((string= step "G") 7)
        ((string= step "A") 9)
        ((string= step "B") 11)
        (t nil)))

(defun midi-pc->default-step+alter (pc)
  "Conservative sharp-oriented fallback spelling.
Only used when OM/editor spelling is unavailable."
  (case (mod pc 12)
    (0  (values "C" 0))
    (1  (values "C" 1))
    (2  (values "D" 0))
    (3  (values "D" 1))
    (4  (values "E" 0))
    (5  (values "F" 0))
    (6  (values "F" 1))
    (7  (values "G" 0))
    (8  (values "G" 1))
    (9  (values "A" 0))
    (10 (values "A" 1))
    (11 (values "B" 0))))

(defun written-base-midi-pitch (midic step alter)
  "Return integer MIDI pitch for the written base note closest to MIDIC.

Examples:
  C quarter-sharp,  midic 6050, step C alter 0 -> 60
  C# quarter-flat,  midic 6050, step C alter 1 -> 61

MSCX <pitch> must always be an integer."
  (let* ((pc (+ (or (step-string->pc step) 0) alter))
         (sounding-midi (/ midic 100.0))
         (octave-index (round (/ (- sounding-midi pc) 12))))
    (round (+ pc (* 12 octave-index)))))

(defun cents-offset-from-base (midic base-midi)
  (- midic (* base-midi 100)))

(defun cents-close-p (a b &optional (epsilon 0.01))
  (< (abs (- a b)) epsilon))

(defun note-to-mscx-tpc (note approx)
  (let* ((ton (om::tonalite note))
         (tpc-from-tonalite (and ton (om-tonalite-to-tpc ton))))
    (or tpc-from-tonalite
        (let* ((note-values (mxml::mc->xmlvalues (om::midic note) approx))
               (step (nth 1 note-values))
               (alteration (nth 2 note-values)))
          (step+alter-to-tpc step alteration))
        14)))


;;; ACCIDENTAL MAPPINGS
;;;
;;; found in MuseScore's ACC_LIST - ./src/engraving/dom/accidental.cpp
;;;
;;;
;;; this doesnt support OMs EDO_nn system, instead notational systems as indicated.
;;;
;;; OM support could be added e.g in a lib, perhaps its already available in some lib?  Check with KARIM
;;;
;;;
;; :family :gould-arrow        -> kvarttone-pilene fra MuseScore-eksemplene
;; :family :stein-zimmermann   -> alternativ 24-EDO-ish
;; :family :wyschnegradsky     -> 72-EDO/twelfths-ish oppløsning
;; :family :persian / :turkish -> der MuseScore faktisk har centOffset i ACC_LIST
;; :family :auto               -> første eksakte treff i tabellen


(defparameter *mscx-accidental-family* :gould-arrow)

(defparameter *mscx-microtone-accidental-table*
  '((:subtype "accidentalQuarterToneFlatArrowUp" :cent-offset -50 :family :gould-arrow :priority 10)
    (:subtype "accidentalQuarterToneSharpArrowDown" :cent-offset 50 :family :gould-arrow :priority 10)
    (:subtype "accidentalThreeQuarterTonesFlatArrowDown" :cent-offset -150 :family :gould-arrow :priority 10)
    (:subtype "accidentalThreeQuarterTonesSharpArrowUp" :cent-offset 150 :family :gould-arrow :priority 10)

    (:subtype "accidentalQuarterToneFlatStein" :cent-offset -50 :family :stein-zimmermann :priority 10)
    (:subtype "accidentalQuarterToneSharpStein" :cent-offset 50 :family :stein-zimmermann :priority 10)
    (:subtype "accidentalThreeQuarterTonesFlatZimmermann" :cent-offset -150 :family :stein-zimmermann :priority 10)
    (:subtype "accidentalThreeQuarterTonesSharpStein" :cent-offset 150 :family :stein-zimmermann :priority 10)

    (:subtype "accidentalWyschnegradsky1TwelfthsFlat" :cent-offset -17 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky1TwelfthsSharp" :cent-offset 17 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky2TwelfthsFlat" :cent-offset -33 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky2TwelfthsSharp" :cent-offset 33 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky3TwelfthsFlat" :cent-offset -50 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky3TwelfthsSharp" :cent-offset 50 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky4TwelfthsFlat" :cent-offset -67 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky4TwelfthsSharp" :cent-offset 67 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky5TwelfthsFlat" :cent-offset -83 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky5TwelfthsSharp" :cent-offset 83 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky7TwelfthsFlat" :cent-offset -117 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky7TwelfthsSharp" :cent-offset 117 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky8TwelfthsFlat" :cent-offset -133 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky8TwelfthsSharp" :cent-offset 133 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky9TwelfthsFlat" :cent-offset -150 :family :wyschnegradsky :priority 10)
    (:subtype "accidentalWyschnegradsky9TwelfthsSharp" :cent-offset 150 :family :wyschnegradsky :priority 10)

    (:subtype "accidentalSori" :cent-offset 33 :family :persian :priority 10)
    (:subtype "accidentalKoron" :cent-offset -67 :family :persian :priority 10)

    (:subtype "accidentalBuyukMucennebFlat" :cent-offset -89 :family :turkish :priority 10)
    (:subtype "accidentalBakiyeFlat" :cent-offset -44 :family :turkish :priority 10)
    (:subtype "accidentalKucukMucennebSharp" :cent-offset 56 :family :turkish :priority 10)
    (:subtype "accidentalBuyukMucennebSharp" :cent-offset 89 :family :turkish :priority 10)))

(defun accidental-row-cent-offset (row)
  (getf row :cent-offset))

(defun accidental-row-family (row)
  (getf row :family))

(defun accidental-row-priority (row)
  (or (getf row :priority) 0))

(defun mscx-accidental-subtype-for-cents (cents &key (family *mscx-accidental-family*))
  "Find MuseScore accidental subtype for CENTS.
FAMILY chooses between multiple MuseScore glyph systems with the same or similar cent offset."
  (let* ((candidates
          (remove-if-not #'(lambda (row)
                             (and (or (eql family :auto) (eql (accidental-row-family row) family))
                                  (cents-close-p cents (accidental-row-cent-offset row))))
                         *mscx-microtone-accidental-table*))
         (best (car (sort (copy-list candidates) #'> :key #'accidental-row-priority))))
    (and best (getf best :subtype))))


(defun note-tonalite-step+alter+tpc (note)
  "Return STEP, ALTER, TPC from OM tonalite, if available."
  (let* ((ton (om::tonalite note))
         (step (and ton (om-tonnote->step-string (om::tonnote ton))))
         (acc-string (and ton (om-tonalt->acc-string (om::tonalt ton))))
         (alter (acc-string->alter acc-string))
         (tpc (and ton (om-tonalite-to-tpc ton))))
    (when (and step tpc)
      (values step alter tpc))))

(defun approx-note-step+alter+tpc (note approx)
  "Explicit approximation fallback, only used when APPROX is not :NONE/NIL."
  (let* ((note-values (mxml::mc->xmlvalues (om::midic note) approx))
         (step (nth 1 note-values))
         (alter (nth 2 note-values))
         (tpc (step+alter-to-tpc step alter)))
    (values step alter tpc)))

(defun nearest-note-step+alter+tpc (note)
  "Preserve-mode fallback when no OM/editor spelling is available.
This does not claim semantic correctness; it is just a conservative display base."
  (let* ((nearest-midi (round (/ (om::midic note) 100)))
         (pc (mod nearest-midi 12)))
    (multiple-value-bind (step alter) (midi-pc->default-step+alter pc)
      (values step alter (step+alter-to-tpc step alter)))))

(defun note-to-mscx-written-pitch (note &key (approx :none) (unsupported-microtones :warn)
                                         (accidental-family *mscx-accidental-family*))
  "Return MSCX-WRITTEN-PITCH for NOTE.
APPROX :NONE means preserve/editor-driven mode."
  (let* ((midic (om::midic note)))
    (multiple-value-bind (step alter tpc) (note-tonalite-step+alter+tpc note)
      (let ((source :tonalite))
        (unless tpc
          (if (approx-none-p approx)
              (progn
                (setf source :nearest)
                (multiple-value-setq (step alter tpc) (nearest-note-step+alter+tpc note)))
            (progn
              (setf source :approx)
              (multiple-value-setq (step alter tpc) (approx-note-step+alter+tpc note approx)))))

        (let* ((base-midi (round (written-base-midi-pitch midic step alter)))
               (cents-offset (cents-offset-from-base midic base-midi))
               (accidental-subtype
                 (unless (cents-close-p cents-offset 0)
                   (mscx-accidental-subtype-for-cents cents-offset :family accidental-family)))
               (warning nil))

          (when (and (not (cents-close-p cents-offset 0)) (null accidental-subtype))
            (setf warning
                  (mscx-export-warning
                   unsupported-microtones
                   "MSCX export: unsupported microtonal offset ~,3F cents for note midic=~A, base pitch=~A, tpc=~A."
                   cents-offset midic base-midi tpc)))

          (when (and (eq source :nearest) (not (cents-close-p cents-offset 0)))
            (let ((msg
                   (mscx-export-warning
                    unsupported-microtones
                    "MSCX export: no OM tonalite/editor spelling found for microtonal note midic=~A. Used nearest fallback spelling: pitch=~A, tpc=~A, offset=~,3F cents."
                    midic base-midi tpc cents-offset)))
              (setf warning (or warning msg))))

          (make-mscx-written-pitch :pitch base-midi :tpc tpc :accidental-subtype accidental-subtype
                                   :midic midic :cents-offset cents-offset :spelling-source source
                                   :warning warning))))))

(defun note-to-mscx-tpc (note approx)
  "Legacy helper. Prefer NOTE-TO-MSCX-WRITTEN-PITCH."
  (mscx-written-pitch-tpc (note-to-mscx-written-pitch note :approx approx)))



;;; ==========================================
;;; CLEFS
;;; ==========================================

(defun clef-sign->mscx-clef (sign &optional line)
  (unless (or (symbolp sign) (stringp sign) (characterp sign))
    (error "Invalid clef sign: ~S" sign))
  (cond
    ((and (string= sign :c) (= line 1)) "C1")
    ((and (string= sign :c) (= line 3)) "C3")
    ((and (string= sign :c) (= line 4)) "C4")
    ((string= sign :g) "G")
    ((string= sign :g_8) "G8vb")
    ((string= sign :g^8) "G8va")
    ((string= sign :f) "F")
    ((string= sign :f_8) "F8va")
    ((string= sign :empty) "PERC")
    (t
     (error "Unsupported OM clef for MSCX export: ~S~@[ line ~A~]"
            sign line))))


;;;
;;; DURATIONS
;;;


(defun xml-head-to-mscx-duration-type (note-head)
  "Current mxml::*note-types* gives MusicXML type names. For this hack we reuse them."
  note-head)

(defun ratio-base-note-name (dur)
  "Map a duration unit to a MuseScore baseNote string.
Hack v1: reuse the same duration naming logic as for Chord/Rest."
  (let* ((head-and-pts (mxml::get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car))))
    note-head))

(defun ratio->mscx-fraction-string (r)
  (cond ((integerp r) (format nil "~D" r))
        ((rationalp r) (format nil "~D/~D" (numerator r) (denominator r)))
        (t (format nil "~A" r))))

(defun mscx-actual-fraction-dur (self free)
  "Real timeline duration for tie/spanner location.
FREE is the written duration used for durationType."
  (let* ((written (if (listp free) (car free) free))
         (ratio (mxml::time-mod-val self)))
    (if (and ratio (not (= (car ratio) (cadr ratio))))
        (* written (/ (cadr ratio) (car ratio)))   ;; normal / actual
      written)))

(defun current-mscx-dur (free)
  (if (listp free) (car free) free))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EXTRAS
;;;


;; TEXT extras

;; ("f" "R" "^" "d" "n" "S" "Q" "P" "_" "`" "a" "b" "c" "d" "e" "f" "g" "h" "i")

;; ((1 "f") (2 "R") (3 "^") (4 "d")
;; (5 "n") (6 "S") (7 "Q") (8 "P")
;; (9 "_") (10 "`") (11 "a") (12 "b")
;; (13 "c") (14 "d") (15 "e") (16 "f")
;; (17 "g") (18 "h") (19 "i"))

(defparameter *om-head-text=>mscx-head-text*
  '(("f" . "triangle-up")
    ("R" . "altbrevis") 				    ; + head-type brevis
    ("^" . "triangle-down")				    ; + head-type semibreve
    ("d" . "diamond")
    ("n" . "do")
    ("S" . "breve + double || bars")			    ; + note-head brevis
    ("Q" . "breve")					    ; + note-head brevis
    ("P" . "altbrevis")					    ; + note-head semi-brevis
    ("_" . "triangle-down")
    ("`" . "heavy-cross")
    ("a" . "diamond")
    ("b" . "la")					    ;(square) + note-head semi-brevis
    ("c" . "large-diamond")				    ;+note-head semi-brevis
    ("d" . "large-diamond")
    ("e" . "la")					    ;(square)
    ("f" . "fa")
    ("g" . "mi")
    ("h" . "xcircle")
    ("i" . "sol")
    ))

(defun get-extra-by-kind (self kind)
  (car (om::get-extras self kind)))

(defun head-extra-for-note (note)
  (get-extra-by-kind note "head"))

(defun head-extra->mscx-head (extra)
  (and extra
       (cdr (assoc (om::thehead extra)
                   *om-head-text=>mscx-head-text*
                   :test #'equal))))

(defun note-head-as-mscx (note)
  (let ((head (head-extra->mscx-head (head-extra-for-note note))))
    (when head
      (list (format nil "<head>~A</head>" head)))))


;;;
;;; manual lookup of various 'staff-text-w-playback-mods' in mscore
;;;
;;; available ":kind"s - treated w. different tags and structure:
;;; 
;;;	:sound-flag
;;;	:staff-text
;;;	:play-tech
;;;
;;; see #'text-extra-as-mscx below for use

(defparameter *om-text-extra-rules* 
  ;; for smart playback w some musesounds
  ;; 
  ;; manually looking up mappings to various <playingTechnique>  in .mscx files

  '(("pizz"      :kind :sound-flag :playing-technique "pizzicato" :text "pizzicato")
    ("pizz."     :kind :sound-flag :playing-technique "pizzicato" :text "pizzicato")
    ("pizzicato" :kind :sound-flag :playing-technique "pizzicato" :text "pizzicato")

    ("arco"      :kind :sound-flag :playing-technique "ordinary_technique" :text "arco")
    ("ord"       :kind :sound-flag :playing-technique "ordinary_technique" :text "ordinary")
    ("ordinary"  :kind :sound-flag :playing-technique "ordinary_technique" :text "ordinary")

    ("col legno" :kind :sound-flag :playing-technique "Col Legno" :text "col legno")
    ("sul pont"  :kind :sound-flag :playing-technique "Sul Ponticello" :text "sul ponticello")
    ("sul pont." :kind :sound-flag :playing-technique "Sul Ponticello" :text "sul ponticello")
    ("sul ponticello" :kind :sound-flag :playing-technique "Sul Ponticello" :text "sul ponticello")
    ("sul tasto" :kind :sound-flag :playing-technique "Sul Tasto" :text "sul tasto"))
  )

(defun staff-text-sound-flag-as-mscx (text playing-technique)
  (list "<StaffText>"
        (format nil "<text>~A</text>" text)
        "<SoundFlag>"
        (format nil "<playingTechnique>~A</playingTechnique>" playing-technique)
        "</SoundFlag>"
        "</StaffText>"))

(defun normalize-extra-text (s)
  (string-downcase (string-trim '(#\Space #\Tab #\Newline) (or s ""))))

(defun text-extra-for-chord (chord)
  (get-extra-by-kind chord "text"))

(defun text-extra-text (extra)
  (and extra (om::thetext extra)))

(defun text-extra-rule (extra)
  (let* ((text (text-extra-text extra))
         (key (and text (normalize-extra-text text))))
    (and key
         (assoc key *om-text-extra-rules* :test #'string=))))

(defun rule-prop (rule key)
  (getf (cdr rule) key))

(defun play-tech-annotation-as-mscx (text &optional play-tech-type)
  (append
   (list "<PlayTechAnnotation>")
   (when play-tech-type
     (list (format nil "<playTechType>~A</playTechType>" play-tech-type)))
   (list (format nil "<text>~A</text>" text)
         "</PlayTechAnnotation>")))

(defun staff-text-as-mscx (text)
  (list "<StaffText>"
        (format nil "<text>~A</text>" text)
        "</StaffText>"))

(defun mscx-accidental-as-lines (subtype)
  (when subtype
    (list "<Accidental>" "<role>1</role>" (format nil "<subtype>~A</subtype>" subtype) "</Accidental>")))

(defun mscx-unsupported-microtone-marker-as-lines (wp unsupported-microtones)
  "Optional visible marker in score. Default is off to not clutter
:mark adds a simple StaffText \"!\" before the chord/note."
  (when (and (eql unsupported-microtones :mark) (mscx-written-pitch-warning wp))
    (staff-text-as-mscx "!")))

(defun text-extra-as-mscx (chord)
  (let* ((extra (text-extra-for-chord chord))
         (raw-text (text-extra-text extra))
         (rule (and extra (text-extra-rule extra))))
    (cond
      ((null extra) nil)
      (rule
       (case (rule-prop rule :kind)
         (:sound-flag
	  (staff-text-sound-flag-as-mscx
	   (or (rule-prop rule :text) raw-text)
	   (rule-prop rule :playing-technique)))
	 (:play-tech
          (play-tech-annotation-as-mscx (or (rule-prop rule :text) raw-text)
                                        (rule-prop rule :play-tech-type)))
         (:staff-text
          (staff-text-as-mscx (or (rule-prop rule :text) raw-text)))
         (otherwise
          (staff-text-as-mscx raw-text))))
      (raw-text
       (staff-text-as-mscx raw-text))
      (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DYNAMICS
;; 
;; velocity, and vel-extras set per note in OMs editors
;;
;;
;; uses OMs various vel lookups etc in editor/scoreeditor/scoretools.lisp
;; 


;; were trying to support OMs - per-note dynamics (vel-extra)
;; 
(defparameter *mscx-dynamics-size* "0.6")
(defparameter *mscx-dynamics-direction* "up")

;; mapping used by mscore

(defparameter *mscx-dynamic-velocities*
  '((:ppp 16)
    (:pp  33)
    (:p   49)
    (:mp  64)
    (:mf  80)
    (:f   96)
    (:ff  112)
    (:fff 126)))

;; 
;; OMs mapping is in om::*dynamics-symbols-list*,set in scoretools.lisp
;; 
;; (mapcar #'(lambda (d) (list (first d) (third d))) om::*dynamics-symbols-list*)
;; 
;; ((:ppp 20)
;;  (:pp 40)
;;  (:p 55)
;;  (:mp 60)
;;  (:mf 85)
;;  (:f 100)
;;  (:ff 115)
;;  (:fff 127))



(defun vel-extra-for-chord (chord)
  (get-extra-by-kind chord "vel"))

(defun chord-dynamic-symbol (chord)
  "Use OM's own velocity->dynamic mapping."
  (when (vel-extra-for-chord chord)
    (let ((dyn (om::get-dyn-from-vel (om::get-object-vel chord))))
      (when (symbolp dyn)
        dyn))))

(defun dynamic-symbol->mscx-subtype (dyn)
  (string-downcase (symbol-name dyn)))

(defun dynamic-symbol->mscx-velocity (dyn)
  (or (cadr (assoc dyn *mscx-dynamic-velocities* :test #'equal))
      80))

(defun dynamic-as-mscx (dyn)
  (list "<Dynamic>"
        (format nil "<subtype>~A</subtype>"
                (dynamic-symbol->mscx-subtype dyn))
        (format nil "<velocity>~D</velocity>"
                (dynamic-symbol->mscx-velocity dyn))
        (format nil "<dynamicsSize>~A</dynamicsSize>"
                *mscx-dynamics-size*)
        (format nil "<direction>~A</direction>"
                *mscx-dynamics-direction*)
        "</Dynamic>"))

(defun vel-extra-as-mscx (chord)
  (let ((dyn (chord-dynamic-symbol chord)))
    (when dyn
      (dynamic-as-mscx dyn))))

;;
;; CHAR-EXTRAS -> MSCX articulations / fermatas
;;

;; a list of some of the articulations from mscore, as named in .mscx files
;; manually grabbed from various mscx-files
;;
;; for those actually used in OMs char-extras, check mapping
;; 

(defparameter *mscx-articulation-symbol-pool*
  '(
    ;; ------------------------------------------------------------
    ;; BOWING / STRINGS / PLUCKED
    ;; ------------------------------------------------------------
    (nil :tag "Articulation" :subtype "stringsUpBow" :group :bowing)
    (nil :tag "Articulation" :subtype "stringsDownBow" :group :bowing)
    (nil :tag "Articulation" :subtype "stringsHarmonic" :group :strings)
    (nil :tag "Articulation" :subtype "stringsThumbPosition" :group :strings)
    (nil :tag "Articulation" :subtype "pluckedSnapPizzicatoAbove" :group :plucked)

    ;; ------------------------------------------------------------
    ;; STANDARD ARTICULATIONS
    ;; ------------------------------------------------------------
    (nil :tag "Articulation" :subtype "articAccentBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articSoftAccentBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articMarcatoAbove" :group :articulation)
    (nil :tag "Articulation" :subtype "articStressBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articUnstressBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articTenutoBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articStaccatoBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articStaccatissimoBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articStaccatissimoStrokeBelow" :group :articulation)
    (nil :tag "Articulation" :subtype "articStaccatissimoWedgeBelow" :group :articulation)

    ;; ------------------------------------------------------------
    ;; COMBINED ARTICULATIONS
    ;; ------------------------------------------------------------
    (nil :tag "Articulation" :subtype "articTenutoStaccatoBelow" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articAccentStaccatoBelow" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articMarcatoStaccatoAbove" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articMarcatoTenutoAbove" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articTenutoAccentBelow" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articSoftAccentStaccatoBelow" :group :articulation-combo)
    (nil :tag "Articulation" :subtype "articSoftAccentTenutoBelow" :group :articulation-combo)

    ;; ------------------------------------------------------------
    ;; FERMATAS
    ;; ------------------------------------------------------------
    (nil :tag "Fermata" :subtype "fermataAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataShortAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataVeryShortAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataLongAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataVeryLongAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataShortHenzeAbove" :group :fermata)
    (nil :tag "Fermata" :subtype "fermataLongHenzeAbove" :group :fermata)

    ;; ------------------------------------------------------------
    ;; ORNAMENTS
    ;; ------------------------------------------------------------
    (nil :tag "Ornament" :subtype "ornamentTrill" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentShortTrill" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTurn" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTurnInverted" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTurnSlash" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTurnUp" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTurnUpS" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentMordent" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentUpMordent" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentDownMordent" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPrallUp" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPrallDown" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPrallMordent" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentLinePrall" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPinceCouperin" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentHaydn" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTremblement" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentTremblementCouperin" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentShake3" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentShakeMuffat1" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPrecompMordentUpperPrefix" :group :ornament)
    (nil :tag "Ornament" :subtype "ornamentPrecompSlide" :group :ornament)
    (nil :tag "Ornament" :subtype "brassMuteClosed" :group :ornament)

    ;; ------------------------------------------------------------
    ;; TRILLS
    ;; ------------------------------------------------------------
    (nil :tag "Trill" :subtype "trill" :group :trill)
    (nil :tag "Trill" :subtype "upprall" :group :trill)
    (nil :tag "Trill" :subtype "downprall" :group :trill)
    (nil :tag "Trill" :subtype "prallprall" :group :trill)

    ;; ------------------------------------------------------------
    ;; TREMOLO
    ;; ------------------------------------------------------------
    (nil :tag "TremoloSingleChord" :subtype "r8" :group :tremolo-single)
    (nil :tag "TremoloSingleChord" :subtype "r16" :group :tremolo-single)
    (nil :tag "TremoloSingleChord" :subtype "r32" :group :tremolo-single)
    (nil :tag "TremoloSingleChord" :subtype "r64" :group :tremolo-single)
    (nil :tag "TremoloSingleChord" :subtype "buzzroll" :group :tremolo-single)

    (nil :tag "TremoloTwoChord" :subtype "c8" :group :tremolo-two)
    (nil :tag "TremoloTwoChord" :subtype "c16" :group :tremolo-two)
    (nil :tag "TremoloTwoChord" :subtype "c32" :group :tremolo-two)
    (nil :tag "TremoloTwoChord" :subtype "c64" :group :tremolo-two)

    ;; ------------------------------------------------------------
    ;; ARPEGGIO / CHORD LINE
    ;; ------------------------------------------------------------
    (nil :tag "Arpeggio" :subtype "0" :group :arpeggio)
    (nil :tag "Arpeggio" :subtype "1" :group :arpeggio)
    (nil :tag "Arpeggio" :subtype "2" :group :arpeggio)
    (nil :tag "Arpeggio" :subtype "3" :group :arpeggio)
    (nil :tag "Arpeggio" :subtype "4" :group :arpeggio)
    (nil :tag "Arpeggio" :subtype "5" :group :arpeggio)

    (nil :tag "ChordLine" :subtype "1" :group :chord-line)
    (nil :tag "ChordLine" :subtype "2" :group :chord-line)
    (nil :tag "ChordLine" :subtype "3" :group :chord-line)
    (nil :tag "ChordLine" :subtype "4" :group :chord-line)

    ;; ------------------------------------------------------------
    ;; OTHER SPECIALS
    ;; ------------------------------------------------------------
    (nil :tag "Articulation" :subtype "brassMuteClosed" :group :brass)
    (nil :tag "Articulation" :subtype "brassMuteOpen" :group :brass)

    (nil :tag "Articulation" :subtype "guitarFadeIn" :group :guitar)
    (nil :tag "Articulation" :subtype "guitarFadeOut" :group :guitar)

    (nil :tag "Articulation" :subtype "luteFingeringRHThumb" :group :lute)
    (nil :tag "Articulation" :subtype "luteFingeringRHFirst" :group :lute)
    (nil :tag "Articulation" :subtype "luteFingeringRHSecond" :group :lute)
    (nil :tag "Articulation" :subtype "luteFingeringRHThird" :group :lute)

    (nil :tag "Articulation" :subtype "pictHalfOpen2" :group :winds)

    (nil :tag "Articulation" :subtype "tremoloDivisiDots2" :group :tremolo-divisi)
    (nil :tag "Articulation" :subtype "tremoloDivisiDots3" :group :tremolo-divisi)
    (nil :tag "Articulation" :subtype "tremoloDivisiDots4" :group :tremolo-divisi)
    (nil :tag "Articulation" :subtype "tremoloDivisiDots6" :group :tremolo-divisi)

    (nil :tag "Articulation" :subtype "wiggleSawtooth" :group :wiggle)
    (nil :tag "Articulation" :subtype "wiggleSawtoothWide" :group :wiggle)
    (nil :tag "Articulation" :subtype "wiggleVibratoLargeFaster" :group :wiggle)
    (nil :tag "Articulation" :subtype "wiggleVibratoLargeSlowest" :group :wiggle)))



;; Mapping from OMs 'char extras - using #'thechar

(defparameter *om-char-extra->mscx*
  ;; NOTE:
  ;; OM-char values below are placeholders / first guesses.

  '(
    ("s" :group :bowing       :tag "Articulation" :subtype "stringsDownBow")
    ("r" :group :bowing	      :tag "Articulation" :subtype "stringsUpBow")
    ("t" :group :articulation :tag "Articulation" :subtype "articStaccatissimoBelow")
    ("u" :group :articulation :tag "Articulation" :subtype "articTenutoStaccatoBelow")
    ("v" :group :articulation :tag "Articulation" :subtype "articTenutoStaccatoBelow")
    ("w" :group :articulation :tag "Articulation" :subtype "articMarcatoAbove")
    ("{" :group :articulation :tag "Articulation" :subtype "articMarcatoAbove")
    ("x" :group :articulation :tag "Articulation" :subtype "articTenutoBelow")
    ("y" :group :articulation :tag "Articulation" :subtype "articAccentStaccatoAbove")
    ("z" :group :articulation :tag "Articulation" :subtype "articAccentStaccatoBelow")
    ("|" :group :fermata      :tag "Fermata"      :subtype "fermataAbove")
    ("}" :group :fermata      :tag "Fermata"      :subtype "fermataAbove")
    ))

(defun char-extra-p (x)
  (typep x 'om::char-extra))

(defun chord-char-extras (chord)
  (remove-if-not #'char-extra-p
                 (om::get-extras chord "all")))

(defun char-extra-char (extra)
  "Return the raw OM char code used to look up the glyph. "
  (and extra (om::thechar extra)))

(defun find-char-extra-mscx-mapping (char)
  (and char
       (assoc char *om-char-extra->mscx* :test #'string=)))

(defun char-extra-mscx-tag (mapping)
  (getf (cdr mapping) :tag))

(defun char-extra-mscx-subtype (mapping)
  (getf (cdr mapping) :subtype))

(defun mscx-tag+subtype->xml (tag subtype)
  (list (format nil "<~A>" tag)
        (format nil "<subtype>~A</subtype>" subtype)
        (format nil "</~A>" tag)))

(defun char-extra->mscx (extra)
  (let* ((char (char-extra-char extra))
         (mapping (find-char-extra-mscx-mapping char)))
    (when mapping
      (mscx-tag+subtype->xml
       (char-extra-mscx-tag mapping)
       (char-extra-mscx-subtype mapping)))))

(defun char-extras-as-mscx (chord)
  (loop for extra in (chord-char-extras chord)
        append (or (char-extra->mscx extra) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; TUPLETS
;;

(defun make-mscx-tuplet-start (actual normal base-note)
  (list "<Tuplet>"
        (format nil "<normalNotes>~D</normalNotes>" normal)
        (format nil "<actualNotes>~D</actualNotes>" actual)
        (format nil "<baseNote>~A</baseNote>" base-note)
        "<Number>"
        "<style>tuplet</style>"
        (format nil "<text>~D</text>" actual)
        "</Number>"
        "</Tuplet>"))

(defun make-mscx-tuplet-end ()
  (list "<endTuplet/>"))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TIES
;; 

(defun tied-kind (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         :start)
        ((and (om::cont-chord-p self)
              (not (om::cont-chord-p (om::next-container self '(om::chord)))))
         :stop)
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
         :continue)
        (t nil)))

(defun same-measure-p (a b)
  (and a b
       (eq (mxml::get-parent-measure a)
           (mxml::get-parent-measure b))))

(defun next-chord (self)
  (om::next-container self '(om::chord)))

(defun prev-chord (self)
  (om::previous-container self '(om::chord)))

(defun obj-written-dur (self)
  "Return written note value as a whole-note fraction."
  (let* ((mesure (mxml::get-parent-measure self))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val)))
    (* symb-beat-val factor)))

(defun obj-actual-dur (self)
  (let ((written (obj-written-dur self))
        (ratio (ignore-errors (time-mod-val self))))
    (if (and ratio
             (listp ratio)
             (= (length ratio) 2)
             (numberp (car ratio))
             (numberp (cadr ratio))
             (not (= (car ratio) (cadr ratio))))
        (* written (/ (cadr ratio) (car ratio)))
      written)))

(defun mscx-next-location (self free)
  (declare (ignore free))
  (let ((nxt (next-chord self)))
    (if (same-measure-p self nxt)
        (list "<location>"
              (format nil "<fractions>~A</fractions>"
                      (ratio->mscx-fraction-string (obj-actual-dur self)))
              "</location>")
      (list "<location>"
            "<measures>1</measures>"
            "</location>"))))

(defun mscx-prev-location (self free)
  (declare (ignore free))
  (let ((prv (prev-chord self)))
    (if (same-measure-p self prv)
        (list "<location>"
              (format nil "<fractions>-~A</fractions>"
                      (ratio->mscx-fraction-string (obj-actual-dur prv)))
              "</location>")
      (list "<location>"
            "<measures>-1</measures>"
            "</location>"))))

(defun mscx-tie-spanner (self free)
  (let ((kind (tied-kind self)))
    (case kind
      (:start
       (list "<Spanner type=\"Tie\">"
             "<Tie/>"
             "<next>"
             (mscx-next-location self free)
             "</next>"
             "</Spanner>"))

      (:stop
       (list "<Spanner type=\"Tie\">"
             "<prev>"
             (mscx-prev-location self free)
             "</prev>"
             "</Spanner>"))
      (:continue
       (append
        (list "<Spanner type=\"Tie\">"
              "<prev>"
              (mscx-prev-location self free)
              "</prev>"
              "</Spanner>")
        (list "<Spanner type=\"Tie\">"
              "<Tie/>"
              "<next>"
              (mscx-next-location self free)
              "</next>"
              "</Spanner>")))
      (otherwise nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SLURS
;;

(defun slur-extra-p (x)
  (typep x 'om::slur))

(defun chord-slur-extras (chord)
  (remove-if-not #'slur-extra-p
                 (om::get-extras chord "all")))

(defun chord-slur-names (chord)
  (remove nil
          (remove-duplicates
           (mapcar #'om::slurname (chord-slur-extras chord))
           :test #'equal)))

(defun chord-has-slur-name-p (chord slurname)
  (find slurname (chord-slur-names chord) :test #'equal))

(defun find-next-chord-with-slur-name (chord slurname)
  (loop for c = (next-chord chord) then (and c (next-chord c))
        while c
        when (chord-has-slur-name-p c slurname)
          do (return c)))

(defun find-prev-chord-with-slur-name (chord slurname)
  (loop for c = (prev-chord chord) then (and c (prev-chord c))
        while c
        when (chord-has-slur-name-p c slurname)
          do (return c)))

(defun chord-offset-in-measure (chord)
  "Offset from start of measure to CHORD, in whole-note fractions.
Computed only from preceding chords in the same measure."
  (loop with sum = 0
        for c = (prev-chord chord) then (prev-chord c)
        while (and c (same-measure-p c chord))
        do (incf sum (obj-actual-dur c))
        finally (return sum)))

(defun chord-distance-forward (from to)
  "Distance in actual time from FROM to TO, excluding TO."
  (loop with sum = 0
        for c = from then (next-chord c)
        while c
        do (when (eq c to)
             (return sum))
           (incf sum (obj-actual-dur c))))

(defun measure-distance-forward (from-measure to-measure)
  "Count measures forward from FROM-MEASURE to TO-MEASURE."
  (loop with count = 0
        for m = from-measure then (om::next-container m '(om::measure))
        while m
        do (when (eq m to-measure)
             (return count))
           (incf count)))

(defun mscx-forward-location-between-chords (from to)
  (let* ((from-measure (mxml::get-parent-measure from))
         (to-measure (mxml::get-parent-measure to)))
    (cond
      ((eq from-measure to-measure)
       (list "<location>"
             (format nil "<fractions>~A</fractions>"
                     (ratio->mscx-fraction-string
                      (chord-distance-forward from to)))
             "</location>"))

      (t
       (let* ((measure-diff (measure-distance-forward from-measure to-measure))
              (to-offset (chord-offset-in-measure to)))
         (append
          (list "<location>"
                (format nil "<measures>~D</measures>" measure-diff))
          (when (not (zerop to-offset))
            (list (format nil "<fractions>~A</fractions>"
                          (ratio->mscx-fraction-string to-offset))))
          (list "</location>")))))))

(defun mscx-backward-location-between-chords (from to)
  "Location from FROM back to TO, encoded as negative measure/fraction offsets."
  (let* ((from-measure (mxml::get-parent-measure from))
         (to-measure (mxml::get-parent-measure to)))
    (cond
      ((eq from-measure to-measure)
       (list "<location>"
             (format nil "<fractions>-~A</fractions>"
                     (ratio->mscx-fraction-string
                      (chord-distance-forward to from)))
             "</location>"))

      (t
       (let* ((measure-diff (measure-distance-forward to-measure from-measure))
              (from-offset (chord-offset-in-measure from)))
         (append
          (list "<location>"
                (format nil "<measures>-~D</measures>" measure-diff))
          (when (not (zerop from-offset))
            (list (format nil "<fractions>-~A</fractions>"
                          (ratio->mscx-fraction-string from-offset))))
          (list "</location>")))))))

(defun mscx-slur-spanner-for-name (chord slurname)
  (let ((prev (find-prev-chord-with-slur-name chord slurname))
        (next (find-next-chord-with-slur-name chord slurname)))
    (cond
      ((and (null prev) next)
       (list "<Spanner type=\"Slur\">"
             "<Slur/>"
             "<next>"
             (mscx-forward-location-between-chords chord next)
             "</next>"
             "</Spanner>"))

      ((and prev (null next))
       (list "<Spanner type=\"Slur\">"
             "<prev>"
             (mscx-backward-location-between-chords chord prev)
             "</prev>"
             "</Spanner>"))

      ((and prev next)
       (append
        (list "<Spanner type=\"Slur\">"
              "<prev>"
              (mscx-backward-location-between-chords chord prev)
              "</prev>"
              "</Spanner>")
        (list "<Spanner type=\"Slur\">"
              "<Slur/>"
              "<next>"
              (mscx-forward-location-between-chords chord next)
              "</next>"
              "</Spanner>")))

      (t nil))))

(defun mscx-slur-spanners (chord)
  (loop for name in (chord-slur-names chord)
        append (mscx-slur-spanner-for-name chord name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; BEAMS
;; 


(defun mscx-beam-mode (self)
  (let* ((beamself (mxml::donne-figure self))
         (beamprev (mxml::donne-figure (mxml::prv-cont self)))
         (beamnext (mxml::donne-figure (mxml::nxt-cont self))))
    (cond
      ;; inside a group, neither first nor last
      ((and (mxml::in-group? self)
            (not (om::first-of-group? self))
            (not (om::last-of-group? self))
            (> beamself 0))
       (cond ((and (> beamprev 0) (> beamnext 0)) "mid")
             ((and (> beamprev 0) (not (> beamnext 0))) "end")
             ((and (not (> beamprev 0)) (> beamnext 0)) "begin")
             (t "no")))

      ;; first of group
      ((and (om::first-of-group? self) (> beamself 0))
       (if (and (mxml::in-group? (mxml::prv-cont self))
                (> beamprev 0)
                (> beamnext 0)
                (mxml::prv-is-samegrp? self))
           "mid"
           "begin"))

      ;; last of group
      ((and (om::last-of-group? self) (> beamself 0))
       (if (and (mxml::in-group? (mxml::nxt-cont self))
                (> beamprev 0)
                (> beamnext 0)
                (mxml::nxt-is-samegrp? self))
           "mid"
           "end"))

      ;; outside groups: optionally say no
      ((> beamself 0) "no")
      (t nil))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;;
;; CONS-MSCX-EXPR - main work for relevant OM classes
;;


(defun mscx-pitch-as-lines (pitch)
  "Write MSCX pitch. MuseScore expects integer pitch values."
  (unless (integerp pitch)
    (error "MSCX export: <pitch> must be integer, got ~S." pitch))
  (list (format nil "<pitch>~D</pitch>" pitch)))

(defun mscx-grace-duration-type-from-count (count)
  "Heuristic visual duration for OM grace groups.
1-3 notes -> eighth, 4-7 -> 16th, 8+ -> 32nd."
  (cond ((<= count 3) "eighth")
        ((<= count 7) "16th")
        (t "32nd")))

(defun cons-mscx-grace-chord (self &key group-size (approx :none)
                                      (unsupported-microtones :warn)
                                      (accidental-family *mscx-accidental-family*))
  "Export one OM grace-chord as a MuseScore acciaccatura.
Grace notes are emitted without extras, beams, ties, slurs, or text.
Only basic chord/note content is preserved."
  (let* ((inside (om::inside self))
         (duration-type (mscx-grace-duration-type-from-count (or group-size 1))))
    (append
     (list "<Chord>" (format nil "<durationType>~A</durationType>" duration-type) "<acciaccatura/>")
     (loop for note in inside
           append
           (let* ((wp (note-to-mscx-written-pitch note :approx approx
                                                       :unsupported-microtones unsupported-microtones
                                                       :accidental-family accidental-family))
                  (vel (om::get-object-vel note)))
             
	     (append
              (list "<Note>")
              (mscx-accidental-as-lines (mscx-written-pitch-accidental-subtype wp))
              (mscx-pitch-as-lines (mscx-written-pitch-pitch wp))
	      (list (format nil "<pitch>~D</pitch>" (mscx-written-pitch-pitch wp))
                    (format nil "<tpc>~D</tpc>" (mscx-written-pitch-tpc wp))
                    (format nil "<velocity>~D</velocity>" vel)
                    "</Note>"))))
     (list "</Chord>"))))


(defgeneric cons-mscx-expr (self &key free clef approx part unsupported-microtones accidental-family))

(defmethod cons-mscx-expr ((self om::chord)
			   &key free clef (approx :none) part (unsupported-microtones :warn) (accidental-family *mscx-accidental-family*))
  (let* ((dur (if (listp free) (car free) free))
	 (head-and-pts (mxml::get-head-and-points dur))
	 (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car)))
	 (nbpoints (cadr head-and-pts))
	 (beam-mode (mscx-beam-mode self))
	 (inside (om::inside self))
	 (tie-spanner (mscx-tie-spanner self free))
	 (slur-spanners (mscx-slur-spanners self))
	 (text-extra (text-extra-as-mscx self))
	 (vel-extra (vel-extra-as-mscx self))
	 (char-extra (char-extras-as-mscx self))
	 (grace-notes-obj (and (fboundp 'om::gnotes) (om::gnotes self)))
	 (graces (and grace-notes-obj
		      (ignore-errors (om::glist grace-notes-obj))))
         (group-size (length graces)))
    (append
     (current-tempo-as-mscx)

     ;; OM grace groups are exported as MuseScore acciaccaturas.
     ;; durationType is chosen heuristically from group size for readability only.
     (loop for grace-chord in graces
	   append (cons-mscx-grace-chord grace-chord :group-size group-size :approx approx
						     :unsupported-microtones unsupported-microtones
						     :accidental-family accidental-family))

     ;; ensure correct list order here, and below in om::rest, order decides semantics in output
     text-extra
     vel-extra
     (list "<Chord>")
     (when beam-mode
       (list (format nil "<BeamMode>~A</BeamMode>" beam-mode)))
     (loop for i from 1 to nbpoints
	   collect "<dots>1</dots>")
     (list (format nil "<durationType>~A</durationType>"
		   (xml-head-to-mscx-duration-type note-head)))

     ;; slurs are chord-level spanners in MSCX
     slur-spanners

     ;; char-extras are also chord-level in MSCX
     char-extra

     (loop for note in inside
	   append
	   (let* ((wp (note-to-mscx-written-pitch note :approx approx
                                                       :unsupported-microtones unsupported-microtones
                                                       :accidental-family accidental-family))
		  (vel (om::get-object-vel note))
		  (head-extra (note-head-as-mscx note)))
             (append
	      (list "<Note>")
	      tie-spanner
	      (mscx-accidental-as-lines (mscx-written-pitch-accidental-subtype wp))
	      (mscx-pitch-as-lines (mscx-written-pitch-pitch wp))
	      (list (format nil "<tpc>~D</tpc>" (mscx-written-pitch-tpc wp))
		    (format nil "<velocity>~D</velocity>" vel))
	      head-extra
	      (list "</Note>"))))
     (list "</Chord>"))))

(defmethod cons-mscx-expr ((self om::rest)
			   &key free clef (approx :none) part unsupported-microtones accidental-family)
  (let* ((dur (if (listp free) (car free) free))
         (head-and-pts (mxml::get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
	 (beam-mode (mscx-beam-mode self)))
    (append
     (current-tempo-as-mscx)
     (list "<Rest>")
     (when beam-mode
       (list (format nil "<BeamMode>~A</BeamMode>" beam-mode)))
     (loop for i from 1 to nbpoints
           collect "<dots>1</dots>")
     (list (format nil "<durationType>~A</durationType>"
                   (xml-head-to-mscx-duration-type note-head)))
     (list "</Rest>"))))

(defmethod cons-mscx-expr ((self om::group) &key free clef (approx :none) part
					      (unsupported-microtones :warn)
					      (accidental-family *mscx-accidental-family*))
  (let* ((durtot (if (listp free) (car free) free))
         (cpt (if (listp free) (cadr free) 0))
         (num (or (om::get-group-ratio self) (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (cadr denom) denom))
         (unite (/ durtot denom)))

    (cond
      ;; not a tuplet-like group: recurse normally
      ((not (om::get-group-ratio self))
       (let ((running-offset 0))
         (loop for obj in (om::inside self)
	       append
	       (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj))
				  (/ (om::extent self) (om::qvalue self))))
		      (obj-free (* dur-obj durtot)))
		 (prog1
		     (let ((*mscx-current-offset*
			     (+ *mscx-current-offset* running-offset)))
		       (cons-mscx-expr obj :free obj-free :approx approx :part part))
		   (incf running-offset obj-free))))))

      ;; ratio simplifies away: recurse normally
      ((= (/ num denom) 1)
       (let ((running-offset 0))
         (loop for obj in (om::inside self)
               append
               (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj))
                                    (/ (om::extent self) (om::qvalue self))))
                      (dur-obj (* num operation))
                      (obj-free (* dur-obj unite)))
                 (prog1
                     (let ((*mscx-current-offset*
                             (+ *mscx-current-offset* running-offset)))
                       (cons-mscx-expr obj :free obj-free :approx approx :part part))
                   (incf running-offset obj-free))))))

      ;; real tuplet
      (t
       (let ((depth 0)
             (rep nil)
             (running-offset 0)
             ;; base note should be the written value of one unit inside the tuplet.
             ;; For 3 eighths in the time of 2 eighths, this should become "eighth".
             (base-note (ratio-base-note-name unite)))
         
         (setf rep (append rep
                           (make-mscx-tuplet-start num denom base-note)))

         (loop for obj in (om::inside self) do
           (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj))
                                (/ (om::extent self) (om::qvalue self))))
                  (dur-obj (* num operation))
                  (obj-free (* dur-obj unite))
                  (tmp (let ((*mscx-current-offset*
                               (+ *mscx-current-offset* running-offset)))
                         (multiple-value-list
                          (cons-mscx-expr obj
                                          :free (list obj-free cpt)
                                          :approx approx
                                          :part part))))
                  (exp (car tmp)))
             (when (and (cadr tmp) (> (cadr tmp) depth))
               (setf depth (cadr tmp)))
             (setf rep (append rep exp))
             (incf running-offset obj-free)))

         (setf rep (append rep (make-mscx-tuplet-end)))
         (values rep (+ depth 1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; TEMPO
;;

(defvar *mscx-tempo-map* nil)
(defvar *mscx-current-measure-index* 0)
(defvar *mscx-current-offset* 0)

(defun tempo-unit->base-and-dots (unit)
  "Return MuseScore metronome base name and dot count for UNIT."
  (cond ((equal unit 4)    (list "Longa" 0))
        ((equal unit 2)    (list "DoubleWhole" 0))
        ((equal unit 1)    (list "Whole" 0))
        ((equal unit 1/2)  (list "Half" 0))
        ((equal unit 1/4)  (list "Quarter" 0))
        ((equal unit 1/8)  (list "8th" 0))
        ((equal unit 1/16) (list "16th" 0))
        ((equal unit 1/32) (list "32nd" 0))
        ((equal unit 1/64) (list "64th" 0))

        ;; dotted values
        ((equal unit 3)    (list "DoubleWhole" 1))
        ((equal unit 3/2)  (list "Whole" 1))
        ((equal unit 3/4)  (list "Half" 1))
        ((equal unit 3/8)  (list "Quarter" 1))
        ((equal unit 3/16) (list "8th" 1))
        ((equal unit 3/32) (list "16th" 1))
        ((equal unit 3/64) (list "32nd" 1))

        (t (list "Quarter" 0))))

(defun tempo-base->met-sym (base)
  (cond ((string= base "Longa")       "metNoteLongaUp")
        ((string= base "DoubleWhole") "metNoteDoubleWholeSquare")
        ((string= base "Whole")       "metNoteWhole")
        ((string= base "Half")        "metNoteHalfUp")
        ((string= base "Quarter")     "metNoteQuarterUp")
        ((string= base "8th")         "metNote8thUp")
        ((string= base "16th")        "metNote16thUp")
        ((string= base "32nd")        "metNote32ndUp")
        ((string= base "64th")        "metNote64thUp")
        (t "metNoteQuarterUp")))

(defun trim-trailing-zeroes (s)
  (let ((out (string-right-trim '(#\0) s)))
    (if (and (> (length out) 0)
             (char= (char out (1- (length out))) #\.))
        (subseq out 0 (1- (length out)))
      out)))

(defun tempo-value->mscx-string (unit bpm)
  "MuseScore <tempo> value = quarter-notes-per-second scaled by beat unit.
Examples:
 1/4=60  -> 1
 1/8=90  -> 0.75
 3/8=35  -> 0.875"
  (trim-trailing-zeroes
   (format nil "~,6F" (/ (* 4.0 unit bpm) 60.0))))

(defun tempo-text->mscx-string (unit bpm)
  (multiple-value-bind (base dots)
      (values-list (tempo-unit->base-and-dots unit))
    (with-output-to-string (s)
      (format s "<sym>~A</sym>" (tempo-base->met-sym base))
      (loop repeat dots do
            (princ "<sym>metAugmentationDot</sym>" s))
      (format s "<font face=\"Edwin\"/> = ~A" bpm))))

(defun make-mscx-tempo (unit bpm)
  (list "<Tempo>"
        (format nil "<tempo>~A</tempo>"
                (tempo-value->mscx-string unit bpm))
        "<followText>1</followText>"
        (format nil "<text>~A</text>"
                (tempo-text->mscx-string unit bpm))
        "</Tempo>"))

(defun measure-beat-unit (measure)
  "Return the symbolic beat unit used for beat-index addressing in this measure."
  (/ 1 (om::find-beat-symbol
        (om::fdenominator (measure-signature measure)))))

(defun tempo-start-event-p (event)
  (equal (car event) '(0 0)))

(defun normalized-voice-tempo-events (voice)
  "Always ensure there is a tempo event at (0 0).
If explicit (0 0) exists, keep it.
Otherwise use the initial tempo value."
  (let* ((tempo-data (om::tempo voice))
         (initial (car tempo-data))
         (events (copy-list (cadr tempo-data))))
    (unless (find-if #'tempo-start-event-p events)
      (push (list '(0 0) initial) events))
    events))

(defun build-voice-tempo-map (voice measures)
  "Create alist entries of the form:
 ((measure-index offset-ratio) unit bpm)"
  (loop for event in (normalized-voice-tempo-events voice)
        for pos = (car event)
        for tempo-spec = (cadr event)
        for measure-index = (car pos)
        for beat-index = (cadr pos)
        for measure = (nth measure-index measures)
        when measure
          collect
          (list (list measure-index
                      (* beat-index (measure-beat-unit measure)))
                (car tempo-spec)
                (cadr tempo-spec))))

(defun current-tempo-entry ()
  (find (list *mscx-current-measure-index* *mscx-current-offset*)
        *mscx-tempo-map*
        :test #'equal
        :key #'car))

(defun current-tempo-as-mscx ()
  (let ((entry (current-tempo-entry)))
    (when entry
      (make-mscx-tempo (cadr entry) (caddr entry)))))


;;;
;;; TIME SIGNATURES
;;;
;;; only emit upon changes for now
;;;

(defun measure-signature (measure)
  (car (om::tree measure)))

(defun previous-measure (measure)
  (om::previous-container measure '(om::measure)))

(defun same-signature-p (a b)
  (equal (measure-signature a)
         (measure-signature b)))

(defun emit-timesig-p (measure mesnum)
  (or (= mesnum 1)
      (let ((prev (previous-measure measure)))
        (or (null prev)
            (not (same-signature-p measure prev))))))

(defmethod cons-mscx-expr ((self om::measure) &key free (clef '(G 2)) (approx :none) part
						(unsupported-microtones :warn)
						(accidental-family *mscx-accidental-family*))
  (let* ((mesnum free)
         (inside (om::inside self))
         (tree (om::tree self))
         (signature (car tree))
         (real-beat-val (/ 1 (om::fdenominator signature)))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator signature)))))
    (list
     "<Measure>"
     "<voice>"
     (remove nil
             (list
              (when (= mesnum 1)
                (and clef
                     (list "<Clef>"
                           "<isHeader>1</isHeader>"
                           (format nil "<concertClefType>~A</concertClefType>"
                                   (clef-sign->mscx-clef (car clef) (cadr clef)))
                           (format nil "<transposingClefType>~A</transposingClefType>"
                                   (clef-sign->mscx-clef (car clef) (cadr clef)))
                           "</Clef>")))
              (when (emit-timesig-p self mesnum)
                (list "<TimeSig>"
                      (format nil "<sigN>~D</sigN>" (car signature))
                      (format nil "<sigD>~D</sigD>" (cadr signature))
                      "</TimeSig>"))))

     (let ((running-offset 0))
       (loop for obj in inside
             append
             (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                    (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                    (obj-free (* symb-beat-val factor)))
               (prog1
                   (let ((*mscx-current-offset* running-offset))
                     (cons-mscx-expr obj :free obj-free :approx approx :part part
					 :unsupported-microtones unsupported-microtones
					 :accidental-family accidental-family))
                 (incf running-offset obj-free)))))

     "<BarLine>"
     "<subtype>normal</subtype>"
     "</BarLine>"

     "</voice>"
     "</Measure>")))

(defmethod cons-mscx-expr ((self om::voice) &key free (clef '(G 2)) (approx :none) part
					      (unsupported-microtones :warn)
					      (accidental-family *mscx-accidental-family*))
  (let ((voicenum part)
        (measures (om::inside self)))
    (let ((*mscx-tempo-map* (build-voice-tempo-map self measures)))
      (list
       (format nil "<Staff id=\"~D\">" voicenum)
       (loop for mes in measures
             for i = 1 then (+ i 1)
             for measure-index = 0 then (+ measure-index 1)
             collect
             (let ((*mscx-current-measure-index* measure-index)
                   (*mscx-current-offset* 0))
               (cons-mscx-expr mes :free i :clef clef :approx approx :part part
				   :unsupported-microtones unsupported-microtones
				   :accidental-family accidental-family)))
       "</Staff>"))))


(defmethod cons-mscx-expr ((self om::poly) &key free (clef '((G 2))) (approx :none) part
					     (unsupported-microtones :warn)
					     (accidental-family *mscx-accidental-family*))
  (let ((voices (om::inside self)))
    (list
     "<museScore version=\"4.60\">"
     "<Score>"

     "<Division>480</Division>"

     ;; Minimal Part declarations
     (loop for v in voices
           for i = 1 then (+ i 1)
           append
           (list
            (format nil "<Part id=\"~D\">" i)
            "<Staff>"
            "<StaffType group=\"pitched\">"
            "<name>stdNormal</name>"
            "</StaffType>"
            "</Staff>"
            (format nil "<trackName>Part ~D</trackName>" i)
            "</Part>"))

     ;; Staff timelines
     (if (= 1 (length clef))
         ;; same clef for all voices
         (loop for v in voices
               for i = 1 then (+ i 1)
               append
               (cons-mscx-expr v :part i :clef (car clef) :approx approx
				 :unsupported-microtones unsupported-microtones
				 :accidental-family accidental-family))
	 ;; one clef per voice
	 (loop for v in voices
               for i = 1 then (+ i 1)
               for k in clef
               append
               (cons-mscx-expr v :part i :clef k :approx approx
				 :unsupported-microtones unsupported-microtones
				 :accidental-family accidental-family)))

     "</Score>"
     "</museScore>")))


;;;===================================
;;; OM INTERFACE / API
;;;===================================


;; MAIN MSCX FILE OUTPUT

(in-package :om)

(defun om-staff-symbol->mscx-clef (staff)
  "Map OM editor staff symbol to legacy clef form (SIGN LINE)
for simple one-staff MSCX export.

If STAFF is a multi-staff display symbol like GF, GGF, FF, etc.,
use only the first clef letter."
  (let ((name (string-upcase (string staff))))
    (cond
      ;; explicit simple clefs
      ((string= name "G") '(g 2))
      ((string= name "G_8") '(g_8 2))
      ((string= name "G^8") '(g^8 2))
      ((string= name "F") '(f 4))
      ((string= name "F_8") '(f_8 4))
      ((string= name "C1") '(c 1))
      ((string= name "C3") '(c 3))
      ((string= name "C4") '(c 4))
      ((string= name "EMPTY") '(empty 0))

      ;; multi-staff display symbols -> first staff only
      ((string= name "GF") '(g 2))
      ((string= name "GG") '(g 2))
      ((string= name "FF") '(f 4))
      ((string= name "GGF") '(g 2))
      ((string= name "GFF") '(g 2))
      ((string= name "GGFF") '(g 2))
      (t
       (error "Unsupported OM staff symbol for MSCX export: ~S" staff)))))

(defun write-mscx-file (list path)
  (with-open-file (out path :direction :output
			    :if-does-not-exist :create :if-exists :supersede)
    (loop for line in (mscx::mscx-header) do (format out "~A~%" line))
    (recursive-write-xml out list -1)))

(defmethod mscx-export ((self t) &key clefs approx path name unsupported-microtones accidental-family) nil)

(defmethod mscx-export ((self voice) &key clefs approx path name
				       (unsupported-microtones :warn)
				       (accidental-family mscx::*mscx-accidental-family*))
  (mscx-export (make-instance 'poly :voices self)
               :clefs clefs :approx approx :path path :name name
               :unsupported-microtones unsupported-microtones
               :accidental-family accidental-family))

(defmethod mscx-export ((self poly) &key clefs approx path name
				      (unsupported-microtones :warn)
				      (accidental-family mscx::*mscx-accidental-family*))
  (let* ((mscx::*mscx-export-warnings* nil)
         (pathname (or path
                       (om-choose-new-file-dialog
                        :name (or name "om-export.mscx")
                        :directory (or (and name (make-pathname :name nil :type nil :defaults name)) nil)
                        :prompt "Export MuseScore MSCX")))
         (content (mscx::cons-mscx-expr self :clef (or clefs '((G 2))) :approx approx
                                             :unsupported-microtones unsupported-microtones
                                             :accidental-family accidental-family)))
    (when pathname
      (write-mscx-file content pathname)
      pathname)))


(defmethod! export-mscx ((self t) &optional (clefs nil) (approx :none) (path nil)
				  (unsupported-microtones :warn) (accidental-family :gould-arrow))
  :icon 351
  :indoc '("a VOICE or POLY object"
           "list of voice clefs"
           "fallback pitch approximation, or :none"
           "a target pathname"
           "unsupported microtone policy"
           "microtonal accidental family")
  :initvals '(nil '((G 2)) :none nil :warn :gould-arrow)
  :menuins '((nil)
             (nil)
             (:none 2 4 8 12 24)
             (nil)
             (:warn :mark :ignore :error)
             (:gould-arrow :stein-zimmermann :wyschnegradsky :persian :turkish :auto))
  :doc "
Exports <self> to MuseScore MSCX format.

Pitch policy:
- :none preserves OM/editor pitch spelling as far as possible.
- numeric approx is explicit fallback approximation.
- unsupported microtones warn by default.
"
  (let* ((staff (get-edit-param (associated-box self) 'staff))
         (clefs (cond ((null staff) '((G 2)))
                      ((listp staff) (loop for i in staff collect (om-staff-symbol->mscx-clef i)))
                      (t (list (om-staff-symbol->mscx-clef staff))))))
    (mscx-export self :clefs (if clefs clefs '((G 2)))
                      :approx approx :path path
                      :unsupported-microtones unsupported-microtones
                      :accidental-family accidental-family)))

(defmethod! export-mscx ((self voice) &optional (clefs nil) (approx :none) (path nil)
				      (unsupported-microtones :warn) (accidental-family :gould-arrow))
  :icon 351
  :indoc '("a VOICE object"
           "list of voice clefs"
           "fallback pitch approximation, or :none"
           "a target pathname"
           "unsupported microtone policy"
           "microtonal accidental family")
  :initvals '(nil ((G 2)) :none nil :warn :gould-arrow)
  :menuins '((nil)
             (nil)
             (:none 2 4 8 12 24)
             (nil)
             (:warn :mark :ignore :error)
             (:gould-arrow :stein-zimmermann :wyschnegradsky :persian :turkish :auto))
  :doc "
Exports <self> to MuseScore MSCX format.

Pitch policy:
- :none preserves OM/editor pitch spelling as far as possible.
- numeric approx is explicit fallback approximation.
- unsupported microtones warn by default.
"
  (let* ((staff (get-edit-param (associated-box self) 'staff))
         (clefs (list (om-staff-symbol->mscx-clef staff))))
    (mscx-export self :clefs (if clefs clefs '((G 2)))
                      :approx approx :path path
                      :unsupported-microtones unsupported-microtones
                      :accidental-family accidental-family)))

(defmethod! export-mscx ((self poly) &optional (clefs '((G 2))) (approx :none) (path nil)
				     (unsupported-microtones :warn) (accidental-family :gould-arrow))
  (call-next-method))


