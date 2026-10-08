;;;========================
;;; OpenMusic export MSCX 
;;;========================
;;;
;;; Anders Vinjar - 2026
;;; 
;;; Time-stamp: <2026-10-08 12:56:45 andersvi>
;;;
;;; mscx-expression.lisp:
;;;
;;; OM_MSCORE generic notation, expression rendering and preserved extras.
;;; Load after mscx-instrument-mappings.lisp, before export/import.
;;; 

(in-package :mscx)

(defvar *mscx-expression-context* nil)

(defun mscx-expression-xml-escape (value)
  "Escape XML character data/attribute values, never element names."
  (with-output-to-string (out)
    (loop for char across (princ-to-string (or value ""))
          do (write-string (case char
                             (#\& "&amp;") (#\< "&lt;") (#\> "&gt;")
                             (#\" "&quot;") (#\' "&apos;")
                             (otherwise (string char))) out))))

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
      (list (format nil "<head>~A</head>" (mscx-expression-xml-escape head))))))


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

(defun staff-text-sound-flag-as-mscx (text playing-technique)
  (list "<StaffText>"
        (format nil "<text>~A</text>" (mscx-expression-xml-escape text))
        "<SoundFlag>"
        (format nil "<playingTechnique>~A</playingTechnique>"
                (mscx-expression-xml-escape playing-technique))
        ;; Confirmed MSCX read460/tread.cpp and write/twrite.cpp property.
        ;; Generated playback changes target this staff; imported raw flags
        ;; keep the original applyToAllStaves choice.
        "<applyToAllStaves>0</applyToAllStaves>"
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
     (list (format nil "<playTechType>~A</playTechType>"
                   (mscx-expression-xml-escape play-tech-type))))
   (list (format nil "<text>~A</text>" (mscx-expression-xml-escape text))
         "</PlayTechAnnotation>")))

(defun staff-text-as-mscx (text)
  (list "<StaffText>"
        (format nil "<text>~A</text>" (mscx-expression-xml-escape text))
        "</StaffText>"))

(defun text-extra-as-mscx (chord &optional context)
  "Compatibility entry point; export every staff expression exactly once.
Explicit CONTEXT selects playback. Without it, use notation only."
  (expression-extras-as-mscx chord :staff
                            (or context (make-expression-context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DYNAMICS
;; 
;; velocity, and vel-extras set per note in OMs editors
;;
;;
;; uses OMs various vel lookups etc in editor/scoreeditor/scoretools.lisp
;; 
;; trying to support OMs - per-note dynamics (vel-extra)
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


(defvar *mscx-current-velocity* :unset)
(defvar *mscx-current-dynamic-symbol* :unset)

;; 
;; :velocities-as-dynamics nil ; default, bare vel-extra gir <Dynamic>
;; :velocities-as-dynamics t   ; velocity-endringer gir ekstra <Dynamic>tegn
;; 
(defparameter *mscx-export-velocities-as-dynamics* t)

;; usage 1: workaround: mscore playbacks only reacts to _symbolic_ dynamic signs, not its velocity-value
;; 
;; usage 2: debug dynamic


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


(defun mscx-velocity->dynamic-symbol (vel)
  "Return nearest dynamic symbol for VEL using *mscx-dynamic-velocities*."
  (car (first
        (sort (copy-list *mscx-dynamic-velocities*)
              #'<
              :key (lambda (entry)
                     (abs (- vel (second entry))))))))

(defun mscx-velocity-dynamic-as-needed (vel)
  "Optionally emit <Dynamic> when velocity maps to a new dynamic symbol."
  (when *mscx-export-velocities-as-dynamics*
    (let ((dyn (mscx-velocity->dynamic-symbol vel)))
      (when (or (eq *mscx-current-dynamic-symbol* :unset)
                (not (equal dyn *mscx-current-dynamic-symbol*)))
        (setf *mscx-current-dynamic-symbol* dyn)
        (dynamic-as-mscx dyn)))))




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
        (format nil "<subtype>~A</subtype>" (mscx-expression-xml-escape subtype))
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

;;; ----------------------------------------------------------------------
;;; Persistent OM extras; deliberately no dependency on SQ.
;;; ----------------------------------------------------------------------

(in-package :om)

(defclass* mscx-expression-extra (text-extra)
  ((mscx-expression-role :initarg :role :initform :text
                         :accessor mscx-expression-role)
   (mscx-expression-value :initarg :value :initform nil
                          :accessor mscx-expression-value)
   (mscx-expression-raw-node :initarg :raw-node :initform nil
                             :accessor mscx-expression-raw-node)
   (mscx-expression-placement :initarg :placement :initform nil
                              :accessor mscx-expression-placement)
   (mscx-expression-anchor :initarg :anchor :initform nil
                           :accessor mscx-expression-anchor)))

(defmethod omng-save ((self mscx-expression-extra) &optional values?)
  (declare (ignorable values?))
  ;; OM saves inherited appearance and positioning. Keep our payload explicit,
  ;; including XML unknown to this version, through project save/reopen.
  `(let ((extra ,(call-next-method)))
     (unless (typep extra 'mscx-expression-extra)
       (change-class extra 'mscx-expression-extra))
     (setf (mscx-expression-role extra) ',(mscx-expression-role self)
           (mscx-expression-value extra) ',(mscx-expression-value self)
           (mscx-expression-raw-node extra) ',(mscx-expression-raw-node self)
           (mscx-expression-placement extra) ',(mscx-expression-placement self)
           (mscx-expression-anchor extra) ',(mscx-expression-anchor self))
     extra))

(defmethod omng-copy ((self mscx-expression-extra))
  ;; OMNG-COPY returns reconstruction code, not the copied object.
  `(let ((extra ,(call-next-method)))
     (unless (typep extra 'mscx-expression-extra)
       (change-class extra 'mscx-expression-extra))
     (setf (mscx-expression-role extra) ',(mscx-expression-role self)
           (mscx-expression-value extra) (copy-tree ',(mscx-expression-value self))
           (mscx-expression-raw-node extra) (copy-tree ',(mscx-expression-raw-node self))
           (mscx-expression-placement extra) ',(mscx-expression-placement self)
           (mscx-expression-anchor extra) (copy-tree ',(mscx-expression-anchor self)))
     extra))

(defun make-mscx-expression-extra (&key role value text raw-node
                                        placement anchor)
  "An OM text extra with semantic intent and/or preserved MSCX XML.
ANCHOR, for voice-owned staff data, is (:measure N :offset R :order N).
Measure numbers are zero-based and R is a rational whole-note fraction."
  (let ((placement (or placement
                       (mscx::expression-effective-placement
                        (list :role (or role :text) :value value :text text
                              :raw-node raw-node)))))
    (make-instance 'mscx-expression-extra
                 :role (or role :text) :value value :raw-node raw-node
                 :placement placement :anchor anchor
                 :thetext (or text
                              (and value (string-downcase (princ-to-string value)))
                              "")
                 :deltax 0 :deltay (if (or (eq placement :staff)
                                          (member role '(:technique :text :raw))) 3 1)
                 :from-file t)))

(in-package :mscx)

;;; ----------------------------------------------------------------------
;;; Common notation grammar. Instrument vocabulary lives in its own file.
;;; ----------------------------------------------------------------------

(defparameter *mscx-expression-notation*
  '((:staccato :tag "Articulation" :subtype "articStaccatoBelow")
    (:staccatissimo :tag "Articulation" :subtype "articStaccatissimoBelow")
    (:tenuto :tag "Articulation" :subtype "articTenutoBelow")
    (:accent :tag "Articulation" :subtype "articAccentBelow")
    (:marcato :tag "Articulation" :subtype "articMarcatoAbove")
    (:portato :tag "Articulation" :subtype "articTenutoStaccatoBelow")
    (:fermata :tag "Fermata" :subtype "fermataAbove")
    (:trill :tag "Ornament" :subtype "ornamentTrill")
    (:trill-half :tag "Ornament" :subtype "ornamentTrill" :interval-above "second,minor")
    (:trill-whole :tag "Ornament" :subtype "ornamentTrill" :interval-above "second,major")
    (:tremolo :tag "TremoloSingleChord" :subtype "r32" :continuation :sustain)
    (:tremolo-8 :tag "TremoloSingleChord" :subtype "r8" :continuation :sustain)
    (:tremolo-16 :tag "TremoloSingleChord" :subtype "r16" :continuation :sustain)
    (:tremolo-32 :tag "TremoloSingleChord" :subtype "r32" :continuation :sustain)
    (:tremolo-64 :tag "TremoloSingleChord" :subtype "r64" :continuation :sustain)))

(defun canonical-expression-value (value)
  (case value
    ((:ordinary :ordinary-technique)
     (if (and *mscx-expression-context*
              (expression-context-family *mscx-expression-context*))
         (or (instrument-ordinary-value
              (expression-context-family *mscx-expression-context*)
              (expression-context-mapping *mscx-expression-context*)) value)
       :arco))
    (:pizzicato :pizz)
    (:sul-ponticello :sul-pont)
    ((:nonvib :senza-vibrato) :non-vibrato)
    (otherwise value)))

(defun expression-vocabularies ()
  "Use the selected instrument; without a family, require global agreement."
  (if (and *mscx-expression-context*
           (expression-context-family *mscx-expression-context*))
      (list (instrument-vocabulary
             (expression-context-family *mscx-expression-context*)
             (expression-context-mapping *mscx-expression-context*)))
    (let ((result nil))
      (maphash (lambda (id family)
                 (declare (ignore id))
                 (dolist (mapping (cons (muse-sounds-family-default-instrument family)
                                        (muse-sounds-family-instruments family)))
                   (push (instrument-vocabulary family mapping) result)))
               *mscx-muse-sounds-families*)
      result)))

(defun expression-term-field (value key &optional default)
  "Compare the requested field, independently of playback IDs or evidence."
  (let ((values nil) (absent (gensym)))
    (dolist (vocabulary (expression-vocabularies))
      (let* ((entry (assoc value vocabulary))
             (field (getf (cdr entry) key absent)))
        (unless (eq field absent) (pushnew field values :test #'equal))))
    (if (= (length values) 1) (values (car values) t) (values default nil))))

(defun expression-technique-from-text (text)
  (when (stringp text)
    (let ((key (normalize-extra-text text)) (values nil))
      (dolist (vocabulary (expression-vocabularies))
        (dolist (entry vocabulary)
          (when (member key (getf (cdr entry) :om-text)
                        :key #'normalize-extra-text :test #'equal)
            (pushnew (car entry) values))))
      (when (= (length values) 1) (car values)))))

(defun expression-technique-text (value)
  (expression-term-field (canonical-expression-value value) :text))

(defun expression-notation-rule (value)
  (let ((value (canonical-expression-value value)))
    (expression-term-field value :mscx-notation
                           (cdr (assoc value *mscx-expression-notation*)))))

(defun expression-placement-key (placement)
  (cond ((member placement '(:staff :chord :note)) placement)
        ((string-equal placement "staff") :staff)
        ((string-equal placement "chord") :chord)
        ((string-equal placement "note") :note)
        (t placement)))

(defgeneric extra-expressions (extra owner)
  (:documentation "Adapter contract: return a list of expression plists.
Each plist has :role, :value, :text, and :placement (:staff/:chord/:note).
Optional :raw-node is an OM XML tree and takes precedence over generated XML.
:continuation :sustain permits repeating local notation on tied segments.
Specialize on another project's extra class; do not call its old XML exporter.
OWNER is the note/chord/voice containing EXTRA."))

(defun expand-semantic-expression (expression)
  "Add notation declared by the selected vocabulary, separately from playback."
  (let ((value (canonical-expression-value (getf expression :value))))
    (setf (getf expression :value) value)
    (when (and (eq (getf expression :role) :technique) (null value)
               (null (getf expression :raw-node)))
      (setf (getf expression :role) :text))
    (cons expression
          (when (and (eq (getf expression :role) :technique)
                     (null (getf expression :raw-node)))
            (copy-tree (expression-term-field value :extra-notation))))))

(defmethod extra-expressions ((extra om::mscx-expression-extra) owner)
  (declare (ignore owner))
  (expand-semantic-expression
   (list :role (om::mscx-expression-role extra)
         :value (om::mscx-expression-value extra)
         :text (om::thetext extra)
         :raw-node (om::mscx-expression-raw-node extra)
         :placement (expression-placement-key (om::mscx-expression-placement extra))
         :anchor (om::mscx-expression-anchor extra))))

(defmethod extra-expressions ((extra om::text-extra) owner)
  (declare (ignore owner))
  (let* ((text (om::thetext extra))
         (technique (expression-technique-from-text text)))
    (expand-semantic-expression
     (list :role (if technique (expression-term-field technique :role :technique) :text)
           :value technique :text text
           :placement (if technique (expression-term-field technique :placement :staff) :staff)))))

(defmethod extra-expressions ((extra om::char-extra) owner)
  (declare (ignore owner))
  (let ((mapping (find-char-extra-mscx-mapping (om::thechar extra))))
    (if mapping
      ;; Keep the existing provisional OM glyph table for native export only.
      ;; Do not infer OM glyph codes on import from this ambiguous table.
      (list (list :role :notation :value (om::thechar extra) :placement :chord
                  :tag (char-extra-mscx-tag mapping)
                  :subtype (char-extra-mscx-subtype mapping)))
      (list (list :role :text :text (format nil "[OM char ~A]" (om::thechar extra))
                  :placement :staff)))))

(defmethod extra-expressions ((extra t) owner)
  (declare (ignore owner))
  (when (and (find-class 'om::head-extra nil) (typep extra 'om::head-extra))
    (let ((head (head-extra->mscx-head extra)))
      (if head
          (list (list :role :head :value head :placement :note))
        (list (list :role :text :text (format nil "[OM head ~A]" (om::thehead extra))
                    :placement :staff))))))

;;; ----------------------------------------------------------------------
;;; One fresh expression context per exported voice. Mapping data live in
;;; mscx-instrument-mappings.lisp; this module renders the selected vocabulary.
;;; ----------------------------------------------------------------------

(defclass expression-context ()
  ((family :initarg :family :reader expression-context-family)
   (mapping :initarg :mapping :reader expression-context-mapping)
   (instrument :initarg :instrument :initform nil :reader expression-context-instrument)
   (current-technique :initform :unset :accessor expression-context-current-technique)
   (current-notation-technique :initform :unset
                             :accessor expression-context-current-notation-technique)))

(defun make-expression-context (&key (instrument-mappings nil mappings-p)
                                     (profile nil profile-p) instrument)
  "Fresh state for one voice. PROFILE is the temporary old keyword alias."
  (let* ((selection (choose-instrument-mappings instrument-mappings mappings-p profile profile-p))
         (family (resolve-instrument-mappings selection)))
    (make-instance 'expression-context :family family
                   :mapping (find-instrument-mapping family instrument)
                   :instrument instrument)))

(defgeneric expression-playback (mapping expression context)
  (:documentation "Return playback XML and MAPPED-P, and update CONTEXT.
Specialize on an instrument-mapping subclass for a Muse Sounds mechanism.
MAPPED-P is true when this method handled the expression's visible text too.
It is NIL for unsupported intent even if XML resets earlier playback state.
Never invent an ID: unsupported expressions retain their notation."))

(defmethod expression-playback ((mapping t) expression context)
  (declare (ignore mapping expression context))
  nil)

(defmethod expression-playback ((mapping instrument-mapping) expression context)
  (when (and (eq (getf expression :role) :technique) (getf expression :value))
    (let* ((*mscx-expression-context* context)
           (family (expression-context-family context))
           (value (canonical-expression-value (getf expression :value)))
           (id (instrument-playing-technique family mapping value))
           (current (expression-context-current-technique context))
           (ordinary-value (instrument-ordinary-value family mapping))
           (ordinary (instrument-playing-technique family mapping ordinary-value)))
      (cond
        (id
         (values
          (unless (equal current id)
            (setf (expression-context-current-technique context) id)
            (staff-text-sound-flag-as-mscx
             (or (getf expression :text) (expression-technique-text value)
                 (string-downcase (princ-to-string value))) id)) t))
        ;; Reset only through this instrument's configured ordinary term.
        ;; Its visible label may be "open", "ordinary", "arco", etc.
        ((and ordinary (stringp current) (not (string= current ordinary)))
         (setf (expression-context-current-technique context) ordinary)
         (values (staff-text-sound-flag-as-mscx
                  (or (getf (instrument-term family mapping ordinary-value) :text)
                      (string-downcase (princ-to-string ordinary-value))) ordinary) nil))))))

;;; ----------------------------------------------------------------------
;;; Lossless XML payload serializer and conservative reverse descriptions.
;;; Independent of importer loading order.
;;; ----------------------------------------------------------------------

(defun mscx-expression-node-name (node)
  (let ((tag (cond ((and (consp node) (consp (car node))) (caar node))
                   ((consp node) (car node))
                   ((symbolp node) node))))
    (and tag (if (symbolp tag) (symbol-name tag) (princ-to-string tag)))))

(defun mscx-expression-node-body (node)
  (and (consp node) (cdr node)))

(defun mscx-expression-node-child (node name)
  (find-if (lambda (child)
             (and (or (consp child) (symbolp child))
                  (string= (or (mscx-expression-node-name child) "") name)))
           (mscx-expression-node-body node)))

(defun mscx-expression-node-text (node)
  (with-output-to-string (out)
    (labels ((walk (item)
               (cond ((stringp item) (write-string item out))
                     ((consp item) (dolist (child (mscx-expression-node-body item))
                                     (walk child))))))
      (when node (walk node)))))

(defun mscx-expression-child-text (node name)
  (let ((child (mscx-expression-node-child node name)))
    (and child (mscx-expression-node-text child))))

(defun mscx-expression-node-as-xml (node)
  "Serialize OM XML trees, including attributes, empty tags and mixed markup.
Returns a one-element list, compatible with the existing exporter XML lists."
  (when node
    (list
     (with-output-to-string (out)
       (labels ((emit (item)
                  (cond
                    ((null item) nil)
                    ((stringp item) (write-string (mscx-expression-xml-escape item) out))
                    ((or (symbolp item) (consp item))
                     (let* ((name (mscx-expression-node-name item))
                            (attrs (and (consp item) (consp (car item)) (cdar item)))
                            (body (mscx-expression-node-body item)))
                       (format out "<~A" name)
                       (loop for (key value) on attrs by #'cddr
                             do (format out " ~A=\"~A\""
                                        (if (symbolp key) (symbol-name key) key)
                                        (mscx-expression-xml-escape value)))
                       (if body
                           (progn (write-char #\> out)
                                  (dolist (child body) (emit child))
                                  (format out "</~A>" name))
                         (write-string "/>" out))))
                    (t (write-string (mscx-expression-xml-escape item) out)))))
         (emit node))))))

(defun expression-playing-technique (node)
  (let ((flag (if (string= (or (mscx-expression-node-name node) "") "SoundFlag")
                  node (mscx-expression-node-child node "SoundFlag"))))
    (and flag (mscx-expression-child-text flag "playingTechnique"))))

(defun expression-value-from-playing-technique (id &optional instrument-mappings)
  "Recognize an ID only when the selected/registered vocabularies agree.
Alias entries (:ordinary reset) are not reverse ID-to-intent mappings."
  (when id
    (let ((values nil))
      (labels ((collect-family (family)
                 (dolist (mapping (cons (muse-sounds-family-default-instrument family)
                                        (muse-sounds-family-instruments family)))
                   (dolist (entry (instrument-vocabulary family mapping))
                     (let ((configured (getf (cdr entry) :playing-technique)))
                       (when (and (stringp configured) (string= id configured))
                         (pushnew (car entry) values)))))))
        (if instrument-mappings
            (let ((family (resolve-instrument-mappings instrument-mappings)))
              (when family (collect-family family)))
          (maphash (lambda (name family) (declare (ignore name)) (collect-family family))
                   *mscx-muse-sounds-families*)))
      (when (= (length values) 1) (car values)))))

(defun expression-notation-entries ()
  "Common grammar and configured instrument notation, for conservative import."
  (let ((result (copy-tree *mscx-expression-notation*)))
    (dolist (vocabulary (expression-vocabularies) result)
      (dolist (entry vocabulary)
        (let ((rule (getf (cdr entry) :mscx-notation)))
          (when rule (pushnew (cons (car entry) rule) result :test #'equal)))))))

(defun expression-subtype-without-orientation (subtype)
  (let ((length (length subtype)))
    (cond ((and (>= length 5) (string= subtype "Above" :start1 (- length 5)))
           (subseq subtype 0 (- length 5)))
          ((and (>= length 5) (string= subtype "Below" :start1 (- length 5)))
           (subseq subtype 0 (- length 5)))
          (t subtype))))

(defun expression-node-description (node placement &optional instrument-mappings)
  "Describe only unambiguous notation/IDs; always retain the original XML.
Unknown or richer data stays raw. No reverse guess of an OM char/font code."
  (let* ((name (mscx-expression-node-name node))
         (text (mscx-expression-child-text node "text"))
         (subtype (mscx-expression-child-text node "subtype"))
         (interval (mscx-expression-child-text node "intervalAbove"))
         (id (expression-playing-technique node))
         (technique (expression-value-from-playing-technique id instrument-mappings))
         (notation (and subtype
                        (find-if
                         (lambda (entry)
                           (and (string= (getf (cdr entry) :tag) (or name ""))
                                (or (string= (getf (cdr entry) :subtype) subtype)
                                    ;; Above/below orientation does not change
                                    ;; semantic articulation; raw XML keeps it.
                                    (and (string= (or name "") "Articulation")
                                         (string= (expression-subtype-without-orientation
                                                   (getf (cdr entry) :subtype))
                                                  (expression-subtype-without-orientation subtype))))
                                (equal (getf (cdr entry) :interval-above) interval)))
                         (expression-notation-entries)))))
    (cond
      (technique
       (list :role :technique :value technique :text (or text (expression-technique-text technique))
             :raw-node node :placement (expression-placement-key placement)))
      ((member name '("StaffText" "SystemText" "PlayTechAnnotation") :test #'equal)
       ;; Plain imported text has no new playback semantics, even if it reads
       ;; "pizzicato". Preserve exactly the original lack of a SoundFlag.
       (list :role (if id :raw :text) :value nil :text (or text "")
             :raw-node node :placement (expression-placement-key placement)))
      (notation
       (list :role (cond ((string= name "Ornament") :ornament)
                         ((string= name "TremoloSingleChord") :tremolo)
                         (t :articulation))
             :value (car notation) :text (string-downcase (symbol-name (car notation)))
             :raw-node node :placement (expression-placement-key placement)))
      ((member name '("head" "headType" "headScheme") :test #'equal)
       (list :role :head :value (mscx-expression-node-text node)
             :text (mscx-expression-node-text node) :raw-node node
             :placement (expression-placement-key placement)))
      (t (list :role :raw :value nil :text (or text subtype name "")
               :raw-node node :placement (expression-placement-key placement))))))

;;; ----------------------------------------------------------------------
;;; Rendering engine shared by staff, chord and note export paths.
;;; ----------------------------------------------------------------------

(defun expression-text (expression)
  (or (getf expression :text)
      (expression-technique-text (getf expression :value))
      (and (getf expression :value)
           (string-downcase (princ-to-string (getf expression :value))))
      ""))

(defun expression-role (expression)
  "Text alone does not define persistent technique intent."
  (let ((role (getf expression :role)))
    (if (and (eq role :technique) (null (getf expression :value))
             (null (getf expression :raw-node)))
        :text role)))

(defun expression-local-rule (expression)
  (if (getf expression :tag)
      (list :tag (getf expression :tag) :subtype (getf expression :subtype)
            :interval-above (getf expression :interval-above)
            :continuation (getf expression :continuation))
    (expression-notation-rule (getf expression :value))))

(defun expression-sustains-p (expression)
  (or (eq (getf expression :continuation) :sustain)
      (eq (getf (expression-local-rule expression) :continuation) :sustain)))

(defun expression-attack-only-p (expression)
  "Own text and noteheads remain explicit on tied segments.
Only local attack signs/ornaments default to attack-only."
  (or (eq (getf expression :continuation) :attack)
      (and (member (expression-role expression) '(:articulation :ornament :tremolo :notation))
           (not (expression-sustains-p expression)))))

(defun expression-effective-placement (expression)
  (let ((role (expression-role expression)))
    (cond
      ;; A local semantic value without a notation mapping survives as text,
      ;; even when an adapter explicitly requested a local placement.
      ((and (null (getf expression :raw-node))
            (member role '(:articulation :ornament :tremolo :notation))
            (null (expression-local-rule expression))) :staff)
      ((and (null (getf expression :raw-node)) (eq role :head)
            (not (member (getf expression :value)
                         (mapcar #'cdr *om-head-text=>mscx-head-text*) :test #'equal)))
       :staff)
      (t (or (and (getf expression :placement)
           (expression-placement-key (getf expression :placement)))
      (case role
        (:head :note)
        ((:articulation :ornament :tremolo :notation)
         (if (expression-local-rule expression) :chord :staff))
        (otherwise :staff)))))))

(defun expression-as-mscx (expression placement context &key continuation-p)
  "Render one descriptor; used also for imported voice-owned timed expressions."
  (let* ((*mscx-expression-context* context)
         (placement (expression-placement-key placement))
         (raw (getf expression :raw-node))
         (role (expression-role expression))
         (value (canonical-expression-value (getf expression :value)))
         (rule (expression-local-rule expression)))
    (when (eq placement (expression-effective-placement expression))
      (cond
        (raw
         ;; Keep explicit repeated source marks and every unknown XML child.
         ;; Observe their ID so a following semantic unknown can reset safely.
         (let ((id (expression-playing-technique raw)))
           (when id
             (setf (expression-context-current-technique context) id
                   ;; A raw source flag may have changed persistent playback
                   ;; after the last generated intent (e.g. pizz -> raw arco).
                   ;; Re-evaluate the next explicit intent against that state.
                   (expression-context-current-notation-technique context) :unset)))
         (mscx-expression-node-as-xml raw))
        ((and continuation-p (expression-attack-only-p expression)) nil)
        ((eq role :technique)
         (unless (equal value (expression-context-current-notation-technique context))
           (setf (expression-context-current-notation-technique context) value)
           (multiple-value-bind (playback mapped-p)
               (expression-playback (expression-context-mapping context) expression context)
             (if mapped-p
                 ;; A harmonic technique has the same ordinary playback ID;
                 ;; still emit its new notation when no SoundFlag is needed.
                 (or playback (staff-text-as-mscx (expression-text expression)))
               (append playback (staff-text-as-mscx (expression-text expression)))))))
        ((and (eq role :head) (eq placement :note))
         (list (format nil "<head>~A</head>" (mscx-expression-xml-escape value))))
        (rule
         (append (list (format nil "<~A>" (getf rule :tag))
                       (format nil "<subtype>~A</subtype>"
                               (mscx-expression-xml-escape (getf rule :subtype))))
                 (when (getf rule :interval-above)
                   (list (format nil "<intervalAbove>~A</intervalAbove>"
                                 (mscx-expression-xml-escape (getf rule :interval-above)))))
                 (list (format nil "</~A>" (getf rule :tag)))))
        ((eq placement :staff) (staff-text-as-mscx (expression-text expression)))))))

(defun expression-signature (expression)
  "Deduplicate intent, so a specialized text-extra is never exported twice."
  (let ((role (expression-role expression))
        (value (canonical-expression-value (getf expression :value)))
        (placement (expression-effective-placement expression)))
    (cond
      ((getf expression :raw-node)
       ;; Distinct imported XML may carry independent text, presets, styling
       ;; or placement despite having the same semantic technique.
       (list placement :raw (getf expression :raw-node)))
      ((eq role :technique) (list placement role value))
      ((expression-local-rule expression)
       (let ((rule (expression-local-rule expression)))
         (list placement :local (getf rule :tag) (getf rule :subtype)
               (getf rule :interval-above) (getf rule :continuation))))
      (t (list placement role value (expression-text expression))))))

(defun owner-expression-descriptions (owner)
  (when owner
    ;; GET-EXTRAS "all" recursively descends through chords and voices.
    ;; The adapter's OWNER must be the actual containing object, including
    ;; after project restore when the extra's OBJECT slot may not be set.
    (loop for extra in (if (fboundp 'om::extra-obj-list)
                           (om::extra-obj-list owner)
                         (om::get-extras owner "all"))
          append (extra-expressions extra owner))))

(defun chord-expression-descriptions (owner)
  "Native note text/char extras retain the existing chord export placement.
Invoke each adapter with its actual note owner; raw note XML stays note-local."
  (append (owner-expression-descriptions owner)
          (when (and owner (find-class 'om::chord nil) (typep owner 'om::chord))
            (loop for note in (om::inside owner)
                  append (owner-expression-descriptions note)))))

(defun expression-extras-as-mscx (owner placement context
                                  &key continuation-p attack-owner)
  "Export all relevant extras for one owner/placement with per-call dedup.
On tied continuations, ATTACK-OWNER supplies only sustaining notation.
The caller creates a fresh context per part for every export."
  (let* ((*mscx-expression-context* context)
         (placement (expression-placement-key placement))
         (own (if (eq placement :note) (owner-expression-descriptions owner)
                (chord-expression-descriptions owner)))
         (inherited (when (and continuation-p attack-owner (not (eq owner attack-owner)))
                      (remove-if-not
                       (lambda (expression)
                         (and (null (getf expression :raw-node))
                              (expression-sustains-p expression)))
                       (chord-expression-descriptions attack-owner))))
         (expressions (append own inherited))
         (seen (make-hash-table :test #'equal)))
    (loop for expression in expressions
          for signature = (expression-signature expression)
          unless (gethash signature seen)
            append (progn (setf (gethash signature seen) t)
                          (expression-as-mscx expression placement context
                                              :continuation-p continuation-p)))))
