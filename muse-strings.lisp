;;;========================
;;; OpenMusic export MSCX 
;;;========================
;;;
;;;
;;; Muse Strings
;;;
;;; aim: mapping technique to instrument-specific palette, as deatailed in a certain MuseSound, e.g "Muse Strings"
;;;
;;; Anders Vinjar - 2026
;;; Time-stamp: <2026-10-08 14:42:28 andersvi>
;;; 
;;;

(in-package :mscx)

;;; Shared vocabulary

;;; <playingTechnique> strings: extracted from supplied - non-comprehensive - div_artikulasjoner.mscz
;;; 
;;; based on: Violin 1 (Solo).
;;; 
;;; Viola/cello not comprehensive, only inherit the prototype (violin)
;;;

;;; Vocabulary is an association list: (TERM . PROPERTY-LIST).
;;; TERM              Semantic key, e.g. :pizz or :harmonics.
;;; :text             Default display text for the term.
;;; :om-text          OM text aliases recognized as this term.
;;; :playing-technique Exact MuseScore SoundFlag/playingTechnique value;
;;;                    :ordinary uses the family's/instrument's reset mapping;
;;;                    NIL or absent means no playback mapping.
;;; :mscx-notation    Local notation rule (:tag, :subtype, etc.).
;;; :extra-notation   Additional expression descriptors emitted with the term.
;;; :continuation     Inside a rule/descriptor: :sustain permits repetition
;;;                    on tied segments; :attack restricts it to the attack.
;;; Instrument overrides merge by TERM, then by property.

(defparameter *muse-strings-vocabulary*
  '((:arco :text "arco" :om-text ("arco" "ord" "ord." "ordinary" "ordinary_technique")
           :playing-technique "ordinary_technique")
    (:pizz :text "pizzicato" :om-text ("pizz" "pizz." "pizzicato")
           :playing-technique "pizzicato")
    (:bartok :text "Bartok" :om-text ("bartok" "bartók" "bartok pizzicato" "snap pizzicato")
             :playing-technique "Bartok")
    (:col-legno :text "col legno" :om-text ("col legno" "col-legno")
                :playing-technique "Col Legno")
    (:espressivo :text "espressivo" :om-text ("espressivo")
                 :playing-technique "Espressivo")
    (:sul-pont :text "sul ponticello" :om-text ("sul pont" "sul pont." "sul ponticello" "sul-pont")
               :playing-technique "Sul Ponticello")
    (:sul-tasto :text "sul tasto" :om-text ("sul tasto" "sul-tasto")
                :playing-technique "Sul Tasto")
    (:non-legato :text "non-legato" :om-text ("non-legato" "non legato")
                 :playing-technique "non-legato")
    (:non-vibrato :text "senza vibrato" :om-text ("nonvib" "non vibrato" "non-vibrato" "senza vibrato")
                  :playing-technique "nonvib")
    (:harmonics :text "harm." :om-text ("harm" "harm." "harmonic" "harmonics")
                :playing-technique :ordinary
                :extra-notation ((:role :articulation :value :harmonics
                                  :placement :chord :continuation :sustain))
                :mscx-notation (:tag "Articulation" :subtype "stringsHarmonic"
                                :continuation :sustain))
    (:up-bow :text "up bow" :mscx-notation (:tag "Articulation" :subtype "stringsUpBow"))
    (:down-bow :text "down bow" :mscx-notation (:tag "Articulation" :subtype "stringsDownBow"))
    (:snap-pizzicato :text "snap pizzicato"
                     :mscx-notation (:tag "Articulation" :subtype "pluckedSnapPizzicatoAbove"))))

;;;   one record per solo instrument.

(register-muse-sounds-family
 (make-instance 'muse-sounds-family
   :id :muse-strings :name "Muse Strings" :ordinary :arco
   :vocabulary *muse-strings-vocabulary*
   :instruments
   (list
    (make-instance 'instrument-mapping
      :id :violin-1-solo :name "Violin 1 Solo"
      :om-names '("violin" "violin solo" "violin 1 solo" "violin-1-solo" "violin (solo)")
      :musescore-id "violin" :musicxml-id "strings.violin"
      :muse-sound-name "Violin 1 (Solo)"
      :evidence '(:xml "div_artikulasjoner.mscz" :audition :user-violin-1-solo))
    (make-instance 'instrument-mapping
      :id :viola-solo :name "Viola Solo"
      :om-names '("viola" "viola solo" "viola-solo" "viola (solo)")
      :musescore-id "viola" :musicxml-id "strings.viola"
      :muse-sound-name "Viola (Solo)"
      :evidence '(:mapping :family-inherited :audition :pending))
    (make-instance 'instrument-mapping
      :id :cello-solo :name "Cello Solo"
      :om-names '("cello" "cello solo" "cello-solo" "cello (solo)")
      :musescore-id "violoncello" :musicxml-id "strings.cello"
      :muse-sound-name "Violoncello (Solo)"
      :evidence '(:mapping :family-inherited :audition :pending)))))

;;; Compatibility data for the earlier standalone TEXT-EXTRA-AS-MSCX helper.
;;; The score exporter uses the instrument vocabulary above, not this table.
(defparameter *om-text-extra-rules*
  (loop for entry in *muse-strings-vocabulary*
        for id = (getf (cdr entry) :playing-technique)
        when (stringp id) append
          (loop for alias in (getf (cdr entry) :om-text) collect
            (list alias :kind :sound-flag :playing-technique id
                  :text (if (member alias '("ord" "ordinary") :test #'equal)
                            "ordinary" (getf (cdr entry) :text))))))
