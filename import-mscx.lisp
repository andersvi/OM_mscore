;;;========================
;;; OpenMusic export MSCX 
;;;========================
;;;
;;; Anders Vinjar - 2026
;;; 
;;; Time-stamp: <2026-05-18 17:27:57 andersvi>
;;; 
;;; import-mscx.lisp
;;; MSCX -> OM poly
;;;
;;; Target, v0.x:
;;; - Import MuseScore .mscx files to OM poly.
;;; - Preserve notated rhythm as OM voice rhythm trees.
;;; - Primary target is the MSCX dialect produced by export-mscx.lisp.
;;;
;;; Depends on OM and the current import-mxml rhythm helpers:
;;;   om::make-tree-builder, om::new-tuplet, om::pop-tuplet,
;;;   om::add-new-pulse, om::get-tree
;;;


(in-package :mscx)

;;; ------------------------------------------------------------
;;; XML helpers
;;; ------------------------------------------------------------

(defun mscx-node-tag (node)
  "Return the tag symbol of a node from om-list-from-xml-file.

Self-closing XML tags may be represented as bare symbols, e.g. :|endTuplet|."
  (cond ((and (consp node) (consp (car node))) (caar node))
        ((consp node) (car node))
        ((symbolp node) node)
        (t nil)))

(defun mscx-node-attrs (node)
  "Return attribute plist from NODE, or NIL."
  (when (and (consp node) (consp (car node)))
    (cdar node)))

(defun mscx-node-body (node)
  "Return children/text body of NODE."
  (cond ((and (consp node) (consp (car node))) (cdr node))
        ((consp node) (cdr node))
        (t nil)))

(defun mscx-tag-equal (node tag)
  (and node (equal (om::interne (mscx-node-tag node)) (om::interne tag))))

(defun mscx-children (node)
  "Return child nodes, ignoring direct string text.

Bare symbol children are included because <endTuplet/> may be represented as :|endTuplet|."
  (remove-if-not #'(lambda (x) (or (consp x) (symbolp x))) (mscx-node-body node)))

(defun mscx-child (node tag)
  (find tag (mscx-children node) :test #'(lambda (wanted child) (mscx-tag-equal child wanted))))

(defun mscx-children-named (node tag)
  (remove-if-not #'(lambda (child) (mscx-tag-equal child tag)) (mscx-children node)))

(defun mscx-direct-text (node)
  "Return direct string body items joined as one string."
  (let ((strings (remove-if-not #'stringp (mscx-node-body node))))
    (when strings (format nil "~{~A~}" strings))))

(defun mscx-child-text (node tag &optional default)
  (let ((child (mscx-child node tag)))
    (or (and child (mscx-direct-text child)) default)))

(defun mscx-child-number (node tag &optional default)
  (let ((txt (mscx-child-text node tag nil)))
    (if txt (read-from-string txt) default)))

(defun mscx-attr (node attr &optional default)
  (let ((attrs (mscx-node-attrs node)))
    (or (getf attrs attr) (getf attrs (om::interne attr)) default)))

;;; ------------------------------------------------------------
;;; Score / staff / measure traversal
;;; ------------------------------------------------------------

(defun mscx-score-node (xml-list)
  "Return the Score node from the top-level MSCX XML list."
  (find-if #'(lambda (node) (mscx-tag-equal node :|Score|)) xml-list))

(defun mscx-score-division (score-node &optional (default 480))
  "Return MSCX Division value. MuseScore normally uses 480."
  (mscx-child-number score-node :|Division| default))

(defun mscx-part-nodes (score-node)
  "Return Part nodes from Score. Mostly informational for v0."
  (mscx-children-named score-node :|Part|))

(defun mscx-staff-nodes (score-node)
  "Return Staff nodes that contain measures.

This intentionally ignores Staff nodes inside Part definitions."
  (remove-if-not #'(lambda (node) (and (mscx-tag-equal node :|Staff|)
                                       (mscx-children-named node :|Measure|)))
                 (mscx-children score-node)))

(defun mscx-staff-id (staff-node)
  (mscx-attr staff-node :|id| nil))

(defun mscx-measure-nodes (staff-node)
  (mscx-children-named staff-node :|Measure|))

(defun mscx-voice-node (measure-node)
  "Return first voice node in MEASURE-NODE. v0 supports one MSCX voice per Staff/Measure."
  (mscx-child measure-node :|voice|))

(defun mscx-voice-children (measure-node)
  "Return children of the first voice in MEASURE-NODE."
  (let ((voice (mscx-voice-node measure-node)))
    (when voice (mscx-children voice))))

;;; ------------------------------------------------------------
;;; Voice item predicates
;;; ------------------------------------------------------------

(defun mscx-chord-p (node) (mscx-tag-equal node :|Chord|))
(defun mscx-rest-p (node) (mscx-tag-equal node :|Rest|))
(defun mscx-note-event-p (node) (or (mscx-chord-p node) (mscx-rest-p node)))
(defun mscx-timesig-p (node) (mscx-tag-equal node :|TimeSig|))
(defun mscx-tempo-p (node) (mscx-tag-equal node :|Tempo|))
(defun mscx-clef-p (node) (mscx-tag-equal node :|Clef|))
(defun mscx-tuplet-p (node) (mscx-tag-equal node :|Tuplet|))
(defun mscx-end-tuplet-p (node) (mscx-tag-equal node :|endTuplet|))
(defun mscx-barline-p (node) (mscx-tag-equal node :|BarLine|))
(defun mscx-stafftext-p (node) (mscx-tag-equal node :|StaffText|))
(defun mscx-dynamic-p (node) (mscx-tag-equal node :|Dynamic|))
(defun mscx-articulation-p (node) (mscx-tag-equal node :|Articulation|))
(defun mscx-fermata-p (node) (mscx-tag-equal node :|Fermata|))

(defun mscx-voice-item-kind (node)
  (cond ((mscx-chord-p node) :chord)
        ((mscx-rest-p node) :rest)
        ((mscx-timesig-p node) :timesig)
        ((mscx-tempo-p node) :tempo)
        ((mscx-clef-p node) :clef)
        ((mscx-tuplet-p node) :tuplet)
        ((mscx-end-tuplet-p node) :end-tuplet)
        ((mscx-barline-p node) :barline)
        ((mscx-stafftext-p node) :stafftext)
        ((mscx-dynamic-p node) :dynamic)
        ((mscx-articulation-p node) :articulation)
        ((mscx-fermata-p node) :fermata)
        (t (mscx-node-tag node))))

;;; ------------------------------------------------------------
;;; Durations
;;; ------------------------------------------------------------

(defun mscx-duration-type-factor (type)
  "Return duration as a factor of one quarter note.

MSCX Division is the number of ticks in one quarter note."
  (cond ((null type) nil)
        ((string= type "measure") :measure)
        ((string= type "longa") 16)
        ((string= type "breve") 8)
        ((string= type "whole") 4)
        ((string= type "half") 2)
        ((string= type "quarter") 1)
        ((or (string= type "eighth") (string= type "8th")) 1/2)
        ((string= type "16th") 1/4)
        ((string= type "32nd") 1/8)
        ((string= type "64th") 1/16)
        ((string= type "128th") 1/32)
        ((string= type "256th") 1/64)
        (t (error "Unsupported MSCX durationType: ~S" type))))

(defun mscx-dots-factor (dots)
  "Return dotted-duration multiplier. 0 -> 1, 1 -> 3/2, 2 -> 7/4, 3 -> 15/8."
  (if (or (null dots) (= dots 0)) 1
      (- 2 (/ 1 (expt 2 dots)))))

(defun mscx-measure-duration-factor (signature)
  "Return full measure duration as quarter-note factor. Example: (4 4) -> 4, (6 8) -> 3."
  (* 4 (/ (first signature) (second signature))))

(defun mscx-event-dots (event-node)
  (mscx-child-number event-node :|dots| 0))

(defun mscx-event-duration-type (event-node)
  (mscx-child-text event-node :|durationType| nil))

(defun mscx-duration-ticks (event-node division signature)
  "Return EVENT-NODE duration in ticks. DIVISION is ticks per quarter note."
  (let* ((type (mscx-event-duration-type event-node))
         (factor (mscx-duration-type-factor type))
         (dots (mscx-event-dots event-node)))
    (cond ((eq factor :measure) (round (* division (mscx-measure-duration-factor signature))))
          (factor (round (* division factor (mscx-dots-factor dots))))
          (t (error "MSCX event without durationType: ~S" event-node)))))

;;; ------------------------------------------------------------
;;; Time signatures and tempo
;;; ------------------------------------------------------------

(defun mscx-timesig (timesig-node &optional (default '(4 4)))
  "Return time signature as (N D), e.g. (4 4)."
  (if timesig-node
      (list (mscx-child-number timesig-node :|sigN| (first default))
            (mscx-child-number timesig-node :|sigD| (second default)))
      default))

(defun mscx-measure-timesig (measure-node current-signature)
  "Return TimeSig found in MEASURE-NODE, or CURRENT-SIGNATURE."
  (let* ((voice (mscx-voice-node measure-node))
         (timesig (and voice (find-if #'mscx-timesig-p (mscx-children voice)))))
    (mscx-timesig timesig current-signature)))

(defun mscx-tempo-raw (tempo-node)
  "Return raw MSCX tempo value. MuseScore stores tempo as quarter-notes per second in <tempo>."
  (mscx-child-number tempo-node :|tempo| nil))

(defun mscx-tempo-bpm (tempo-node)
  "Return BPM from MSCX Tempo node. In MuseScore, <tempo>1</tempo> means quarter = 60."
  (let ((raw (mscx-tempo-raw tempo-node)))
    (when raw (* 60 raw))))

(defun mscx-measure-tempo-bpm (measure-node)
  "Return first tempo marking in measure as BPM, or NIL."
  (let* ((voice (mscx-voice-node measure-node))
         (tempo (and voice (find-if #'mscx-tempo-p (mscx-children voice)))))
    (and tempo (mscx-tempo-bpm tempo))))

(defstruct mscx-import-state
  (division 480)
  (signature '(4 4))
  (measure-index 0)
  tempos)

;;; ------------------------------------------------------------
;;; Note / chord / rest decoding
;;; ------------------------------------------------------------

(defstruct mscx-decoded-note
  midic
  velocity
  tpc)

(defstruct mscx-decoded-event
  type        ; :chord or :rest
  duration    ; integer ticks
  notes       ; list of mscx-decoded-note, NIL for rests
  source)     ; original MSCX node, useful for later extras/debug

(defun mscx-note-pitch (note-node)
  "Return MSCX MIDI pitch number, e.g. 60 for middle C."
  (mscx-child-number note-node :|pitch| nil))

(defun mscx-note-midic (note-node)
  "Return OM midic from MSCX pitch."
  (let ((pitch (mscx-note-pitch note-node)))
    (when pitch (* 100 pitch))))

(defun mscx-note-velocity (note-node &optional (default 100))
  (mscx-child-number note-node :|velocity| default))

(defun mscx-note-tpc (note-node)
  (mscx-child-number note-node :|tpc| nil))

(defun decode-mscx-note (note-node)
  (make-mscx-decoded-note :midic (mscx-note-midic note-node)
                          :velocity (mscx-note-velocity note-node)
                          :tpc (mscx-note-tpc note-node)))

(defun decode-mscx-chord (chord-node division signature)
  (make-mscx-decoded-event :type :chord
                           :duration (mscx-duration-ticks chord-node division signature)
                           :notes (loop for note in (mscx-children-named chord-node :|Note|)
                                        collect (decode-mscx-note note))
                           :source chord-node))

(defun decode-mscx-rest (rest-node division signature)
  (make-mscx-decoded-event :type :rest
                           :duration (mscx-duration-ticks rest-node division signature)
                           :notes nil
                           :source rest-node))

(defun decode-mscx-event (event-node division signature)
  (cond ((mscx-chord-p event-node) (decode-mscx-chord event-node division signature))
        ((mscx-rest-p event-node) (decode-mscx-rest event-node division signature))
        (t (error "Not an MSCX note event: ~S" event-node))))

(defun mscx-decoded-chord->om-chord (decoded-event)
  "Convert a decoded :chord event to an OM chord."
  (unless (eq (mscx-decoded-event-type decoded-event) :chord)
    (error "Not a decoded chord event: ~S" decoded-event))
  (let* ((notes (mscx-decoded-event-notes decoded-event))
         (midics (remove nil (mapcar #'mscx-decoded-note-midic notes)))
         (vels (remove nil (mapcar #'mscx-decoded-note-velocity notes))))
    (make-instance 'om::chord :lmidic midics :lvel vels)))

(defun mscx-decoded-event-pulse (decoded-event)
  "Return positive pulse for chords, negative pulse for rests."
  (let ((dur (mscx-decoded-event-duration decoded-event)))
    (if (eq (mscx-decoded-event-type decoded-event) :rest) (- dur) dur)))

(defun mscx-note-event->om-chord-or-nil (decoded-event)
  "Return OM chord for decoded chord events, NIL for rests."
  (when (eq (mscx-decoded-event-type decoded-event) :chord)
    (mscx-decoded-chord->om-chord decoded-event)))

(defun mscx-measure-has-tuplets-p (measure-node)
  "Return true if MEASURE-NODE contains Tuplet nodes."
  (some #'mscx-tuplet-p (mscx-voice-children measure-node)))

(defun mscx-measure-has-explicit-beam-modes-p (measure-node)
  "Return true if any Chord/Rest in MEASURE-NODE has explicit BeamMode."
  (some #'(lambda (item)
            (and (mscx-note-event-p item)
                 (mscx-event-beam-mode item)))
        (mscx-voice-children measure-node)))

(defun mscx-use-om-simple-tree-for-measure-p (measure-node)
  "Return true when OM simple->tree should be used as AUTO grouping fallback.

This is only for implicit AUTO material: no MSCX Tuplet and no explicit BeamMode."
  (and *mscx-import-use-om-simple-tree-for-auto*
       (not (mscx-measure-has-tuplets-p measure-node))
       (not (mscx-measure-has-explicit-beam-modes-p measure-node))))

;;; ------------------------------------------------------------
;;; Tuplets and beam-span grouping
;;; ------------------------------------------------------------

;; Known issue:
;; Adjacent rests inside tuplets may be normalized/merged by OM after voice construction.
;; Example: (-1 -2) may become (-3)

(defparameter *mscx-import-use-beam-spans* t
  "If true, use conservative BeamMode spans as pure grouping in imported rhythm trees.")

(defparameter *mscx-import-use-om-simple-tree-for-auto* t
  "Use OM simple->tree for measures with no MSCX Tuplet and no explicit BeamMode.")

(defstruct mscx-measure-data
  signature
  tree
  chords
  tempo
  division)

(defstruct mscx-active-tuplet
  id
  time-info
  remaining-events)

(defstruct mscx-rhythm-token
  index             ; index in MSCX voice children
  order             ; note/rest event order inside measure
  kind              ; :chord or :rest
  duration-type
  beam-mode
  written-pulse     ; ticks before tuplet scaling
  actual-pulse      ; exact pulse after tuplet scaling
  tuplet-stack      ; list of time-info, innermost first
  decoded-event)

(defun mscx-measure-has-end-tuplets-p (measure-node)
  "Return true if MEASURE-NODE contains explicit endTuplet markers."
  (some #'mscx-end-tuplet-p (mscx-voice-children measure-node)))

(defun mscx-event-beam-mode (event-node)
  "Return MSCX BeamMode string for Chord/Rest, or NIL."
  (mscx-child-text event-node :|BeamMode| nil))

(defun mscx-tuplet-normal-notes (tuplet-node)
  (mscx-child-number tuplet-node :|normalNotes| nil))

(defun mscx-tuplet-actual-notes (tuplet-node)
  (mscx-child-number tuplet-node :|actualNotes| nil))

(defun mscx-tuplet-base-note (tuplet-node)
  (mscx-child-text tuplet-node :|baseNote| nil))

(defun mscx-tuplet-number-text (tuplet-node)
  (let ((number-node (mscx-child tuplet-node :|Number|)))
    (and number-node (mscx-child-text number-node :|text| nil))))

(defun mscx-tuplet-id (tuplet-node &optional (fallback "1"))
  "Return printed tuplet label, not a structural id.

Nested tuplets may all print the same number, e.g. \"3\". Do not use this as the id passed to tree-builder."
  (or (mscx-tuplet-number-text tuplet-node) fallback))

(defun mscx-tuplet-time-info (tuplet-node)
  "Return tree-builder time-info as (actual normal), e.g. 3:2 -> (3 2)."
  (let ((actual (mscx-tuplet-actual-notes tuplet-node))
        (normal (mscx-tuplet-normal-notes tuplet-node)))
    (when (and actual normal) (list actual normal))))

(defun mscx-active-tuplet-scale (tuplet)
  "Return actual-time scale for one active tuplet. For 3:2, written durations are scaled by 2/3."
  (let* ((time-info (mscx-active-tuplet-time-info tuplet))
         (actual (first time-info))
         (normal (second time-info)))
    (/ normal actual)))

(defun mscx-active-tuplets-scale (active-tuplets)
  "Return combined actual-time scale for nested tuplets."
  (loop for tuplet in active-tuplets
        for scale = (mscx-active-tuplet-scale tuplet)
        for total = scale then (* total scale)
        finally (return (or total 1))))

(defun mscx-scale-pulse-for-active-tuplets (pulse active-tuplets)
  "Scale written MSCX pulse to actual rhythmic pulse, preserving sign.

Return exact rationals when nested tuplets require it. Do not round per event."
  (let* ((sign (if (minusp pulse) -1 1))
         (scaled (* (abs pulse) (mscx-active-tuplets-scale active-tuplets))))
    (* sign scaled)))

(defun mscx-measure-rhythm-tokens (measure-node state)
  "Return rhythm tokens for one MSCX measure.

This does not build an OM tree. It decodes Chord/Rest events with exact actual pulses,
tuplet-stack information, and BeamMode."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state))
         (explicit-tuplet-ends-p (mscx-measure-has-end-tuplets-p measure-node))
         (active-tuplets nil)
         (next-tuplet-id 0)
         (tokens nil)
         (event-order 0))
    (setf (mscx-import-state-signature state) signature)
    (labels ((start-mscx-tuplet (tuplet-node)
               (let* ((time-info (mscx-tuplet-time-info tuplet-node))
                      (actual (mscx-tuplet-actual-notes tuplet-node))
                      (id (format nil "mscx-tuplet-~D" (incf next-tuplet-id))))
                 (when (and time-info actual)
                   (push (make-mscx-active-tuplet :id id :time-info time-info :remaining-events actual)
                         active-tuplets))))
             (finish-current-tuplet ()
               (when active-tuplets (pop active-tuplets)))
             (count-note-event-in-active-tuplet ()
               (when (and active-tuplets (not explicit-tuplet-ends-p))
                 (decf (mscx-active-tuplet-remaining-events (car active-tuplets)))
                 (when (<= (mscx-active-tuplet-remaining-events (car active-tuplets)) 0)
                   (finish-current-tuplet)))))
      (loop for item in (mscx-voice-children measure-node)
            for i from 0 do
              (cond ((mscx-tuplet-p item) (start-mscx-tuplet item))
                    ((mscx-note-event-p item)
                     (let* ((decoded (decode-mscx-event item division signature))
                            (written (mscx-decoded-event-pulse decoded))
                            (actual (mscx-scale-pulse-for-active-tuplets written active-tuplets))
                            (kind (mscx-decoded-event-type decoded)))
                       (push (make-mscx-rhythm-token
                              :index i :order event-order :kind kind
                              :duration-type (mscx-event-duration-type item)
                              :beam-mode (mscx-event-beam-mode item)
                              :written-pulse written :actual-pulse actual
                              :tuplet-stack (mapcar #'mscx-active-tuplet-time-info active-tuplets)
                              :decoded-event decoded)
                             tokens)
                       (incf event-order)
                       (count-note-event-in-active-tuplet)))
                    ((mscx-end-tuplet-p item) (finish-current-tuplet)))))
    (when active-tuplets
      (warn "Unclosed MSCX tuplets while tokenizing measure ~A: ~S"
            (mscx-import-state-measure-index state)
            (mapcar #'mscx-active-tuplet-id active-tuplets)))
    (reverse tokens)))


;;; ------------------------------------------------------------
;;; OM simple->tree fallback for implicit AUTO grouping
;;; ------------------------------------------------------------

;; main OM interface is #'simple->tree


(defun mscx-pulse->duration-ratio (pulse division)
  "Convert MSCX ticks to musical duration ratio.

Examples with division=480:
  480  -> 1/4
  240  -> 1/8
 -120  -> -1/16"
  (/ pulse (* 4 division)))

(defun mscx-measure-simple-durations (measure-node state)
  "Return flat duration ratios for one measure, based on rhythm tokens.

This uses actual-pulse, so rests keep negative sign and tuplets are represented
as their actual rhythmic durations. This function is mainly for debugging and
for simple non-tuplet AUTO grouping."
  (let* ((tokens (mscx-measure-rhythm-tokens measure-node state))
         (division (mscx-import-state-division state)))
    (loop for token in tokens
          collect (mscx-pulse->duration-ratio (mscx-rhythm-token-actual-pulse token) division))))

(defun mscx-simple-tree-result-measure-tree (simple-tree)
  "Extract the measure tree from OM simple->tree result.

OM simple->tree returns something like:
  (? (((4 4) tree)))

This returns only:
  tree"
  (let* ((measures (second simple-tree))
         (first-measure (first measures)))
    (second first-measure)))

(defun mscx-tokens->om-simple-measure-tree (tokens signature division)
  "Use OM simple->tree to build default grouping for a single measure.

This should only be used for plain measures where MSCX has no Tuplet and no
explicit BeamMode. Tuplets and explicit BeamMode are handled elsewhere."
  (let* ((durations (loop for token in tokens
                          collect (mscx-pulse->duration-ratio
                                   (mscx-rhythm-token-actual-pulse token)
                                   division)))
         (simple-tree (om::simple->tree durations (list signature))))
    (mscx-simple-tree-result-measure-tree simple-tree)))

(defun mscx-measure-simple-tree-debug (measure-node state)
  "Return what OM simple->tree would build for this measure.

This is a debug helper and should be tested before plugging simple->tree into
the real import path."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (tokens (mscx-measure-rhythm-tokens measure-node state))
         (division (mscx-import-state-division state))
         (durations (loop for token in tokens
                          collect (mscx-pulse->duration-ratio
                                   (mscx-rhythm-token-actual-pulse token)
                                   division)))
         (simple-tree (om::simple->tree durations (list signature))))
    (list :signature signature
          :durations durations
          :simple-tree simple-tree
          :measure-tree (mscx-simple-tree-result-measure-tree simple-tree))))


;; beam-span-seksjonen

(defun mscx-primary-beam-mode-p (beam-mode)
  (and beam-mode (or (string= beam-mode "begin")
                     (string= beam-mode "mid")
                     (string= beam-mode "end")
                     (string= beam-mode "no"))))

(defun mscx-beam-mode-begin-p (beam-mode) (and beam-mode (string= beam-mode "begin")))
(defun mscx-beam-mode-mid-p (beam-mode) (and beam-mode (string= beam-mode "mid")))
(defun mscx-beam-mode-end-p (beam-mode) (and beam-mode (string= beam-mode "end")))
(defun mscx-beam-mode-no-p (beam-mode) (and beam-mode (string= beam-mode "no")))

(defun mscx-make-beam-span (tokens start-pos end-pos)
  (let ((start-token (nth start-pos tokens))
        (end-token (nth end-pos tokens)))
    (list :start-order (mscx-rhythm-token-order start-token)
          :end-order (mscx-rhythm-token-order end-token)
          :start-index (mscx-rhythm-token-index start-token)
          :end-index (mscx-rhythm-token-index end-token)
          :length (1+ (- end-pos start-pos)))))

(defun mscx-parse-primary-beam-spans (tokens)
  "Parse conservative primary beam spans from rhythm TOKENS.

This identifies explicit begin...end spans. Nested/secondary beams such as begin16 are ignored for now."
  (let ((spans nil)
        (open-start nil))
    (labels ((close-span (end-pos)
               (when (and open-start end-pos (>= end-pos open-start))
                 (when (> (1+ (- end-pos open-start)) 1)
                   (push (mscx-make-beam-span tokens open-start end-pos) spans)))
               (setf open-start nil)))
      (loop for token in tokens
            for pos from 0
            for beam = (mscx-rhythm-token-beam-mode token)
	    do
	       (cond ((mscx-beam-mode-begin-p beam)
                      ;; Conservative: begin inside active span does not start nested group yet.
                      (unless open-start (setf open-start pos)))
                     ((mscx-beam-mode-mid-p beam)
                      ;; If a mid appears without begin, start a forgiving span here.
                      (unless open-start (setf open-start pos)))
                     ((mscx-beam-mode-end-p beam)
                      (when open-start (close-span pos)))
                     ((mscx-beam-mode-no-p beam)
                      (when open-start (close-span (1- pos))))
                     ((null beam)
                      (when open-start (close-span (1- pos))))))
      (when open-start (close-span (1- (length tokens)))))
    (reverse spans)))

(defun mscx-parse-strict-primary-beam-spans (tokens)
  "Parse only complete explicit BeamMode begin...end spans.

Unlike mscx-parse-primary-beam-spans, this does not start spans from MID.
This is useful for deciding whether MSCX contains enough explicit beam info
to override OM simple->tree AUTO grouping."
  (let ((spans nil)
        (open-start nil))
    (labels ((close-span (end-pos)
               (when (and open-start end-pos (>= end-pos open-start))
                 (when (> (1+ (- end-pos open-start)) 1)
                   (push (mscx-make-beam-span tokens open-start end-pos) spans)))
               (setf open-start nil)))

      (loop for token in tokens
            for pos from 0
            for beam = (mscx-rhythm-token-beam-mode token) do
              (cond
                ((mscx-beam-mode-begin-p beam)
                 (setf open-start pos))

                ((mscx-beam-mode-end-p beam)
                 (when open-start
                   (close-span pos)))

                ((or (mscx-beam-mode-no-p beam)
                     (null beam))
                 ;; Explicit break or no explicit beam: abandon incomplete span.
                 (setf open-start nil))))

      ;; Dangling begin without end is not a complete span.
      )

    (reverse spans)))

(defun mscx-parse-beam-spans (tokens division signature)
  "Return beam spans for TOKENS.

For now, use only explicit BeamMode begin/mid/end spans.
DIVISION and SIGNATURE are accepted for later AUTO-beam fallback, but ignored here."
  (declare (ignore division signature))
  (mscx-parse-primary-beam-spans tokens))

(defun mscx-rhythm-token-tuplet-depth (token)
  (length (mscx-rhythm-token-tuplet-stack token)))

(defun mscx-rhythm-tokens-max-tuplet-depth (tokens)
  (loop for token in tokens maximize (mscx-rhythm-token-tuplet-depth token) into max-depth
        finally (return (or max-depth 0))))

(defun mscx-use-beam-spans-for-tokens-p (tokens)
  "Return true if beam spans are safe enough to use for TOKENS.

v0 policy: do not apply beam grouping in measures with nested tuplets. Tuplets themselves are still imported."
  (<= (mscx-rhythm-tokens-max-tuplet-depth tokens) 1))

(defun mscx-beam-spans-starting-at (spans order)
  (remove-if-not #'(lambda (span) (= (getf span :start-order) order)) spans))

(defun mscx-beam-spans-ending-at (spans order)
  (remove-if-not #'(lambda (span) (= (getf span :end-order) order)) spans))



;;; ------------------------------------------------------------
;;; MAIN WORKHORSE: Measure decoding -> OM chords + rhythm tree
;;; ------------------------------------------------------------

(defun measure-from-mscx (measure-node state)
  "Decode one MSCX measure to an MSCX-MEASURE-DATA struct.

Tuplets are structural. BeamMode is optional grouping only: if *MSCX-IMPORT-USE-BEAM-SPANS* is true,
BeamMode is first parsed into conservative spans, then applied as pure grouping."

  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
	 (division (mscx-import-state-division state))
	 (tempo (mscx-measure-tempo-bpm measure-node))
	 (tree-builder (om::make-tree-builder))
	 (explicit-tuplet-ends-p (mscx-measure-has-end-tuplets-p measure-node))

	 ;; Token pass used both for explicit beam-spans and OM simple->tree fallback.
	 (token-state (make-mscx-import-state :division division
                                     :signature signature
                                     :measure-index (mscx-import-state-measure-index state)))

	 (tokens (and (or *mscx-import-use-beam-spans*
			  *mscx-import-use-om-simple-tree-for-auto*)
		      (mscx-measure-rhythm-tokens measure-node token-state)))

	 ;; Explicit BeamMode spans.
	 (beam-spans (and *mscx-import-use-beam-spans*
			  tokens
			  (mscx-parse-beam-spans tokens division signature)))

	 (use-beam-spans-p (and beam-spans
				(mscx-use-beam-spans-for-tokens-p tokens)))

	 ;; OM default grouping fallback for implicit AUTO material.
	 
	 (use-om-simple-tree-p (and *mscx-import-use-om-simple-tree-for-auto*
				    tokens
				    (not (mscx-measure-has-tuplets-p measure-node))
				    ;; Important: partial BeamMode overrides should not block OM AUTO.
				    (null beam-spans)))

	 (chords nil)
	 (active-tuplets nil)
	 (active-beams nil)
	 (next-tuplet-id 0)
	 (next-beam-id 0)
	 (event-order 0))
    
    (setf (mscx-import-state-signature state) signature)
    (labels ((start-mscx-tuplet (tuplet-node)
               (let* ((time-info (mscx-tuplet-time-info tuplet-node))
                      (actual (mscx-tuplet-actual-notes tuplet-node))
                      (id (format nil "mscx-tuplet-~D" (incf next-tuplet-id))))
                 (when (and time-info actual)
                   (om::new-tuplet tree-builder id time-info)
                   (push (make-mscx-active-tuplet :id id :time-info time-info :remaining-events actual)
                         active-tuplets))))
             (finish-current-tuplet ()
               (when active-tuplets
                 (om::pop-tuplet tree-builder)
                 (pop active-tuplets)))
             (count-note-event-in-active-tuplet ()
               (when (and active-tuplets (not explicit-tuplet-ends-p))
                 (decf (mscx-active-tuplet-remaining-events (car active-tuplets)))
                 (when (<= (mscx-active-tuplet-remaining-events (car active-tuplets)) 0)
                   (finish-current-tuplet))))
             (start-beam-span (span)
               (declare (ignore span))
               (let ((id (format nil "mscx-beam-~D" (incf next-beam-id))))
                 ;; NIL time-info means pure group, no tuplet scaling.
                 (om::new-tuplet tree-builder id nil)
                 (push id active-beams)))
             (finish-beam-span (span)
               (declare (ignore span))
               (when active-beams
                 (om::pop-tuplet tree-builder)
                 (pop active-beams)))
             (start-beam-spans-at (order)
               (when use-beam-spans-p
                 (dolist (span (mscx-beam-spans-starting-at beam-spans order))
                   (start-beam-span span))))
             (finish-beam-spans-at (order)
               (when use-beam-spans-p
                 (dolist (span (reverse (mscx-beam-spans-ending-at beam-spans order)))
                   (finish-beam-span span)))))
      (loop for item in (mscx-voice-children measure-node) do
        (cond ((mscx-tuplet-p item) (start-mscx-tuplet item))
              ((mscx-note-event-p item)
               (let* ((decoded (decode-mscx-event item division signature))
                      (written-pulse (mscx-decoded-event-pulse decoded))
                      (pulse (mscx-scale-pulse-for-active-tuplets written-pulse active-tuplets))
                      (om-chord (mscx-note-event->om-chord-or-nil decoded)))
                 (start-beam-spans-at event-order)
                 (om::add-new-pulse tree-builder pulse)
                 (when om-chord (push om-chord chords))
                 (finish-beam-spans-at event-order)
                 (incf event-order)
                 (count-note-event-in-active-tuplet)))
              ((mscx-end-tuplet-p item) (finish-current-tuplet)))))
    (when active-beams
      (warn "Unclosed MSCX beam spans in measure ~A: ~S"
            (mscx-import-state-measure-index state) active-beams)
      (loop while active-beams do (om::pop-tuplet tree-builder) (pop active-beams)))
    (when active-tuplets
      (warn "Unclosed MSCX tuplets in measure ~A: ~S"
            (mscx-import-state-measure-index state)
            (mapcar #'mscx-active-tuplet-id active-tuplets))
      (loop while active-tuplets do (om::pop-tuplet tree-builder) (pop active-tuplets)))
    (make-mscx-measure-data :signature signature
                            :tree (if use-om-simple-tree-p
                                      (mscx-tokens->om-simple-measure-tree tokens signature division)
                                      (om::get-tree tree-builder))
                            :chords (reverse chords)
                            :tempo tempo
                            :division division)))

;;; ------------------------------------------------------------
;;; Debug utilities
;;; ------------------------------------------------------------

(defun mscx-score-summary (xml-list)
  "Return a small summary of the MSCX score structure."
  (let* ((score (mscx-score-node xml-list))
         (division (and score (mscx-score-division score)))
         (parts (and score (mscx-part-nodes score)))
         (staffs (and score (mscx-staff-nodes score))))
    (list :division division
          :parts (length parts)
          :staffs (length staffs)
          :measures-per-staff (loop for staff in staffs collect (length (mscx-measure-nodes staff)))
          :staff-ids (loop for staff in staffs collect (mscx-staff-id staff)))))

(defun mscx-measure-item-kinds (measure-node)
  "Return symbolic item kinds for the first voice in MEASURE-NODE."
  (loop for item in (mscx-voice-children measure-node) collect (mscx-voice-item-kind item)))

(defun mscx-staff-item-summary (staff-node)
  "Return one item-kind list per measure."
  (loop for measure in (mscx-measure-nodes staff-node) collect (mscx-measure-item-kinds measure)))

(defun mscx-score-item-summary (xml-list)
  "Return voice item summaries for all staffs."
  (let* ((score (mscx-score-node xml-list))
         (staffs (and score (mscx-staff-nodes score))))
    (loop for staff in staffs collect (list :staff-id (mscx-staff-id staff)
                                            :measures (mscx-staff-item-summary staff)))))

(defun mscx-decode-measure-events (measure-node state)
  "Decode Chord/Rest events in MEASURE-NODE, ignoring other voice items."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state)))
    (setf (mscx-import-state-signature state) signature)
    (loop for item in (mscx-voice-children measure-node)
          when (mscx-note-event-p item)
            collect (decode-mscx-event item division signature))))

(defun mscx-rhythm-token-debug-row (token)
  (list :order (mscx-rhythm-token-order token)
        :i (mscx-rhythm-token-index token)
        :kind (mscx-rhythm-token-kind token)
        :duration-type (mscx-rhythm-token-duration-type token)
        :beam (mscx-rhythm-token-beam-mode token)
        :written (mscx-rhythm-token-written-pulse token)
        :actual (mscx-rhythm-token-actual-pulse token)
        :tuplets (mscx-rhythm-token-tuplet-stack token)))

(defun mscx-measure-rhythm-token-debug (measure-node state)
  (mapcar #'mscx-rhythm-token-debug-row (mscx-measure-rhythm-tokens measure-node state)))

(defun mscx-measure-beam-span-debug (measure-node state)
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (tokens (mscx-measure-rhythm-tokens measure-node state))
         (division (mscx-import-state-division state))
         (loose-spans (mscx-parse-primary-beam-spans tokens))
         (strict-spans (mscx-parse-strict-primary-beam-spans tokens))
         (beam-spans (mscx-parse-beam-spans tokens division signature)))
    (list :signature signature
          :tokens (mapcar #'mscx-rhythm-token-debug-row tokens)
          :loose-spans loose-spans
          :strict-spans strict-spans
          :spans beam-spans)))

(defun mscx-measure-beam-policy-debug (measure-node state)
  (let* ((tokens (mscx-measure-rhythm-tokens measure-node state))
         (max-depth (mscx-rhythm-tokens-max-tuplet-depth tokens)))
    (list :max-tuplet-depth max-depth
          :use-beam-spans (mscx-use-beam-spans-for-tokens-p tokens)
          :spans (when (mscx-use-beam-spans-for-tokens-p tokens)
                   (mscx-parse-primary-beam-spans tokens)))))

(defun mscx-measure-rhythm-debug (measure-node state)
  "Return readable rhythm-token info for one MSCX measure, including explicit tuplet start/end rows."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state))
         (explicit-tuplet-ends-p (mscx-measure-has-end-tuplets-p measure-node))
         (active-tuplets nil)
         (next-tuplet-id 0)
         (rows nil))
    (labels ((start-mscx-tuplet (tuplet-node)
               (let* ((time-info (mscx-tuplet-time-info tuplet-node))
                      (actual (mscx-tuplet-actual-notes tuplet-node))
                      (printed (mscx-tuplet-id tuplet-node nil))
                      (id (format nil "mscx-tuplet-~D" (incf next-tuplet-id))))
                 (push (make-mscx-active-tuplet :id id :time-info time-info :remaining-events actual)
                       active-tuplets)
                 (push (list :tuplet-start :id id :printed printed :time-info time-info :remaining actual) rows)))
             (finish-current-tuplet ()
               (when active-tuplets
                 (push (list :tuplet-end :id (mscx-active-tuplet-id (car active-tuplets))) rows)
                 (pop active-tuplets)))
             (count-note-event-in-active-tuplet ()
               (when (and active-tuplets (not explicit-tuplet-ends-p))
                 (decf (mscx-active-tuplet-remaining-events (car active-tuplets)))
                 (when (<= (mscx-active-tuplet-remaining-events (car active-tuplets)) 0)
                   (finish-current-tuplet)))))
      (loop for item in (mscx-voice-children measure-node)
            for i from 0 do
              (cond ((mscx-tuplet-p item) (start-mscx-tuplet item))
                    ((mscx-note-event-p item)
                     (let* ((decoded (decode-mscx-event item division signature))
                            (written (mscx-decoded-event-pulse decoded))
                            (actual (mscx-scale-pulse-for-active-tuplets written active-tuplets)))
                       (push (list :i i
                                   :kind (mscx-voice-item-kind item)
                                   :duration-type (mscx-event-duration-type item)
                                   :beam (mscx-event-beam-mode item)
                                   :written written
                                   :actual actual
                                   :tuplets (mapcar #'mscx-active-tuplet-time-info active-tuplets))
                             rows)
                       (count-note-event-in-active-tuplet)))
                    ((mscx-end-tuplet-p item) (finish-current-tuplet)))))
    (reverse rows)))

(defstruct mscx-struct-node
  type          ; :root, :tuplet, :chord, :rest
  id
  time-info
  duration-type
  written-pulse
  beam-mode
  children)

(defun mscx-struct-node-add-child (parent child)
  (setf (mscx-struct-node-children parent)
        (append (mscx-struct-node-children parent) (list child)))
  child)

(defun mscx-measure-struct-tree (measure-node state)
  "Return an explicit nested structure from Tuplet/endTuplet and Chord/Rest.

This is a debug/validation structure, not an OM tree."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state))
         (root (make-mscx-struct-node :type :root :children nil))
         (stack nil)
         (next-tuplet-id 0))
    (setf (mscx-import-state-signature state) signature)
    (push root stack)
    (labels ((current-parent () (car stack))
             (start-tuplet (tuplet-node)
               (let* ((id (format nil "mscx-tuplet-~D" (incf next-tuplet-id)))
                      (node (make-mscx-struct-node :type :tuplet
                                                   :id id
                                                   :time-info (mscx-tuplet-time-info tuplet-node)
                                                   :children nil)))
                 (mscx-struct-node-add-child (current-parent) node)
                 (push node stack)))
             (end-tuplet ()
               (if (> (length stack) 1)
                   (pop stack)
                   (warn "endTuplet with no open tuplet in measure ~A" (mscx-import-state-measure-index state))))
             (add-event (item)
               (let* ((decoded (decode-mscx-event item division signature))
                      (node (make-mscx-struct-node :type (mscx-decoded-event-type decoded)
                                                   :duration-type (mscx-event-duration-type item)
                                                   :written-pulse (mscx-decoded-event-pulse decoded)
                                                   :beam-mode (mscx-event-beam-mode item)
                                                   :children nil)))
                 (mscx-struct-node-add-child (current-parent) node))))
      (loop for item in (mscx-voice-children measure-node)
	    do
               (cond ((mscx-tuplet-p item) (start-tuplet item))
		     ((mscx-note-event-p item) (add-event item))
		     ((mscx-end-tuplet-p item) (end-tuplet)))))
    (when (> (length stack) 1)
      (warn "Unclosed tuplets in structural parse of measure ~A: depth ~A"
            (mscx-import-state-measure-index state)
            (1- (length stack))))
    root))

(defun mscx-struct-node->sexp (node)
  "Return compact s-expression for MSCX structural debug tree."
  (case (mscx-struct-node-type node)
    (:root (mapcar #'mscx-struct-node->sexp (mscx-struct-node-children node)))
    (:tuplet (list :tuplet
                   (mscx-struct-node-time-info node)
                   (mapcar #'mscx-struct-node->sexp (mscx-struct-node-children node))))
    (:chord (list :chord (mscx-struct-node-duration-type node) (mscx-struct-node-written-pulse node)))
    (:rest (list :rest (mscx-struct-node-duration-type node) (mscx-struct-node-written-pulse node)))
    (otherwise (list :unknown (mscx-struct-node-type node)))))

(defun mscx-score-struct-debug (path)
  "Return structural Tuplet/Chord/Rest parse for all measures in first staff."
  (let* ((x (om::om-list-from-xml-file path))
         (score (mscx-score-node x))
         (state (make-mscx-import-state :division (mscx-score-division score)))
         (staff (first (mscx-staff-nodes score))))
    (loop for measure in (mscx-measure-nodes staff)
          for i from 1
          do (setf (mscx-import-state-measure-index state) (1- i))
          collect (list :measure i
                        :struct (mscx-struct-node->sexp (mscx-measure-struct-tree measure state))))))

(defun mscx-measure-debug-data (measure-node state)
  "Return readable measure data for quick REPL testing."
  (let ((data (measure-from-mscx measure-node state)))
    (list :signature (mscx-measure-data-signature data)
          :tree (mscx-measure-data-tree data)
          :tempo (mscx-measure-data-tempo data)
          :division (mscx-measure-data-division data)
          :n-chords (length (mscx-measure-data-chords data))
          :chord-midics (loop for ch in (mscx-measure-data-chords data) collect (om::lmidic ch))
          :chord-vels (loop for ch in (mscx-measure-data-chords data) collect (om::lvel ch)))))

(defun mscx-score-rhythm-debug (path)
  (let* ((x (om::om-list-from-xml-file path))
         (score (mscx-score-node x))
         (division (mscx-score-division score)))
    (loop for staff in (mscx-staff-nodes score)
          for s from 1
          collect
          (let ((state (make-mscx-import-state :division division)))
            (list :staff s
                  :measures
                  (loop for measure in (mscx-measure-nodes staff)
                        for m from 1
                        do (setf (mscx-import-state-measure-index state) (1- m))
                        collect (list :measure m
                                      :items (mscx-measure-rhythm-debug measure state))))))))

(defun mscx-import-summary (path)
  (let ((p (om::import-mscx path)))
    (loop for voice in (om::inside p)
          for i from 1
          collect (list :voice i
                        :tree (om::tree voice)
                        :tempo (om::tempo voice)
                        :n-chords (length (om::chords voice))
                        :midics (mapcar #'om::lmidic (om::chords voice))))))

;;; ------------------------------------------------------------
;;; Staff -> OM voice
;;; ------------------------------------------------------------

(defun mscx-measure-data->tree-item (measure-data)
  "Return one OM measure tree item: ((N D) tree)."
  (list (mscx-measure-data-signature measure-data)
        (mscx-measure-data-tree measure-data)))

(defun mscx-measure-data-list->tree (measure-data-list)
  "Return full OM voice tree. OM may normalize ? to 1 on construction."
  (list '? (loop for data in measure-data-list collect (mscx-measure-data->tree-item data))))

(defun mscx-measure-data-list->chords (measure-data-list)
  "Return flat chord list for OM voice."
  (loop for data in measure-data-list append (mscx-measure-data-chords data)))

(defun mscx-measure-data-list->tempo (measure-data-list)
  "Return OM voice tempo structure.

First tempo becomes the initial tempo. Later tempos are returned as measure-positioned tempo events.
For now all MSCX tempos are normalized to quarter = BPM."
  (let ((tempo-events nil)
        (initial nil))
    (loop for data in measure-data-list
          for i from 0
          for bpm = (mscx-measure-data-tempo data)
          when bpm do
            (unless initial (setf initial (list 1/4 bpm)))
            (push (list (list i 0) (list 1/4 bpm nil)) tempo-events))
    (list (or initial '(1/4 60))
          (cdr (reverse tempo-events)))))

(defun staff-from-mscx (staff-node state &key name)
  "Decode one MSCX Staff into one OM voice."
  (let ((measure-data-list nil))
    (loop for measure in (mscx-measure-nodes staff-node)
          for i from 0 do
            (setf (mscx-import-state-measure-index state) i)
            (push (measure-from-mscx measure state) measure-data-list))
    (let* ((data (reverse measure-data-list))
           (tree (mscx-measure-data-list->tree data))
           (chords (mscx-measure-data-list->chords data))
           (tempo (mscx-measure-data-list->tempo data)))
      (make-instance 'om::voice :tree tree :chords chords :tempo tempo :name name))))

(defun mscx-staff-debug-data (staff-node state)
  "Return readable Staff import data."
  (let ((voice (staff-from-mscx staff-node state :name (or (mscx-staff-id staff-node) "MSCX Staff"))))
    (list :voice voice
          :tree (om::tree voice)
          :tempo (om::tempo voice)
          :n-chords (length (om::chords voice))
          :chord-midics (loop for ch in (om::chords voice) collect (om::lmidic ch)))))

;;; ------------------------------------------------------------
;;; Score -> OM poly
;;; ------------------------------------------------------------

(defun mscx-staff-name (staff-node)
  "Return a simple Staff name for v0."
  (or (mscx-staff-id staff-node) "MSCX Staff"))

(defun score-from-mscx (score-node)
  "Decode MSCX Score into OM poly."
  (let* ((division (mscx-score-division score-node))
         (staffs (mscx-staff-nodes score-node))
         (voices (loop for staff in staffs
                       collect (staff-from-mscx staff
                                                (make-mscx-import-state :division division)
                                                :name (mscx-staff-name staff)))))
    (make-instance 'om::poly :voices voices)))

(defun read-mscx-list (xml-list)
  "Decode an om-list-from-xml-file MSCX list into OM poly."
  (let ((score (mscx-score-node xml-list)))
    (unless score (error "No Score node found in MSCX list."))
    (score-from-mscx score)))

;;; ------------------------------------------------------------
;;; OM interface
;;; ------------------------------------------------------------

(in-package :om)

(defmethod! import-mscx ((path t))
  :icon 352
  :indoc '("MSCX file path")
  :initvals '(nil)
  :doc "Import a MuseScore MSCX file as an OM poly object."
  (let ((file (or (and path (probe-file path))
                  (om-choose-file-dialog
                   :prompt "Choose MuseScore MSCX file"
                   :button-string "Import"
                   :types '("MuseScore MSCX" "*.mscx" "All Documents" "*.*")))))
    (when file
      (mscx::read-mscx-list (om-list-from-xml-file file)))))
