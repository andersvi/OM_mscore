;;;========================
;;; OpenMusic import .mscx Musescore files
;;;========================
;;;
;;; Anders Vinjar - 2026
;;; 
;;; Time-stamp: <2026-05-13 11:15:14 andersvi>
;;;
;;;
;;; ------------------------------------------------------------
;;; import-mscx.lisp
;;; MSCX -> OM poly
;;; First target: MSCX files produced by export-mscx.lisp.
;;; ------------------------------------------------------------
;;; requires existing code in OMs 'import-export/export-mxml.lisp' and 'import-export/export-mxml.lisp
;;; 
;;; Public entry points are defined at bottom in OM package.
;;; Core implementation lives in MSCX package.
;;; 

(in-package :mscx)

;;; ------------------------------------------------------------
;;; XML helpers
;;; ------------------------------------------------------------

(defun mscx-node-tag (node)
  "Return the tag symbol of a node from om-list-from-xml-file."
  (cond
    ((and (consp node) (consp (car node))) (caar node))
    ((consp node) (car node))
    (t nil)))

(defun mscx-node-attrs (node)
  "Return attribute plist from node, or NIL."
  (when (and (consp node) (consp (car node)))
    (cdar node)))

(defun mscx-node-body (node)
  "Return children/text body of NODE."
  (cond
    ((and (consp node) (consp (car node))) (cdr node))
    ((consp node) (cdr node))
    (t nil)))

(defun mscx-tag-equal (node tag)
  (and node
       (equal (om::interne (mscx-node-tag node))
              (om::interne tag))))

(defun mscx-children (node)
  "Return child nodes only, ignoring direct string text."
  (remove-if-not #'consp (mscx-node-body node)))

(defun mscx-child (node tag)
  (find tag (mscx-children node)
        :test #'(lambda (wanted child)
                  (mscx-tag-equal child wanted))))

(defun mscx-children-named (node tag)
  (remove-if-not #'(lambda (child)
                     (mscx-tag-equal child tag))
                 (mscx-children node)))

(defun mscx-direct-text (node)
  "Return direct string body items joined as one string."
  (let ((strings (remove-if-not #'stringp (mscx-node-body node))))
    (when strings
      (format nil "~{~A~}" strings))))

(defun mscx-child-text (node tag &optional default)
  (let ((child (mscx-child node tag)))
    (or (and child (mscx-direct-text child))
        default)))

(defun mscx-child-number (node tag &optional default)
  (let ((txt (mscx-child-text node tag nil)))
    (if txt
        (read-from-string txt)
        default)))

(defun mscx-attr (node attr &optional default)
  (let ((attrs (mscx-node-attrs node)))
    (or (getf attrs attr)
        (getf attrs (om::interne attr))
        default)))


;; (setq x (om::om-list-from-xml-file "mscores/articulations_text_extras.mscx"))

;; (mscx::mscx-node-tag (first x))
;; ;; => :|museScore|

;; (mscx::mscx-child-text (second x) :|Division|)
;; ;; => "480"


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
  (remove-if-not #'(lambda (node)
                     (and (mscx-tag-equal node :|Staff|)
                          (mscx-children-named node :|Measure|)))
                 (mscx-children score-node)))

(defun mscx-staff-id (staff-node)
  (mscx-attr staff-node :|id| nil))

(defun mscx-measure-nodes (staff-node)
  (mscx-children-named staff-node :|Measure|))

(defun mscx-voice-node (measure-node)
  "Return first voice node in MEASURE-NODE.

v0 supports one MSCX voice per Staff/Measure."
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



;;; ------------------------------------------------------------
;;; Debug helpers
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

;; (mscx-score-summary om::x)
;; (:division 480 :parts 1 :staffs 1 :measures-per-staff (1) :staff-ids ("1"))


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


;; (mscx::mscx-score-item-summary om::x)
;; - ((:staff-id "1" :measures ((:clef :timesig :tempo :chord :stafftext :chord :chord :chord :barline))))


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
  "Return dotted-duration multiplier.

0 -> 1, 1 -> 3/2, 2 -> 7/4, 3 -> 15/8."
  (if (or (null dots) (= dots 0)) 1
      (- 2 (/ 1 (expt 2 dots)))))

(defun mscx-measure-duration-factor (signature)
  "Return full measure duration as quarter-note factor.

Example: (4 4) -> 4, (3 4) -> 3, (6 8) -> 3."
  (* 4 (/ (first signature) (second signature))))

(defun mscx-event-dots (event-node)
  (mscx-child-number event-node :|dots| 0))

(defun mscx-event-duration-type (event-node)
  (mscx-child-text event-node :|durationType| nil))

(defun mscx-duration-ticks (event-node division signature)
  "Return EVENT-NODE duration in integer ticks.

DIVISION is ticks per quarter note."
  (let* ((type (mscx-event-duration-type event-node))
         (factor (mscx-duration-type-factor type))
         (dots (mscx-event-dots event-node)))
    (cond ((eq factor :measure)
           (round (* division (mscx-measure-duration-factor signature))))
          (factor
           (round (* division factor (mscx-dots-factor dots))))
          (t
           (error "MSCX event without durationType: ~S" event-node)))))


;; (let* ((score (mscx::mscx-score-node om::x))
;;        (staff (first (mscx::mscx-staff-nodes score)))
;;        (measure (first (mscx::mscx-measure-nodes staff)))
;;        (chord (find-if #'mscx::mscx-chord-p (mscx::mscx-voice-children measure))))
;;   (mscx::mscx-duration-ticks chord 480 '(4 4)))
;; ;; => 480
;; (mscx::mscx-duration-type-factor "measure")
;; ;; => :MEASURE
;; (mscx::mscx-measure-duration-factor '(6 8))
;; ;; => 3


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
  "Return raw MSCX tempo value.

MuseScore stores tempo as quarter-notes per second in <tempo>."
  (mscx-child-number tempo-node :|tempo| nil))

(defun mscx-tempo-bpm (tempo-node)
  "Return BPM from MSCX Tempo node.

In MuseScore, <tempo>1</tempo> means quarter = 60."
  (let ((raw (mscx-tempo-raw tempo-node)))
    (when raw (* 60 raw))))

(defun mscx-measure-tempo-bpm (measure-node)
  "Return first tempo marking in measure as BPM, or NIL."
  (let* ((voice (mscx-voice-node measure-node))
         (tempo (and voice (find-if #'mscx-tempo-p (mscx-children voice)))))
    (and tempo (mscx-tempo-bpm tempo))))

;;; ------------------------------------------------------------
;;; Import state
;;; ------------------------------------------------------------

(defstruct mscx-import-state
  (division 480)
  (signature '(4 4))
  (measure-index 0)
  tempos)

(defun mscx-measure-basic-info (measure-node state)
  "Return basic measure info and update signature in STATE."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (tempo (mscx-measure-tempo-bpm measure-node)))
    (setf (mscx-import-state-signature state) signature)
    (list :measure-index (mscx-import-state-measure-index state)
          :signature signature
          :tempo tempo
          :items (mscx-measure-item-kinds measure-node))))

;; (let* ((score (mscx::mscx-score-node om::x))
;;        (state (mscx::make-mscx-import-state :division (mscx::mscx-score-division score)))
;;        (staff (first (mscx::mscx-staff-nodes score)))
;;        (measure (first (mscx::mscx-measure-nodes staff))))
;;   (mscx::mscx-measure-basic-info measure state))
;;   
;; -> (:measure-index 0 :signature (4 4) :tempo 60
;;  :items (:clef :timesig :tempo :chord :stafftext :chord :chord :chord :barline))


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
  (make-mscx-decoded-note
   :midic (mscx-note-midic note-node)
   :velocity (mscx-note-velocity note-node)
   :tpc (mscx-note-tpc note-node)))

(defun decode-mscx-chord (chord-node division signature)
  (make-mscx-decoded-event
   :type :chord
   :duration (mscx-duration-ticks chord-node division signature)
   :notes (loop for note in (mscx-children-named chord-node :|Note|)
                collect (decode-mscx-note note))
   :source chord-node))

(defun decode-mscx-rest (rest-node division signature)
  (make-mscx-decoded-event
   :type :rest
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

;; debug events

(defun mscx-decode-measure-events (measure-node state)
  "Decode Chord/Rest events in MEASURE-NODE, ignoring other voice items."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state)))
    (setf (mscx-import-state-signature state) signature)
    (loop for item in (mscx-voice-children measure-node)
          when (mscx-note-event-p item)
          collect (decode-mscx-event item division signature))))

(let* ((score (mscx::mscx-score-node om::x))
       (state (mscx::make-mscx-import-state :division (mscx::mscx-score-division score)))
       (staff (first (mscx::mscx-staff-nodes score)))
       (measure (first (mscx::mscx-measure-nodes staff))))
  (mapcar #'(lambda (ev)
              (list :type (mscx::mscx-decoded-event-type ev)
                    :duration (mscx::mscx-decoded-event-duration ev)
                    :midics (mapcar #'mscx::mscx-decoded-note-midic
                                    (mscx::mscx-decoded-event-notes ev))
                    :vels (mapcar #'mscx::mscx-decoded-note-velocity
                                  (mscx::mscx-decoded-event-notes ev))
                    :tpcs (mapcar #'mscx::mscx-decoded-note-tpc
                                  (mscx::mscx-decoded-event-notes ev))))
          (mscx::mscx-decode-measure-events measure state)))


;; ((:type :chord :duration 480 :midics (6000) :vels (100) :tpcs (14))
;;  (:type :chord :duration 480 :midics (6000) :vels (100) :tpcs (14))
;;  (:type :chord :duration 480 :midics (6000) :vels (100) :tpcs (14))
;;  (:type :chord :duration 480 :midics (6000) :vels (100) :tpcs (14)))


;;; ------------------------------------------------------------
;;; Measure decoding -> OM chords + rhythm tree
;;; ------------------------------------------------------------

(defstruct mscx-measure-data
  signature
  tree
  chords
  tempo
  division)

(defun mscx-decoded-event-pulse (decoded-event)
  "Return positive pulse for chords, negative pulse for rests."
  (let ((dur (mscx-decoded-event-duration decoded-event)))
    (if (eq (mscx-decoded-event-type decoded-event) :rest) (- dur) dur)))

(defun mscx-note-event->om-chord-or-nil (decoded-event)
  "Return OM chord for decoded chord events, NIL for rests."
  (when (eq (mscx-decoded-event-type decoded-event) :chord)
    (mscx-decoded-chord->om-chord decoded-event)))


;; defined in "projects/musicproject/import-export/import-mxml-new.lisp":
;; om::get-tree
;; om::make-tree-builder
;; om::add-new-pulse

(defun measure-from-mscx (measure-node state)
  "Decode one MSCX measure.

Returns an MSCX-MEASURE-DATA struct."
  (let* ((signature (mscx-measure-timesig measure-node (mscx-import-state-signature state)))
         (division (mscx-import-state-division state))
         (tempo (mscx-measure-tempo-bpm measure-node))
         (tree-builder (om::make-tree-builder))		    ;from import-mxml-new.lisp
         (chords nil))
    (setf (mscx-import-state-signature state) signature)
    ;; v0.0.1: Chord/Rest only. Tuplet/endTuplet are handled further down
    (loop for item in (mscx-voice-children measure-node) do
          (when (mscx-note-event-p item)
            (let* ((decoded (decode-mscx-event item division signature))
                   (pulse (mscx-decoded-event-pulse decoded))
                   (om-chord (mscx-note-event->om-chord-or-nil decoded)))
	      (om::add-new-pulse tree-builder pulse)	    ;from import-mxml-new.lisp
              (when om-chord (push om-chord chords)))))
    (make-mscx-measure-data :signature signature
                            :tree (om::get-tree tree-builder) ;from import-mxml-new.lisp
                            :chords (reverse chords)
                            :tempo tempo
                            :division division)))

;; debug measure data

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

(let* ((score (mscx::mscx-score-node om::x))
       (state (mscx::make-mscx-import-state :division (mscx::mscx-score-division score)))
       (staff (first (mscx::mscx-staff-nodes score)))
       (measure (first (mscx::mscx-measure-nodes staff))))
  (mscx::mscx-measure-debug-data measure state))


;;; ------------------------------------------------------------
;;; Staff -> OM voice
;;; ------------------------------------------------------------

(defun mscx-measure-data->tree-item (measure-data)
  "Return one OM measure tree item: ((N D) tree)."
  (list (mscx-measure-data-signature measure-data)
        (mscx-measure-data-tree measure-data)))

(defun mscx-measure-data-list->tree (measure-data-list)
  "Return full OM voice tree."
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

;; debugger
(defun mscx-staff-debug-data (staff-node state)
  "Return readable Staff import data."
  (let ((voice (staff-from-mscx staff-node state :name (or (mscx-staff-id staff-node) "MSCX Staff"))))
    (list :voice voice
          :tree (om::tree voice)
          :tempo (om::tempo voice)
          :n-chords (length (om::chords voice))
          :chord-midics (loop for ch in (om::chords voice) collect (om::lmidic ch)))))

;; (let* ((score (mscx::mscx-score-node om::x))
;;        (state (mscx::make-mscx-import-state :division (mscx::mscx-score-division score)))
;;        (staff (first (mscx::mscx-staff-nodes score))))
;;   (mscx::mscx-staff-debug-data staff state))

;; (let* ((score (mscx::mscx-score-node om::x))
;;        (state (mscx::make-mscx-import-state :division (mscx::mscx-score-division score)))
;;        (staff (first (mscx::mscx-staff-nodes score)))
;;        (data (loop for measure in (mscx::mscx-measure-nodes staff)
;;                    collect (mscx::measure-from-mscx measure state))))
;;   (mscx::mscx-measure-data-list->tree data))



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
                       collect (staff-from-mscx
                                staff
                                (make-mscx-import-state :division division)
                                :name (mscx-staff-name staff)))))
    (make-instance 'om::poly :voices voices)))

(defun read-mscx-list (xml-list)
  "Decode an om-list-from-xml-file MSCX list into OM poly."
  (let ((score (mscx-score-node xml-list)))
    (unless score
      (error "No Score node found in MSCX list."))
    (score-from-mscx score)))



;; OM interface

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

;; (setq p (import-mscx "/home/andersvi/prosjekter/OM/OM_MSCORE_EXPORT/mscores/articulations_text_extras.mscx"))

;; (length (inside p))
;; ;; => 1
;; (om::tree (first (inside p)))
;; ;; => typisk (1 (((4 4) (1 1 1 1))))

;; (length (om::chords (first (inside p))))
;; ;; => 4
