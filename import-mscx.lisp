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
;; ((:staff-id "1" :measures ((:clef :timesig :tempo :chord :stafftext :chord :chord :chord :barline))))

