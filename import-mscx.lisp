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

