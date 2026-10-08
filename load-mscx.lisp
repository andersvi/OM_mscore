;;;========================
;;; OpenMusic export MSCX 
;;;========================
;;;
;;; Anders Vinjar - 2026
;;; 
;;; Time-stamp: <2026-10-08 12:56:45 andersvi>
;;
;; requires some existing code in OMs 'import-export/export-mxml.lisp'
;; 
;;;; Load after OpenMusic's standard MusicXML import/export helpers.
;;;; 
(let ((directory (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (name '("mscx-instrument-mappings.lisp"
		  "mscx-expression.lisp"
		  "export-mscx.lisp"
		  "import-mscx.lisp"
		  "muse-strings.lisp"))
    (load (merge-pathnames name directory))))
