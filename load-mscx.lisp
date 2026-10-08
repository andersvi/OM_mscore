;;;; Load after OpenMusic's standard MusicXML import/export helpers.
;;;; This loader has no dependency on SQ or any generators.
(let ((directory (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (name '("mscx-expression.lisp" "export-mscx.lisp" "import-mscx.lisp"))
    (load (merge-pathnames name directory))))
